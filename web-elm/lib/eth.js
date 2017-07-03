

if (typeof web3 !== 'undefined') {
  web3 = new Web3(web3.currentProvider);
} else {
  // set the provider you want from Web3.providers
  web3 = new Web3(
    new Web3.providers.HttpProvider("https://mainnet.infura.io/mew", 5000));
}

const defaultHdPathString = "m/44'/60'/0'/0";

app.ports.validatePhrase.subscribe(function(seedPhrase) {
  var valid;

  try {
    valid = lightwallet.keystore.isSeedValid(seedPhrase);
  } catch(err) {
    valid = false;
  }
  app.ports.validatePhraseResult.send(valid);
});

var _ks = null;
var _pwDerivedKey = null;

app.ports.generateAccounts.subscribe(function(args) {
  var [password, seedPhrase, count] = args;

  var req = {
    password: password,
    hdPathString: defaultHdPathString
  };

  // If seedPhrase provided then validate and use it
  // If invalid then let the app know
  if (seedPhrase.length !== 0) {

    if (lightwallet.keystore.isSeedValid(seedPhrase) === false) {
      app.ports.validatePhraseResult.send(true);
      return;
    }
    req['seedPhrase'] = seedPhrase;
  }

  lightwallet.keystore.createVault(req, function (err, ks) {
    _ks = ks;

    // Some methods will require providing the `pwDerivedKey`,
    // Allowing you to only decrypt private keys on an as-needed basis.
    // You can generate that value with this convenient method:
    ks.keyFromPassword(req.password, function (err, pwDerivedKey) {
      if (err) throw err;

      _pwDerivedKey = pwDerivedKey;

      // generate five new address/private key pairs
      // the corresponding private keys are also encrypted
      ks.generateNewAddress(pwDerivedKey, count);

      var result = {
        seed_phrase: (seedPhrase.length !== 0 ) ? '' : ks.getSeed(pwDerivedKey),
        accounts: ks.getAddresses()
      }

      for (var i in result.accounts) {
        result.accounts[i] = add0x(result.accounts[i]);
        // For testing with real balance values
        //result.accounts[i] = "b794f5ea0ba39494ce839613fffba74279579268";
      }

      app.ports.generateAccountsResult.send(result);

      // return since this is a new wallet (no balance)
      if (seedPhrase.length === 0) {
        return;
      }

      // request balances for all the generated accounts
      getBalances(result.accounts);

      // Now set ks as transaction_signer in the hooked web3 provider
      // and you can start using web3 using the keys/addresses in ks!
    });
  });
});


app.ports.sendTransaction.subscribe(function(t) {
  if (_ks === null || _pwDerivedKey === null) {
    return;
  }

  let from  = add0x(t.from);
  let to    = add0x(t.to);
  let nonce = web3.toHex(web3.eth.getTransactionCount(from));

  var tx = {
    nonce     : nonce,
    gasPrice  : web3.toHex(web3.eth.gasPrice),
    gasLimit  : web3.toHex(t.gas),
    to        : to,
    from      : from,
    value     : web3.toHex(web3.toWei(t.value, "ether")),
    data      : undefined
  };
  var valueTx = lightwallet.txutils.valueTx(tx);

  var signedValueTx = add0x(
    lightwallet.signing.signTx(_ks, _pwDerivedKey, valueTx, from)
    .toString('hex'));

  web3.eth.sendRawTransaction(signedValueTx, function(err, r) {
    if (err) {
      console.error("sendRawTransaction", err);
      app.ports.failure.send(err.message);
      return;
    }
    app.ports.sendTransactionResult.send(r);
  });
});

app.ports.refreshBalances.subscribe(function(accounts) {
  getBalances(accounts);
});

function getBalances(accounts) {
  var batch = web3.createBatch();
  var total = new web3.BigNumber(0.0);

  for (var i in accounts) {
    var accId = accounts[i];

    var cb = (function(_accId) {
      return function(err, weiB) {
        if (err) {
          console.error("getBalances", err);
          return;
        }
        var ethB = web3.fromWei(weiB);
        total = total.plus(ethB);

        app.ports.accountInfo.send({
            account_id: add0x(_accId),
            value:      ethB.isZero() ? '0.0' : ethB.toFormat(5),
            total:      total.isZero() ? '0.0' : total.toFormat(5)
        });
      }
    })(accId);
    var req = web3.eth.getBalance.request(accId, 'latest', cb);
    batch.add(req);
  }

  batch.execute();
}

function add0x(input) {
  if (typeof(input) !== 'string') {
    return input;
  }
  else if (input.length < 2 || input.slice(0,2) !== '0x') {
    return '0x' + input;
  }
  else {
    return input;
  }
}