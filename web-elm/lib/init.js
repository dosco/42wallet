var node = document.getElementById('my-app');
var app = Elm.App.embed(node, { queryString: window.location.search });

/*
function getRandomString(len) {
  var crypto = window.crypto || window.msCrypto; // for IE 11

  var array = new Uint8Array(len);
  crypto.getRandomValues(array);

  var result = "";
  for (var i = 0; i < array.length; i++) {
    result += String.fromCharCode(array[i]);
  }

  return result;
};
*/

