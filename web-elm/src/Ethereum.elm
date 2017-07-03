port module Ethereum exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import List.Extra


--import Css exposing (Mixin, width, px)
--import Storage.Session as SessionStorage
--import Storage.Error as StorageError
--import QueryString exposing (parse, one, string)

import Common exposing (..)


-- DATA


type alias Transaction =
    { from : String
    , to : String
    , value : String
    , gas : Float
    , internal : Bool
    }


encodeTransaction : Transaction -> Encode.Value
encodeTransaction r =
    Encode.object
        [ ( "from", Encode.string r.from )
        , ( "to", Encode.string r.to )
        , ( "value", Encode.string r.value )
        , ( "gas", Encode.float r.gas )
        ]


type alias Account =
    { id : String
    , value : String
    , total : String
    }


accountDecoder : Decode.Decoder Account
accountDecoder =
    decode Account
        |> Pipeline.required "account_id" Decode.string
        |> Pipeline.required "value" Decode.string
        |> Pipeline.required "total" Decode.string


decodeAccount : Decode.Value -> Result String Account
decodeAccount =
    Decode.decodeValue accountDecoder


type alias GeneratedAccounts =
    { seedPhrase : String
    , accounts : List String
    }


generatedAccountsDecoder : Decode.Decoder GeneratedAccounts
generatedAccountsDecoder =
    decode GeneratedAccounts
        |> Pipeline.required "seed_phrase" Decode.string
        |> Pipeline.required "accounts" (Decode.list Decode.string)


decodeGeneratedAccounts : Decode.Value -> Result String GeneratedAccounts
decodeGeneratedAccounts =
    Decode.decodeValue generatedAccountsDecoder



-- MODEL


type ViewState
    = MainView
    | ExistingAccountsView
    | TransactionView
    | TransactionProcessingView
    | TransactionDoneView
    | SetecAstronomyView
    | AccountsListView
    | AccountDetailView Account


type alias Model =
    { password : String
    , seedPhrase : String
    , validPhrase : Bool
    , displaySeed : Bool
    , accounts : List Account
    , total : String
    , transaction : Transaction
    , viewState : ViewState
    , error : Maybe String
    }


initModel : Model
initModel =
    { password = ""
    , seedPhrase = ""
    , validPhrase = False
    , displaySeed = False
    , accounts = []
    , total = "0.0"
    , transaction =
        { from = ""
        , to = ""
        , value = ""
        , gas = 21000
        , internal = False
        }
    , viewState = MainView
    , error = Nothing
    }



{-
   initModel : Model
   initModel =
       { password = "1234567890"
       , validPhrase = True
       , seedPhrase = ""
       , displaySeed = False
       , accounts =
           [ { id = "e85b77beefbd13b8ce3bd3f43c15dab40feeca5f"
             , value = "40.0"
             , total = "50.0"
             }
           , { id = "73302fa4fe5b66e7eeb091f9e6dc883d7a4ee5ea"
             , value = "10.0"
             , total = "50.0"
             }
           ]
       , total = "50.0"
       , transaction =
           { from = ""
           , to = ""
           , value = ""
           , gas = "21000"
           , internal = False
           }
       , viewState = AccountsListView
       }
-}


setPassword : String -> Model -> Model
setPassword value model =
    { model | password = value }


setSeedPhrase : String -> Model -> Model
setSeedPhrase seedPhrase model =
    { model | seedPhrase = seedPhrase }


setValidPhrase : Bool -> Model -> Model
setValidPhrase validPhrase model =
    { model | validPhrase = validPhrase }


setDisplaySeed : Bool -> Model -> Model
setDisplaySeed displaySeed model =
    { model | displaySeed = displaySeed }


newAccount : String -> Account
newAccount id =
    { id = id, value = "0.0", total = "0.0" }


setAccounts : List Account -> Model -> Model
setAccounts accounts model =
    { model | accounts = accounts }


setAccount : Account -> List Account -> List Account
setAccount account accounts =
    List.Extra.replaceIf (\a -> a.id == account.id) account accounts


setTotal : String -> Model -> Model
setTotal total model =
    { model | total = total }


setViewState : ViewState -> Model -> Model
setViewState viewState model =
    { model | viewState = viewState }


isWalletLocked : Model -> Bool
isWalletLocked model =
    List.isEmpty model.accounts


setTransactionFrom : String -> Transaction -> Transaction
setTransactionFrom from transaction =
    { transaction | from = from }


setTransactionTo : String -> Transaction -> Transaction
setTransactionTo to transaction =
    { transaction | to = to }


setTransactionValue : String -> Transaction -> Transaction
setTransactionValue value transaction =
    { transaction | value = value }


setTransactionGas : Float -> Transaction -> Transaction
setTransactionGas gas transaction =
    { transaction | gas = gas }


setTransactionInternal : Bool -> Transaction -> Transaction
setTransactionInternal internal transaction =
    { transaction | internal = internal }


setTransaction : Transaction -> Model -> Model
setTransaction transaction model =
    { model | transaction = transaction }


setError : Maybe String -> Model -> Model
setError error model =
    { model | error = error }



-- PORT


port generateAccounts : ( String, String, Int ) -> Cmd msg


port generateAccountsResult : (Decode.Value -> msg) -> Sub msg


port refreshBalances : List String -> Cmd msg


port accountInfo : (Decode.Value -> msg) -> Sub msg


port validatePhrase : String -> Cmd msg


port validatePhraseResult : (Bool -> msg) -> Sub msg


port sendTransaction : Transaction -> Cmd msg


port sendTransactionResult : (String -> msg) -> Sub msg


port failure : (String -> msg) -> Sub msg



-- UPDATE


type Msg
    = SetViewState ViewState
    | SetPassword String
    | SetSeedPhrase String
    | SetDisplaySeed Bool
    | SetTransactionFrom String
    | SetTransactionTo String
    | SetTransactionValue String
    | SetTransactionGas String
    | SetTransactionInternal Bool
    | GenerateAccounts String
    | GenerateAccountsResult (Result String GeneratedAccounts)
    | ValidatePhraseResult Bool
    | AccountInfo (Result String Account)
    | TransactionFrom String
    | SendTransaction
    | SendTransactionResult String
    | GoHome
    | LockWallet
    | RefreshWallet
    | Failure String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetViewState viewState ->
            setViewState viewState model ! []

        SetPassword value ->
            setPassword value model ! []

        SetSeedPhrase value ->
            setSeedPhrase value model ! [ validatePhrase value ]

        SetDisplaySeed value ->
            setDisplaySeed value model ! []

        GenerateAccounts seedPhrase ->
            let
                sp =
                    String.trim seedPhrase
            in
                model
                    |> setViewState SetecAstronomyView
                    |> setAccounts []
                    |> setPassword ""
                    |> setDisplaySeed (String.isEmpty sp)
                    |> setValidPhrase False
                    |> flip (!) [ generateAccounts ( model.password, sp, 5 ) ]

        GenerateAccountsResult result ->
            case result of
                Result.Ok value ->
                    model
                        |> setAccounts (List.map newAccount value.accounts)
                        |> setSeedPhrase value.seedPhrase
                        |> setViewState AccountsListView
                        |> flip (!) []

                Result.Err err ->
                    model ! []

        ValidatePhraseResult valid ->
            setValidPhrase valid model ! []

        AccountInfo result ->
            case result of
                Result.Ok value ->
                    let
                        _ =
                            Debug.log ">" value
                    in
                        setAccount value model.accounts
                            |> flip setAccounts model
                            |> setTotal value.total
                            |> flip (!) []

                Result.Err err ->
                    let
                        _ =
                            Debug.log "Error parsing AccountInfo" err
                    in
                        model ! []

        SetTransactionFrom value ->
            setTransactionFrom value model.transaction
                |> flip setTransaction model
                |> flip (!) []

        SetTransactionTo value ->
            setTransactionTo value model.transaction
                |> flip setTransaction model
                |> flip (!) []

        SetTransactionValue value ->
            setTransactionValue value model.transaction
                |> flip setTransaction model
                |> flip (!) []

        SetTransactionGas value ->
            let
                gas =
                    String.toFloat value |> Result.withDefault 21000
            in
                setTransactionGas gas model.transaction
                    |> flip setTransaction model
                    |> flip (!) []

        SetTransactionInternal value ->
            setTransactionInternal value model.transaction
                |> flip setTransaction model
                |> flip (!) []

        TransactionFrom accountID ->
            model.transaction
                |> setTransactionFrom accountID
                |> flip setTransaction model
                |> setViewState TransactionView
                |> flip (!) []

        SendTransaction ->
            model
                |> setViewState TransactionProcessingView
                |> flip (!) [ sendTransaction model.transaction ]

        SendTransactionResult txID ->
            model
                |> setViewState TransactionDoneView
                |> flip (!) []

        GoHome ->
            setViewState MainView model ! []

        LockWallet ->
            model
                |> setAccounts []
                |> setSeedPhrase ""
                |> setViewState MainView
                |> flip (!) []

        RefreshWallet ->
            model
                ! [ List.map .id model.accounts
                        |> refreshBalances
                  ]

        Failure error ->
            setError (Just error) model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ validatePhraseResult ValidatePhraseResult
        , generateAccountsResult (decodeGeneratedAccounts >> GenerateAccountsResult)
        , accountInfo (decodeAccount >> AccountInfo)
        , sendTransactionResult SendTransactionResult
        , failure Failure
        ]



-- HTTP
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navView model
        , viewRouting model
        ]


viewRouting : Model -> Html Msg
viewRouting model =
    case model.viewState of
        MainView ->
            mainView model

        ExistingAccountsView ->
            existingAccountsView model

        TransactionView ->
            transactionView model

        TransactionProcessingView ->
            activityView
                "Sending Transfer"
                "Attempting to send your tranfer to the Ethereum blockchain"
                TransactionView
                model

        TransactionDoneView ->
            transactionDoneView model

        SetecAstronomyView ->
            activityView
                "Computing Ethereum Accounts"
                "Account creation happens securely within your browser no data is sent over the internet."
                MainView
                model

        AccountsListView ->
            accountsListView model

        AccountDetailView account ->
            accountDetailView account model


mainView : Model -> Html Msg
mainView model =
    div []
        [ div [ class "bg-light-gray pa4 br2-ns ba b--black-10 mb3" ]
            [ if isWalletLocked model then
                span []
                    [ button
                        [ class "f3 fw2 dim pa3 mb3 bn db w-100 white bg-gold tracked"
                        , onClick (GenerateAccounts "")
                        ]
                        [ text "NEW WALLET"
                        ]
                    , button
                        [ class "f3 fw2 dim pa3 mb3 bn db w-100 white bg-green tracked"
                        , onClick (SetViewState ExistingAccountsView)
                        ]
                        [ text "USE EXISTING"
                        ]
                    ]
              else
                span []
                    [ button
                        [ class "f3 fw2 dim pa3 mb3 bn db w-100 white bg-green tracked"
                        , onClick (SetViewState AccountsListView)
                        ]
                        [ text "OPEN WALLET"
                        ]
                    , button
                        [ class "f3 fw2 dim pa3 mb3 ba br1 db w-100 tracked"
                        , onClick LockWallet
                        ]
                        [ text "CLOSE WALLET"
                        ]
                    ]
            ]
        , div [ class "tc w-100 ph3" ]
            [ p [ class "f3 fw2 lh-copy" ]
                [ text "Simple, Secure & Free"
                ]
            , p [ class "f4 fw2 lh-copy" ]
                [ text "Your keys stay safe on your device. This is a serverless wallet. Beta release."
                ]
            ]
        , footer [ class "ph3 ph4-ns pv6 bt b--black-10 black-70" ]
            [ div [ class "mt5 tc" ]
                [ a
                    [ href "/readme"
                    , title "Readme Disclaimer"
                    , class "f6 dib pr2 mid-gray dim"
                    ]
                    [ text "Terms of Use / Disclaimer"
                    ]
                , p [ class "dib mh2" ] []
                , a
                    [ href "https://twitter.com/dosco"
                    , target "_blank"
                    , title "Make by Vik"
                    , class "f6 dib pr2 mid-gray dim"
                    ]
                    [ text "Make by Vik" ]
                ]
            ]
        ]


activityView : String -> String -> ViewState -> Model -> Html Msg
activityView title description vs model =
    div [ class "mw7 center pa4 br2-ns ba b--black-10" ]
        [ div [ class "cf" ]
            [ span [ class "w-100 w-40-ns fl-ns center" ]
                [ case model.error of
                    Just err ->
                        h1 [ class "bg-near-white red f2 fw5 pa2" ]
                            [ text err ]

                    Nothing ->
                        img [ src "/app/loading.svg", class "w-100" ] []
                ]
            , span [ class "w-100 w-50-ns fr-ns" ]
                [ h1 [ class "f3" ]
                    [ text <| title ++ "..."
                    ]
                , small [ class "f5 black-60 db" ]
                    [ text description ]
                ]
            , if not (model.error == Nothing) then
                button
                    [ class "f4 fw2 dim pa2 bn bg-dark-gray near-white br1 db w-100 mv3 tracked"
                    , onClick (SetViewState vs)
                    ]
                    [ text "GO BACK" ]
              else
                text ""
            ]
        ]


seedPhraseView : Model -> Html Msg
seedPhraseView model =
    let
        jsCopySnippet =
            "javascript:document.getElementById(\"seed-phrase-text\").select();document.execCommand('copy');"
    in
        div [ class "bg-yellow near-black pa4" ]
            [ h2 [ class "mb1" ] [ text "Important Secret Phrase" ]
            , small [ class "f5 db mb4" ]
                [ text "This phrase listed below is your Ethereum wallet. Keep it in a safe place!"
                ]
            , textarea
                [ class "f4 f2-ns b db pa2 ba pa3 mb3 white w-100 bn bg-near-black"
                , rows 4
                , id "seed-phrase-text"
                , readonly True
                ]
                [ text model.seedPhrase
                ]
            , a
                [ class "link f6 b fw7 f5-l button-reset pa3 tc mt2 dn db-ns bn bg-animate bg-light-yellow black pointer br2-ns br--right-ns"
                , href jsCopySnippet
                ]
                [ text "Copy Secret Phrase"
                ]
            , small [ class "f4 ul db mt4 mb4" ]
                [ i [ class "fa fa-bolt mr2" ] []
                , text "Loose this phrase listed above and you loose everything!"
                ]
            , div [ class "db tc w-100 mt4" ]
                [ a
                    [ class "f5 db mb2 underline tracking"
                    , onClick (SetDisplaySeed False)
                    ]
                    [ text "CLOSE"
                    ]
                ]
            ]


accountsListHeaderView : Model -> Html Msg
accountsListHeaderView model =
    div [ class "bg-green near-white pa4 tc" ]
        [ span [ class "f3 fw6 dib pv2 mr2" ]
            [ text "ETH " ]
        , span
            [ class "f4 f3-ns fw2 dib pv2" ]
            [ text model.total ]
        , button
            [ class "f4 fw2 dim pa2 mt2 bn bg-light-green dark-green br1 db w-100 tracked"
            , onClick (SetViewState TransactionView)
            ]
            [ text "TRANSFER"
            ]
        ]


accountItemView : Int -> Account -> Html Msg
accountItemView id account =
    a
        [ class "dt w-100 bb b--black-05 pb3 mt3"
        , onClick <| SetViewState (AccountDetailView account)
        ]
        [ div [ class "dtc w2 w3-ns v-mid" ]
            [ h1 [ class "f3 f2-ns fw3 lh-title gray mv1" ]
                [ text <| toString (id + 1) ++ "."
                ]
            ]
        , div [ class "dtc v-mid pl1" ]
            [ h1 [ class "f7 f4-ns fw5 lh-title black mv1" ] [ text account.id ]
            , h2 [ class "f6 f4-ns fw4 mt0 mb0 blue" ]
                [ text <| "ETH " ++ account.value
                ]
            ]
        ]


accountsListView : Model -> Html Msg
accountsListView model =
    div []
        [ if model.displaySeed then
            seedPhraseView model
          else
            accountsListHeaderView model
        , div [ class "near-black pa3" ]
            [ h2 [ class "mb2 p0 bb b--black-03" ]
                [ text "Accounts" ]
            , div
                [ class "mt0 p0 pl0" ]
                (List.indexedMap accountItemView model.accounts)
            ]
        ]


accountDetailHeaderView : Account -> Model -> Html Msg
accountDetailHeaderView account model =
    div [ class "bg-green near-white pv4 tc" ]
        [ div [ class "w-100" ]
            [ span [ class "f3 fw6 dib pv2 mr2" ]
                [ text "ETH " ]
            , span
                [ class "f4 f3-ns fw2 dib pv2" ]
                [ text account.value ]
            ]
        , div [ class "w-100" ]
            [ h1 [ class "f7 f4-ns fw6 lh-title light-green ttu p0" ]
                [ text account.id
                ]
            ]
        , div [ class "w-100" ]
            [ a
                [ class "f4 fw2 dim pa2 mt2 bn bg-light-green dark-green br1 dib w-33 tracked mr1"
                , onClick (SetViewState AccountsListView)
                ]
                [ text "BACK"
                ]
            , a
                [ class "no-underline dn-ns f4 fw2 dim pa2 mt2 bn bg-light-green dark-green br1 dib w-33 tracked"
                , href <| "sms:;?&body=Send%20ETH%20to%20account%20" ++ account.id
                ]
                [ text "SHARE" ]
            ]
        ]


accountDetailView : Account -> Model -> Html Msg
accountDetailView account model =
    div []
        [ accountDetailHeaderView account model
        , div [ class "w-90 center" ]
            [ a
                [ href <| "https://etherscan.io/address/" ++ account.id
                , target "_blank"
                , class
                    "no-underline f4 fw2 dim pa3 mt3 ba b--dark-gray dark-gray br1 db tc w-100 tracked"
                ]
                [ text "VIEW TRANSFERS"
                ]
            , a
                [ href <| "https://etherscan.io/txsPending?a=" ++ account.id
                , target "_blank"
                , class
                    "no-underline f4 fw2 dim pa3 mt3 ba b--dark-gray dark-gray br1 db tc w-100 tracked"
                ]
                [ text "PENDING TRANSFERS"
                ]
            , a
                [ href "#"
                , onClick <| TransactionFrom account.id
                , class
                    "no-underline f4 fw2 dim pa3 mt3 ba b--dark-gray bg-white dark-gray br1 db tc w-100 tracked"
                ]
                [ text "TRANSFER"
                ]
            ]
        ]


transactionHeaderView : Model -> Html Msg
transactionHeaderView model =
    div [ class "bg-green near-white pa4 tc" ]
        [ span [ class "f3 fw6 dib pv2 mr2" ]
            [ text "Transfer ETH" ]
        , button
            [ class "f4 fw2 dim pa2 mt2 bn bg-light-green dark-green br1 db w-50 center tracked"
            , onClick (SetViewState AccountsListView)
            ]
            [ text "BACK"
            ]
        ]


transactionItemView : Int -> Account -> Html Msg
transactionItemView id account =
    div [ class "dt w-100 bb b--black-05 pb3 mt3" ]
        [ div [ class "dtc w2 w3-ns v-mid" ]
            [ h1 [ class "f3 f2-ns fw3 lh-title gray mv1" ]
                [ text <| toString (id + 1) ++ "."
                ]
            ]
        , div [ class "dtc v-mid pl1" ]
            [ h1 [ class "f7 f4-ns fw5 lh-title black mv1" ] [ text account.id ]
            , h2 [ class "f6 f4-ns fw4 mt0 mb0 blue" ]
                [ text <| "ETH " ++ account.value
                ]
            ]
        ]


transactionView : Model -> Html Msg
transactionView model =
    div []
        [ transactionHeaderView model
        , div [ class "near-black pa3" ]
            [ div
                [ class "center" ]
                [ transactionFormView model ]
            ]
        ]


transactionFormView : Model -> Html Msg
transactionFormView model =
    div []
        [ label
            [ class "f5 f4-ns", for "from-acc" ]
            [ text "From Account:" ]
        , select
            [ class "f6 f5-l input-reset ba b--black-80 pa3 w-100 mv3"
            , style [ ( "font-size", "16px" ) ]
            , id "from-acc"
            , onInput SetTransactionFrom
            ]
            ((option [] [ text "Select an account" ])
                :: (List.map (accountOptionView model.transaction.from) model.accounts)
            )
        , if model.transaction.internal then
            div []
                [ label
                    [ class "f5 f4-ns black-80", for "to-acc" ]
                    [ text "To Account:"
                    , a [ class "fr underline", onClick (SetTransactionInternal False) ]
                        [ text "Custom"
                        ]
                    ]
                , select
                    [ class "f6 f5-l input-reset ba b--black-80 pa3 w-100 mv3"
                    , style [ ( "font-size", "16px" ) ]
                    , id "to-acc"
                    , onInput SetTransactionTo
                    ]
                    ((option [] [ text "Select an account" ])
                        :: (List.map (accountOptionView model.transaction.to) model.accounts)
                    )
                ]
          else
            div []
                [ label
                    [ class "f5 f4-ns black-80 ", for "to-acc" ]
                    [ text "To Account:"
                    , a [ class "fr underline", onClick (SetTransactionInternal True) ]
                        [ text "Own Accounts"
                        ]
                    ]
                , input
                    [ class "f5 f4-l input-reset ba b--black-80 pa3 w-100 mv3"
                    , type_ "text"
                    , style [ ( "font-size", "16px" ) ]
                    , id "to-acc"
                    , onInput SetTransactionTo
                    , value model.transaction.to
                    ]
                    []
                ]
        , label
            [ class "f5 f4-ns black-80", for "ammount" ]
            [ text "Transfer Amount in ETH:" ]
        , input
            [ class "f5 f4-l input-reset ba b--black-80 pa3 w-100 mv3"
            , type_ "text"
            , style [ ( "font-size", "16px" ) ]
            , id "ammount"
            , onInput SetTransactionValue
            , value model.transaction.value
            ]
            []
        , label
            [ class "f5 f4-ns black-80", for "gas-limit" ]
            [ text "Transaction Fee (Gas):" ]
        , input
            [ class "f5 f4-l input-reset pv3 w-100 mt3"
            , type_ "range"
            , Html.Attributes.min "1000"
            , Html.Attributes.max "60000"
            , step "1000"
            , style [ ( "font-size", "16px" ) ]
            , id "gas-limit"
            , onInput SetTransactionGas
            , value (toString model.transaction.gas)
            ]
            []
        , small [ class "f7 mb3" ]
            [ text <|
                (toString model.transaction.gas)
                    ++ " Gas = "
                    ++ (toString (model.transaction.gas * 0.00000002))
                    ++ " ETH approximately"
            ]
        , button
            [ class "f4 fw2 dim pa2 bn bg-dark-gray near-white br1 db w-100 mv3 tracked"
            , onClick SendTransaction
            ]
            [ text "SEND" ]
        ]


transactionDoneView : Model -> Html Msg
transactionDoneView model =
    div [ class "mw7 center pa4 br2-ns ba b--black-10" ]
        [ div [ class "center pa3 bg-near-white w-100" ]
            [ div [ class "w-100 tc" ]
                [ i [ class "fa fa-grav mt5 mr1 f1" ] []
                ]
            , div [ class "w-100 tc" ]
                [ h1 [ class "green f2 fw5 mb1" ]
                    [ text "Transfer Sent" ]
                , p [ class "near-black f3 fw3 mt1" ]
                    [ text <|
                        "A transaction to send ETH "
                            ++ model.transaction.value
                            ++ " was added to Ethereum."
                    ]
                ]
            ]
        , button
            [ class "f4 fw2 dim pa2 bn bg-dark-gray near-white br1 db w-100 mv3 tracked"
            , onClick (SetViewState AccountsListView)
            ]
            [ text "DONE" ]
        ]


accountOptionView : String -> Account -> Html Msg
accountOptionView selectedID acc =
    option
        [ value acc.id
        , selected (acc.id == selectedID)
        ]
        [ text <| acc.id ++ " (ETH " ++ acc.value ++ ")" ]


existingAccountsView : Model -> Html Msg
existingAccountsView model =
    let
        invalidPhrase =
            String.length model.seedPhrase > 0 && not model.validPhrase
    in
        div [ class "bg-light-red pa4 br2-ns ba b--black-10" ]
            [ h2 [ class "mb1" ] [ text "Unlock Ethereum Wallet" ]
            , small [ class "f5 db mb4" ]
                [ text "Get to access your existing wallet"
                ]
            , fieldset [ class "bn ma0 pa0" ]
                [ label
                    [ class "f5 f4-ns black-80", for "seed-phrase" ]
                    [ text "Your Secret Seed Phrase" ]
                , textarea
                    [ class "f6 f5-l input-reset bn black-80 bg-white pa3 lh-solid w-100 br2-ns br--left-ns mv3"
                    , style [ ( "font-size", "16px" ) ]
                    , rows 4
                    , id "seed-phrase"
                    , placeholder "Enter your Seed Phrase"
                    , onInput SetSeedPhrase
                    ]
                    [ text "" ]
                , if invalidPhrase then
                    small [ class "f5 db mb3 white" ]
                        [ text "You've entered an invalid secret phrase, please correct it."
                        ]
                  else
                    text ""
                , button
                    [ class "f6 f5-l button-reset pv3 tc bn bg-animate bg-black-70 hover-bg-black white pointer w-100 w-25-m w-20-l br2-ns br--right-ns"
                    , disabled (not model.validPhrase)
                    , onClick (GenerateAccounts model.seedPhrase)
                    ]
                    [ text "OPEN WALLET" ]
                ]
            ]


navView : Model -> Html Msg
navView model =
    let
        lockIcon =
            if isWalletLocked model then
                "fa-lock"
            else
                "fa-unlock"
    in
        nav [ class "pa2" ]
            [ div [ class "w-100 relative" ]
                [ span [ class "absolute top-0 left-1" ]
                    [ i
                        [ class ("f3 fa ml2 " ++ lockIcon)
                        , onClick LockWallet
                        ]
                        []
                    ]
                , h3
                    [ class "mln2 dim black fw2 tc"
                    , onClick GoHome
                    ]
                    [ text "Simple Ethereum Wallet"
                    ]
                , if not <| isWalletLocked model then
                    span [ class "absolute top-0 right-1" ]
                        [ i
                            [ class ("f3 fa fa-refresh ml2 ")
                            , onClick RefreshWallet
                            ]
                            []
                        ]
                  else
                    text ""
                ]
            ]
