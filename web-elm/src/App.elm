module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import Ethereum


--import Css exposing (Mixin, width, px)
--import Storage.Session as SessionStorage
--import Storage.Error as StorageError
--import QueryString exposing (parse, one, string)
--import Common exposing (..)


type alias Flags =
    { queryString : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type DialogType
    = Welcome
    | None


type Token
    = Ethereum


type alias Model =
    { errors : List String
    , token : Token
    , eth : Ethereum.Model
    }


setEth : Model -> Ethereum.Model -> Model
setEth model eth =
    { model | eth = eth }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { errors = []
    , token = Ethereum
    , eth = Ethereum.initModel
    }
        ! []



-- UPDATE


type Msg
    = NoOp
    | EthMsg Ethereum.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        EthMsg subMsg ->
            let
                ( um, scmd ) =
                    Ethereum.update subMsg model.eth
            in
                ( um |> setEth model, Cmd.map EthMsg scmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    (case model.token of
        Ethereum ->
            Sub.map EthMsg (Ethereum.subscriptions model.eth)
    )



-- HTTP
-- VIEW


view : Model -> Html Msg
view model =
    div [ class "animated fadeIn w-100" ]
        [ div [ class "mw7 center" ]
            [ currentView model
            ]
        ]


currentView : Model -> Html Msg
currentView model =
    (case model.token of
        Ethereum ->
            Html.map EthMsg <| Ethereum.view model.eth
    )
