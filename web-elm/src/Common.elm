module Common exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


empty : Html msg
empty =
    text ""


onClickAjax : msg -> Attribute msg
onClickAjax message =
    onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed message)


