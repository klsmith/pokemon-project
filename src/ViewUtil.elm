module ViewUtil exposing (..)

import Element exposing (Attribute, Element, text)
import Html.Events
import Json.Decode as Decode exposing (Decoder)


debugView : a -> Element msg
debugView thing =
    Element.paragraph [] [ text (Debug.toString thing) ]


{-| -}
onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
