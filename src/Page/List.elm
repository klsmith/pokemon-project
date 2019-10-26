module Page.List exposing (Model, Results, resultsDecoder, update, view, viewList)

import Array exposing (Array)
import ColorTheme exposing (ColorTheme)
import Element exposing (Element, column, px, text, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Http
import Json.Decode as Decode exposing (Decoder)
import Pokemon exposing (PokemonLight, pokemonLightDecoder)



-- MODEL


type Model
    = Loading
    | Fail Http.Error
    | Success Results


type alias Results =
    { count : Int
    , pokemon : Array PokemonLight
    }


resultsDecoder : Decoder Results
resultsDecoder =
    Decode.map2 Results
        (Decode.field "count" Decode.int)
        (Decode.field "results" (Decode.array pokemonLightDecoder))



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : ColorTheme -> Model -> Element Msg
view theme model =
    case model of
        Loading ->
            Element.none

        Fail err ->
            Element.none

        Success results ->
            viewList theme results


viewList : ColorTheme -> Results -> Element Msg
viewList theme results =
    column []
        [ wrappedRow [ Element.spacing 8 ]
            [ text "Count:"
            , text (String.fromInt results.count)
            ]
        , wrappedRow [ Element.spacing 8 ]
            (Array.toList (Array.map viewPokemonLight results.pokemon))
        ]


viewPokemonLight : PokemonLight -> Element Msg
viewPokemonLight pokemonLight =
    column
        [ Element.spacing 8
        , Font.center
        ]
        [ Element.image
            [ Border.width 1
            , Element.width (px 96)
            , Element.height (px 96)
            ]
            { src =
                Maybe.withDefault
                    "https://upload.wikimedia.org/wikipedia/en/1/13/Parabolic_dish_motion_circle.gif"
                    pokemonLight.sprite
            , description = pokemonLight.name
            }
        , text pokemonLight.name
        ]
