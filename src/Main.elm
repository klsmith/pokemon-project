module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import ColorTheme exposing (ColorTheme, darkTheme, lightTheme)
import Element exposing (Attribute, Color, Element, FocusStyle, alignLeft, alignRight, centerX, centerY, column, el, fill, focusStyle, layout, rgb, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Http
import Json.Decode as Decode



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( { theme = darkTheme
      , state = LoadingList
      }
    , Http.get
        { url = "https://pokeapi.co/api/v2/pokemon?offset=0&limit=20"
        , expect = Http.expectString (\result -> StateMsg (LoadList result))
        }
    )


type alias Model =
    { theme : ColorTheme
    , state : State
    }


type State
    = LoadingList
    | FailLoadingList Http.Error
    | ViewList Results
    | LoadingSingle Results
    | FailLoadingSingle Results Http.Error
    | ViewSingle Results PokemonHeavy


type alias Results =
    { count : Int
    , pokemon : List PokemonLight
    , raw : String
    }


type alias PokemonLight =
    { name : String
    , url : String
    , sprite : Maybe String
    }


type alias PokemonHeavy =
    { id : Int
    , name : String
    , sprite : String
    , types : TypeField
    , height : Int
    , weight : Int
    }


type TypeField
    = SingleTypeField String
    | DoubleTypeField String String



-- UPDATE


type Msg
    = ChangeTheme ColorTheme
    | StateMsg StateMsg


type StateMsg
    = LoadList (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme newTheme ->
            ( { model | theme = newTheme }, Cmd.none )

        StateMsg stateMsg ->
            case stateMsg of
                LoadList httpResult ->
                    ( updateViewList model httpResult, Cmd.none )


updateViewList : Model -> Result Http.Error String -> Model
updateViewList model httpResult =
    case httpResult of
        Ok json ->
            { model
                | state =
                    ViewList
                        { count = 151
                        , pokemon = []
                        , raw = json
                        }
            }

        Err error ->
            { model | state = FailLoadingList error }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        theme =
            model.theme
    in
    layout
        [ Background.color theme.background
        , Font.color theme.text
        ]
        (column []
            [ themeButton theme
            , case model.state of
                LoadingList ->
                    debugView "LoadingList"

                FailLoadingList error ->
                    debugView ( "FailLoadingList", error )

                ViewList results ->
                    viewList theme results

                LoadingSingle results ->
                    debugView ( "LoadingSingle", results )

                FailLoadingSingle results error ->
                    debugView ( "FailLoadingSingle", results, error )

                ViewSingle results pokemon ->
                    debugView ( "ViewSingle", results, pokemon )
            ]
        )


debugView : a -> Element Msg
debugView thing =
    Element.paragraph [] [ text (Debug.toString thing) ]


viewList : ColorTheme -> Results -> Element Msg
viewList theme results =
    column []
        [ debugView results
        ]


themeButton : ColorTheme -> Element Msg
themeButton theme =
    let
        nextTheme =
            computeNextTheme theme
    in
    Input.button
        [ Border.width 1
        , Element.padding 8
        ]
        { label = text nextTheme.name
        , onPress = Just (ChangeTheme nextTheme)
        }


computeNextTheme : ColorTheme -> ColorTheme
computeNextTheme theme =
    if "Light Theme" == theme.name then
        ColorTheme.darkTheme

    else
        ColorTheme.lightTheme



-- External Code


{-| -}
onEnter : Msg -> Element.Attribute Msg
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
