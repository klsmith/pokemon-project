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
    ( LoadingList
        { theme = darkTheme
        , page =
            { pageNumber = 1
            , pageSize = 20
            }
        }
    , Http.get
        { url = "https://pokeapi.co/api/v2/pokemon?offset=0&limit=20"
        , expect = Http.expectString LoadList
        }
    )


type Model
    = LoadingList Common
    | FailLoadingList Common Http.Error
    | ViewList Common Results
    | LoadingSingle Common Results
    | FailLoadingSingle Common Results Http.Error
    | ViewSingle Common Results PokemonHeavy


type alias Common =
    { theme : ColorTheme
    , page : Page
    }


type alias Page =
    { pageNumber : Int
    , pageSize : Int
    }


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
    = LoadList (Result Http.Error String)
    | ChangeTheme ColorTheme


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadList httpResult ->
            ( updateViewList model httpResult, Cmd.none )

        ChangeTheme newTheme ->
            ( changeTheme newTheme model, Cmd.none )


updateViewList : Model -> Result Http.Error String -> Model
updateViewList model httpResult =
    let
        common =
            extractCommon model
    in
    case httpResult of
        Ok json ->
            ViewList common
                { count = 151
                , pokemon = []
                , raw = json
                }

        Err error ->
            FailLoadingList common error


changeTheme : ColorTheme -> Model -> Model
changeTheme newTheme model =
    case model of
        LoadingList common ->
            LoadingList { common | theme = newTheme }

        FailLoadingList common error ->
            FailLoadingList { common | theme = newTheme } error

        ViewList common results ->
            ViewList { common | theme = newTheme } results

        LoadingSingle common results ->
            LoadingSingle { common | theme = newTheme } results

        FailLoadingSingle common results error ->
            FailLoadingSingle { common | theme = newTheme } results error

        ViewSingle common results pokemon ->
            ViewSingle { common | theme = newTheme } results pokemon


extractCommon : Model -> Common
extractCommon model =
    case model of
        LoadingList common ->
            common

        FailLoadingList common _ ->
            common

        ViewList common _ ->
            common

        LoadingSingle common _ ->
            common

        FailLoadingSingle common _ _ ->
            common

        ViewSingle common _ _ ->
            common



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        theme =
            (extractCommon model).theme
    in
    layout
        [ Background.color theme.background
        , Font.color theme.text
        ]
        (case model of
            LoadingList common ->
                debugView "LoadingList"

            FailLoadingList common error ->
                debugView ( "FailLoadingList", error )

            ViewList common results ->
                debugView ( "ViewList", results )

            LoadingSingle common results ->
                debugView ( "LoadingSingle", results )

            FailLoadingSingle common results error ->
                debugView ( "FailLoadingSingle", results, error )

            ViewSingle common results pokemon ->
                debugView ( "ViewSingle", results, pokemon )
        )


debugView : a -> Element Msg
debugView thing =
    Element.paragraph [] [ text (Debug.toString thing) ]


themeButton theme =
    Input.button
        [ Border.width 1
        , Element.padding 8
        ]
        { label = text theme.name
        , onPress = Just (ChangeTheme theme)
        }



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
