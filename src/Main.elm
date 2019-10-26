module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import ColorTheme exposing (ColorTheme, darkTheme, lightTheme)
import Element exposing (Element, column, el, fill, layout, px, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Page.List exposing (Results, resultsDecoder, viewList)
import Pokemon exposing (PokemonHeavy, PokemonLight, pokemonLightDecoder)
import ViewUtil exposing (debugView)



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
      , page = LoadingList
      }
    , Http.get
        { url = "https://pokeapi.co/api/v2/pokemon?offset=0&limit=20"
        , expect =
            Http.expectJson
                (\result -> StateMsg (LoadList result))
                resultsDecoder
        }
    )


type alias Model =
    { theme : ColorTheme
    , page : Page
    }


type Page
    = LoadingList
    | FailLoadingList Http.Error
    | ViewList Results
    | LoadingSingle Results
    | FailLoadingSingle Results Http.Error
    | ViewSingle Results PokemonHeavy



-- UPDATE


type Msg
    = ChangeTheme ColorTheme
    | StateMsg StateMsg


type StateMsg
    = LoadList (Result Http.Error Results)
    | LoadImage Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme newTheme ->
            ( { model | theme = newTheme }, Cmd.none )

        StateMsg stateMsg ->
            case stateMsg of
                LoadList httpResult ->
                    ( updateLoadList model httpResult, Cmd.none )

                LoadImage index url ->
                    ( updateLoadImage index url model, Cmd.none )


updateLoadList : Model -> Result Http.Error Results -> Model
updateLoadList model httpResult =
    case httpResult of
        Ok results ->
            { model | page = ViewList results }

        Err error ->
            { model | page = FailLoadingList error }


updateLoadImage : Int -> String -> Model -> Model
updateLoadImage index url model =
    model



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
            , case model.page of
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
