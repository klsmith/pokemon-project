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
import Pokemon exposing (PokemonList, pokemonListDecoder)
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
                (\result -> PageMsg (LoadList result))
                pokemonListDecoder
        }
    )


type alias Model =
    { theme : ColorTheme
    , page : Page
    }


type Page
    = LoadingList
    | FailLoadingList Http.Error
    | ViewList PokemonList



-- UPDATE


type Msg
    = ChangeTheme ColorTheme
    | PageMsg PageMsg


type PageMsg
    = LoadList (Result Http.Error PokemonList)
    | RequestLoad String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme newTheme ->
            ( { model | theme = newTheme }, Cmd.none )

        PageMsg pageMsg ->
            let
                ( newPage, cmd ) =
                    updatePageMsg pageMsg model.page
            in
            ( { model | page = newPage }, cmd )


updatePageMsg : PageMsg -> Page -> ( Page, Cmd Msg )
updatePageMsg pageMsg page =
    case page of
        LoadingList ->
            updateLoadingListPage pageMsg page

        FailLoadingList err ->
            ( page, Cmd.none )

        ViewList pokemonList ->
            updateViewListPage pageMsg page


updateLoadingListPage : PageMsg -> Page -> ( Page, Cmd Msg )
updateLoadingListPage pageMsg page =
    case pageMsg of
        LoadList httpResult ->
            case httpResult of
                Ok pokemonList ->
                    ( ViewList pokemonList, Cmd.none )

                Err error ->
                    ( FailLoadingList error, Cmd.none )

        _ ->
            ( page, Cmd.none )


updateViewListPage : PageMsg -> Page -> ( Page, Cmd Msg )
updateViewListPage pageMsg page =
    case pageMsg of
        RequestLoad url ->
            ( LoadingList
            , Http.get
                { url = url
                , expect =
                    Http.expectJson
                        (\result -> PageMsg (LoadList result))
                        pokemonListDecoder
                }
            )

        _ ->
            ( page, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout
        [ Background.color model.theme.background
        , Font.color model.theme.text
        ]
        (column [ Element.spacing 8 ]
            [ themeButton model.theme
            , case model.page of
                LoadingList ->
                    debugView "LoadingList"

                FailLoadingList error ->
                    debugView ( "FailLoadingList", error )

                ViewList pokemonList ->
                    viewList model.theme pokemonList
            ]
        )


viewList : ColorTheme -> PokemonList -> Element Msg
viewList theme pokemonList =
    column [ Element.spacing 8 ]
        [ wrappedRow [ Element.spacing 8 ]
            [ text "Count:"
            , text (String.fromInt pokemonList.count)
            , previousButton pokemonList
            , nextButton pokemonList
            ]
        , wrappedRow [ Element.spacing 8 ]
            (Array.toList (Array.map viewPokemonListResult pokemonList.results))
        ]


viewPokemonListResult : Pokemon.ListResult -> Element Msg
viewPokemonListResult result =
    column
        [ Element.spacing 8
        ]
        [ Element.image
            [ Border.width 1
            , Element.width (px 96)
            , Element.height (px 96)
            ]
            { src =
                Maybe.withDefault
                    "https://i.pinimg.com/originals/0d/03/45/0d0345f662a757ef17eef497fa8e83fc.gif"
                    result.sprite
            , description = result.name
            }
        , el
            [ Font.center
            , Element.centerX
            ]
            (text result.name)
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


previousButton : PokemonList -> Element Msg
previousButton pokemonList =
    Input.button
        [ Border.width 1
        , Element.padding 8
        ]
        { label = text "Previous"
        , onPress =
            Maybe.map
                (\url -> PageMsg (RequestLoad url))
                pokemonList.previous
        }


nextButton : PokemonList -> Element Msg
nextButton pokemonList =
    Input.button
        [ Border.width 1
        , Element.padding 8
        ]
        { label = text "Next"
        , onPress =
            Maybe.map
                (\url -> PageMsg (RequestLoad url))
                pokemonList.next
        }


computeNextTheme : ColorTheme -> ColorTheme
computeNextTheme theme =
    if "Light Theme" == theme.name then
        ColorTheme.darkTheme

    else
        ColorTheme.lightTheme
