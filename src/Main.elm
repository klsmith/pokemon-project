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
import Pokemon exposing (PokemonList, PokemonSprite, pokemonListDecoder, pokemonSpriteDecoder)
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
      , page = LoadingPage
      }
    , Http.get
        { url = "https://pokeapi.co/api/v2/pokemon?offset=0&limit=20"
        , expect =
            Http.expectJson
                (asPageMsg AcceptListResponse)
                pokemonListDecoder
        }
    )


type alias Model =
    { theme : ColorTheme
    , page : Page
    }


type Page
    = LoadingPage
    | LoadingFailurePage Http.Error
    | ListPage PokemonList



-- UPDATE


type Msg
    = ChangeTheme ColorTheme
    | PageMsg PageMsg


type PageMsg
    = AcceptListResponse PokemonListResponse
    | RequestList String
    | AcceptImageResponse Int PokemonSpriteResponse


type alias PokemonListResponse =
    Result Http.Error PokemonList


type alias PokemonSpriteResponse =
    Result Http.Error PokemonSprite


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
        LoadingPage ->
            updateLoadingListPage pageMsg page

        LoadingFailurePage err ->
            ( page, Cmd.none )

        ListPage pokemonList ->
            updateViewListPage pageMsg page pokemonList


updateLoadingListPage : PageMsg -> Page -> ( Page, Cmd Msg )
updateLoadingListPage pageMsg page =
    case pageMsg of
        AcceptListResponse httpResult ->
            case httpResult of
                Ok newPokemonList ->
                    ( ListPage newPokemonList
                    , Cmd.batch (pokemonImageCommandList newPokemonList)
                    )

                Err error ->
                    ( LoadingFailurePage error, Cmd.none )

        RequestList _ ->
            ( page, Cmd.none )

        AcceptImageResponse _ _ ->
            ( page, Cmd.none )


pokemonImageCommandList : PokemonList -> List (Cmd Msg)
pokemonImageCommandList pokemonList =
    Array.toList (Array.indexedMap pokemonImageCommand pokemonList.results)


pokemonImageCommand : Int -> Pokemon.ListResult -> Cmd Msg
pokemonImageCommand index result =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/" ++ result.name
        , expect =
            Http.expectJson
                (asPageMsg (AcceptImageResponse index))
                pokemonSpriteDecoder
        }


updateViewListPage : PageMsg -> Page -> PokemonList -> ( Page, Cmd Msg )
updateViewListPage pageMsg page pokemonList =
    case pageMsg of
        RequestList url ->
            ( LoadingPage
            , Http.get
                { url = url
                , expect =
                    Http.expectJson
                        (asPageMsg AcceptListResponse)
                        pokemonListDecoder
                }
            )

        AcceptImageResponse index httpResult ->
            let
                results =
                    pokemonList.results

                listResult =
                    Array.get index results
            in
            case httpResult of
                Ok pokemonSprite ->
                    let
                        sprite =
                            pokemonSprite.sprites.frontDefault

                        newListResult =
                            case listResult of
                                Just lr ->
                                    Just { lr | sprite = Just sprite }

                                Nothing ->
                                    Nothing
                    in
                    case newListResult of
                        Just newLr ->
                            ( ListPage
                                { pokemonList
                                    | results = Array.set index newLr results
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( page, Cmd.none )

                Err err ->
                    let
                        newListResult =
                            case listResult of
                                Just lr ->
                                    Just { lr | sprite = Just "https://friconix.com/png/fi-htluxl-question-mark.png" }

                                Nothing ->
                                    Nothing
                    in
                    case newListResult of
                        Just newLr ->
                            ( ListPage
                                { pokemonList
                                    | results = Array.set index newLr results
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( page, Cmd.none )

        AcceptListResponse _ ->
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
                LoadingPage ->
                    text "Loading..."

                LoadingFailurePage error ->
                    debugView ( "FailLoadingList", error )

                ListPage pokemonList ->
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
        [ Border.width 1
        ]
        [ el
            [ Element.width (px 96)
            , Element.height (px 96)
            , Element.centerX
            ]
            (Element.image
                [ Element.centerX
                , Element.centerY
                , Element.width (fill |> Element.maximum 96)
                , Element.height (fill |> Element.maximum 96)
                ]
                { src =
                    Maybe.withDefault spinnerUrl result.sprite
                , description = result.name
                }
            )
        , el
            [ Font.center
            , Element.centerX
            , Element.padding 8
            ]
            (text result.name)
        ]


spinnerUrl : String
spinnerUrl =
    "https://i.pinimg.com/originals/0d/03/45/0d0345f662a757ef17eef497fa8e83fc.gif"


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
                (asPageMsg RequestList)
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
                (asPageMsg RequestList)
                pokemonList.next
        }


computeNextTheme : ColorTheme -> ColorTheme
computeNextTheme theme =
    if "Light Theme" == theme.name then
        ColorTheme.darkTheme

    else
        ColorTheme.lightTheme



-- UTILITY


asPageMsg : (a -> PageMsg) -> a -> Msg
asPageMsg constructor value =
    PageMsg (constructor value)
