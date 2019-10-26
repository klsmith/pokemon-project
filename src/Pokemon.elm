module Pokemon exposing (..)

import Array exposing (Array)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)



-- PokemonList


type alias PokemonList =
    { count : Int
    , previous : Maybe String
    , next : Maybe String
    , results : Array ListResult
    }


pokemonListDecoder : Decoder PokemonList
pokemonListDecoder =
    succeed PokemonList
        |> andMap (field "count" int)
        |> andMap (maybe (field "previous" string))
        |> andMap (maybe (field "next" string))
        |> andMap (field "results" (array listResultDecoder))



-- ListResult


type alias ListResult =
    { name : String
    , url : String
    , sprite : Maybe String
    }


listResultDecoder : Decoder ListResult
listResultDecoder =
    succeed ListResult
        |> andMap (field "name" string)
        |> andMap (field "url" string)
        |> andMap (maybe (field "sprite" string))



-- Details


type alias Details =
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
