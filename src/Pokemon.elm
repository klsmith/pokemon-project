module Pokemon exposing (..)

import Json.Decode as Decode exposing (Decoder)


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


pokemonLightDecoder : Decoder PokemonLight
pokemonLightDecoder =
    Decode.map3 PokemonLight
        (Decode.field "name" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.maybe (Decode.field "sprite" Decode.string))
