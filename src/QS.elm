module QS
    exposing
        ( Query
        , OneOrMany(..)
        , Primitive(..)
        , parse
        , serialize
        , Config
        , config
        , parseBooleans
        , parseNumbers
        , encodeBrackets
        , decoder
        , encode
        , empty
        , get
        , getAsStringList
        , set
        , setOne
        , setList
        , setBool
        , setNum
        , setStr
        , setListBool
        , setListNum
        , setListStr
        , push
        , pushBool
        , pushNum
        , pushStr
        , merge
        , remove
        )

{-| Parse an manipulate query strings

# Types

@docs Query, OneOrMany, Primitive

# Parse and Serialize

@docs parse, serialize

# Config

@docs Config, config, parseBooleans, parseNumbers, encodeBrackets

# Decode and Encode

@docs decoder, encode

# Transform

@docs empty
@docs get, getAsStringList
@docs set, setOne, setList, setStr, setBool, setNum, setListStr, setListBool, setListNum
@docs push, pushBool, pushNum, pushStr
@docs merge, remove
-}

import Dict
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Regex


-- QUERY


{-|
A parsed query string

    "?a=x&b[]=1&b=2"

    ==

    Dict.fromList
        [ ( "a", One <| Str "x" )
        , ( "b", Many [ Number 1, Number 2 ] )
        ]
-}
type alias Query =
    Dict.Dict String OneOrMany


{-|
A query value can be a unique value (One) e.g.

    a=1

    ==

    One (Number 1)

Or it can be a list (Many) e.g.

    a[]=1&a[]=2

    ==

    Many [ Number 1, Number 2 ]
-}
type OneOrMany
    = One Primitive
    | Many (List Primitive)


{-|
Type for storing query values

    "a=x" == One <| Str "x"

    "a=1" == One <| Number 1

    "a=true" == One <| Boolean True

    "a[]=x&a[]=1&a[]=true" == Many [ Str "x", Number 1, Boolean True ]
-}
type Primitive
    = Boolean Bool
    | Number Float
    | Str String


{-| @priv
-}
primitiveToString : Primitive -> String
primitiveToString value =
    case value of
        Boolean bool ->
            boolToString bool

        Number num ->
            numberToString num

        Str str ->
            str



-- Configuration


type alias ConfigPriv =
    { encodeBrackets : Bool
    , parseBooleans : Bool
    , parseNumbers : Bool
    }


{-|
Opaque configuration type
-}
type Config
    = Config ConfigPriv


{-|
Get a default configuration

    encodeBrackets = True
    parseBooleans = True
    parseNumbers = True
-}
config : Config
config =
    Config
        { encodeBrackets = True
        , parseBooleans = True
        , parseNumbers = True
        }


{-|
Wherever to encode brackets or not

    QS.serialize (Qs.config |> Qs.encodeBrackets False) query

    ==

    "a[]=1&a[]=2"
-}
encodeBrackets : Bool -> Config -> Config
encodeBrackets val (Config config) =
    Config { config | encodeBrackets = val }


{-|
Wherever to parse booleans. If false then "true" and "false" will be strings.

    QS.parse (Qs.config |> Qs.parseBooleans False) "?a=true"

    ==

    Dict.fromList [ ("a", One <| Str "true") ]
-}
parseBooleans : Bool -> Config -> Config
parseBooleans val (Config config) =
    Config { config | parseBooleans = val }


{-|
Wherever to parse numbers. If false then numbers will be strings.

    QS.parse (Qs.config |> Qs.parseNumbers False) "?a=1"

    ==

    Dict.fromList [ ("a", One <| Str "1") ]
-}
parseNumbers : Bool -> Config -> Config
parseNumbers val (Config config) =
    Config { config | parseNumbers = val }



-------------------------------------------------------------------------------
-- PARSE
-------------------------------------------------------------------------------


{-|
Parse a query string.
This loosely follows https://github.com/ljharb/qs

    QS.parse
        QS.config
        "?a=1&b=x"

    == Dict.fromList [ ( "a", One <| Number 1 ), ( "b", One <| Str "x" ) ]

## Booleans

By default QS will parse "true" and "false" into booleans. You can change this with:

    QS.parse
        (QS.config |> QS.parseBooleans False)
        "?a=false"

## Numbers

The same applies for numbers, by default QS will try to parse numbers

    QS.parse QS.config "?a=1"

    ==

    Dict.fromList [ ( "a", One <| Number 1 ) ]

Change this with `parseNumbers False`
-}
parse : Config -> String -> Query
parse (Config config) queryString =
    let
        trimmed =
            queryString
                |> String.split "?"
                |> String.join ""
    in
        if String.isEmpty trimmed then
            empty
        else
            trimmed
                |> String.split "&"
                |> List.foldl (addSegmentToQuery config) empty


{-| @priv
Add a segment like a=1 to the query
-}
addSegmentToQuery : ConfigPriv -> String -> Query -> Query
addSegmentToQuery config segment query =
    let
        ( key, val ) =
            querySegmentToTuple segment
    in
        if String.endsWith "[]" key then
            let
                newKey =
                    String.dropRight 2 key
            in
                addListValToQuery config newKey val query
        else
            addUniqueValToQuery config key val query


{-| @priv
-}
addListValToQuery : ConfigPriv -> String -> String -> Query -> Query
addListValToQuery config key rawValue query =
    let
        currentVals =
            get key query

        push value =
            case currentVals of
                Just (Many vals) ->
                    Many (List.append vals [ value ])

                Just (One preValue) ->
                    Many [ preValue, value ]

                Nothing ->
                    Many [ value ]
    in
        case rawValueToValue config rawValue of
            Nothing ->
                query

            Just value ->
                set key (push value) query


{-| @priv
-}
addUniqueValToQuery : ConfigPriv -> String -> String -> Query -> Query
addUniqueValToQuery config key val query =
    case rawValueToValue config val of
        Nothing ->
            query

        Just value ->
            set key (One value) query


{-| @priv
-}
rawValueToValue : ConfigPriv -> String -> Maybe Primitive
rawValueToValue config val =
    let
        trimmed =
            String.trim val

        isEmpty =
            trimmed == ""

        true =
            "true"

        false =
            "false"

        isBool =
            trimmed == true || trimmed == false

        maybeFloat =
            String.toFloat trimmed
                |> Result.toMaybe

        isNum =
            maybeFloat /= Nothing
    in
        if isEmpty then
            Nothing
        else if isBool && config.parseBooleans then
            case trimmed of
                "true" ->
                    Boolean True |> Just

                "false" ->
                    Boolean False |> Just

                _ ->
                    Str trimmed |> Just
        else if isNum && config.parseNumbers then
            case maybeFloat of
                Just n ->
                    Number n |> Just

                Nothing ->
                    Str trimmed |> Just
        else
            Str trimmed |> Just


{-| @priv
Split a segment into a tuple

    "a=1" ==> ("a", "1")
-}
querySegmentToTuple : String -> ( String, String )
querySegmentToTuple element =
    let
        splitted =
            String.split "=" element

        first =
            Maybe.withDefault "" (List.head splitted)

        firstDecoded =
            Http.decodeUri first |> Maybe.withDefault ""

        second =
            Maybe.withDefault "" (List.head (List.drop 1 splitted))

        secondDecoded =
            Http.decodeUri second |> Maybe.withDefault ""
    in
        ( firstDecoded, secondDecoded )



-------------------------------------------------------------------------------
-- Serialize
-------------------------------------------------------------------------------


{-| Serialize the query.
This follows https://github.com/ljharb/qs serialization.

    QS.serialize Qs.config <| Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]

    ==

    "?a=1&b=2"

List are serialized by adding `[]`

    QS.serialize Qs.config <| Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]

    ==

    "?a%5B%5D=1&a%5B%5D=2" (equivalent to "?a[]=1&a[]=2")

If your don't want to encode `[]` use `encodeBrackets False`.

    QS.serialize
        (Qs.config |> encodeBrackets False) ...

    ==

    "?a[]=1&a[]=2"

However brackets in the value are always encoded.
-}
serialize : Config -> Query -> String
serialize (Config config) query =
    if Dict.isEmpty query then
        ""
    else
        let
            addUniqueKey : List String -> String -> String -> List String
            addUniqueKey acc key value =
                let
                    encodedKey =
                        percentageEncode config.encodeBrackets key

                    encodedVal =
                        percentageEncode True value
                in
                    acc ++ [ encodedKey ++ "=" ++ encodedVal ]

            addListKey : List String -> String -> List String -> List String
            addListKey acc key values =
                let
                    encodedKey =
                        percentageEncode config.encodeBrackets (key ++ "[]")

                    encodeVal v =
                        percentageEncode True v
                in
                    values
                        |> List.map (\val -> encodedKey ++ "=" ++ encodeVal val)
                        |> List.append acc

            addKey ( key, queryVal ) acc =
                case queryVal of
                    One val ->
                        addUniqueKey acc key (primitiveToString val)

                    Many list ->
                        list
                            |> List.map primitiveToString
                            |> addListKey acc key

            values =
                query
                    |> Dict.toList
                    |> List.foldl addKey []
                    |> String.join "&"
        in
            "?" ++ values



-------------------------------------------------------------------------------
-- TRANSFORMATIONS
-------------------------------------------------------------------------------


{-|
Get an empty QS.Query
-}
empty : Query
empty =
    Dict.empty


{-|
Get a value from the query

    QS.get "a" query

    ==

    Maybe (One <| Str "1")
-}
get : String -> Query -> Maybe OneOrMany
get key query =
    Dict.get key query


{-|
Get values from the query as a list of strings (regardless if one or many)

    query =
        Dict.fromList [ ("a", Many [Boolean True, Number 1]) ]

    QS.getAsStringList "a" query

    ==

    ["true", "1"]
-}
getAsStringList : String -> Query -> List String
getAsStringList key query =
    let
        values =
            get key query

        makeStringValues queryVal =
            case queryVal of
                One val ->
                    [ primitiveToString val ]

                Many list ->
                    List.map primitiveToString list
    in
        Maybe.map makeStringValues values
            |> Maybe.withDefault []


{-| Merge two Querys.
Values in the first override values in the second.
-}
merge : Query -> Query -> Query
merge =
    Dict.union


{-|
Set a value in the query

    QS.set "a" (One <| Str "1") query
-}
set : String -> OneOrMany -> Query -> Query
set key value query =
    Dict.insert key value query


{-|
Set a unique value in the query

    QS.setOne "a" (Str "1") query
-}
setOne : String -> Primitive -> Query -> Query
setOne key value query =
    Dict.insert key (One value) query


{-|
Set a list of values in the query

    QS.setList "a" [Str "1", Boolean True] query
-}
setList : String -> List Primitive -> Query -> Query
setList key value query =
    Dict.insert key (Many value) query


{-|
Set a string value in the query

    QS.setStr "a" "1" Qs.empty

    ==

    Dict.fromList [ ("a", One <| Str "1") ]
-}
setStr : String -> String -> Query -> Query
setStr key value query =
    setOne key (Str value) query


{-|
Set a boolean value in the query

    QS.setBool "a" True Qs.empty

    ==

    Dict.fromList [ ("a", One <| Boolean True) ]
-}
setBool : String -> Bool -> Query -> Query
setBool key value query =
    setOne key (Boolean value) query


{-|
Set a numeric value in the query

    QS.setBool "a" 2 Qs.empty

    ==

    Dict.fromList [ ("a", One <| Number 2) ]
-}
setNum : String -> Float -> Query -> Query
setNum key value query =
    setOne key (Number value) query


{-|
Set a list of string values in the query

    QS.setListStr "a" ["1", "x"] Qs.empty

    ==

    Dict.fromList [ ("a", Many [ Str "1", Str "x" ] ]
-}
setListStr : String -> List String -> Query -> Query
setListStr key values query =
    setList key (values |> List.map Str) query


{-|
Set a list of boolean values in the query

    QS.setListBool "a" [True, False] Qs.empty
-}
setListBool : String -> List Bool -> Query -> Query
setListBool key values query =
    setList key (values |> List.map Boolean) query


{-|
Set a list of numeric values in the query

    QS.setListNum "a" [2, 3] Qs.empty

-}
setListNum : String -> List Float -> Query -> Query
setListNum key values query =
    setList key (values |> List.map Number) query


{-|
Adds one value to a list

    QS.push "a" (Number 2) Qs.empty

- If the key is not a list then it will be promoted to a list
- If the key doesn't exist then it will be added a list of one item
-}
push : String -> Primitive -> Query -> Query
push key value query =
    let
        newValues =
            case get key query of
                Just (One existing) ->
                    [ existing, value ]

                Just (Many existing) ->
                    List.append existing [ value ]

                Nothing ->
                    [ value ]
    in
        setList key newValues query


{-|
Add one string to a list
-}
pushStr : String -> String -> Query -> Query
pushStr key value query =
    push key (Str value) query


{-|
Add one boolean to a list
-}
pushBool : String -> Bool -> Query -> Query
pushBool key value query =
    push key (Boolean value) query


{-|
Add one number to a list
-}
pushNum : String -> Float -> Query -> Query
pushNum key value query =
    push key (Number value) query


{-|
Remove a key from the query
-}
remove : String -> Query -> Query
remove key query =
    Dict.remove key query



-------------------------------------------------------------------------------
-- DECODER
-------------------------------------------------------------------------------


{-|
Decode JSON into a QS.Query

    json =
        """{"a":["x", 1, true]}"""

    Decode.decodeString QS.decoder json

    ===

    Dict.fromList [ ( "a", Many [ Str "x", Number 1, Boolean True ] ) ]

This decoder doesn't handle nested values. Nested data will fail the decoder.
-}
decoder : Decoder Query
decoder =
    Decode.dict queryValueDecoder


queryValueDecoder : Decoder OneOrMany
queryValueDecoder =
    Decode.oneOf
        [ Decode.map Many (Decode.list valueDecoder)
        , Decode.map One valueDecoder
        ]


valueDecoder : Decoder Primitive
valueDecoder =
    Decode.oneOf
        [ Decode.map Boolean Decode.bool
        , Decode.map Number Decode.float
        , Decode.map Str Decode.string
        ]



-------------------------------------------------------------------------------
-- ENCODE
-------------------------------------------------------------------------------


{-| Encode a QS.Query to a JSON value

    query =
        Many [ Str "x", Boolean True ] )

    encodedQuery =
        QS.encode query

    Encode.encode 0 encodedQuery

    ==

    """{"a":["x",true]}"""
-}
encode : Query -> Encode.Value
encode query =
    let
        encodeQueryTuple ( key, val ) =
            ( key, encodeQueryValue val )
    in
        query
            |> Dict.toList
            |> List.map encodeQueryTuple
            |> Encode.object


{-| @priv
-}
encodeQueryValue : OneOrMany -> Encode.Value
encodeQueryValue value =
    case value of
        One value ->
            encodeValue value

        Many list ->
            list
                |> List.map encodeValue
                |> Encode.list


{-| @priv
-}
encodeValue : Primitive -> Encode.Value
encodeValue value =
    case value of
        Str str ->
            Encode.string str

        Boolean bool ->
            Encode.bool bool

        Number num ->
            Encode.float num



-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------


boolToString =
    Encode.bool >> Encode.encode 0


{-| @priv
Decode one symbol in a string

    decodeSymbol ">" "hello%3Eworld"

    ==

    "hello>world"
-}
decodeSymbol : String -> String -> String
decodeSymbol symbol =
    let
        encoded =
            Http.encodeUri symbol
    in
        Regex.replace Regex.All (Regex.regex encoded) (\_ -> symbol)


{-|

    encodeUri encodes = and &
    We want those as normal chars
-}
percentageEncode : Bool -> String -> String
percentageEncode encodeBrackets =
    let
        maybeDecodeBrackets =
            if encodeBrackets then
                identity
            else
                decodeSymbol "["
                    >> decodeSymbol "]"
    in
        Http.encodeUri
            >> maybeDecodeBrackets


numberToString =
    toString
