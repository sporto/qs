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
        , encodeBrackets
        )

{-| Parse an manipulate query strings

# Types

@docs Query, OneOrMany, Primitive

# Parse and Serialize

@docs parse, serialize

# Config

@docs Config, config, parseBooleans, encodeBrackets

# Decode

# Endoce

# Transform
-}

import Dict
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Regex


-- QUERY


{-|
A parsed query string

    "?a[]=1&a[]=2&b=3"

    ==

    Dict.fromList
        [ ( "a", QueryStringList [ "1", "2" ] )
        , ( "b", QueryString "3" )
        ]
-}
type alias Query =
    Dict.Dict String OneOrMany


type OneOrMany
    = One Primitive
    | Many (List Primitive)


{-|
Type for storing query values

    "a=true" == QueryBool True

    "a[]=true&a[]=false" == QueryBoolList [ True, False ]

    "a=1" == QueryNumber 1

    "a[]=1&a[]=2" == QueryNumberList [ 1, 2 ]

    "a=z" == QueryString "z"

    "a[]=x&a[]=y" == QueryStringList [ "x", "y" ]
-}
type Primitive
    = Boolean Bool
    | Number Float
    | Text String
    | Unrecognised Decode.Value


valueToString : Primitive -> String
valueToString value =
    case value of
        Boolean bool ->
            boolToString bool

        Number num ->
            numberToString num

        Text str ->
            str

        Unrecognised v ->
            ""



-- PARSE Config


type alias ConfigPriv =
    { encodeBrackets : Bool
    , parseBooleans : Bool
    , parseNumbers : Bool
    }


type Config
    = Config ConfigPriv


config : Config
config =
    Config
        { encodeBrackets = True
        , parseBooleans = True
        , parseNumbers = True
        }


encodeBrackets : Bool -> Config -> Config
encodeBrackets val (Config config) =
    Config { config | encodeBrackets = val }


parseBooleans : Bool -> Config -> Config
parseBooleans val (Config config) =
    Config { config | parseBooleans = val }



-- PARSE


{-|
Parse a query string.
This losely follows https://github.com/ljharb/qs parsing

    QS.parse
        QS.config
        "?a=1&b=2"

    == Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]

## Booleans

By default QS will parse "true" and "false" into booleans. You can change this with:

    QS.parse
        (QS.config |> QS.parseBooleans False)
        "?a=false"

Booleans in lists will be parsed too:

    "?a[]=false&a[]=true" == Dict.fromList [ ( "a", Many [ False, True ] ) ]

But if you have non-booleans then the whole list will be strings:

    "?a[]=false&a[]=monkey" == Dict.fromList [ ( "a", Many [ "false", "monkey" ] ) ]

## Numbers

The same applies for numbers, by default QS will try to parse numbers

    QS.parse QS.config "?a=1"

    ==

    Dict.fromList [ ( "a", One 1 ) ]
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
            emptyQuery
        else
            trimmed
                |> String.split "&"
                |> List.foldl (addSegmentToQuery config) emptyQuery


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
            getQueryValues key query

        add value =
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
                setQuery key (add value) query


{-| @priv
-}
addUniqueValToQuery : ConfigPriv -> String -> String -> Query -> Query
addUniqueValToQuery config key val query =
    case rawValueToValue config val of
        Nothing ->
            query

        Just value ->
            setQuery key (One value) query


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
                    Text trimmed |> Just
        else if isNum && config.parseNumbers then
            case maybeFloat of
                Just n ->
                    Number n |> Just

                Nothing ->
                    Text trimmed |> Just
        else
            Text trimmed |> Just


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



-- Serialize


{-|
Serialize the query
This follows https://github.com/ljharb/qs serialization

    QS.serialize Qs.config <| Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]

    ==

    "?a=1&b=2"

List are serialized by adding []

    QS.serialize Qs.config <| Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]

    ==

    "?a%5B%5D=1&a%5B%5D=2" ("?a[]=1&a[]=2")

If your don't want to encode [] use `encodeBrackets False`.

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
                        addUniqueKey acc key (valueToString val)

                    Many list ->
                        list
                            |> List.map valueToString
                            |> addListKey acc key

            values =
                query
                    |> Dict.toList
                    |> List.foldl addKey []
                    |> String.join "&"
        in
            "?" ++ values


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


decodeSymbol : String -> String -> String
decodeSymbol symbol =
    let
        encoded =
            Http.encodeUri symbol
    in
        Regex.replace Regex.All (Regex.regex encoded) (\_ -> symbol)


emptyQuery : Query
emptyQuery =
    Dict.empty


{-| First takes preference
-}
mergeQuery : Query -> Query -> Query
mergeQuery =
    Dict.union


removeQuery : String -> Query -> Query
removeQuery key query =
    Dict.remove key query


setQuery : String -> OneOrMany -> Query -> Query
setQuery key value query =
    Dict.insert key value query


getQueryValues : String -> Query -> Maybe OneOrMany
getQueryValues key query =
    Dict.get key query


getQueryValuesAsStringList : String -> Query -> List String
getQueryValuesAsStringList key query =
    let
        values =
            getQueryValues key query

        makeStringValues queryVal =
            case queryVal of
                One val ->
                    [ valueToString val ]

                Many list ->
                    List.map valueToString list
    in
        Maybe.map makeStringValues values
            |> Maybe.withDefault []



-- DECODER


queryDecoder : Decoder Query
queryDecoder =
    Decode.dict queryValueDecoder


queryValueDecoder : Decoder OneOrMany
queryValueDecoder =
    Decode.oneOf
        [ Decode.map One valueDecoder
        , Decode.map Many (Decode.list valueDecoder)
        ]


valueDecoder : Decoder Primitive
valueDecoder =
    Decode.oneOf
        [ Decode.map Boolean Decode.bool
        , Decode.map Number Decode.float
        , Decode.map Text Decode.string
        , Decode.map Unrecognised Decode.value
        ]



-- ENCODE


encodeQuery : Query -> Encode.Value
encodeQuery query =
    let
        encodeQueryTuple ( key, val ) =
            ( key, encodeQueryValue val )
    in
        query
            |> Dict.toList
            |> List.map encodeQueryTuple
            |> Encode.object


encodeQueryValue : OneOrMany -> Encode.Value
encodeQueryValue value =
    case value of
        One value ->
            encodeValue value

        Many list ->
            list
                |> List.map encodeValue
                |> Encode.list


encodeValue : Primitive -> Encode.Value
encodeValue value =
    case value of
        Text str ->
            Encode.string str

        Boolean bool ->
            Encode.bool bool

        Number num ->
            Encode.float num

        Unrecognised value ->
            value



-- UTILS


boolToString =
    Encode.bool >> Encode.encode 0


numberToString =
    toString
