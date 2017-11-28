module QS
    exposing
        ( Query
        , QueryValue(..)
        , ParseConfig
        , parse
        , parseConfig
        , parseBooleans
        , queryToString
        )

{-| Parse an manipulate query strings

# Types

@docs Query, QueryValue, ParseConfig

# Parse

@docs parse, parseConfig, parseBooleans

# Serialize

@docs queryToString

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
    Dict.Dict String QueryValue


{-|
Type for storing query values

    "a=true" == QueryBool True

    "a[]=true&a[]=false" == QueryBoolList [ True, False ]

    "a=1" == QueryNumber 1

    "a[]=1&a[]=2" == QueryNumberList [ 1, 2 ]

    "a=z" == QueryString "z"

    "a[]=x&a[]=y" == QueryStringList [ "x", "y" ]
-}
type QueryValue
    = QueryBool Bool
    | QueryBoolList (List Bool)
    | QueryNumber Float
    | QueryNumberList (List Float)
    | QueryString String
    | QueryStringList (List String)
    | QueryUnrecognised Decode.Value


{-| @priv
-}
type ParseValue
    = ParseValueEmpty
    | ParseValueBool Bool
    | ParseValueString String



-- PARSE Config


type alias ParseConfigPriv =
    { parseBooleans : Bool
    }


type ParseConfig
    = ParseConfig ParseConfigPriv


parseConfig : ParseConfig
parseConfig =
    ParseConfig
        { parseBooleans = True
        }


parseBooleans : Bool -> ParseConfig -> ParseConfig
parseBooleans val (ParseConfig config) =
    ParseConfig { config | parseBooleans = val }



-- PARSE


{-|
Parse a query string.
This losely follows https://github.com/ljharb/qs parsing

    QS.parse
        QS.parseConfig
        "?a=1&b=2"

    == Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]

By default QS will parse "true" and "false" into booleans. You can change this with:

    QS.parse
        (QS.parseConfig |> QS.parseBooleans False)
        "?a=false"

Booleans in lists will be parsed too:

    "?a[]=false&a[]=true" == Dict.fromList [ ( "a", QueryBoolList [ False, True ] ) ]

But if you have non-booleans then the whole list will be strings:

    "?a[]=false&a[]=monkey" == Dict.fromList [ ( "a", QueryStringList [ "false", "monkey" ] ) ]
-}
parse : ParseConfig -> String -> Query
parse (ParseConfig config) queryString =
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
addSegmentToQuery : ParseConfigPriv -> String -> Query -> Query
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
            addValToQuery config key val query


{-| @priv
-}
addListValToQuery : ParseConfigPriv -> String -> String -> Query -> Query
addListValToQuery config key val query =
    let
        currentVals =
            getQueryValues key query

        newValsForBool bool =
            case currentVals of
                Just (QueryStringList vals) ->
                    -- If the previous ones are string, then this one is string too
                    QueryStringList (List.append vals [ val ])

                Just (QueryBoolList vals) ->
                    QueryBoolList (List.append vals [ bool ])

                _ ->
                    QueryBoolList [ bool ]

        newValsForStr str =
            case currentVals of
                Just (QueryStringList vals) ->
                    -- If the previous ones are string, then this one is string too
                    QueryStringList (List.append vals [ str ])

                Just (QueryBoolList vals) ->
                    QueryStringList (List.append (List.map boolToString vals) [ str ])

                _ ->
                    QueryStringList [ str ]
    in
        case valueToParseValue config val of
            ParseValueEmpty ->
                query

            ParseValueBool bool ->
                setQuery key (newValsForBool bool) query

            ParseValueString str ->
                setQuery key (newValsForStr str) query


{-| @priv
-}
addValToQuery : ParseConfigPriv -> String -> String -> Query -> Query
addValToQuery config key val query =
    case valueToParseValue config val of
        ParseValueEmpty ->
            query

        ParseValueBool bool ->
            setQuery key (QueryBool bool) query

        ParseValueString str ->
            setQuery key (QueryString str) query


{-| @priv
-}
valueToParseValue : ParseConfigPriv -> String -> ParseValue
valueToParseValue config val =
    let
        trimmed =
            String.trim val
    in
        if config.parseBooleans then
            case trimmed of
                "" ->
                    ParseValueEmpty

                "true" ->
                    ParseValueBool True

                "false" ->
                    ParseValueBool False

                _ ->
                    ParseValueString trimmed
        else
            case trimmed of
                "" ->
                    ParseValueEmpty

                _ ->
                    ParseValueString trimmed


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



-- TO STRING


{-|
Serialize the query
This follows https://github.com/ljharb/qs serialization

    queryToString Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]

    ==

    "?a=1&b=2"

List are serialized by adding []

    Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]

    ==

    "?a%5B%5D=1&a%5B%5D=2" ("?a[]=1&a[]=2")
-}
queryToString : Query -> String
queryToString query =
    if Dict.isEmpty query then
        ""
    else
        let
            addUniqueKey : List String -> String -> String -> List String
            addUniqueKey acc key value =
                acc ++ [ key ++ "=" ++ value ]

            addListKey : List String -> String -> List String -> List String
            addListKey acc key values =
                List.map (\val -> key ++ "[]=" ++ val) values
                    |> List.append acc

            addKey ( key, value ) acc =
                case value of
                    QueryBool bool ->
                        addUniqueKey acc key (boolToString bool)

                    QueryBoolList bools ->
                        List.map boolToString bools
                            |> addListKey acc key

                    QueryNumber num ->
                        addUniqueKey acc key (toString num)

                    QueryNumberList nums ->
                        List.map toString nums
                            |> addListKey acc key

                    QueryString str ->
                        addUniqueKey acc key str

                    QueryStringList strs ->
                        addListKey acc key strs

                    _ ->
                        acc

            values =
                query
                    |> Dict.toList
                    |> List.foldl addKey []
                    |> String.join "&"
                    |> percentageEncode
        in
            "?" ++ values


{-|

    encodeUri encodes = and &
    We want those as normal chars
-}
percentageEncode : String -> String
percentageEncode =
    Http.encodeUri
        >> perEncodedEqualToSymbol
        >> perEncodedAmpersandToSymbol


perEncodedEqual =
    Http.encodeUri "="


perEncodedAmpersand =
    Http.encodeUri "&"


perEncodedEqualToSymbol =
    Regex.replace Regex.All (Regex.regex perEncodedEqual) (\_ -> "=")


perEncodedAmpersandToSymbol =
    Regex.replace Regex.All (Regex.regex perEncodedAmpersand) (\_ -> "&")


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


setQuery : String -> QueryValue -> Query -> Query
setQuery key value query =
    Dict.insert key value query


getQueryValues : String -> Query -> Maybe QueryValue
getQueryValues key query =
    Dict.get key query


getQueryValuesAsStringList : String -> Query -> List String
getQueryValuesAsStringList key query =
    let
        values =
            getQueryValues key query

        makeStringValues val =
            case val of
                QueryBool bool ->
                    [ boolToString bool ]

                QueryBoolList bools ->
                    List.map boolToString bools

                QueryNumber num ->
                    [ toString num ]

                QueryNumberList nums ->
                    List.map toString nums

                QueryString str ->
                    [ str ]

                QueryStringList strs ->
                    strs

                QueryUnrecognised _ ->
                    []
    in
        Maybe.map makeStringValues values
            |> Maybe.withDefault []



-- DECODER


queryDecoder : Decoder Query
queryDecoder =
    let
        valueDecoder =
            Decode.oneOf
                [ Decode.map QueryString Decode.string
                , Decode.map QueryStringList (Decode.list Decode.string)
                , Decode.map QueryNumber Decode.float
                , Decode.map QueryNumberList (Decode.list Decode.float)
                , Decode.map QueryBool Decode.bool
                , Decode.map QueryBoolList (Decode.list Decode.bool)
                , Decode.map QueryUnrecognised Decode.value
                ]
    in
        Decode.dict valueDecoder



-- ENCODE


encodeQueryValue : QueryValue -> Encode.Value
encodeQueryValue value =
    case value of
        QueryBool bool ->
            Encode.bool bool

        QueryBoolList list ->
            list
                |> List.map Encode.bool
                |> Encode.list

        QueryNumber num ->
            Encode.float num

        QueryNumberList list ->
            list
                |> List.map Encode.float
                |> Encode.list

        QueryString str ->
            Encode.string str

        QueryStringList list ->
            list
                |> List.map Encode.string
                |> Encode.list

        QueryUnrecognised value ->
            value


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



-- UTILS


boolToString =
    Encode.bool >> Encode.encode 0
