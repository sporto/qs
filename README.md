# QS

[ ![Codeship Status for sporto/qs](https://app.codeship.com/projects/5ac23660-b7e5-0135-9542-662cc8a72824/status?branch=master)](https://app.codeship.com/projects/258900)

An opinionated query string parser for Elm. Based on https://github.com/ljharb/qs

QS users `[]` for lists.

E.g.

```
?ids[]=1&ids[]=2
```

## Parse query

Convert a query string to a QS.Query

```
QS.parse
    QS.config
    "?a=1&b=x"

== Dict.fromList
    [ ( "a", One <| Number 1 )
    , ( "b", One <| Str "x" ) 
    ]
```

## Serialize

Convert a QS.Query to a query string

```
query =
    Dict.fromList
        [ ( "a", QueryString "1" )
        , ( "b", QueryString "2" ) 
        ]

QS.serialize
    Qs.config 
    query

==

"?a=1&b=2"
```

## Decode query

Convert JSON to a QS.Query

```
json =
        """{"a":["x", 1, true]}"""

Decode.decodeString QS.decoder json
```

## Encode query

Convert a QS.Query to JSON

```
query =
    Many [ Str "x", Boolean True ] )

encodedQuery =
    QS.encode query

Encode.encode 0 encodedQuery
```

## Transform a query

Function for getting, setting, removing keys in the query. See the documentation.