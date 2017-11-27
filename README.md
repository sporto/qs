# QS

An opinionated query string parser for Elm. Based on https://github.com/ljharb/qs

QS users `[]` for lists.

E.g.

```
?ids[]=1&ids[]=2
```

## Parse query

Conver a query string to a QS.Query

## Query to string

Convert a QS.Query to a query string

## Decode query

Convert JSON to a QS.Query

## Encode query

Convert a QS.Query to JSON

## Transform a query

There are function for adding, updating, removing keys in the query, see the documentation at ...