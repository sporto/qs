# Changelog

## 2.0.0

- Do not trim empty spaces on values. These spaces can be semantically valid.

## 2.0.0

- Removed `Primitive`. All values now parse into a `String`

In version 1 this lib will attempt to automatically parse numbers and booleans. You could turn all these on or off, but there was no way to configure this for certain keys.

Turns out this auto-magical decoding lead to surprising behaviour e.g. you have a query like
`account=0123`. This was parsed to `123`. But the app expected `0123`. There is too much unexpected magic here.

V2 parses all values to a string. So it gives you exactly what is in the query string. Parsing these to number or boolean has to be done as further step.

## 1.2.0

### Added:

- Added `getAsMaybeStringList`

## 1.1.0

### Added:

- Added `addQuestionMark`
- Added 'has'
