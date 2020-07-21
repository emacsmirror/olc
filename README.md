# Open Location Code for Emacs

Open Location Code is a way to encode locations in a format that is
easier for people (not computers) to use than latitude and longitude.

For example, the code 9FCQ9HXG+4C refers to the location 58°23'52.1"N
15°34'33.8"E (58.397813, 15.576063).

Codes can be shortened by removing characters from the beginning
andding a reference location: 9HXG+4C with the reference "Linköping"
would refer to the same set of coordinates.

For details about open location code and implementations in other
languages, see https://github.com/google/open-location-code.

## Description

This package implements an open location code library for emacs. It
implements all the required and most of the optional features in the
standard, and passes the test cases published in the open location
code github repository (see above).

### Data structures

#### OLC Area

An OLC is the area represented by an open location code. All fields
are read-only once the object has been created.

`(olc-area-create :latlo LATLO :lonlo LONLO :lathi LATHI :lonhi LONHI)`
: Creates an OLC area with southwest corner `LATLO`,`LONLO` and
  northeast corner `LATHI`,`LONHI`.

`(olc-area-p OBJ)`
: Return non-nil if `OBJ` is an OLC area.

`(olc-area-latlo AREA)`
`(olc-area-lonlo AREA)`
`(olc-area-lathi AREA)`
`(olc-area-lonhi AREA)`
: Get the south, west, north, and east coordinates of the area,
  respectively.

`(olc-area-lat AREA)`
`(olc-area-lon AREA)`
: Get the center latitude and longitude of the area, respectively.

#### OLC Parse

The OLC parse is a structure mainly used internally. Unless you call
`olc-parse-code` you will probably never see one.

`(olc-parse-create &keys pairs grid short prec code)`
: Creates an OLC parse structure. Don't call this: use
  `olc-parse-code` instead.

`(olc-parse-pairs PARSE)`
: Returns the list of parsed pairs from the code (pairs are before the
  plus sign and the first two characters after, if present).

`(olc-parse-grid PARSE)`
: Returns the list of parsed grid digits from the code (the optional
  digits that follow the last pair).

`(olc-parse-short PARSE)`
: Non-nil if the parsed code was shortened.

`(olc-parse-precision PARSE)`
: Precision of the parsed code. Padded codes can have precisions lower
  than 8. All other full and all short codes have precision of at
  least 8 (although, don't cound on short codes always having
  precision 8 or more).

`(olc-parse-code PARSE)`
: The parsed code.


### Functions

`(olc-encode lat lon len)`
: Encode a latitude LAT, longitude LON, into an open location code of
  length LEN. All arguments will be clipped to acceptable values.

`(olc-decode code)`
: Decode a code CODE. Returns an OLC area (see above).

`(olc-recover code lat lon &optional format)`
: Recover the closest point to coordinates `LAT` and `LON` with a code
  that can be shortened to `CODE`. If FORMAT is `'latlon`, then the
  center of the recovered area `(LAT . LON)` is returned. If FORMAT is
  `'area` (or any other value), the returned value is an full open
  location code.

`(olc-recover-string arg1 &optional arg2 arg3)`
: Recover a shortened code *without* the reference latitude and
  longitude. When called with one argument, it must be a string
  consisting of a shortened open location code followed by whitespace
  and a geographical location. When called with two strings, the first
  must be a shortened open location code and the second if the
  geographical location. Optionally, the last argument in either case
  can be a symbol indicating the format of the return value (see
  `olc-recover`, above). This function requires the `request` package
  to be installed, and uses the Open Streetmap API to convert the
  geographical reference to coordinates. Please make sure you follow
  the acceptable use policy for the API (e.g., one request per second,
  tops, allowed).

`(olc-is-valid CODE)`
: Returns non-nil if `CODE` is a valid open location code.

`(olc-is-short CODE)`
: Returns non-nil if `CODE` is a valid short location code. Returns
  nil for valid short and for invalid codes.

`(olc-is-full CODE)`
: Returns non-nil if `CODE` is a valid full open location code.
  Returns nil for valid long and for invalid codes.
