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

## Installing

To use `olc` you will need Emacs 25.1 or newer. To use the compound
code functions you will also need to install `request`. You have two
options: installing from MELPA or installing from source.

### Installing from MELPA

This is the easiest option. Load `package` and add MELPA to your
`init.el` or `.emacs`::

    (require 'package)
    (add-to-list 'package-archives 
                 '("melpa" . "https://melpa.org/packages/") t)

Then::

    M-x package-refresh-contents
    M-x package-install RET olc RET

Complete instructions for adding instructions at
<https://melpa.org/#/getting-started>.


### Installing from source

If you download or clone the repository from https://gitlab.liu.se,
then you can install by running `make` in the top-level directory of
the source, and manually copying the `.el`, `.elc`, and `.info` files
to your preferred location.


## Description

This package implements an open location code library for emacs. It
implements all the required and most of the optional features in the
standard, and passes the test cases published in the open location
code github repository (see above).

The complete documentation is available in texinfo format. The
following examples may be helpful.

## Examples

### Encoding

````
(olc-encode 58.397813 15.576063 11)
"9FCQ9HXG+4CG"
````

### Decoding

````
(olc-decode "9FCQ9HXG+4CG")
#s(olc-area 58.397800000000004 15.5760625 58.397825000000005 15.57609375)
(olc-area-lat (olc-decode "9FCQ9HXG+4CG"))
58.3978125
(olc-area-lon (olc-decode "9FCQ9HXG+4CG"))
````

### Shortening

````
(olc-shorten "9C3W9QCJ+2VX" 51.3701125 -1.217765625)
"+2VX"
(olc-shorten "9C3W9QCJ+2VX" 51.3701125 -1.217765625 4)
"9QCJ+2VX"
````

### Recovery

Recovery using latitude and longitude as reference:

````
(olc-recover "+2VX" 51.3701125 -1.217765625)
"9C3W9QCJ+2VX"
````

Recovery using a geographical reference (requires `request` and uses
the OpenStreetMap API):

````
(olc-recover-compound "M24Q+89 Mutitjulu")
"5Q6HM24Q+89"
````
