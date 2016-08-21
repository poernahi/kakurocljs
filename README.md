# kakurocljs

Kakuro solver that runs on your browser.

## Demo

https://www.entropyparticle.com/puzzle/kakurocljs

## Overview

kakurocljs was written as an exercise in Clojurescript. User interface rendering
is done using Reagent. Source is combined in one file as it is short enough. The algorithm
was developed from scratch and not too optimized, but can solve 9x9 puzzles
reasonably quickly.

## Usage

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload.

## License

Copyright © 2016 Hendrik Poernama

Distributed under the Eclipse Public License, the same as Clojure.
