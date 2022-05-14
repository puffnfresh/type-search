# Type Search

Largely a port of Hoogle's [core type search algorithm](http://neilmitchell.blogspot.com/2020/06/hoogle-searching-overview.html) with a bit more abstraction.

* [Search](src/Search.hs) - types and core driver for type searching
* [Search/Structured](src/Search/Structured.hs) - core algorithm for fingerprinting and signature comparison, with lots of intermediate data structures
* [Search/Hoogle](src/Search/Hoogle.hs) - tries to use the same cost calculations as Hoogle, for ranking of results
