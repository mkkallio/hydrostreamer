# hydrostreamer 0.4.0



# hydrostreamer 0.3.2

Fixed many issues, including:

* added `ForecastComb` in imports

* fixed `delineate_basin()`

* Hopefully fixed the conflict issue between `raster` and `hydroGOF` packages.
The issue appears to have been caused by `hydroGOF` importing `hydroTSM` 
importing `gstat` importing `spacetime`, which redefines class `RasterStack`.
Sometimes manifests itself when loading an .RData file with a raster in it,
causing automatic attachment of `raster`.

* Some other smaller fixes.

# hydrostreamer 0.3.1

Small update with mostly bugfixes.

* Fixed [issue](https://github.com/mkkallio/hydrostreamer/issues/2) where Strahler
based weighting of river segments did not work.

* Fixed an issue involving packages `hydroGOF` and `raster`, causing `optimise_point()` to fail if `raster` was loaded.

* `HSgrid`, `HSweights`, `HSrunoff` and `HSflow` no longer lose their `list` class.

# hydrostreamer 0.3.0

A major overhaul of the downscaling system and a number of additional features included. This update changes names of many functions and their outputs and thus introduces breaking changes. The overhaul, however, makes the package much more usable. Many of the changes are made to make further development of the package easier. This is a fairly experimental package with a number of bugs to be expected, but the main workflow works correctly.

## Changes include, among smaller changes:

*   date handling introduced, instead of sequential timestep numbers.

*   Package now supports multiple input runoff timeseries, and the central functions support processing them all in one command.

*   function names changed:
    +   `polygrid_timeseries()` --> `raster_to_HSgrid()`
    +   `compute_weights()` --> `compute_HSweights()`
    +   `compute_segment_runoff()` --> `downscale_runoff()`
    +   `accumulate_flow()` --> `accumulate_runoff()`
    +   `flow_network()` --> `river_network()`

*   New S3 object classes added: 
    +   `HSgrid` to store input runoff information
    +   `HSweights` remains the same as in previous versions
    +   `HSrunoff` for the downscaled runoff timeseries
    +   `HSflow` to store discharge information after applying river routing
    +   `HSobs` to store observation data
    +   `HSoptim` to hold optimization information

*   Added two new routing functions:
    +   `accumulate_runoff.muskingum()` - Muskingum routing
    +   `accumulate_runoff.simple()` - Simple lag routing

*   Added forecast combination capabilities. It is now possible to get optimized flow estimates by
    +   `optimise_point()` for optimising in point locations
    +   `optimise_region()`for regionalizing weights obtained in point locations

*   Added `ensemble_summary()` function for summarising HS* objects

*   Added `HSwrite` for easy expor of HS* objects to GeoPackage format.

## Future developments

The current update contains minimal documentation and examples. In the immediate future, we'll

*   Work with bringing all documentation up-to-date

*   Add further regionalization options

*   Provide a number of vignettes exploring the capabilities of `hydrostreamer`

*   Provide river network generation functions, so that river network input can be replaced with a DEM input.
    
    

# hydrostreamer 0.2.2

*   `compute_weights()` now calls the correct function when delineating basins from drainage directions.

*   `compute_segment_runoff()` has been partially re-implemented in Fortran, making it much faster

*   `river_voronoi()` is changed in the way it ensures no nodes are overlapping when tesselating Voronoi
    diagram. Should be faster now.


# hydrostreamer 0.2.1

*   `flow_network()` now works correctly even if the last features in the river network
    did not have any downstream segments.
    
*   `delineate_basin()` is now re-implemented partly in Fortran, yielding a very large speed
    boost compared to the previous R native implementation.


# hydrostreamer 0.2.0

## Minor improvements and fixes

*   Column order in objects output by all function have been changed so that
    columns are always in the same order, starting from ID's. See individual 
    function reference to find the order.

*   Names of the ID's have been made uniform: in the output, columns with river
    segment ID's are now always "riverID", and grids "gridID".

*   Documentation has been (mostly) rewritten, and an error in the vignette has been 
    corrected. Examples have been added to almost all functions.

*   Functions have been moved to their own .R files which include the function
    and methods, if any.

*   Added simple checks for input arguments for all functions.

*   Added`verbose` (TRUE/FALSE) option to all functions to control wherther to print progress
    indication. By default, functions do not.



## Breaking changes

*   `compute_weights()` now always outputs an object of class *HSweights*. As
    a result, line-based weighting now outputs the original river network, not
    one which has been split at runoff polygon borders.
    
## New functions

*   `switch_grid()` can be used to safely switch the runoff input in a *HSweights*
    object without needing to recompute weights every time. The replacement has
    to include all features in the grid being replaced, with identical ID's.
    
*   `upstream()` can be used to identify all river segments which are upstream
    from a certain segment.
    
    
