hydrostreamer 1.0.1
===================

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4739223.svg)](https://doi.org/10.5281/zenodo.4739223)

**hydrostreamer** is an R package designed to allow swift investigation
of surface water resources from existing distributed runoff datasets
with arbitrary areal representations (regular or irregular polygons).

If used in a publication, please cite the following model description
paper: Kallio, M., Guillaume, J. H. A., Virkki, V., Kummu, M., and
Virrantaus, K.: [Hydrostreamer v1.0 – improved streamflow predictions
for local applications from an ensemble of downscaled global runoff
products](https://doi.org/10.5194/gmd-14-5155-2021), Geosci. Model Dev.
Discuss.,
<a href="https://doi.org/10.5194/gmd-14-5155-2021" class="uri">https://doi.org/10.5194/gmd-14-5155-2021</a>, 2021.

In addition, **hydrostreamer** has featured in the following
publications and conference presentations:

-   Model description paper is under review at Geoscientific Model
    Development:
-   Nauditt, A., Stahl, K., Rodríguez, E., Birkel, C.,
    Formiga-Johnsson, R. M., Kallio, M., Hann, H., Ribbe, L.,
    Baez-Villanueva, O. M., and Thurner, J.: [Tropical drought risk:
    estimates combining gridded vulnerability and hazard
    data](https://doi.org/10.5194/nhess-2020-360), Nat. Hazards Earth
    Syst. Sci. Discuss.,
    <a href="https://doi.org/10.5194/nhess-2020-360" class="uri">https://doi.org/10.5194/nhess-2020-360</a>,
    in review, 2020.
-   MODSIM 2019: Kallio et al, [Downscaling runoff products using areal
    interpolation: a combined pycnophylactic-dasymetric
    method](10.36334/modsim.2019.k8.kallio)
-   EGU 2018: Kallio et al, [Spatial allocation of low resolution runoff
    model outputs to high resolution stream
    network](https://www.researchgate.net/publication/325066501_Spatial_allocation_of_low_resolution_runoff_model_outputs_to_high_resolution_stream_network)
-   AGU 2018: Kallio et al, [Generating improved estimates of streamflow
    using model averaging of downscaled runoff products under
    uncertainty](https://www.researchgate.net/publication/330213179_Generating_improved_estimates_of_streamflow_using_model_averaging_of_downscaled_runoff_products_under_uncertainty)
-   AGU 2018: Virkki et al, [The value of open-source river discharge
    estimation in the water governance context of the 3S river basin in
    Southeast
    Asia](https://www.researchgate.net/publication/331320855_AGU_2018_Poster_Presentation)
-   EGU 2019: Nauditt et al, [Evaluating drought risk in data scarce
    tropical
    catchments](https://meetingorganizer.copernicus.org/EGU2019/EGU2019-18370.pdf)

Quickstart guide
----------------

Hydrostreamer workflow generally consists of three steps:

1.  Areal interpolation of runoff to explicit river segments
2.  Routing down the river network to estimate discharge
3.  Model averaging, if streamflow observations are available.

At the bare minimum, discharge can be estimated using only a single
distributed runoff product and a river network. To include model
averaging to the step, one further needs additional runoff datasets to
provide multiple estimates of discharge, and a reference timeseries to
perfgorm model averaging against.

Installing
----------

**hydrostreamer** is not yet on CRAN, so to install, and needs to be
installed from Github.

    # devtools::install_github("mkkallio/hydrostreamer")

Basic usage
-----------

Hydrostreamer includes some example data for demonstration purposes.
First, load example data to R:

    library(hydrostreamer)
    library(raster)
    library(lubridate)
    library(sf)

    data(example_rivers)
    data(example_basins)
    runoff <- brick(system.file("extdata", "runoff.tif", package = "hydrostreamer"))

### 1. Convert raster timeseries to a **HS** object

The first step in hydrostreamer is converting runoff into a *HS* object.
For a runoff input in a raster format, use *raster\_to\_HS()* function.
For input we need the raster, starting date of the timeseries and the
granularity of input timesteps.

    source_runoff <- raster_to_HS(runoff, 
                       date = ymd("1980-01-01"), 
                       timestep = "month", 
                       unit = "mm/s",
                       aoi = st_union(basins),
                       names = "LORA")
    source_runoff

    ## 
    ## Hydrostreamer
    ## No. objects: 4
    ## No. runoff timeseries: 1
    ##   Included runoff timeseries: LORA
    ## 
    ## Simple feature collection with 4 features and 2 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 107.275 ymin: 12.25 xmax: 107.625 ymax: 12.995
    ## CRS:            +proj=longlat +datum=WGS84 +no_defs
    ## # A tibble: 4 x 3
    ##   zoneID runoff_ts                                                      geometry
    ##    <int> <named list>                                              <POLYGON [°]>
    ## 1      1 <tibble [396 x~ ((107.5 12.995, 107.5 12.5, 107.31 12.5, 107.31 12.505~
    ## 2      2 <tibble [396 x~ ((107.625 12.5, 107.615 12.5, 107.5 12.5, 107.5 12.995~
    ## 3      3 <tibble [396 x~ ((107.31 12.5, 107.5 12.5, 107.5 12.325, 107.495 12.32~
    ## 4      4 <tibble [396 x~ ((107.5 12.5, 107.615 12.5, 107.615 12.485, 107.62 12.~

The output of the function is an sf object with each raster cell as
their own object, and two attributes: an ID and a list column with
timeseries of values for each of the raster cell.

### 2. Interpolate

Next, we interpolate the values in the source zones to individual river
reaches in the river network.

    interpolated_runoff <- interpolate_runoff(source_runoff, river, 
                                              riverID = "SEGMENT_ID")

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

    interpolated_runoff

    ## 
    ## Hydrostreamer
    ## No. objects: 41
    ## No. runoff timeseries: 1
    ##   Included runoff timeseries: LORA
    ## 
    ## Simple feature collection with 41 features and 3 fields
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 107.3325 ymin: 12.2975 xmax: 107.6075 ymax: 12.9975
    ## geographic CRS: WGS 84
    ## # A tibble: 41 x 4
    ##    riverID runoff_ts     SEGMENT_ID                                         geom
    ##      <dbl> <named list>       <dbl>                             <LINESTRING [°]>
    ##  1      56 <tibble [396~         56 (107.4125 12.2975, 107.4075 12.3025, 107.40~
    ##  2      65 <tibble [396~         65 (107.4425 12.3425, 107.4375 12.3475, 107.43~
    ##  3      67 <tibble [396~         67 (107.5475 12.3625, 107.5475 12.3675, 107.54~
    ##  4      69 <tibble [396~         69 (107.3525 12.3675, 107.3575 12.3675, 107.36~
    ##  5      70 <tibble [396~         70 (107.3675 12.3725, 107.3725 12.3775, 107.37~
    ##  6      77 <tibble [396~         77 (107.5175 12.4125, 107.5175 12.4175, 107.51~
    ##  7      79 <tibble [396~         79 (107.5625 12.4275, 107.5575 12.4325, 107.55~
    ##  8      83 <tibble [396~         83 (107.5575 12.4375, 107.5625 12.4425, 107.56~
    ##  9      85 <tibble [396~         85 (107.5925 12.4475, 107.5875 12.4525, 107.58~
    ## 10      88 <tibble [396~         88 (107.4525 12.4575, 107.4475 12.4625, 107.44~
    ## # ... with 31 more rows

The warning received here is from the sf package because the coordinate
system in the example dataset is in geographic coordinate system. The
warning goes away if we first project the dataset to an appropriate
coordinate reference system.

The output changes so that zoneID identifying the source of runoff
becomes riverID. runoff\_ts now contains timeseries of the interpolated
runoff at each individual river segment.

### 3. Apply river routing

Next, we route runoff down the river network in order to estimate
discharge at each segment. Since the example data has a monthly
timestep, and the river network is a small one, we use the simplest
routing algorithm available: instantaneous routing. In this routing
method, all runoff is assumed to flow through the entire network at the
timestep it is created.

    streamflow <- accumulate_runoff(interpolated_runoff, routing_method = "instant")
    streamflow

    ## 
    ## Hydrostreamer
    ## No. objects: 41
    ## No. runoff timeseries: 1
    ##   Included runoff timeseries: LORA
    ## No. discharge timeseries: 1
    ##   Included discharge timeseries: LORA
    ## 
    ## Simple feature collection with 41 features and 7 fields
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 107.3325 ymin: 12.2975 xmax: 107.6075 ymax: 12.9975
    ## geographic CRS: WGS 84
    ## # A tibble: 41 x 8
    ##    riverID NEXT  PREVIOUS runoff_ts discharge_ts UP_SEGMENTS SEGMENT_ID
    ##      <dbl> <nam> <named > <named l> <named list>       <dbl>      <dbl>
    ##  1      56 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         56
    ##  2      65 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         65
    ##  3      67 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         67
    ##  4      69 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         69
    ##  5      70 <dbl~ <dbl [2~ <tibble ~ <tibble [39~           2         70
    ##  6      77 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         77
    ##  7      79 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         79
    ##  8      83 <dbl~ <dbl [2~ <tibble ~ <tibble [39~           2         83
    ##  9      85 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         85
    ## 10      88 <dbl~ <dbl [1~ <tibble ~ <tibble [39~           0         88
    ## # ... with 31 more rows, and 1 more variable: geom <LINESTRING [°]>

We see that there are now additional columns. NEXT and PREVIOUS give the
routing information for the routing algorithm. discharge\_ts is a list
column which contains timeseries of the estimated runoff, and
UP\_SEGMENTS was created with the routing information, giving the number
of river reaches upstream of each individual reach in the network.

### These functions are pipable

The function usage has been developed so that they are all pipable.

    streamflow <- raster_to_HS(runoff, 
                               date=ymd("1980-01-01"), 
                               timestep="month", 
                               unit = "mm/s",
                               aoi=st_union(basins),
                               names="LORA") %>%
        interpolate_runoff(river, riverID = "SEGMENT_ID") %>%
        accumulate_runoff()

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

For a more thorough tutorial, see the included vignett e.g. from the
documentation site:
<a href="https://mkkallio.github.io/hydrostreamer/" class="uri">https://mkkallio.github.io/hydrostreamer/</a>

License
-------

The MIT License (MIT)

Copyright (c) 2018-2021 Marko K. Kallio

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
