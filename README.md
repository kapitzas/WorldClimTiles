# WorldClimTiles

## Installation

``` r
devtools::install_github("kapitzas/WorldClimTiles")
```

## Background

Due to the size of the global data sets, high resolution WorldClim and SRTM data are downloadable in spatial tiles. One has to manually identify the tiles that are needed for a study area, download them and then merge them into single raster objects, which can be a bit cumbersome and messy to do. Here, I provide three simple functions that a) identify which tiles you need for your study area (simply supply a raster or feature), b) allow you to download the data and c) merge the data to the final raster object for your study area.

## Examples

### Get tile names
``` r
boundary <- getData("GADM", country = "FRA", level =0) #France country borders
wc_tiles <- tile_name(boundary, "worldclim") #for 0.5 arcmin worldclim tiles of france
srtm_tiles <- tile_name(boundary, "srtm") #for 90m srtm tiles of france
```

### Download tiles
```r 
tiles <- tile_get(wc_tiles, "worldclim", path = my_path) #download and load worldclim
tiles <- tile_get(srtm_tiles, "srtm", path = my_path) #download and load srtm
```

### Merge tiles
``` r
final <- tile_merge(tiles)
final <- tile_merge(tiles, fac = 10) #Reprojects data to 10 times smaller res, i.e. to get srtm data from 90m to approx 1km resolution.
```
===========
