#############################################################################################
#################### (6A) Thesis: NDVI annual means (stacking)   ############################
#############################################################################################

library(raster)    # GIS stuff
library(rgdal)     # GIS stuff
library(rgeos)     # GIS stuff
library(sf)        # GIS stuff
library(sp)        # GIS stuff
library(tidyverse) # Tidyverse

stations <- shapefile("...")

## we will stack ndvi layers per year and calculate the mean 

# ------ 2017 ------
sat2 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/NDVI/S2A1C_20170602_010__NDVI_10.vrt")
sat2 <- raster::crop(sat2, st_as_sf(stations)) * 0.0001
sat3 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/NDVI/S2A1C_20170712_010__NDVI_10.vrt")
sat3 <- raster::crop(sat3, st_as_sf(stations)) * 0.0001
sat4 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/NDVI/S2B1C_20171015_010__NDVI_10.vrt")
sat4 <- raster::crop(sat4, st_as_sf(stations)) * 0.0001
sat_2017 <- stack(sat2, sat3, sat4)
sat_2017 <-  calc(sat_2017, fun = mean, na.rm = TRUE)
writeRaster(sat_2017, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI/ndvi_2017", overwrite = TRUE)

# ----- 2018 -----
sat6 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/NDVI/S2B1C_20180523_010__NDVI_10.vrt")
sat6 <- raster::crop(sat6, st_as_sf(stations)) * 0.0001
sat7 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/NDVI/S2A1C_20181204_010__NDVI_10.vrt")
sat7 <- raster::crop(sat7, st_as_sf(stations)) * 0.0001
sat8 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/NDVI/S2A1C_20181204_010__NDVI_10.vrt")
sat8 <- raster::crop(sat8, st_as_sf(stations)) * 0.0001

sat_2018 <- stack(sat6, sat7, sat8)
sat_2018 <-  calc(sat_2018, fun = mean, na.rm = TRUE)
writeRaster(sat_2018, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI/ndvi_2018", overwrite = TRUE)

# ----- 2019 -----
sat10 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/NDVI/S2A2A_20190612_010__NDVI_10.vrt")
sat10 <- raster::crop(sat10, st_as_sf(research_area_outline)) * 0.0001
sat11 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/NDVI/S2A2A_20190811_010__NDVI_10.vrt")
sat11 <- raster::crop(sat11, st_as_sf(research_area_outline)) * 0.0001
sat_new <- raster("/Users/serpent/Desktop/NDVI/S2A2A_20190702_010__NDVI_10.vrt")
sat_new <- raster::crop(sat_new, st_as_sf(research_area_outline)) * 0.0001


sat_2019 <- stack(sat10, sat11, sat_new)
sat_2019 <-  calc(sat_2019, fun = mean, na.rm = TRUE)
writeRaster(sat_2019, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2019", overwrite = TRUE)

# ----- 2020 -----

sat13 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/NDVI/S2A2A_20200227_010__NDVI_10.vrt")
sat13 <- raster::crop(sat13, st_as_sf(stations)) * 0.0001
sat14 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/NDVI/S2A2A_20200527_010__NDVI_10.vrt")
sat14 <- raster::crop(sat14, st_as_sf(stations)) * 0.0001
sat15 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/NDVI/S2B2A_20200909_010__NDVI_10.vrt")
sat15 <- raster::crop(sat15, st_as_sf(stations)) * 0.0001
sat16 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/NDVI/S2B2A_20201009_010__NDVI_10.vrt")
sat16 <- raster::crop(sat16, st_as_sf(stations)) * 0.0001

sat_2020 <- stack(sat13, sat14, sat15, sat16)
sat_2020 <-  calc(sat_2020, fun = mean, na.rm = TRUE)
writeRaster(sat_2020, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI/ndvi_2020", overwrite = TRUE)

# ----- 2021 -----

sat17 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/NDVI/S2B2A_20210206_010__NDVI_10.vrt")
sat17 <- raster::crop(sat17, st_as_sf(stations)) * 0.0001
sat18 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/NDVI/S2B2A_20210427_010__NDVI_10.vrt")
sat18 <- raster::crop(sat18, st_as_sf(stations)) * 0.0001
sat19 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/NDVI/S2B2A_20210815_010__NDVI_10.vrt")
sat19 <- raster::crop(sat19, st_as_sf(stations)) * 0.0001
sat20 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/NDVI/S2A2A_20211228_010__NDVI_10.vrt")
sat20 <- raster::crop(sat20, st_as_sf(stations)) * 0.0001

sat_2021 <- stack(sat17, sat18, sat19, sat20)
sat_2021 <-  calc(sat_2021, fun = mean, na.rm = TRUE)
writeRaster(sat_2021, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI/ndvi_2021", overwrite = TRUE)

# ----- 2022 -----
sat22 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/NDVI/S2A2A_20220507_010__NDVI_10.vrt")
sat22 <- raster::crop(sat22, st_as_sf(stations)) * 0.0001
sat23 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/NDVI/S2A2A_20220726_010__NDVI_10.vrt")
sat23 <- raster::crop(sat23, st_as_sf(stations)) * 0.0001
sat24 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/NDVI/S2B2A_20221108_010__NDVI_10.vrt")
sat24 <- raster::crop(sat24, st_as_sf(stations)) * 0.0001

sat_2022 <- stack(sat22, sat23, sat24)
sat_2022 <-  calc(sat_2022, fun = mean, na.rm = TRUE)
writeRaster(sat_2022, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI/ndvi_2022", overwrite = TRUE)

#----- 2023 -----

sat25 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/NDVI/S2B2A_20230107_010__NDVI_10.vrt")
sat25 <- raster::crop(sat25, st_as_sf(stations)) * 0.0001
sat26 <- raster("/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/NDVI/S2B2A_20230626_010__NDVI_10.vrt")
sat26 <- raster::crop(sat26, st_as_sf(stations)) * 0.0001

sat_2023 <- stack(sat25, sat26)
sat_2023 <-  calc(sat_2023, fun = mean, na.rm = TRUE)
writeRaster(sat_2023, filename = "/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI/ndvi_2023", overwrite = TRUE)



