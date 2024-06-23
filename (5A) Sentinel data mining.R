#############################################################################################
#################### (4) Thesis: GIS data mining & preparation ##############################
#############################################################################################

library(sen2r)     # Access sentinel data 
library(raster)    # GIS stuff
library(rgdal)     # GIS stuff
library(rgeos)     # GIS stuff
library(sf)        # GIS stuff
library(sp)        # GIS stuff
library(tidyverse) # Tidyverse


setwd("...") # set working directory

## Read mammal data 
wildlive <- read.csv("/Users/serpent/Documents/BSc/Thesis/Data/wildlive_data_30.10.23.csv") # read species data
wildlive$DateTimeOriginal <- as.POSIXct(wildlive$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S") # set format

## -------- (1) Create research area outline and a buffer zone around every station --------

camtraps <- read.csv("/Users/serpent/Documents/BSc/Thesis/Data/camtraps.csv") # read camtrap meta data 

wildlive$Station <- gsub(" ", "", wildlive$Station) # remove any white spaces from species data
camtraps$Station <- gsub(" ", "", camtraps$Station) # remove any white spaces from camtrap data 

camtraps <- dplyr::distinct(camtraps, Station, .keep_all = TRUE) # disregard cameras per station 


## Briefly check the sampling period for which sentinel data is required (in month): 

wildlive %>% 
  dplyr::group_by(paste(format(as.Date(wildlive$DateTimeOriginal), format = "%Y"), 
                 format(as.Date(wildlive$DateTimeOriginal), format = "%m"), sep = "/")) %>% 
  dplyr::summarise(captures = n()) %>%
	rename(month = `paste(...)`) %>%
	ggplot() + 
	geom_bar(aes(x=month, y = captures), stat = "identity") +
	theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, margin = margin(b=20))) +
	xlab(NULL)
	
## First we make sure R understands that the current coordinates are in lat long 
camtraps_clean <- dplyr::select(camtraps, Station, lat, long)

## We need to create 2 buffer zones: One large buffer zone around the entire research area that serves as the 
## map extent that is used to access the sentinel data, and one shape file that contains the buffer zone around 
## every station that is used to calculate the specific covariates per station. 

## Convert camtrap coords to a simple feature 
camtraps_sf <- st_as_sf(camtraps_clean, coords = c("long", "lat"), crs = 4326) # sf object

## Second: Research area buffer zone. We generate a minimum bounding box (MBR) around all 
## station coordinates that describes the area that all points cover together

mbr_polygon <- st_make_grid(st_bbox(camtraps_sf), 
                            cellsize = c(st_bbox(camtraps_sf)[3]-st_bbox(camtraps_sf)[1],
                                         st_bbox(camtraps_sf)[4]-st_bbox(camtraps_sf)[2]), 
                            what = "polygons")

## Now we convert the bounding box to a polygon and extend it by 10km 
research_area_outline <- st_buffer(mbr_polygon, dist = 10000)

## Set Projections: The research area outline as well as the station buffers need to match the projection 
## of the sentinel2 files that we are going to retrieve next, so it may be used as a map extent. 

research_area_outline <- st_as_sf(research_area_outline) # set format of the outline to simple feature (sf)
research_area_outline <- st_transform(research_area_outline, # match projections
                                      crs = st_crs("+proj=utm +zone=20 +south +datum=WGS84 +units=m +no_defs")) 
research_area_outline <- st_as_sfc(st_bbox(research_area_outline)) # simplify extent 
plot(research_area_outline) # plot outline 


## Write research area shapefile & station buffer with 500m radius
st_write(research_area_outline, dsn = "/Users/serpent/Documents/BSc/Thesis/GIS/research_area_10km.shp", delete_layer = TRUE)
rm(camtraps_sf, mbr_polygon) # remove redundant data

## -------- (2) Access sentinel 2 satellite data (only once) --------

## Check availability of cloudless sentinel 2 images with the research area extent and minimum cloud coverage.
## Then download per sensing period and write in respective directory
## Note: we need 4 tiles to cover the the entire research area which have the same date and orbit!!

# sat1 = Jan17 - Mar17
# sat1 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2017-01-01", "2017-03-31"))))
# sat1 <- sat1[21:24, ] # keep only rows with the lowest cloud perc per tile sensing
# rownames(sat1) <- NULL # reset rownames
# s2_download(s2_prodlist = sat1, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1")
# sat1$sat <- "sat1" # add sat id column

## sat2 = Apr17 - Jun17
# sat2 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2017-04-01", "2017-06-30"))))
# sat2 <- sat2 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1) # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat2, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2")
# sat2$sat <- "sat2" # add sat id column

## sat3 = Jul17 - Sep17
# sat3 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2017-06-30", "2017-09-30"))))
# sat3 <- sat3 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1) # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat3, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3")
# sat3$sat <- "sat3" # add sat id column

# sat4 = Oct17 - Dec17
# sat4 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2017-09-30", "2017-12-31"))))
# sat4 <- sat4[5:8, ] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat4, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4")
# sat4$sat <- "sat4" # add sat id column
# 
# # sat5 = Jan18 - Mar18
# sat5 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2018-01-01", "2018-03-31"))))
# sat5 <- sat5[65:68,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat5, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5")
# sat5$sat <- "sat5" # add sat id column
# 
# sat6 = Apr18 - Jun18
# sat6 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2018-04-01", "2018-06-30"))))
# sat6 <- sat6 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1) # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat6, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6")
# sat6$sat <- "sat6" # add sat id column
# 
# # sat7 = Jul18 - Sep18
# sat7 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2018-06-30", "2018-09-30"))))
# sat7 <- sat7[21:24,]  # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat7, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7")
# sat7$sat <- "sat7" # add sat id column
# 
# # sat8 = Oct18 - Dec18
# sat8 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2018-09-30", "2018-12-31"))))
# sat8 <- sat8[55:58,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat8, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8")
# sat8$sat <- "sat8" # add sat id column

# # sat9 = Jan19 - Mar19
# sat9 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2019-01-01", "2019-03-31"))))
# sat9 <- sat9[1:4, ] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat9, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9")
# sat9$sat <- "sat9" # add sat id column
# 
# sat10 = Apr19 - Jun19
# sat10 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2019-04-01", "2019-06-30"))))
# sat10 <- sat10[57:60,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat10, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10")
# sat10$sat <- "sat10" # add sat id column
# 
# # sat11 = Jul19 - Sep19
# sat11 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2019-06-30", "2019-09-30"))))
# sat11 <- sat11[33:36,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat11, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11")
# sat11$sat <- "sat11" # add sat id column
# 
# # sat12 = Oct19 - Dec19
# sat12 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2019-09-30", "2019-12-31"))))
# sat12 <- sat12[29:32,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat12, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12")
# sat12$sat <- "sat12" # add sat id column
# 
# sat13 = Jan20 - Mar20
# sat13 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2020-01-01", "2020-03-31"))))
# sat13 <- sat13[45:48,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat13, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13")
# sat13$sat <- "sat13" # add sat id column
# 
# # sat14 = Apr20 - Jun20
# sat14 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2020-04-01", "2020-06-30"))))
# sat14 <- sat14 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1) # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat14, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14")
# sat14$sat <- "sat14" # add sat id column
# 
# # sat15 = Jul20 - Sep20
# sat15 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2020-06-30", "2020-09-30"))))
# sat15 <- sat15[53:56,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat15, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15")
# sat15$sat <- "sat15" # add sat id column
# 
# # sat16 = Oct20 - Dec20
# sat16 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2020-09-30", "2020-12-31"))))
# sat16 <- sat16[5:8,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat16, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16")
# sat16$sat <- "sat16" # add sat id column
# 
# # sat17 = Jan21 - Mar21
# sat17 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2021-01-01", "2021-03-31"))))
# sat17 <- sat17 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1) # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat17, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17")
# sat17$sat <- "sat17" # add sat id column
# 
# # sat18 = Apr21 - Jun21
# sat18 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2021-04-01", "2021-06-30"))))
# sat18 <- sat18[21:24,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat18, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18")
# sat18$sat <- "sat18" # add sat id column
# 
# # sat19 = Jul21 - Sep21
# sat19 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2021-06-30", "2021-09-30"))))
# sat19 <- sat19[39:42,] # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat19, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19")
# sat19$sat <- "sat19" # add sat id column
# 
# # # sat20 = Oct21 - Dec21
# sat20 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2021-09-30", "2021-12-31"))))
# sat20 <- sat20 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1) # keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat20, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20")
# sat20$sat <- "sat20" # add sat id column

# sat21 = Jan22 - Mar22
# sat21 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                            time_interval = as.Date(c("2022-01-01", "2022-03-31"))))
# sat21 <- sat21[c(5:8) , ]# keep lowest cloud percentage per tile
# s2_download(s2_prodlist = sat21, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21")
# sat21$sat <- "sat21" # add sat id column

# sat22 = Apr22 - Jun22
# sat22 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                             time_interval = as.Date(c("2022-04-01", "2022-06-30"))))
# sat22 <- sat22 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1)
# s2_download(s2_prodlist = sat22, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22")
# sat22$sat <- "sat22" # add sat id column

# sat23 = Jul22 - Sep22
# sat23 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                             time_interval = as.Date(c("2022-07-01", "2022-09-30"))))
# sat23 <- sat23[c(21:24), ]
# s2_download(s2_prodlist = sat23, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23")
# sat23$sat <- "sat23" # add sat id column

# sat24 = Oct22 - Dec22
# sat24 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                             time_interval = as.Date(c("2022-10-01", "2022-12-31"))))
# sat24 <- sat24[c(29:32), ]
# s2_download(s2_prodlist = sat24, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24")
# sat24$sat <- "sat24" # add sat id column

# sat25 = Jan23 - Mar23
# sat25 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                             time_interval = as.Date(c("2023-01-01", "2023-03-30"))))
# sat25 <- sat25 %>% group_by(id_tile) %>% arrange(clouds) %>% filter(row_number() == 1)
# s2_download(s2_prodlist = sat25, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25")
# sat25$sat <- "sat25" # add sat id column

# sat26 = Apr23 - Jun23
# sat26 <- data.frame(s2_list(spatial_extent = research_area_outline, # get available images
#                             time_interval = as.Date(c("2023-04-01", "2023-06-30"))))
# sat26 <- sat26[c(65:68), ]
# s2_download(s2_prodlist = sat26, order_lta = TRUE, abort = FALSE, overwrite = TRUE, # download
#             outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26")
# sat26$sat <- "sat26" # add sat id column

# ## Gather sat data and export as metadata table
# sat_list <- data.frame(rbind(sat1, sat2, sat3, sat4, sat5, sat6, sat7, sat8, sat9, sat10, sat11,
#                              sat12, sat14, sat14, sat15, sat16, sat17, sat18, sat19, sat20, sat21,
#                              sat22, sat23, sat24, sat25, sat26))
# write.csv(sat_list, file ="sat_metadata.csv", row.names = FALSE) # write metadata table
# rm(sat1, sat2, sat3, sat4, sat5, sat6, sat7, sat8, sat9, sat10, sat11, sat12, sat13, sat14,
#    sat15, sat16, sat17, sat18, sat19, sat20) # remove redundant data

## -------- (3) Process sentinel 2 satellite data (only once) --------

## Note: We have 4 tiles for every sensing period (sat1-sat20). The first step to translate the raw SAFE files to .vrt files.
## Then, we merge the files of a sensing period together, so we can then clip it to the research area and calculate the NDVI.
## We need to do this separately for every period (sat1 - sat20). We first translate the raw SAFE files to a .vrt file,
## then clip it to the research area extent and calculate the NDVI.

## First: Prepare all files:

## ~~~~~ sat1 (01/17 - 03/17) ~~~~~

# First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/S2A_MSIL1C_20170314T142031_N0204_R010_T20KNG_20170314T142035.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/S2A_MSIL1C_20170314T142031_N0204_R010_T20KPG_20170314T142035.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/S2A_MSIL1C_20170314T142031_N0204_R010_T20LNH_20170314T142035.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/S2A_MSIL1C_20170314T142031_N0204_R010_T20LPH_20170314T142035.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/processed")
# 
# # # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/merged",
#          overwrite = TRUE)

# ## ~~~~~ sat2 (04/17 - 06/17) ~~~~~
#
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/S2A_MSIL1C_20170602T142041_N0205_R010_T20KNG_20170602T142039.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/S2A_MSIL1C_20170602T142041_N0205_R010_T20KPG_20170602T142039.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/S2A_MSIL1C_20170602T142041_N0205_R010_T20LNH_20170602T142039.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/S2A_MSIL1C_20170602T142041_N0205_R010_T20LPH_20170602T142039.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/processed")
#
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/merged",
#          overwrite = TRUE)
# #
# ## ~~~~~ sat3 (07/17 - 09/17) ~~~~~
#
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/S2A_MSIL1C_20170712T142041_N0205_R010_T20KNG_20170712T142710.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/S2A_MSIL1C_20170712T142041_N0205_R010_T20KPG_20170712T142710.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/S2A_MSIL1C_20170712T142041_N0205_R010_T20LNH_20170712T142710.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/S2A_MSIL1C_20170712T142041_N0205_R010_T20LPH_20170712T142710.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/processed")
#
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/merged",
#          overwrite = TRUE)
#
# ## ~~~~~ sat4 (10/17 - 12/17) ~~~~~
#
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/S2B_MSIL1C_20171015T142029_N0205_R010_T20KNG_20171015T142023.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/S2B_MSIL1C_20171015T142029_N0205_R010_T20KPG_20171015T142023.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/S2B_MSIL1C_20171015T142029_N0205_R010_T20LNH_20171015T142023.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/S2B_MSIL1C_20171015T142029_N0205_R010_T20LPH_20171015T142023.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/processed")
#
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/merged",
#          overwrite = TRUE)
#
# ## ~~~~~ sat5 (01/18 - 03/18) ~~~~~
#
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/S2A_MSIL1C_20180329T142041_N0206_R010_T20KNG_20180329T174533.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/S2A_MSIL1C_20180329T142041_N0206_R010_T20KPG_20180329T174533.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/S2A_MSIL1C_20180329T142041_N0206_R010_T20LPH_20180329T174533.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/S2A_MSIL1C_20180329T142041_N0206_R010_T20LNH_20180329T174533.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/processed")
#
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/merged",
#          overwrite = TRUE)
#
## ~~~~~ sat6 (04/18 - 06/18) ~~~~~

# # # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/S2B_MSIL1C_20180523T142039_N0206_R010_T20KNG_20180523T185544.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/S2B_MSIL1C_20180523T142039_N0206_R010_T20KPG_20180523T185544.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/S2B_MSIL1C_20180523T142039_N0206_R010_T20LNH_20180523T185544.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/S2B_MSIL1C_20180523T142039_N0206_R010_T20LPH_20180523T185544.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/processed")
# # 
# # # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/merged",
#          overwrite = TRUE)
# # 
# ## ~~~~~ sat7 (07/18 - 09/18) ~~~~~
# # 
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/S2A_MSIL1C_20180727T142041_N0206_R010_T20KNG_20180727T180405.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/S2A_MSIL1C_20180727T142041_N0206_R010_T20KPG_20180727T180405.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/S2A_MSIL1C_20180727T142041_N0206_R010_T20LNH_20180727T180405.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/S2A_MSIL1C_20180727T142041_N0206_R010_T20LPH_20180727T180405.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/merged",
#          overwrite = TRUE)

# ## ~~~~~ sat8 (10/18 - 12/18) ~~~~~
# 
# First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/S2A_MSIL1C_20181204T142031_N0207_R010_T20KNG_20181204T160243.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/S2A_MSIL1C_20181204T142031_N0207_R010_T20KPG_20181204T160243.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/S2A_MSIL1C_20181204T142031_N0207_R010_T20LNH_20181204T160243.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/S2A_MSIL1C_20181204T142031_N0207_R010_T20LPH_20181204T160243.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/processed")
# 
# # # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/merged",
#          overwrite = TRUE)

# ## ~~~~~ sat9 (01/19 - 03/19) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/S2A_MSIL2A_20190103T142031_N0211_R010_T20KNG_20190103T164448.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/S2A_MSIL2A_20190103T142031_N0211_R010_T20KPG_20190103T164448.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/S2A_MSIL2A_20190103T142031_N0211_R010_T20LNH_20190103T164448.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/S2A_MSIL2A_20190103T142031_N0211_R010_T20LPH_20190103T164448.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/merged",
#          overwrite = TRUE)
# # 
# ## ~~~~~ sat10 (04/19 - 06/19) ~~~~~

# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/S2A_MSIL2A_20190612T142041_N0212_R010_T20KNG_20190612T163304.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/S2A_MSIL2A_20190612T142041_N0212_R010_T20KPG_20190612T163304.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/S2A_MSIL2A_20190612T142041_N0212_R010_T20LNH_20190612T163304.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/S2A_MSIL2A_20190612T142041_N0212_R010_T20LPH_20190612T163304.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/merged",
#          overwrite = TRUE)

# ## ~~~~~ sat11 (07/19 - 09/19) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/S2A_MSIL2A_20190811T142041_N0213_R010_T20KNG_20190811T163606.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/S2A_MSIL2A_20190811T142041_N0213_R010_T20KPG_20190811T163606.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/S2A_MSIL2A_20190811T142041_N0213_R010_T20LNH_20190811T163606.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/S2A_MSIL2A_20190811T142041_N0213_R010_T20LPH_20190811T163606.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/processed") 
# 
# # Second: Merge tiles and safe in new directory 
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/merged", 
#          overwrite = TRUE)
# 
# ## ~~~~~ sat12 (10/19 - 12/19) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/S2B_MSIL2A_20191104T141739_N0213_R010_T20KNG_20191104T164129.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/S2B_MSIL2A_20191104T141739_N0213_R010_T20KPG_20191104T164129.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/S2B_MSIL2A_20191104T141739_N0213_R010_T20LNH_20191104T164129.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/S2B_MSIL2A_20191104T141739_N0213_R010_T20LPH_20191104T164129.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/processed") 
# 
# # Second: Merge tiles and safe in new directory 
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/merged", 
#          overwrite = TRUE)
# 
# # ## ~~~~~ sat13 (01/20 - 03/20) ~~~~~
# # 
# First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/S2A_MSIL2A_20200227T141731_N0214_R010_T20KNG_20200227T185707.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/S2A_MSIL2A_20200227T141731_N0214_R010_T20KPG_20200227T185707.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/S2A_MSIL2A_20200227T141731_N0214_R010_T20LNH_20200227T185707.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/S2A_MSIL2A_20200227T141731_N0214_R010_T20LPH_20200227T185707.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/merged",
#          overwrite = TRUE)
# # 
# ## ~~~~~ sat14 (04/20 - 06/20) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/S2A_MSIL2A_20200527T141741_N0214_R010_T20KNG_20200527T182427.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/S2A_MSIL2A_20200527T141741_N0214_R010_T20KPG_20200527T182427.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/S2A_MSIL2A_20200527T141741_N0214_R010_T20LNH_20200527T182427.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/processed") 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/S2A_MSIL2A_20200527T141741_N0214_R010_T20LPH_20200527T182427.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/processed") 
# 
# # Second: Merge tiles and safe in new directory 
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/merged", 
#          overwrite = TRUE)
# 
## ~~~~~ sat15 (07/20 - 09/20) ~~~~~

# #First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/S2B_MSIL2A_20200909T141739_N0214_R010_T20KNG_20200909T180346.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/S2B_MSIL2A_20200909T141739_N0214_R010_T20KPG_20200909T180346.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/S2B_MSIL2A_20200909T141739_N0214_R010_T20LNH_20200909T180346.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/S2B_MSIL2A_20200909T141739_N0214_R010_T20LPH_20200909T180346.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat16 (10/20 - 12/20) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/S2B_MSIL2A_20201009T141739_N0214_R010_T20LNH_20201009T164331.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/S2B_MSIL2A_20201009T141739_N0214_R010_T20KNG_20201009T164331.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/S2B_MSIL2A_20201009T141739_N0214_R010_T20LPH_20201009T164331.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/S2B_MSIL2A_20201009T141739_N0214_R010_T20KPG_20201009T164331.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat17 (01/21 - 03/21) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/S2B_MSIL2A_20210206T141739_N0214_R010_T20KNG_20210206T164644.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/S2B_MSIL2A_20210206T141739_N0214_R010_T20KPG_20210206T164644.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/S2B_MSIL2A_20210206T141739_N0214_R010_T20LNH_20210206T164644.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/S2B_MSIL2A_20210206T141739_N0214_R010_T20LPH_20210206T164644.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat18 (04/21 - 06/21) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/S2B_MSIL2A_20210427T141729_N0300_R010_T20KNG_20210427T164150.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/S2B_MSIL2A_20210427T141729_N0300_R010_T20KPG_20210427T164150.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/S2B_MSIL2A_20210427T141729_N0300_R010_T20LNH_20210427T164150.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/S2B_MSIL2A_20210427T141729_N0300_R010_T20LPH_20210427T164150.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat19 (07/21 - 09/21) ~~~~~
# 
# # First: translate raw SAFE images to .vrt format
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/S2B_MSIL2A_20210815T141739_N0301_R010_T20KNG_20210815T164429.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/S2B_MSIL2A_20210815T141739_N0301_R010_T20KPG_20210815T164429.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/S2B_MSIL2A_20210815T141739_N0301_R010_T20LNH_20210815T164429.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/S2B_MSIL2A_20210815T141739_N0301_R010_T20LPH_20210815T164429.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat20 (10/21 - 12/21) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/S2A_MSIL2A_20211228T141741_N0301_R010_T20LPH_20211228T165755.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/S2A_MSIL2A_20211228T141741_N0301_R010_T20LNH_20211228T165755.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/S2A_MSIL2A_20211228T141741_N0301_R010_T20KPG_20211228T165755.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/S2A_MSIL2A_20211228T141741_N0301_R010_T20KNG_20211228T165755.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/processed")
#
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/merged",
#          overwrite = TRUE)

# ## ~~~~~ sat21 (01/22 - 03/22) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/S2A_MSIL2A_20220107T141741_N0301_R010_T20KNG_20220107T164306.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/S2A_MSIL2A_20220107T141741_N0301_R010_T20KPG_20220107T164306.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/S2A_MSIL2A_20220107T141741_N0301_R010_T20LNH_20220107T164306.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/S2A_MSIL2A_20220107T141741_N0301_R010_T20LPH_20220107T164306.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat22 (04/22 - 06/22) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/S2A_MSIL2A_20220507T141741_N0400_R010_T20KNG_20220507T202317.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/S2A_MSIL2A_20220507T141741_N0400_R010_T20KPG_20220507T202317.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/S2A_MSIL2A_20220507T141741_N0400_R010_T20LNH_20220507T202317.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/S2A_MSIL2A_20220507T141741_N0400_R010_T20LPH_20220507T202317.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat23 (07/22 - 09/22) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/S2A_MSIL2A_20220726T141751_N0400_R010_T20KNG_20220726T224659.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/S2A_MSIL2A_20220726T141751_N0400_R010_T20KPG_20220726T224659.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/S2A_MSIL2A_20220726T141751_N0400_R010_T20LNH_20220726T224659.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/S2A_MSIL2A_20220726T141751_N0400_R010_T20LPH_20220726T224659.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat23 (10/22 - 12/22) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/S2B_MSIL2A_20221108T141709_N0400_R010_T20KNG_20221108T181517.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/S2B_MSIL2A_20221108T141709_N0400_R010_T20KPG_20221108T181517.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/S2B_MSIL2A_20221108T141709_N0400_R010_T20LNH_20221108T181517.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/S2B_MSIL2A_20221108T141709_N0400_R010_T20LPH_20221108T181517.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat25 (01/23 - 03/23) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/S2B_MSIL2A_20230107T141709_N0509_R010_T20KNG_20230107T164611.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/S2B_MSIL2A_20230107T141709_N0509_R010_T20KPG_20230107T164611.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/S2B_MSIL2A_20230107T141709_N0509_R010_T20LNH_20230107T164611.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/S2B_MSIL2A_20230107T141709_N0509_R010_T20LPH_20230107T164611.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/merged",
#          overwrite = TRUE)
# 
# ## ~~~~~ sat26 (04/23 - 06/23) ~~~~~
# 
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/S2B_MSIL2A_20230626T141719_N0509_R010_T20KNG_20230626T182310.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/S2B_MSIL2A_20230626T141719_N0509_R010_T20KPG_20230626T182310.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/S2B_MSIL2A_20230626T141719_N0509_R010_T20LNH_20230626T182310.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/processed")
# s2_translate(infile = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/S2B_MSIL2A_20230626T141719_N0509_R010_T20LPH_20230626T182310.SAFE",
#              outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/processed")
# 
# # Second: Merge tiles and safe in new directory
# files <- as.character(list.files("/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/processed", full.names = TRUE))
# s2_merge(infiles = files, # merge .vrt files in processed directory
#          outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/merged",
#          overwrite = TRUE)

# ## -------- (4) Calculate NDVI --------
# 
# ## sat1 = 01/17 - 03/17
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/merged/S2A1C_20170314_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat1/NDVI",
#                indices = "NDVI")

# ## sat2 = 04/17 - 06/17
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/merged/S2A1C_20170602_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat2/NDVI_Int32",
#                indices = "NDVI", overwrite = TRUE)

# ## sat3 = 07/17 - 09/17
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/merged/S2A1C_20170712_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat3/NDVI",
#                indices = "NDVI")
# 
# ## sat4 = 10/17 - 12/17
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/merged/S2B1C_20171015_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat4/NDVI",
#                indices = "NDVI")
# 
# ## sat5 = 01/18 - 03/18
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/merged/S2A1C_20180329_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat5/NDVI",
#                indices = "NDVI")
# 
# ## sat6 = 04/18 - 06/18
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/merged/S2B1C_20180523_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat6/NDVI",
#                indices = "NDVI", overwrite = TRUE)
#
# # ## sat7 = 07/18 - 09/18
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/merged/S2A1C_20180727_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat7/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# # ## sat8 = 10/18 - 12/18
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/merged/S2A1C_20181204_010__TOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat8/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat9 = 01/19 - 03/19
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/merged/S2A2A_20190103_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat9/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat10 = 04/19 - 06/19
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/merged/S2A2A_20190612_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat10/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat11 = 07/19 - 09/19
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/merged/S2A2A_20190811_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat11/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat12 = 10/19 - 12/19
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/merged/S2B2A_20191104_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat12/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat13 = 01/20 - 03/20
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/merged/S2A2A_20200227_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat13/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat14 = 04/20 - 06/20
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/merged/S2A2A_20200527_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat14/NDVI",
#                indices = "NDVI", overwrite = TRUE)
# 
# ## sat15 = 07/20 - 09/20
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/merged/S2B2A_20200909_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat15/NDVI",
#                indices = "NDVI", overwrite = TRUE)
# 
# # ## sat16 = 10/20 - 12/20
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/merged/S2B2A_20201009_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat16/NDVI",
#                indices = "NDVI", overwrite = TRUE)
# 
# ## sat17 = 01/21 - 03/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/merged/S2B2A_20210206_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat17/NDVI",
#                indices = "NDVI", overwrite = TRUE)
# 
# ## sat18 = 04/21 - 06/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/merged/S2B2A_20210427_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat18/NDVI",
#                indices = "NDVI", overwrite = TRUE)
# 
# ## sat19 = 07/21 - 09/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/merged/S2B2A_20210815_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat19/NDVI",
#                indices = "NDVI", overwrite = TRUE)
# 
# ## sat20 = 10/21 - 12/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/merged/S2A2A_20211228_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat20/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat21 = 01/22 - 03/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/merged/S2A2A_20220107_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat21/NDVI",
#                indices = "NDVI", overwrite = TRUE)

## sat22 = 04/22 - 06/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/merged/S2A2A_20220507_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat22/NDVI",
#                indices = "NDVI", overwrite = TRUE)

## sat23 = 07/22 - 09/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/merged/S2A2A_20220726_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat23/NDVI",
#                indices = "NDVI", overwrite = TRUE)

## sat24 = 10/22 - 12/21
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/merged/S2B2A_20221108_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat24/NDVI",
#                indices = "NDVI", overwrite = TRUE)

# ## sat25 = 01/23 - 03/23
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/merged/S2B2A_20230107_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat25/NDVI",
#                indices = "NDVI", overwrite = TRUE)

## sat26 = 04/23 - 06/23
# s2_calcindices(infiles = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/merged/S2B2A_20230626_010__BOA_10.vrt",
#                outdir = "/Volumes/External/BSc Thesis/Sentinel2 Data/sat26/NDVI",
#                indices = "NDVI", overwrite = TRUE)

