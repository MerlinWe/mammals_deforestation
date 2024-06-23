##### Camera operation ##########

library(camtrapR)  # camtrapR
library(tidyverse) # Tidyverse
library(raster)    # GIS stuff
library(rgdal)     # GIS stuff
library(rgeos)     # GIS stuff
library(sf)        # GIS stuff
library(sp)        # GIS stuff

camtraps <- read.csv("...")


## Make sure formats are understood appropriately 
camtraps$Setup_date <- as.Date(camtraps$Setup_date, format = "%Y-%m-%d")         # set formats
camtraps$Retrieval_date <- as.Date(camtraps$Retrieval_date, format = "%Y-%m-%d") # set formats
for (col in names(camtraps)) { ## Format dates of problem columns
	if (grepl("^Problem", col)) {
		camtraps[[col]] <- as.Date(camtraps[[col]], format = "%Y-%m-%d")
	}
}

# Build camera operation table 
camop_problem <- cameraOperation(CTtable      = camtraps,
																 stationCol   = "Station",
																 cameraCol    = "Camera", 
																 setupCol     = "Setup_date",
																 retrievalCol = "Retrieval_date",
																 hasProblems  = TRUE,
																 byCamera     = FALSE,
																 allCamsOn    = FALSE,
																 camerasIndependent = FALSE,
																 dateFormat   = "%Y-%m-%d")

# Plot camera operation table
camopPlot <- function(camOp, 
											palette = "Heat"){
	
	which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
	label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
	at.tmp <- which.tmp / ncol(camOp)
	
	values_tmp <- sort(na.omit(unique(c(camOp))))

	if(getRversion() >= "3.6.0") {
		image_colors <- grDevices::hcl.colors(n = length(values_tmp), palette = palette, rev = TRUE)
	} else {
		image_colors <- heat.colors(n = length(values_tmp), rev = TRUE)
	}
	
	image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = image_colors)
	axis(1, at = at.tmp, labels = label.tmp)
	axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
	abline(v = at.tmp, col = rgb(0,0,0, 0.2))
	box()
	
}

camopPlot(camOp = camop_problem) # plot camp-op

## sampling history

# recordTable <- read.csv("/Users/serpent/Documents/BSc/Thesis/Data/wildlive_data_30.10.23.csv")
# detach("package:camtrapRdeluxe", unload = TRUE)
# camop_problem[camop_problem < 0] <- NA
# camtrapR::surveyReport(recordTable = recordTable,
# 											 CTtable = camtraps,
# 											 camOp = camop_problem,
# 											 speciesCol = "Classification",
# 											 stationCol = "Station",
# 											 cameraCol = "Camera",
# 											 setupCol = "Setup_date",
# 											 retrievalCol = "Retrieval_date",
# 											 CTDateFormat = "%Y-%m-%d",
# 											 recordDateTimeCol = "DateTimeOriginal",
# 											 recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
# 											 Xcol = "long",
# 											 Ycol = "lat",
# 											 sinkpath = "/Users/serpent/Documents/BSc/Thesis/Data/Survey Report",
# 											 makezip = TRUE)


## Import camtrap data and sync station names & coordinates with wildlive data
camtraps <- read.csv("/Users/serpent/Documents/BSc/Thesis/Data/camtraps.csv", na.strings = c("", " ", "NA") ) # read camtrap meta data 

## Set correct spatial projections 
camtraps_sf <- camtraps %>% dplyr::select(Station, lat, long) %>% distinct(Station, .keep_all = TRUE)
camtraps_sf <- st_as_sf(camtraps_sf, coords = c("long", "lat"), crs = 4326) # sf object

## Build buffer zone. Use 1.5km radius (2 would be better, see Semper-Pascual et al. (2023), 
## but that would create too much overlap between buffer zones). 

station_buffer_1500 <- st_buffer(camtraps_sf, dist = 1500) # buffer zone 
station_buffer_1500 <- st_as_sf(station_buffer_1500) # set format & match projection
station_buffer_1500 <- st_transform(station_buffer_1500, crs = st_crs("+proj=utm +zone=20 +south +datum=WGS84 +units=m +no_defs")) 

plot(station_buffer_1500, main = "buffer zones") # plot station buffers & write file
st_write(station_buffer_1500, dsn = "/Users/serpent/Documents/BSc/Thesis/GIS/station_buffer_1500m.shp", delete_layer = TRUE)
rm(camtraps_sf, station_buffer_1500) # remove redundant data

