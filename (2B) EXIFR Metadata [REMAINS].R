############# Download relevant photos and extract meta data #############

## Note: This script accesses those images that are NOT YET downloaded from the original thesis set, i.e., 
## those that originate from any new JSON exports 

library(exifr) # exif tool
library(dplyr) # manipulation
library(Hmisc) # manipulation

setwd() # set WD to data source
dat.all <- read.csv("json_parsed_all_30.10.23.csv") # read json data 

## Keep only photos relevant for analysis
dat.all <- subset(dat.all, Group != "others") # exclude "others" (vehicles, unidentifiable, etc.)

## Keep only new images that had not been downloaded before  
dat.old <- list.files(path = "/Volumes/External/BSc Thesis/WildLive Photos", full.names = FALSE)
dat.all <- dat.all %>% subset(!(External_ID %in% dat.old)) # get new photos

## Prepare input df including URLs and External ID's
all_links <- distinct(dat.all, External_ID, .keep_all = TRUE)[, c("External_ID", "Link")] # get unique links

## Set WD to photo destination on external volume
setwd("/Volumes/External/BSc Thesis/WildLive Photos") 

## Loop to download photos to external hard drive and assign External ID's [ONLY RUN IF YOU ARE 100% CERTAIN YOU WANT TO RUN THIS !!!!]
lapply(seq_along(all_links$Link), function(i)
  tryCatch(download.file(all_links$Link[i], all_links$External_ID[i]),
  error = function(e) message("error occured")))

## Get source file for JPGs
jpg <- list.files(pattern = "*.JPG") # JPGs

## Read meta data for both file types 
jpg <- read_exif(jpg, tags = c("FileName", "FileType", "DateTimeOriginal", "CreateDate")) # read meta data

setwd("/Users/serpent/Documents/BSc/Thesis/Data") # set WD to data folder
write.csv(jpg, file = "jpg_metadata.csv", row.names = FALSE)

## Add Metadata to database
dat.all <- read.csv("json_parsed_all_30.10.23.csv") # read json data 
jpg <- read.csv("jpg_metadata.csv") # jpg metadata

identical(jpg$SourceFile,jpg$FileName) # true
identical(jpg$DateTimeOriginal,jpg$CreateDate) # true

jpg <- dplyr::select(jpg, FileName, DateTimeOriginal)
colnames(jpg) <- c("External_ID", "DateTimeOriginal")
dat.all <- Merge(dat.all, jpg, all = TRUE, id = ~ External_ID)

write.csv(dat.all, file = "json_parsed_with_jpg_timestamps_30.10.23.csv", row.names = FALSE)
