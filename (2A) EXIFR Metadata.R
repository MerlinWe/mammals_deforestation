############# Download relevant photos and extract meta data #############

library(exifr) # exif tool
library(dplyr) # manipulation

setwd() # set WD to data source
dat.all <- read.csv("json_parsed_all.csv") # read json data 

## Keep only mammals relevant for analysis
dat.all <- subset(dat.all, Group %in% c("Artiodactyla","Carnivora", "Cattle or Human","Lagomorpha", "Marsupialia",
                                        "Perissodactyla", "Primates", "Rodentia", "Xenarthra"))

## Prepare input df including URLs and External ID's
all_links <- distinct(dat.all, External_ID, .keep_all = TRUE)[, c("External_ID", "Link")] # get unique links

## Set WD to photo destination on external volume
setwd("/Volumes/External/WildLive Photos") 

## Loop to download photos and assign External ID's  [ DO NOT RUN !!!!]
# lapply(seq_along(all_links$Link), function(i)
#   tryCatch(download.file(all_links$Link[i], all_links$External_ID[i]),
#   error = function(e) message("error occured")))
 
## Get source file for JPGs and PNGs 
jpg <- list.files(pattern = "*.JPG") # JPGs
png <- list.files(pattern = "*.PNG") # PNGs

## Read meta data for both file types 
jpg <- read_exif(jpg, tags = c("FileName", "FileType", "DateTimeOriginal", "CreateDate")) # read meta data
png <- read_exif(png, tags = c("FileName", "FileType", "DateTimeOriginal", "CreateDate")) # read meta data

setwd("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Data") # set WD to data folder
# write.csv(jpg, file = "jpg_metadata.csv", row.names = FALSE)
# write.csv(png, file = "png_metadata.csv", row.names = FALSE)

## Add Metadata to database
dat.all <- read.csv("json_parsed_all.csv") # read json data 
jpg <- read.csv("jpg_metadata.csv") # jpg metadata
# png <- read.csv("png_metadata.csv")# png metadata --- wrong time stanmps !! ---

identical(jpg$SourceFile,jpg$FileName)
identical(jpg$DateTimeOriginal,jpg$CreateDate)
jpg <- dplyr::select(jpg, FileName, DateTimeOriginal)
colnames(jpg) <- c("External_ID", "DateTimeOriginal")
dat.all <- Merge(dat.all, jpg, all = TRUE, id = ~ External_ID)

write.csv(dat.all, file = "json_parsed_with_jpg_mammals_timestamps.csv", row.names = FALSE)

## DateTimeOriginalof PNGs is NOT correct!!!

# identical(png$SourceFile,png$FileName)
# png <- dplyr::select(png, FileName, DateTimeOriginal)
# colnames(png) <- c("External_ID", "DateTimeOriginal")
# dat.all <- Merge(dat.all, jpg, all = TRUE, id = ~ External_ID)



