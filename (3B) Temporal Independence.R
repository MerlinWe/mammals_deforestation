########## Assessing temporal independence ##########

library(camtrapRdeluxe, lib.loc = "/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
setwd() # set WD

list.files()

con1 <- read.csv("species_consensus_v1.csv") # v1
con2 <- read.csv("species_consensus_v2.csv") # v2
con3 <- read.csv("species_consensus_v3.csv") # v3
con4 <- read.csv("species_consensus_v4.csv") # v4

## ----- Version 1 -----

con1 <- na.omit(con1, cols = c("DateTimeOriginal", "Species")) # remove missing time stamps and species level classification
rownames(con1) <- NULL

colnames(con1) <- c("External_ID", "Link", "Order", "Family", "Genus", "Species", "Trivial", "Station", "Camera", "n_indiv", "DateTimeOriginal")
con1$Classification <- paste(con1$Genus, con1$Species) # get Genus + Species as classification

con1$DateTimeOriginal <- as.POSIXct(con1$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S") # set format 
con1$n_indiv <- as.integer(con1$n_indiv) # set format 
con1 <- con1[ order(con1$DateTimeOriginal , decreasing = FALSE ),] # sort by time
con1 <- subset(con1, DateTimeOriginal > "2017-01-01 00:00:01") # exclude non-sense timestamps

## Keep only records that are at least 30 minutes after the last independent record of the same species
con1 <- assessTemporalIndependence(intable = con1,      
																	 deltaTimeComparedTo = "lastIndependentRecord", 
																	 columnOfInterest    = "Classification",
																	 cameraCol           = "Camera",
																	 stationCol          = "Station", 
																	 camerasIndependent  = FALSE, 
																	 minDeltaTime        = 30)
con1 <- con1[order(con1$DateTimeOriginal , decreasing = FALSE ), ] # sort by time

con1$Order[con1$Order == "Cattle or Human"] = "Artiodactyla" # repair cattle order
con1$delta.time.secs <- NULL # drop non-relevant column
con1$delta.time.days <- NULL
con1$delta.time.mins <- NULL
con1$delta.time.hours <- NULL

## ----- Version 2 -----

con2 <- na.omit(con2, cols = c("DateTimeOriginal", "Species")) # remove missing time stamps and species level classification
rownames(con2) <- NULL

colnames(con2) <- c("External_ID", "Link", "Order", "Family", "Genus", "Species", "Trivial", "Station", "Camera", "n_indiv", "DateTimeOriginal")
con2$Classification <- paste(con2$Genus, con2$Species) # get Genus + Species as classification

con2$DateTimeOriginal <- as.POSIXct(con2$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S") # set format 
con2$n_indiv <- as.integer(con2$n_indiv) # set format 
con2 <- con2[ order(con2$DateTimeOriginal , decreasing = FALSE ),] # sort by time
con2 <- subset(con2, DateTimeOriginal > "2017-01-01 00:00:01") # exclude non-sense timestamps

## Keep only records that are at least 30 minutes after the last independent record of the same species
con2 <- assessTemporalIndependence(intable = con2,      
																	 deltaTimeComparedTo = "lastIndependentRecord", 
																	 columnOfInterest    = "Classification",
																	 stationCol          = "Station", 
																	 cameraCol           = "Camera",
																	 camerasIndependent  = FALSE, 
																	 minDeltaTime        = 30)
con2 <- con2[order(con2$DateTimeOriginal , decreasing = FALSE ), ] # sort by time

con2$Order[con2$Order == "Cattle or Human"] = "Artiodactyla" # repair cattle order
con2$delta.time.secs <- NULL # drop non-relevant column
con2$delta.time.days <- NULL
con2$delta.time.mins <- NULL
con2$delta.time.hours <- NULL


## ----- Version 3 -----

con3 <- na.omit(con3, cols = c("DateTimeOriginal", "Species")) # remove missing time stamps and species level classification
rownames(con3) <- NULL

colnames(con3) <- c("External_ID", "Link", "Order", "Family", "Genus", "Species", "Trivial", "Station", "Camera", "n_indiv", "DateTimeOriginal")
con3$Classification <- paste(con3$Genus, con3$Species) # get Genus + Species as classification

con3$DateTimeOriginal <- as.POSIXct(con3$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S") # set format 
con3$n_indiv <- as.integer(con3$n_indiv) # set format 
con3 <- con3[ order(con3$DateTimeOriginal , decreasing = FALSE ),] # sort by time
con3 <- subset(con3, DateTimeOriginal > "2017-01-01 00:00:01") # exclude non-sense timestamps

## Keep only records that are at least 30 minutes after the last independent record of the same species
con3 <- assessTemporalIndependence(intable = con3,      
																	 deltaTimeComparedTo = "lastIndependentRecord", 
																	 columnOfInterest    = "Classification",
																	 stationCol          = "Station", 
																	 cameraCol           = "Camera",
																	 camerasIndependent  = FALSE, 
																	 minDeltaTime        = 30)
con3 <- con3[order(con3$DateTimeOriginal , decreasing = FALSE ), ] # sort by time

con3$Order[con3$Order == "Cattle or Human"] = "Artiodactyla" # repair cattle order
con3$delta.time.secs <- NULL # drop non-relevant column
con3$delta.time.days <- NULL
con3$delta.time.mins <- NULL
con3$delta.time.hours <- NULL

## ----- Version 4 -----

con4 <- na.omit(con4, cols = c("DateTimeOriginal", "Species")) # remove missing time stamps and species level classification
rownames(con4) <- NULL

colnames(con4) <- c("External_ID", "Link", "Order", "Family", "Genus", "Species", "Trivial", "Station", "Camera","n_indiv", "DateTimeOriginal")
con4$Classification <- paste(con4$Genus, con4$Species) # get Genus + Species as classification

con4$DateTimeOriginal <- as.POSIXct(con4$DateTimeOriginal, format = "%Y-%m-%d %H:%M:%S") # set format 
con4$n_indiv <- as.integer(con4$n_indiv) # set format 
con4 <- con4[ order(con4$DateTimeOriginal , decreasing = FALSE ),] # sort by time
con4 <- subset(con4, DateTimeOriginal > "2017-01-01 00:00:01") # exclude non-sense timestamps

## Keep only records that are at least 30 minutes after the last independent record of the same species
con4 <- assessTemporalIndependence(intable = con4,      
																	 deltaTimeComparedTo = "lastIndependentRecord", 
																	 columnOfInterest    = "Classification",
																	 stationCol          = "Station", 
																	 camera              = "Camera",
																	 camerasIndependent  = FALSE, 
																	 minDeltaTime        = 30)
con4 <- con4[order(con4$DateTimeOriginal , decreasing = FALSE ), ] # sort by time

con4$Order[con4$Order == "Cattle or Human"] = "Artiodactyla" # repair cattle order
con4$delta.time.secs <- NULL # drop non-relevant column
con4$delta.time.days <- NULL
con4$delta.time.mins <- NULL
con4$delta.time.hours <- NULL

## Write data files
write.csv(con1, "wildlive_data_con1.csv", row.names = FALSE) # write data 
write.csv(con2, "wildlive_data_con2.csv", row.names = FALSE) # write data 
write.csv(con3, "wildlive_data_con3.csv", row.names = FALSE) # write data 
write.csv(con4, "wildlive_data_con4.csv", row.names = FALSE) # write data 
