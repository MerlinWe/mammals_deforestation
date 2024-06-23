#################################################################################################
#################### (5) Thesis: Analysis - Multi-species occupancy modelling ###################
#################################################################################################

# see: https://cran.r-project.org/web/packages/camtrapR/vignettes/camtrapr5.html

library(tidyverse) # data manipulation
library(camtrapR)  # camtrapR 
library(purrr)     # data preparation
library(DT)        # data manipulation
library(Hmisc)     # data wrangling
library(knitr)     # reports 

setwd("/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Data") # set WD

## ~~~~~~~~~~  Prepare camtrap data ~~~~~~~~~~ 

## About the structure: We have 11 stations and 16 environmental measurements for each stations. 
## Thus, we consider every combination of station and environmental measurement one session. 
## This adds up to 176 sessions. 

camtraps <- read.csv("camtraps.csv") # read camtrap file
camtraps <- subset(camtraps, NewGrid == "y") # keep only grid traps
camtraps <- distinct(camtraps, Station, .keep_all = TRUE) # keep only distinct stations 
camtraps$Station <- str_trim(camtraps$Station, side = "both") # remove white spaces 

forest  <- read.csv("station_covariates_new.csv") # read deforestation covariates
forest$station_period <- paste(forest$station, forest$period, sep = "_") # merge station + period
colnames(forest) <- c("Station", "forcov", "foredg", "period", "station_period") # set column names 

camtraps <- Merge(forest, camtraps, id = ~ Station) # merge camtrap data and covariates
camtraps$Station_old_name <- camtraps$Station
camtraps$Station <- camtraps$station_period # set session based Stations
camtraps$station_period <- NULL # drop redundant data 
camtraps <- dplyr::select(camtraps, Station, Station_old_name, period, forcov, foredg, lat, long) # keep only relevant columns 

## Set factor levels of environmental periods: sat2 - sat20 
camtraps$period <- factor(camtraps$period, levels = c("sat2", "sat4", "sat6", "sat8", "sat10", "sat11", "sat14", "sat16", "sat18", "sat19"))
camtraps <- arrange(camtraps, period) # arrange by period 

## Add Set-up Date based on the env. periods
camtraps <- mutate(camtraps, Setup_date = ifelse(grepl("sat2", period), "2017-01-01",
                                                        ifelse(grepl("sat4", period), "2017-10-01",
                                                               ifelse(grepl("sat6", period), "2018-01-01",
                                                                             ifelse(grepl("sat8", period), "2018-10-01",
                                                                                    ifelse(grepl("sat10", period), "2019-01-01",
                                                                                           ifelse(grepl("sat11", period), "2019-07-01",
                                                                                                         ifelse(grepl("sat14", period), "2020-04-01",
                                                                                                                       ifelse(grepl("sat16", period), "2020-10-01",
                                                                                                                                     ifelse(grepl("sat18", period), "2021-04-01",
                                                                                                                                            ifelse(grepl("sat19", period), "2021-07-01", NA)))))))))))

# Add retrieval Date
camtraps <- mutate(camtraps, Retrieval_date = ifelse(grepl("sat2", period), "2017-09-30",
                                                        ifelse(grepl("sat4", period), "2017-12-31",
                                                               ifelse(grepl("sat6", period), "2018-09-30",
                                                                             ifelse(grepl("sat8", period), "2018-12-31",
                                                                                    ifelse(grepl("sat10", period), "2019-06-30",
                                                                                           ifelse(grepl("sat11", period), "2020-03-31",
                                                                                                         ifelse(grepl("sat14", period), "2020-09-30",
                                                                                                                       ifelse(grepl("sat16", period), "2021-03-31",
                                                                                                                                     ifelse(grepl("sat18", period), "2021-06-30",
                                                                                                                                            ifelse(grepl("sat19", period), "2021-12-31", NA)))))))))))

## Get camera operation table
camtraps$Setup_date <- as.Date(camtraps$Setup_date) # set format 
camtraps$Retrieval_date <- as.Date(camtraps$Retrieval_date) # set format 

camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = TRUE,
                                    dateFormat   = "%Y-%m-%d")


## ~~~~~~~~ Prepare record table (the mammal data) ~~~~~~~~~

recordTable <- read.csv("mammals_data.csv") # read mammal data 
recordTable$Station <- str_trim(recordTable$Station, side = "both") # remove white spaces 

## Define survey periods as date sequences 
sat2 <- seq(as.Date("2017-01-01"), as.Date("2017-09-30"), by = "days")
sat4 <- seq(as.Date("2017-10-01"), as.Date("2017-12-31"), by = "days") 
sat6 <- seq(as.Date("2018-01-01"), as.Date("2018-09-30"), by = "days") 
sat8 <- seq(as.Date("2018-10-01"), as.Date("2018-12-31"), by = "days") 
sat10 <- seq(as.Date("2019-01-01"), as.Date("2019-06-30"), by = "days") 
sat11 <- seq(as.Date("2019-07-01"), as.Date("2020-03-30"), by = "days") 
sat14 <- seq(as.Date("2020-04-01"), as.Date("2020-09-30"), by = "days") 
sat16 <- seq(as.Date("2020-10-01"), as.Date("2021-03-31"), by = "days") 
sat18 <- seq(as.Date("2021-03-01"), as.Date("2021-06-30"), by = "days") 
sat19 <- seq(as.Date("2021-07-01"), as.Date("2021-12-31"), by = "days") 

## Add period as variable to record table as well  
recordTable$DTO <- recordTable$DateTimeOriginal
recordTable$DateTimeOriginal <- as.Date(recordTable$DateTimeOriginal)
recordTable$period <- ifelse(recordTable$DateTimeOriginal %in% sat2, "sat2",
                                ifelse(recordTable$DateTimeOriginal %in% sat4, "sat4",
                                       ifelse(recordTable$DateTimeOriginal %in% sat6, "sat6",
                                                     ifelse(recordTable$DateTimeOriginal %in% sat8, "sat8",
                                                            ifelse(recordTable$DateTimeOriginal %in% sat10, "sat10",
                                                                   ifelse(recordTable$DateTimeOriginal %in% sat11, "sat11",
                                                                                 ifelse(recordTable$DateTimeOriginal %in% sat14, "sat14",
                                                                                               ifelse(recordTable$DateTimeOriginal %in% sat16, "sat16",
                                                                                                             ifelse(recordTable$DateTimeOriginal %in% sat18, "sat18",
                                                                                                                    ifelse(recordTable$DateTimeOriginal %in% sat19, "sat19", NA))))))))))
recordTable$Station_old_name <- recordTable$Station
recordTable$Station <- paste(recordTable$Station, recordTable$period, sep = "_")

## Subset record table to stations present in camtraps 
recordTable <- subset(recordTable, Station %in% camtraps$Station) # keep only grid stations
#recordTable <- subset(recordTable, Classification != "Bos taurus") # disregard cattle
rm(sat2, sat3, sat4, sat6, sat7, sat8, sat10, sat11, sat13, sat14, sat15, sat16, sat17, sat18, sat19, sat20, forest) # clean gl. env. 
recordTable$DateTimeOriginal <- recordTable$DTO # recover proper DateTimeOrginial 
recordTable$DTO <- NULL # drop redundant data

## Check covariates
ggplot(data = camtraps, aes(x = period, y = forcov, group = Station_old_name)) +
  geom_point(aes(color = Station_old_name), size = 2, alpha = 0.7) +
  geom_line(aes(color = Station_old_name), size = 0.8, alpha = 0.7) +
  scale_x_discrete(limits = levels(camtraps$period)) +
  labs(x = "Period", y = "Forcov") +
  facet_wrap(~ Station_old_name, ncol = 2) +
  theme_classic()

## Cattle count as covariate 

cattle <- subset(recordTable, Classification == "Bos taurus")
cattle <- cattle %>% group_by(Station) %>% summarise(cattle = n())
camtraps <- Merge(camtraps, cattle, id = ~Station)
camtraps$cattle[is.na(camtraps$cattle)] <- 0

write.csv(camtraps, file = "/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/OccModel/camtraps_covariates.csv", row.names = FALSE)
write.csv(recordTable, file = "/Users/serpent/OneDrive - Van Hall Larenstein/Thesis/Scripts/OccModel/mammals_final.csv", row.names = FALSE)
