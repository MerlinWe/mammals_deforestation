#############################################################################################
#################### (6B) Thesis: NDVI preparation and validation ############################
#############################################################################################

library(raster)    # GIS stuff
library(rgdal)     # GIS stuff
library(rgeos)     # GIS stuff
library(sf)        # GIS stuff
library(sp)        # GIS stuff
library(terra)     # GIS stuff
library(betareg)   # beta regression
library(car)       # test vif
library(tidyverse) # Tidyverse

# First, we read in all data we need for the NDVI: Our stacked raster layers and our station layer in UTM 

stations <- shapefile("/Users/serpent/Documents/BSc/Thesis/GIS/station_buffer_1500m.shp")
ndvi_2017 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2017.grd")
ndvi_2018 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2018.grd")
ndvi_2019 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2019.grd")
ndvi_2020 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2020.grd")
ndvi_2021 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2021.grd")
ndvi_2022 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2022.grd")
ndvi_2023 <- raster("/Users/serpent/Documents/BSc/Thesis/Data/Sat/NDVI (Annual means)/ndvi_2023.grd")

# Second, we read in our reference data, i.e., the GFW tropical tree cover tile 

describe("/Users/serpent/Documents/BSc/Thesis/Data/Sat/10S_070W.tif")
trees_info <- capture.output(describe("/Users/serpent/Documents/BSc/Thesis/Data/Sat/10S_070W.tif"))
trees <- terra::rast("/Users/serpent/Documents/BSc/Thesis/Data/Sat/10S_070W.tif")
trees

# Next, we get a station layer in the same projection as the GFW tree cover, thus lat long 

mask <- read_csv("/Users/serpent/Documents/BSc/Thesis/Data/camtraps.csv") %>%
	dplyr::select(Station, lat, long) %>% distinct(Station, .keep_all = TRUE) %>%
	st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	st_buffer(dist = 1500) %>%
	st_as_sf() %>%
	st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs")) 

# Quickly check if the projections match 

crs(trees, proj = TRUE)
crs(mask, proj = TRUE)

# Next, we clip the tree cover to our stations and extract 

trees <- raster::raster(raster::crop(trees, st_as_sf(mask))) 
plot(trees)
plot(mask, colour = NA, add = TRUE)

trees_df <- raster::extract(trees, mask)
trees_df <- lapply(trees_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

trees_df <- as_tibble(do.call(rbind, trees_df))
trees_df$station <- mask$Station 

colnames(trees_df) <- c("mean_treecover", "sd_treecover", "station")

trees_df$mean_treecover <- as.numeric(trees_df$mean_treecover)
trees_df$sd_treecover <- as.numeric(trees_df$sd_treecover)

########## NDVI validation & predict forest cover ##########

# Now we get the same data frame for our 2020 NDVI values and build a regression model explaining tree cover 
# via the NDVI 

plot(ndvi_2020)
plot(stations, colour = NA, add = TRUE)

ndvi_2020_df <- raster::extract(ndvi_2020, stations)
ndvi_2020_df <- lapply(ndvi_2020_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2020_df <- as_tibble(do.call(rbind, ndvi_2020_df))
ndvi_2020_df$station <- mask$Station 

colnames(ndvi_2020_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2020_df$mean_ndvi <- as.numeric(ndvi_2020_df$mean_ndvi)
ndvi_2020_df$sd_ndvi <- as.numeric(ndvi_2020_df$sd_ndvi)

# Build a data set to train our regression model 
training_data <- trees_df %>% left_join(ndvi_2020_df, by = "station") %>%
	dplyr::select(station, mean_treecover, mean_ndvi, sd_treecover, sd_ndvi)

training_data$mean_treecover <- training_data$mean_treecover/100

########### Fit a Beta regression model ##########

# Start with the most complex (fully factorial) model 
mod_beta1 <- betareg(mean_treecover ~ mean_ndvi*sd_ndvi ,data = training_data)
#plot(mod_beta1)

# Try a more simple, additive model 
mod_beta2 <- betareg(mean_treecover ~ mean_ndvi + sd_ndvi, data = training_data)
#plot(mod_beta2) 

# Try a simple model with just the mean 
mod_beta3 <- betareg(mean_treecover ~ mean_ndvi, data = training_data)
#plot(mod_beta3) # validate

# Try a simple model with just the sd
mod_beta4 <- betareg(mean_treecover ~ sd_ndvi, data = training_data)
#plot(mod_beta4) # validate


# Compare model metrics with and without sd_ndvi

summary(mod_beta1) # full factorial
summary(mod_beta2) # additive
summary(mod_beta3) # mean
summary(mod_beta4) # sd

AIC(mod_beta1, mod_beta2, mod_beta3, mod_beta4)

# mod_beta2 (the additive model) is the best performing one (when considering multi-collinearity). 

# Check for multicollinearity
cor(training_data$mean_ndvi, training_data$sd_ndvi) # bad 
vif(mod_beta2) # does not suggest sever multicollinearity 

# Judgement: Include SD !! 

# Predict on training data
training_data$predicted_mean_treecover <- predict(mod_beta2, newdata = training_data)

# Next, we assess model performance using the training data set: 

# R-squared (or pseudo R-squared): Indicates the proportion of variance explained by the model
rsquared <- cor(training_data$predicted_mean_treecover, training_data$mean_treecover)^2 

# Root Mean Squared Error (RMSE): Measures the average difference between predicted values and actual values.
rmse <- sqrt(mean((training_data$predicted_mean_treecover - training_data$mean_treecover)^2)) 

# Mean Absolute Error (MAE): Measures the average absolute differences between predicted and actual values.
mae <- mean(abs(training_data$predicted_mean_treecover - training_data$mean_treecover))

# Summary of metrics
cat("R-squared:", rsquared, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# mod2 is our final model :) 
tree_mod <- betareg(mean_treecover ~ mean_ndvi + sd_ndvi, data = training_data)
rm(mod_beta1, mod_beta2, mod_beta3, mod_beta4, trees, trees_df, mae, rmse, rsquared, trees_info)

# Next, we get mean and sd ndvi values for the years 2017 - 2023 and predict forest cover using our model. 

# 2017 
ndvi_2017_df <- raster::extract(ndvi_2017, stations)
ndvi_2017_df <- lapply(ndvi_2017_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2017_df <- as_tibble(do.call(rbind, ndvi_2017_df))
ndvi_2017_df$station <- mask$Station 

colnames(ndvi_2017_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2017_df$mean_ndvi <- as.numeric(ndvi_2017_df$mean_ndvi)
ndvi_2017_df$sd_ndvi <- as.numeric(ndvi_2017_df$sd_ndvi)
rm(ndvi_2017)
ndvi_2017_df$predicted_mean_treecover <- predict(tree_mod, newdata = ndvi_2017_df)
ndvi_2017_df$year <- "2017"

# 2018
ndvi_2018_df <- raster::extract(ndvi_2018, stations)
ndvi_2018_df <- lapply(ndvi_2018_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2018_df <- as_tibble(do.call(rbind, ndvi_2018_df))
ndvi_2018_df$station <- mask$Station 

colnames(ndvi_2018_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2018_df$mean_ndvi <- as.numeric(ndvi_2018_df$mean_ndvi)
ndvi_2018_df$sd_ndvi <- as.numeric(ndvi_2018_df$sd_ndvi)
rm(ndvi_2018)
ndvi_2018_df$predicted_mean_treecover <- predict(tree_mod, newdata = ndvi_2018_df)
ndvi_2018_df$year <- "2018"

# 2019
ndvi_2019_df <- raster::extract(ndvi_2019, stations)
ndvi_2019_df <- lapply(ndvi_2019_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2019_df <- as_tibble(do.call(rbind, ndvi_2019_df))
ndvi_2019_df$station <- mask$Station 

colnames(ndvi_2019_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2019_df$mean_ndvi <- as.numeric(ndvi_2019_df$mean_ndvi)
ndvi_2019_df$sd_ndvi <- as.numeric(ndvi_2019_df$sd_ndvi)
rm(ndvi_2019)
ndvi_2019_df$predicted_mean_treecover <- predict(tree_mod, newdata = ndvi_2019_df)
ndvi_2019_df$year <- "2019"

ndvi_2020_df <- training_data %>% dplyr::select(mean_ndvi, sd_ndvi, station, predicted_mean_treecover)
ndvi_2020_df$year <- "2020"

# 2021
ndvi_2021_df <- raster::extract(ndvi_2021, stations)
ndvi_2021_df <- lapply(ndvi_2021_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2021_df <- as_tibble(do.call(rbind, ndvi_2021_df))
ndvi_2021_df$station <- mask$Station 

colnames(ndvi_2021_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2021_df$mean_ndvi <- as.numeric(ndvi_2021_df$mean_ndvi)
ndvi_2021_df$sd_ndvi <- as.numeric(ndvi_2021_df$sd_ndvi)
rm(ndvi_2021)
ndvi_2021_df$predicted_mean_treecover <- predict(tree_mod, newdata = ndvi_2021_df)
ndvi_2021_df$year <- "2021"


# 2022
ndvi_2022_df <- raster::extract(ndvi_2022, stations)
ndvi_2022_df <- lapply(ndvi_2022_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2022_df <- as_tibble(do.call(rbind, ndvi_2022_df))
ndvi_2022_df$station <- mask$Station 

colnames(ndvi_2022_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2022_df$mean_ndvi <- as.numeric(ndvi_2022_df$mean_ndvi)
ndvi_2022_df$sd_ndvi <- as.numeric(ndvi_2022_df$sd_ndvi)
rm(ndvi_2022)
ndvi_2022_df$predicted_mean_treecover <- predict(tree_mod, newdata = ndvi_2022_df)
ndvi_2022_df$year <- "2022"


# 2023
ndvi_2023_df <- raster::extract(ndvi_2023, stations)
ndvi_2023_df <- lapply(ndvi_2023_df, FUN = function(x) {
	list(mean = mean(x),
			 std_dev = sd(x))})

ndvi_2023_df <- as_tibble(do.call(rbind, ndvi_2023_df))
ndvi_2023_df$station <- mask$Station 

colnames(ndvi_2023_df) <- c("mean_ndvi", "sd_ndvi", "station")
ndvi_2023_df$mean_ndvi <- as.numeric(ndvi_2023_df$mean_ndvi)
ndvi_2023_df$sd_ndvi <- as.numeric(ndvi_2023_df$sd_ndvi)
rm(ndvi_2023)
ndvi_2023_df$predicted_mean_treecover <- predict(tree_mod, newdata = ndvi_2023_df)
ndvi_2023_df$year <- "2023"

treecover <- data.frame(rbind(ndvi_2017_df, ndvi_2018_df, ndvi_2019_df, ndvi_2020_df, ndvi_2021_df, ndvi_2022_df, ndvi_2023_df))
treecover <- as_tibble(treecover) %>% dplyr::select(station, year, predicted_mean_treecover)

write_csv(treecover, file = "/Users/serpent/Documents/BSc/Thesis/Data/treecover.csv")


