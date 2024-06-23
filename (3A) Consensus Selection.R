################################################################################
########### Data prep - step 3A: Explore consensus & find best method ##########
################################################################################
 
# This script explores four different methods towards a consensus.
# (1) with the use of expert review
# (2) without the use of expert review
# (3) with a weight parameter on expert review
# (4) expert review only considered when there is disagreement

library(tidyverse)  # ggplot2, dplyr, strings, etc. 
library(reshape2)   # data manipulation
library(data.table) # data manipulation
library(Hmisc)      # data manipulation
library(lubridate)  # Dates & Time
library(zoo)        # NA handling 
library(camtrapR)   # camera traps
library(camtrapRdeluxe) # camera traps
 
# ~~~~~~~~~~ Prep data for consensus variations ~~~~~~~~~~ 

setwd() # set WD

## Note: Data contains only time stamps of JPG photos!
## PNG time stamps are not correct. 

dat.all <- read.csv("json_parsed_with_jpg_timestamps_30.10.23.csv") # get data
dat.all <- dplyr::select(dat.all, -Created_At, -Updated_At, - Reviewer, -Bbox_tlwh, -Quality, -Comment) # keep only relevant columns
dat.all <- arrange(dat.all, External_ID, Index) # arrange by external ID and index
dat.all <- setDT(dat.all) # set as data table
dat.all <- na.omit(dat.all, cols = "Classification") # drop NAs in Classification

## --- Fix species strings & add taxonomic levels ---

dat.all$Classification <- tolower(dat.all$Classification) # set all mammal strings to lower class
dat.all$Classification <- sub(" ", "_", dat.all$Classification) # replace white spaces with underscores
dat.all <- subset(dat.all, !(Classification %in% c("human", "something_unidentifiable", "equipment", "vehicle"))) # exclude non-animals

names <- read.csv("names_tax.csv", na.strings = "") # cross-reference with taxonomic species data 
identical(nrow(names), length(unique(dat.all$Classification))) # check if length match (must be TRUE)
identical(sort(as.character(names$Classification)), sort(as.character(unique(dat.all$Classification)))) # check is strings match (must be TRUE)

dat.all <- merge(dat.all, names, by = "Classification") # merge mammal data with taxonomic labels
dat.all <- dplyr::select(dat.all, External_ID, Link, Index, Group, Family, Genus, Species, Classification, Created_By, Score, Station, Camera, n_indiv, DateTimeOriginal)
rm(names) # remove redundant data
table(dat.all$Classification) # check classifications

dat.all$Score <- ifelse(is.na(dat.all$Score) | dat.all$Score != 1, 0, dat.all$Score) # also make sure all non-reviewed labels get the score value 0
table(dat.all$Score)

########## Version 1: Consensus using expert review ##########

# [see consensus PDF for guidance]

con1 <- dat.all # get input data to develop consensus 1 
length(unique(con1$External_ID))
table(con1$Index)

## --- First batch: Capture events that were reviewed at least once and where the review is unanimous ---

con1$Score <- as.integer(con1$Score) # set format 

## We will pivot the reviewed labels by the index to rearrange the data to the level of the External IDs. 
## Then we need to exclude capture events where two different labels were reviewed and approved. 

review <- subset(con1, Score == 1) # get reviewed photos
review <- review %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list) %>% 
  mutate_all(as.character) # set format 
colnames(review) <- gsub("Class", "", colnames(review)) # clean column names 
review <- review %>% mutate_all(~ ifelse(. == "NULL", NA, .)) # set NA values 

## The review data contains all labels that were reviewed at least once. Thus it also contains a lot of
## missing values. In order to check for disagreement, we fill up all missing values with the closest
## string on the left in a new data set (thumb_test) and check if the strings match in a new column. 

review_test <- review %>% # fill Na's from the values on the left to check for agreement 
  pmap_dfr(., ~ na.locf(c(...)) %>% as.list %>% as_tibble)

review_test <- transform(review_test, Agree = apply(review_test[, 2:ncol(review_test)], 1, function(x) length(unique(x)) == 1)) # check agreement
review$Agree <- review_test$Agree # now we add this column to our original review data
review <- subset(review, Agree == TRUE) # We keep only unanimously reviewed photos and treat non-unanimous reviews as non-reviewed
rm(review_test) # remove redundant data

## Get the first batch of clean data (here all label classifications are confirmed):
batch1 <- subset(con1, External_ID %in% review$External_ID) # grab unanimously reviewed photos from raw data
con1 <- subset(con1, !(External_ID %in% batch1$External_ID)) # exclude batch1 photos from raw data
con1$Score <- 0 # set all remaining mammal scores to 0 (those are discordant reviews)

batch1 <- dplyr::select(batch1, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch1 <- distinct(batch1, External_ID, .keep_all = TRUE) # disregard label classifications and keep only one row per image 
batch1$DateTimeOriginal <- as.POSIXct(batch1$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(review) # remove redundant data

nrow(batch1)/length(unique(dat.all$External_ID))*100 # check percentage of Batch 1

## Regarding non-unanimous reviews: In some occasions, two different label classifications were reviewed with a thumbs up. 
## We treat them the same as if they were not reviewed only exclude the reviewed ones from the raw data. Accordingly,
## the con1 file now only contains images with no expert review. 

## --- Second batch: Capture events that were not reviewed but labelled unanimously ---

## Widen format to classifications by External ID 
no_review <- con1 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(no_review) <- gsub("Class", "", colnames(no_review)) # clean column names 
no_review <- dplyr::select(no_review, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Here we face the problem that some External_IDs contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con1 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
no_review$mult_species <- test$mult_species
mult_species <- no_review %>% subset(mult_species == TRUE) # get External_IDs with multiple species
no_review <- no_review %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
no_review <- no_review %>% mutate_all(~ ifelse(. == "NULL", NA, .))
no_review$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

## There is no elegant way (at least that I can think of) of dealing with External_IDs where only 
## few citizen scientists identified more than one species. Given that this only concerns a tiny
## fraction of the images and only few people actually identified more than one species, I (for now)
## disregard those labels that give more than once species. 

clean_list_column <- function(col) {
  if (is.null(col) || length(col) > 1) {
    NA
  } else {
    col
  }
}

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
no_review <- data.frame(rbind(no_review, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## We check for unanimous classifications using the same procedure as with the thumbs. However, since sometimes 
## the first classification index is not filled out, we need to shift all classification values "to the left". 
shift_to_left <- function(row) {
  non_na_values <- row[!is.na(row)]
  shifted_row <- c(non_na_values, rep(NA, length(row) - length(non_na_values)))
  shifted_row }

names <- colnames(no_review) # get column names
no_review <- data.frame(t(apply(no_review, 1, shift_to_left))) # shift values to the left
colnames(no_review) <- names
no_review <- no_review %>% filter(!rowSums(is.na(.[, 2:9])) == length(2:9)) # remove images with no labels
no_review_test <- no_review %>% # fill Na's from the values on the left to check for agreement 
  pmap_dfr(., ~ na.locf(c(...)) %>% as.list %>% as_tibble)
no_review_test <- transform(no_review_test, Agree = apply(no_review_test[, 2:ncol(no_review_test)], 1, function(x) length(unique(x)) == 1)) # check agreement
no_review$Agree <- no_review_test$Agree # now we add this column to our original review data
rm(no_review_test, names) # remove redundant data 

batch2 <- subset(no_review, Agree == TRUE)$External_ID # get unanimous classifications the were not reviewed
batch2 <- subset(con1, External_ID %in% batch2) # extract unanimous External ID's from mammal data

con1 <- subset(con1, !(External_ID %in% batch2$External_ID)) # exclude batch2 photos from mammal data

batch2 <- dplyr::select(batch2, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch2 <- distinct(batch2, External_ID, .keep_all = TRUE) # disregard label classifications and keep only one row per image 
batch2$DateTimeOriginal <- as.POSIXct(batch2$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(no_review) # remove redundant data

## ---- Batch 3: Capture events that were not reviewed, are not unanimous, but reached >70% agreement -----

remainings <- con1 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(remainings) <- gsub("Class", "", colnames(remainings)) # clean column names 
remainings <- dplyr::select(remainings, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Again, some labels contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con1 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
remainings$mult_species <- test$mult_species
mult_species <- remainings %>% subset(mult_species == TRUE) # get External_IDs with multiple species
remainings <- remainings %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
remainings <- remainings %>% mutate_all(~ ifelse(. == "NULL", NA, .))
remainings$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

## Again, as there is no elegant way (that I can think of) of dealing with External_IDs where only 
## few citizen scientists identified more than one species, we (for now) disregard those labels.

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
remainings <- data.frame(rbind(remainings, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## From these remaining External ID's, we find the most frequently given answer and calculate the percentage it occurred: 
External_ID <- remainings$External_ID
remainings <- apply(remainings, 1, function(row) {
  counts <- table(row)
  most_common <- names(which.max(counts))
  percentage <- counts[which.max(counts)] / sum(counts) * 100
  return(list(most_common = most_common, percentage = percentage))
})

## We unpack the returned list and get the most frequently given answer including its percentage. 
remainings <- data.frame(do.call(rbind, remainings)) # un-list df
remainings <- remainings %>% unnest_longer(., percentage, indices_include = TRUE) # expand percentage 
remainings$External_ID <- External_ID # add External ID's
remainings <- dplyr::select(remainings, External_ID, most_common, percentage) # sort and set column order
remainings$most_common <- as.character(remainings$most_common)
rm(External_ID) # remove redundant data

## Next: Batch 3 are all Classifications that reached a consensus of >70 %
hist(remainings$percentage, main = "Agreement (%) in discordant capt. events (Vers. 1)", xlab = "percentage") # check histogram
abline(v = 70, col = "red", lwd = 2) # add reference line
batch3 <- subset(remainings, percentage > 70)[, 1:2] # these are the ones with a consensus of >70 percent
colnames(batch3) <- c("External_ID", "Classification")

id <- paste0(batch3$External_ID, batch3$Classification) # get identifier based on External ID and Classification 
con1$ID <- paste0(con1$External_ID, con1$Classification) # get same identifier in the mammal data 
batch3 <- distinct(subset(con1, ID %in% id), ID, .keep_all = TRUE)[ ,-15] # extract batch 3 from mammal data

batch3 <- dplyr::select(batch3, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch3$DateTimeOriginal <- as.POSIXct(batch3$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(id) # remove redundant data 

## The ones that remain after excluding 'batch3' from the 'remainings' are the ones with no review and no consensus: 

no_consensus <- subset(con1, !(External_ID %in% batch3$External_ID))
remainings <- subset(remainings, percentage <= 70)
nrow(distinct(remainings, External_ID))
write.csv(remainings, file = "no_consensus_vers1.csv", row.names = FALSE) # write file of no-consensus

## Thus we are left with the following: 
con1 <- rbind(batch1, batch2, batch3) # these are all External ID where a consensus could be reached 
rm(batch1, batch2, batch3, remainings, no_consensus) # remove redundant data 

########## Version 2: Consensus without the use of the expert review ##########

con2 <- dat.all # get input data to develop consensus 2
length(unique(con2$External_ID))
table(con2$Index)

## --- First batch: Capture events that were labelled unanimously ---

## Widen format to classifications by External ID 
unanimous <- con2 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(unanimous) <- gsub("Class", "", colnames(unanimous)) # clean column names 
unanimous <- dplyr::select(unanimous, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Here we face the problem that some External_IDs contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con2 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
unanimous$mult_species <- test$mult_species
mult_species <- unanimous %>% subset(mult_species == TRUE) # get External_IDs with multiple species
unanimous <- unanimous %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
unanimous <- unanimous %>% mutate_all(~ ifelse(. == "NULL", NA, .))
unanimous$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

## There is no elegant way (at least that I can think of) of dealing with External_IDs where only 
## few citizen scientists identified more than one species. Given that this only concerns a tiny
## fraction of the images and only few people actually identified more than one species, I (for now)
## disregard those labels that give more than once species. 

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
unanimous <- data.frame(rbind(unanimous, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## We check for unanimous classifications using the same procedure as with the thumbs. However, since sometimes 
## the first classification index is not filled out, we need to shift all classification values "to the left". 

names <- colnames(unanimous) # get column names
unanimous <- data.frame(t(apply(unanimous, 1, shift_to_left))) # shift values to the left
colnames(unanimous) <- names
unanimous <- unanimous %>% filter(!rowSums(is.na(.[, 2:9])) == length(2:9)) # remove images with no labels
unanimous_test <- unanimous %>% # fill Na's from the values on the left to check for agreement 
  pmap_dfr(., ~ na.locf(c(...)) %>% as.list %>% as_tibble)
unanimous_test <- transform(unanimous_test, Agree = apply(unanimous_test[, 2:ncol(unanimous_test)], 1, function(x) length(unique(x)) == 1)) # check agreement
unanimous$Agree <- unanimous_test$Agree # now we add this column to our original review data
rm(unanimous_test, names) # remove redundant data 

batch1 <- subset(unanimous, Agree == TRUE)$External_ID # get unanimous classifications the were not reviewed
batch1 <- subset(con2, External_ID %in% batch1) # extract unanimous External ID's from mammal data

con2 <- subset(con2, !(External_ID %in% batch1$External_ID)) # exclude batch2 photos from mammal data
length(unique(con2$External_ID)) # count remaining External IDs

batch1 <- dplyr::select(batch1, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch1 <- distinct(batch1, External_ID, .keep_all = TRUE) # disregard label classifications and keep only one row per image 
batch1$DateTimeOriginal <- as.POSIXct(batch1$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(unanimous) # remove redundant data

## ---- Batch 2: Capture events that are not unanimous, but reached >70% agreement -----

discordant <- con2 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(discordant) <- gsub("Class", "", colnames(discordant)) # clean column names 
discordant <- dplyr::select(discordant, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Again, some labels contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con2 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
discordant$mult_species <- test$mult_species
mult_species <- discordant %>% subset(mult_species == TRUE) # get External_IDs with multiple species
discordant <- discordant %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
discordant <- discordant %>% mutate_all(~ ifelse(. == "NULL", NA, .))
discordant$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

## Again, as there is no elegant way (that I can think of) of dealing with External_IDs where only 
## few citizen scientists identified more than one species, we (for now) disregard those labels.

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
discordant <- data.frame(rbind(discordant, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## From these remaining External ID's, we find the most frequently given answer and calculate the percentage it occurred: 
External_ID <- discordant$External_ID
discordant <- apply(discordant, 1, function(row) {
  counts <- table(row)
  most_common <- names(which.max(counts))
  percentage <- counts[which.max(counts)] / sum(counts) * 100
  return(list(most_common = most_common, percentage = percentage))
})

## We unpack the returned list and get the most frequently given answer including its percentage. 
discordant <- data.frame(do.call(rbind, discordant)) # un-list df
discordant <- discordant %>% unnest_longer(., percentage, indices_include = TRUE) # expand percentage 
discordant$External_ID <- External_ID # add External ID's
discordant <- dplyr::select(discordant, External_ID, most_common, percentage) # sort and set column order
discordant$most_common <- as.character(discordant$most_common)
rm(External_ID) # remove redundant data

## Next: Batch 2 are all Classifications that reached a consensus of >70 %
hist(discordant$percentage, main = "Agreement (%) in discordant capt. events (Vers. 2)", xlab = "percentage") # check histogram
abline(v = 70, col = "red", lwd = 2) # add reference line
batch2 <- subset(discordant, percentage > 70)[, 1:2] # these are the ones with a consensus of >70 percent
colnames(batch2) <- c("External_ID", "Classification")

id <- paste0(batch2$External_ID, batch2$Classification) # get identifier based on External ID and Classification 
con2$ID <- paste0(con2$External_ID, con2$Classification) # get same identifier in the mammal data 
batch2 <- distinct(subset(con2, ID %in% id), ID, .keep_all = TRUE)[ ,-15] # extract batch 2 from mammal data

batch2 <- dplyr::select(batch2, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch2$DateTimeOriginal <- as.POSIXct(batch2$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(id) # remove redundant data 

## The ones that remain after excluding 'batch2' from the 'remainings' are the ones with no review and no consensus: 

no_consensus <- subset(con2, !(External_ID %in% batch2$External_ID))
remainings <- subset(discordant, percentage <= 70)
nrow(distinct(remainings, External_ID))
write.csv(remainings, file = "no_consensus_vers2.csv", row.names = FALSE) # write file of no-consensus

## Thus we are left with the following: 
con2 <- rbind(batch1, batch2) # these are all External ID where a consensus could be reached 
rm(batch1, batch2, remainings, no_consensus, discordant) # remove redundant data 

########## Version 3: Weight parameter on expert review ##########

con3 <- dat.all # get input data to develop consensus 3
length(unique(con3$External_ID))
table(con3$Index)

## To be able to calculate the weight parameter, we first need to count the number of labels that are found per External ID. 
## We start by pivoting the data as done in the previous steps. This also includes dropping labels with multiple species. 

count <- con3 %>% dplyr::select(External_ID, Index, Classification) %>% 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(count) <- gsub("Class", "", colnames(count)) 
count <- dplyr::select(count, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

test <- con3 %>% dplyr::select(External_ID, Index, Classification) %>% 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)

count$mult_species <- test$mult_species
mult_species <- count %>% subset(mult_species == TRUE) 
count <- count %>% subset(mult_species == FALSE) %>% 
  mutate_all(as.character) # set format

count <- count %>% mutate_all(~ ifelse(. == "NULL", NA, .))
count$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

mult_species <- mult_species %>% 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
count <- data.frame(rbind(count, mult_species)) 
rm(mult_species) 

names <- colnames(count) # get column names
count <- data.frame(t(apply(count, 1, shift_to_left))) # shift values to the left
colnames(count) <- names
count <- count %>% mutate(n_labels = rowSums(!is.na(select(count, starts_with("class"))))) %>%
  select(External_ID, n_labels)

# 'count' at this point is a df that gives the total number of labels per capture event (i.e., External ID). This is used as 
# 100% when calculating the weight parameter of reviewed labels. Basically, all of the code above was used to create this column...

# Next, we get the reviewed labels as in con1 
review <- subset(con3, Score == 1) # get reviewed photos
review <- merge(review, count, by = "External_ID") # merge with label count variable 

review$expand <- ceiling(review$n_labels*.3) # get expansion parameter (30% of label count)
review <- review %>% uncount(weights = expand) %>% # expand row wise by expansion parameter
  group_by_all() %>% # group
  mutate(rep_letter = letters[row_number()]) # get suffix column
review$Index <- paste(review$Index, review$rep_letter, sep = "_") # update index column 

# Now we pivot the data back to a wider format so we can develop the consensus 
review <- review %>% ungroup() %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list) %>% 
  mutate_all(as.character) # set format 
colnames(review) <- gsub("Class", "", colnames(review)) # clean column names 
review <- review %>% mutate_all(~ ifelse(. == "NULL", NA, .)) # set NA values

## Note: Every single label in review was reviewed by experts! This data does not include the un-reviewed labels! 
## We need to merge capture events with no expert review back into the data at a later point! 

names <- colnames(review) # get column names
review <- data.frame(t(apply(review, 1, shift_to_left))) # shift values to the left
colnames(review) <- names # re-set column names (order doesn't matter at this point)
review <- review %>% filter(!rowSums(is.na(.[, 2:25])) == length(2:25)) # remove capture events with no labels (there should not be any)

## There are 27 capture events lost in the expansion process (not exactly sure why)
## Next, we deal with the un-reviewed labels. We arrange them in the same structure as 'review' and then merge both data frames column-wise. 

remainings <- subset(con3, Score != 1) # these are the ones without expert review

remainings <- remainings %>% dplyr::select(External_ID, Index, Classification) %>% 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(remainings) <- gsub("Class", "", colnames(remainings)) 
remainings <- dplyr::select(remainings, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

test <- subset(con3, Score != 1) %>% dplyr::select(External_ID, Index, Classification) %>% 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)

remainings$mult_species <- test$mult_species
mult_species <- remainings %>% subset(mult_species == TRUE) 
remainings <- remainings %>% subset(mult_species == FALSE) %>% mutate_all(as.character) # set format

remainings <- remainings %>% mutate_all(~ ifelse(. == "NULL", NA, .))
remainings$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL # drop non-relevant column
rm(test)

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
remainings <- data.frame(rbind(remainings, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## At this point we have two df's of interest: remaining & review. If we combine both, we get the un-reviewed data in its normal 
## structure and the reviewed data expanded by a 30% weight parameter. Using the combination of both, we can develop the consensus 
## the way we are used to (i.e., batch-wise).

weighted <- merge(review, remainings, by = "External_ID", all = TRUE) # merge both data frames
weighted <- weighted[, colSums(is.na(weighted)) != nrow(weighted)] # check for empty columns
weighted <- data.frame(t(apply(weighted, 1, shift_to_left))) # shift values to the left
colnames(weighted) <- c("External_ID", paste0("Class", 1:(ncol(weighted) - 1))) # set column names 
rm(review, remainings, names, count)

## ----- Now we get batch 1 (i.e., those capture events that are unanimous (after weighting)) -----

weighted_test <- weighted %>% # fill Na's from the values on the left to check for agreement 
  pmap_dfr(., ~ na.locf(c(...)) %>% as.list %>% as_tibble)
weighted_test <- transform(weighted_test, Agree = apply(weighted_test[, 2:ncol(weighted_test)], 1, function(x) length(unique(x)) == 1)) # check agreement
weighted$Agree <- weighted_test$Agree # now we add this column to our original weighted data
rm(weighted_test) # remove redundant data 

batch1 <- subset(weighted, Agree == TRUE)$External_ID # get unanimous classifications the were not reviewed
batch1 <- subset(con3, External_ID %in% batch1) # extract unanimous External ID's from mammal data

con3 <- subset(con3, !(External_ID %in% batch1$External_ID)) # exclude batch1 photos from data
length(unique(con3$External_ID)) # count remaining External IDs

batch1 <- dplyr::select(batch1, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch1 <- distinct(batch1, External_ID, .keep_all = TRUE) # disregard label classifications and keep only one row per image 
batch1$DateTimeOriginal <- as.POSIXct(batch1$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format

## ----- Batch 2: Capture events that reached a 70% agreement (after weighting) -----

discordant <- con3 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(discordant) <- gsub("Class", "", colnames(discordant)) # clean column names 
discordant <- dplyr::select(discordant, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Again, some labels contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con3 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
discordant$mult_species <- test$mult_species
mult_species <- discordant %>% subset(mult_species == TRUE) # get External_IDs with multiple species
discordant <- discordant %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
discordant <- discordant %>% mutate_all(~ ifelse(. == "NULL", NA, .))
discordant$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)
mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
discordant <- data.frame(rbind(discordant, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## From these remaining External ID's, we find the most frequently given answer and calculate the percentage it occurred: 
External_ID <- discordant$External_ID
discordant <- apply(discordant, 1, function(row) {
  counts <- table(row)
  most_common <- names(which.max(counts))
  percentage <- counts[which.max(counts)] / sum(counts) * 100
  return(list(most_common = most_common, percentage = percentage))
})

## We unpack the returned list and get the most frequently given answer including its percentage. 
discordant <- data.frame(do.call(rbind, discordant)) # un-list df
discordant <- discordant %>% unnest_longer(., percentage, indices_include = TRUE) # expand percentage 
discordant$External_ID <- External_ID # add External ID's
discordant <- dplyr::select(discordant, External_ID, most_common, percentage) # sort and set column order
discordant$most_common <- as.character(discordant$most_common)
rm(External_ID) # remove redundant data

## Batch 2 are all capture events that reached a consensus of >70 %
hist(discordant$percentage, main = "Agreement (%) in discordant capt. events (Vers. 3)", xlab = "percentage") # check histogram
abline(v = 70, col = "red", lwd = 2) # add reference line
batch2 <- subset(discordant, percentage > 70)[, 1:2] # these are the ones with a consensus of >70 percent
colnames(batch2) <- c("External_ID", "Classification")

id <- paste0(batch2$External_ID, batch2$Classification) # get identifier based on External ID and Classification 
con3$ID <- paste0(con3$External_ID, con3$Classification) # get same identifier in the mammal data 
batch2 <- distinct(subset(con3, ID %in% id), ID, .keep_all = TRUE)[ ,-15] # extract batch 2 from mammal data

batch2 <- dplyr::select(batch2, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch2$DateTimeOriginal <- as.POSIXct(batch2$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(id) # remove redundant data 

## The ones that remain after excluding 'batch2' from the 'remainings' are the ones with no review and no consensus: 
no_consensus <- subset(con3, !(External_ID %in% batch2$External_ID))
no_consensus <- subset(discordant, percentage <= 70)
nrow(distinct(no_consensus, External_ID))
write.csv(no_consensus, file = "no_consensus_vers3.csv", row.names = FALSE) # write file of no-consensus

## Thus we are left with the following: 
con3 <- rbind(batch1, batch2) # these are all External ID where a consensus could be reached 
rm(batch1, batch2, weighted, no_consensus, discordant) # remove redundant data 

########## Version 4: Expert review only considered when there is disagreement ##########

con4 <- dat.all # get input data to develop consensus 4
length(unique(con4$External_ID))
table(con4$Index)

# --- First batch: Capture events that were labelled unanimously ---

## Widen format to classifications by External ID 
unanimous <- con4 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(unanimous) <- gsub("Class", "", colnames(unanimous)) # clean column names 
unanimous <- dplyr::select(unanimous, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Here we face the problem that some External_IDs contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con4 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
unanimous$mult_species <- test$mult_species
mult_species <- unanimous %>% subset(mult_species == TRUE) # get External_IDs with multiple species
unanimous <- unanimous %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
unanimous <- unanimous %>% mutate_all(~ ifelse(. == "NULL", NA, .))
unanimous$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

## There is no elegant way (at least that I can think of) of dealing with External_IDs where only 
## few citizen scientists identified more than one species. Given that this only concerns a tiny
## fraction of the images and only few people actually identified more than one species, I (for now)
## disregard those labels that give more than once species. 

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
unanimous <- data.frame(rbind(unanimous, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## We check for unanimous classifications using the same procedure as with the thumbs. However, since sometimes 
## the first classification index is not filled out, we need to shift all classification values "to the left". 

names <- colnames(unanimous) # get column names
unanimous <- data.frame(t(apply(unanimous, 1, shift_to_left))) # shift values to the left
colnames(unanimous) <- names
unanimous <- unanimous %>% filter(!rowSums(is.na(.[, 2:9])) == length(2:9)) # remove images with no labels
unanimous_test <- unanimous %>% # fill Na's from the values on the left to check for agreement 
  pmap_dfr(., ~ na.locf(c(...)) %>% as.list %>% as_tibble)
unanimous_test <- transform(unanimous_test, Agree = apply(unanimous_test[, 2:ncol(unanimous_test)], 1, function(x) length(unique(x)) == 1)) # check agreement
unanimous$Agree <- unanimous_test$Agree # now we add this column to our original review data
rm(unanimous_test, names) # remove redundant data 

batch1 <- subset(unanimous, Agree == TRUE)$External_ID # get unanimous classifications the were not reviewed
batch1 <- subset(con4, External_ID %in% batch1) # extract unanimous External ID's from mammal data

con4 <- subset(con4, !(External_ID %in% batch1$External_ID)) # exclude batch2 photos from mammal data
length(unique(con4$External_ID)) # count remaining External IDs

batch1 <- dplyr::select(batch1, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch1 <- distinct(batch1, External_ID, .keep_all = TRUE) # disregard label classifications and keep only one row per image 
batch1$DateTimeOriginal <- as.POSIXct(batch1$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(unanimous) # remove redundant data

## ---- Batch 2: Capture events that are not unanimous, but reached >70% agreement -----

discordant <- con4 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(discordant) <- gsub("Class", "", colnames(discordant)) # clean column names 
discordant <- dplyr::select(discordant, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9)

## Again, some labels contain >1 species. These Classifications become lists when 
## we pivot the data. We will exclude them and deal with them separately. 

# Identify External_IDs with multiple species classifications and subset no_review accordingly 
test <- con4 %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class3, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:10] >= 2, na.rm = TRUE) > 0)
discordant$mult_species <- test$mult_species
mult_species <- discordant %>% subset(mult_species == TRUE) # get External_IDs with multiple species
discordant <- discordant %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
discordant <- discordant %>% mutate_all(~ ifelse(. == "NULL", NA, .))
discordant$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)

## Again, as there is no elegant way (that I can think of) of dealing with External_IDs where only 
## few citizen scientists identified more than one species, we (for now) disregard those labels.

mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
discordant <- data.frame(rbind(discordant, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## From these remaining External ID's, we find the most frequently given answer and calculate the percentage it occurred: 
External_ID <- discordant$External_ID
discordant <- apply(discordant, 1, function(row) {
  counts <- table(row)
  most_common <- names(which.max(counts))
  percentage <- counts[which.max(counts)] / sum(counts) * 100
  return(list(most_common = most_common, percentage = percentage))
})

## We unpack the returned list and get the most frequently given answer including its percentage. 
discordant <- data.frame(do.call(rbind, discordant)) # un-list df
discordant <- discordant %>% unnest_longer(., percentage, indices_include = TRUE) # expand percentage 
discordant$External_ID <- External_ID # add External ID's
discordant <- dplyr::select(discordant, External_ID, most_common, percentage) # sort and set column order
discordant$most_common <- as.character(discordant$most_common)
rm(External_ID) # remove redundant data

## Next: Batch 2 are all Classifications that reached a consensus of >70 %
hist(discordant$percentage, main = "Agreement (%) in discordant capt. events (Vers. 4)", xlab = "percentage") # check histogram
abline(v = 70, col = "red", lwd = 2) # add reference line
batch2 <- subset(discordant, percentage > 70)[, 1:2] # these are the ones with a consensus of >70 percent
colnames(batch2) <- c("External_ID", "Classification")
discordant <- subset(discordant, percentage < 70) # exclude consensus events from discordant data

id <- paste0(batch2$External_ID, batch2$Classification) # get identifier based on External ID and Classification 
con4$ID <- paste0(con4$External_ID, con4$Classification) # get same identifier in the mammal data 
batch2 <- distinct(subset(con4, ID %in% id), ID, .keep_all = TRUE)[ ,-15] # extract batch 2 from mammal data

batch2 <- dplyr::select(batch2, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch2$DateTimeOriginal <- as.POSIXct(batch2$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(id)# remove redundant data 

## ----- Batch 3: Discordant capture events that did not reach >70% agreement BUT include at least one reviewed label -----

con4 <- subset(con4, !(External_ID %in% batch2$External_ID)) # exclude images that reached a consensus from con4
reviewed <- subset(con4, Score == 1) # these are the ones for batch 3

reviewed <- reviewed %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = list)
colnames(reviewed) <- gsub("Class", "", colnames(reviewed)) # clean column names 
reviewed <- dplyr::select(reviewed, External_ID, class1, class2, class4, class5, class6, class7, class8, class9)

# Again, we deal with multi-species labels the way we did before. 

test <- subset(con4, Score == 1) %>% dplyr::select(External_ID, Index, Classification) %>% # pivot data by index 
  pivot_wider(names_from = Index, values_from = Classification, names_prefix = "Class", values_fn = length)
colnames(test) <- gsub("Class", "", colnames(test)) # clean column names 
test <- dplyr::select(test, External_ID, class1, class2, class4, class5, class6, class7, class8, class9) %>%
  mutate(mult_species = rowSums(.[, 2:9] >= 2, na.rm = TRUE) > 0)
reviewed$mult_species <- test$mult_species
mult_species <- reviewed %>% subset(mult_species == TRUE) # get External_IDs with multiple species
reviewed <- reviewed %>% subset(mult_species == FALSE) %>% # exclude External_IDs with multiple species
  mutate_all(as.character) # set format
reviewed <- reviewed %>% mutate_all(~ ifelse(. == "NULL", NA, .))
reviewed$mult_species <- NULL # drop non-relevant column
mult_species$mult_species <- NULL
rm(test)
mult_species <- mult_species %>% # get rid of nested lists 
  mutate(across(where(is.list), ~ map_chr(., clean_list_column)))
reviewed <- data.frame(rbind(reviewed, mult_species)) # add cleaned data back to the no_review data
rm(mult_species) # clean global environment

## At this point, 'reviewed' only contains reviewed labels from capture events that were labelled discordantly and
## did not reach at least 70% agreement. We briefly check for any disagreement among reviewers. 

reviewed <- data.frame(t(apply(reviewed, 1, shift_to_left))) # shift values to the left
colnames(reviewed) <- c("External_ID", paste0("Class", 1:(ncol(reviewed) - 1))) # set column names 
reviewed <- reviewed %>% filter(!rowSums(is.na(.[, 2:9])) == length(2:9)) # remove images with no labels
reviewed_test <- reviewed %>% # fill Na's from the values on the left to check for agreement 
  pmap_dfr(., ~ na.locf(c(...)) %>% as.list %>% as_tibble)
reviewed_test <- transform(reviewed_test, Agree = apply(reviewed_test[, 2:ncol(reviewed_test)], 1, function(x) length(unique(x)) == 1)) # check agreement
reviewed$Agree <- reviewed_test$Agree # now we add this column to our original review data
rm(reviewed_test) # remove redundant data 

batch3 <- subset(reviewed, Agree == TRUE)$External_ID # get unanimous reviewed capture events
batch3 <- subset(con4, External_ID %in% batch3) # extract unanimous External ID's from mammal data

con4 <- subset(con4, !(External_ID %in% batch3$External_ID)) # exclude batch2 photos from mammal data

batch3 <- dplyr::select(batch3, External_ID, Link, Group, Family, Genus, Species, Classification, Station, Camera, n_indiv, DateTimeOriginal) # keep only relevant columns
batch3 <- distinct(batch3, External_ID, .keep_all = TRUE) # disregard label classifications and keep only one row per image 
batch3$DateTimeOriginal <- as.POSIXct(batch3$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S") # set date format
rm(reviewed) # remove redundant data

no_consensus <- subset(discordant, External_ID %in% con4$External_ID) # get file with highest percentage
write.csv(no_consensus, file = "no_consensus_vers4.csv", row.names = FALSE) # write file of no-consensus

## Thus we are left with the following: 
con4 <- rbind(batch1, batch2, batch3) # these are all External ID where a consensus could be reached 
rm(batch1, batch2, batch3, no_consensus, discordant, clean_list_column, shift_to_left, dat.all) # remove redundant data 

#504 images are lost for some reason ... need to check where that happens 

write.csv(con1, file = "species_consensus_v1.csv", row.names = FALSE)
write.csv(con2, file = "species_consensus_v2.csv", row.names = FALSE)
write.csv(con3, file = "species_consensus_v3.csv", row.names = FALSE)
write.csv(con4, file = "species_consensus_v4.csv", row.names = FALSE)

########## Comparisons ##########

combined_data <- bind_rows(
  data.frame(Data = "v1", dplyr::select(con1, External_ID, Classification)),
  data.frame(Data = "v2", dplyr::select(con2, External_ID, Classification)),
  data.frame(Data = "v3", dplyr::select(con3, External_ID, Classification)),
  data.frame(Data = "v4", dplyr::select(con3, External_ID, Classification)))

table(combined_data$Classification, combined_data$Data) # Check frequency table 
write.csv(table(combined_data$Classification, combined_data$Data), "freq_table.csv")

## Venn diagramm

require(VennDiagram)
venn_data <- list(
  v1 = unique(con1$External_ID),
  v2 = unique(con2$External_ID),
  v3 = unique(con3$External_ID),
  v4 = unique(con4$External_ID))

venn.diagram(
  x = venn_data,
  category.names = names(venn_data),
  filename = "venn_diagram.png")


????""
