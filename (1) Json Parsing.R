###########################################################################
################### Thesis: WildLive! Data Preparation ####################
###########################################################################

## This Script cleans and processes the raw JSON data retrieved from the WildLive! Project. 
## Written by: Merlin Weiss (2022); merlin.s.weiss@gmail.com

## ~~~~~~~~~ Prepare Work space ~~~~~~~~~~ 

## First: active required package libraries. Note: These lines install packages if they are not found 
## on the device that is used to run the script! 

if(!require(jsonlite)){install.packages("jsonlite")}   # Handling JSON files 
if(!require(dplyr)){install.packages("dplyr")}         # Data manipulation
if(!require(plyr)){install.packages("plyr")}           # Data manipulation
if(!require(purrr)){install.packages("purrr")}         # Data manipulation
if(!require(tidyr)){install.packages("tidyr")}         # Data manipulation
if(!require(reshape2)){install.packages("reshape2")}   # Data manipulation
if(!require(Hmisc)){install.packages("Hmisc")}         # Data manipulation
if(!require(data.table)){install.packages("data.table")} # Data manipulation
if(!require(ggplot2)){install.packages("ggplot2")}     # Graphics
if(!require(lubridate)){install.packages("lubridate")} # Dates & Times
if(!require(stringr)){install.packages("stringr")}     # Characters & Strings
if(!require(gdata)){install.packages("gdata")}         # Global Environment Management 

## Second: Set working directory and check file availability 

## Working directory must be set to the location that contains the JSON export. Note: 
## Must contain the absolute pathway (separated by "/"). Windows does NOT paste that automatically!

setwd("") # set WD 
list.files() # check availability

## ~~~~~~~~~~ Read JSON file & prepare directories  ~~~~~~~~~~ 

## >>> Import raw JSON file (this takes a LONG time depending on device & the JSON export) <<<
#json_dat <- jsonlite::fromJSON("/Users/serpent/Documents/BSc/Thesis/Data/JSON Export 23-10-2023/export-2023-10-23T11_02_46.952Z.json", simplifyDataFrame = TRUE)

## After the file was read into the global environment, it should be exported to the working directory. 
## This makes another import into R much faster:

#dir <- getwd()
#save.image(paste0(dir, "/JSON Export 23-10-2023/json_workspace.RData")) # set absolute pathway 

## Load from workplace (this is much faster) 
load("/Users/serpent/Documents/BSc/Thesis/Data/JSON Export 23-10-2023/json_workspace.RData") # raw data 

## ~~~~~~~~~~~~~~~~~~ Data preparation that must be done on the entire data set ~~~~~~~~~~~~~~~~~~ 

## First, we flatten the nested objects as far as we can without manual intervention and set the column names properly. 

json_dat <- jsonlite::flatten(json_dat) # flatten nested data frames within JSON file 
# Briefly write a simple function to remove all columns that only contain NA's
remove_na_columns <- function(json_dat) {
  na_columns <- which(colSums(is.na(json_dat)) == nrow(json_dat))
  json_dat <- json_dat[, -na_columns]
  return(json_dat)}
json_dat <- remove_na_columns(json_dat)

# Adjust column names 
json_dat <- json_dat[ ,-c(23:26)] # remove irrellevant workflow columns
colnames(json_dat) <- c("ID","DataRow_ID", "Link", "Created_By", "Project_Name", "Created_At", "Updated_At",
                        "Seconds_to_Label", "Seconds_to_Review", "Seconds_to_Create", "External_ID", #"Global_Key",
                        "Agreement", "Is_Benchmark", "Benchmark_Agreement", "Dataset_Name", 
                        "Review", "View_Label", "Has_Open_Issues", "Skipped", "Object", "Meta", "Label.relationships")

## Next, some formatting: Make sure R understands 'Created At' as POSIXct (Date format). This should refer to the 
## point in time when the label was created.. The format contains weird elements which we need to clean out before we are able to format it as POSIXct. 

json_dat$Created_At <- sub("[^0-9.-]", " ", json_dat$Created_At) # we remove all non-numeric elements from the Created_At column
json_dat$Created_At <- gsub(".{5}$", "",    json_dat$Created_At) # remove the last 5 characters, as they do not contain any relevant information
json_dat$Created_At <- as.POSIXct(json_dat$Created_At, format = "%Y-%m-%d %H:%M:%S") # now we can set the format properly

## Same goes for 'Updated_At'
json_dat$Updated_At <- sub("[^0-9.-]", " ", json_dat$Updated_At) # we remove all non-numeric elements from the Updated_At column
json_dat$Updated_At <- gsub(".{5}$", "",    json_dat$Updated_At) # remove the last 5 characters, as they do not contain any relevant information
json_dat$Updated_At <- as.POSIXct(json_dat$Updated_At, format = "%Y-%m-%d %H:%M:%S") # now we can set the format properly

## To speed up further processing, we drop non-relevant columns.
json_dat <- dplyr::select(json_dat, External_ID, Created_By, Link, Created_At, Updated_At, Agreement, Object, Review, Meta)

## For exploration purposes, we also extract the station and camera from the External ID and store in in a new column. 
## To do so we create a vector containing all station IDs and use it to match station id inside the External ID. This 
# is needed since the position of this information in the External_ID is not always the same. We get out input vector 
# from our camtrap.csv file. 

stations <- c("AS_CaminoCachuela", "AS_Jaguar", "AS_LaCachuela", "AS_LaCachuela_arriba", 
              "AS_LaCachuela_CaminoCurichi", "AS_Lagarto", "AS_LajaDeNoviquia", 
              "AS_Orquidia", "AS_Salitral", "AS_VacaMuerta", "CaminoCachuela", "Jaguar", 
              "LaCachuelaArriba", "CaminoCurichi", "Lagarto", "LajaDeNoviquia", 
              "Orquidia", "Salitral", "Vacamuerta", "Popo", "Quebrada", "nuevo-1", 
              "nuevo-2", "nuevo-3", "nuevo-4", "Manantial", "NuevoManantial", 
              "nuevo-5", "LaCruz", "VacaMuerta", "CaminoSur", "CaminoPrincipal", 
              "CercaCachuela", "Cachuela2", "Noviquia2", "mario cachuela", "LaCachuela",
              "mario_cachuela", "Provicional", "Curichi", "PIF", "G-01", "G-02", 
              "G-03", "G-04", "G-05", "G-06", "G-07", "G-08", "G-09", "G-10", 
              "G-11", "G-12", "G-13", "G-14", "G-15", "G-16", "G-17", "G-18", 
              "G-19", "G-20", "G-21")

cameras <- c("52", "97", "19", "82", "32", "41", "9", "10", "201_A", "201_B", 
             "201_C", "20", "93", "18", "23", "31", "48", "24", "25", "34", 
             "37", "101_A", "102_A", "102_B", "103_A", "103_B", "104_A", "104_B", 
             "104_C", "104_D", "105_A", "105_B", "106_A", "106_B", "28", "107_A", 
             "107_B", "108_A", "108_B", "109_A", "109_B", "110_A", "111_A", 
             "111_B", "112_A", "112_B", "113_A", "113_B", "52B", "101_B", 
             "115_A", "115_B", "116_A", "116_B", "117_A", "118_A", "118_B", 
             "119_A", "119_B", "120_A", "120_B", "121_A", "121_B", "110_B")

# Extract station information from external ID as good as possible 
bad_rows <- grepl("EK", json_dat$External_ID)
json_dat <- json_dat[!bad_rows, ]
json_dat <- json_dat %>% mutate(Station = map_chr(External_ID, ~ stations[str_detect(.x, stations)]))

# Extracting the camera information is more tricky. For now, we only extract the camera information of images recorded at grid stations that 
# are also JPEG images. Their External_ID should all be in the same structure and we can more or less easily extract the required information. 

cams <- json_dat %>% subset(Station %in% stations[42:length(stations)])
cams <- as.data.frame(cams[!endsWith(cams$External_ID, ".PNG"), 1])
colnames(cams) <- "External_ID"

# Iterate through camera values and find matches 
cams$camera <- NA
for (i in 1:length(cameras)) {
  pattern <- paste(cameras[i], collapse = "|")  # Create a pattern for matching
  matches <- grepl(pattern, cams$External_ID, ignore.case = TRUE)  # Find matches
  cams$camera[matches] <- cameras[i]  # Store the matching 'camera' value in 'test' column
}

json_dat <- distinct(merge(json_dat, cams, by = "External_ID", all.x = TRUE)) # add camera data 

## Now, to start the processing of json_dat, we will split it by the rounds of classifications. This way, we imagine the 
## entire file to consist of n different data sets, each representing one classification of a picture and its meta data. 
## Once these files are all clean, we can merge them and develop a consensus. 

## First, we create an index for each classification that was given per External_ID (the picture ID)
json_dat <- ddply(json_dat, .(External_ID), mutate, 
                  index = paste0('class', 1:length(External_ID)))   

table(json_dat$index) # there are 19 levels, i.e. two pictures have up to 19 classifications. 

# However, after the 9th round we suddenly only have a small number of reviews. To make the consensus easier we thus only keep the first 9. 
json_dat <- subset(json_dat, !(index %in% c("class10", "class11", "class12", "class13", "class14", "class15", "class16", "class17", "class18", "class19")))

## Now we can use that index as a splitting argument to create a list of 16 data frames, each containing only one classification per image. 
json_split <- split(json_dat, json_dat$index, drop = FALSE)
rm(stations, dir, remove_na_columns, bad_rows, cameras, i, matches, pattern, cams) # clean gl. env. 

## ~~~~~~~~~~~~~~~~~~~ (1) First Round of classification ~~~~~~~~~~~~~~~~~~~ 

dat1 <- json_split$class1 # get first round as df 
rownames(dat1) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

## Now we need to get rid of all empty lists inside the list that contains the bounding box objects. To do so, 
## we count the number of elements within each nested object. Then we remove those that are empty (have a length of zero).

nrow(subset(dat1, unlist(lapply(dat1$Object, function(x) length(x))) == 0)) # there should be 50752 empty images 
dat1_empty <- subset(dat1, unlist(lapply(dat1$Object, function(x) length(x))) == 0) # get empty pictures
dat1_empty$Object <- "empty" # tell R these pics are empty

dat1 <- subset(dat1, unlist(lapply(dat1$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat1) <- NULL # reset row names 

## Next, we start to step wise un-pack all the nested information that is inside 'object', 'meta', and 'review'.

## Un-nest first level of the "Object" and drop non-relevant columns 
dat1 <- dat1 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat1 <- dat1 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format. However, this is somewhat problematic 
## as the nested lists contain varying numbers of nested lists inside other nested lists... We need to have every 
## object as a singular data frame in order to properly widen it while maintaining the respective External ID and other
## meta data. To do so, we first need to expand the rows by the number of nested lists. This is tricky in a tidy context.
## Therefore, one solution is to check for the format after each iteration of unnest_longer() and only expand 
## those rows that already are at df level. Those that are still lists need to be expanded as >long< as it takes 
# until they are on df level. 

id_check <- unique(dat1$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat1$class_type <- as.character(lapply(dat1$classifications, class)) # 
table(dat1$class_type) # check distribution
dat1 <- subset(dat1, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat1 <- dat1 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat1$class_type <- as.character(lapply(dat1$classifications, class)) # check format 
table(dat1$class_type) # check distribution 
dat1 <- subset(dat1, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat1_ls <- subset(dat1, class_type == "list")  # these need further processing 

dat1_df <- subset(dat1, class_type == "data.frame") # these are ready
dat1_df <- dat1_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat1_df is the first batch of processed classifications. Therefore, we use it to overwrite dat1 and 
## continue to merge it with more readily parsed rows... 
dat1 <- dat1_df 
dat1$answer <- dat1$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat1_ls <- dat1_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat1_ls$class_type <- as.character(lapply(dat1_ls$classifications, class)) # check format
table(dat1_ls$class_type) # check format distribution 

dat1_df <- subset(dat1_ls, class_type == "data.frame") # these are the ones that are done
dat1_df <- dat1_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat1_df) <- colnames(dat1)  # make sure column names are equal 
dat1 <- data.frame(rbind(dat1, dat1_df)) # add readily parsed rows to dat1 

dat1_ls <- subset(dat1_ls, class_type == "list") # these are the ones that still require processing
dat1_ls$class_type <- as.character(lapply(dat1_ls$classifications, class))
table(dat1_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat1_ls <- dat1_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat1_ls) <- colnames(dat1)  # make sure column names are equal 
dat1 <- data.frame(rbind(dat1, dat1_ls)) # add readily parsed rows to dat1 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat1$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat1$External_ID)] # these are the ones 
lost1 <- subset(json_split$class1, External_ID %in% lost1)
rm(dat1_df, dat1_ls) # remove redundant data

## Now, from 55,458 unique External ID's, the row count of dat1 expanded to 77,006. This is due to duplicates 
## that occur when there are multiple bounding boxes on an image. As we are not interested in abundances but 
## rather presence / absence, we can disregard these images and reduce dat1 to only unique External IDs.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat1 <- dat1 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat1 <- distinct(dat1, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat1$class_type <- NULL # ditch non-relevant column 
dat1$position <- NULL # ditch non-relevant column 
colnames(dat1) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat1$Answer, class)))

## First, we ditch rows that have empty answers. 
dat1 <- subset(dat1, as.character(lapply(dat1$Answer, class)) != "NULL") # remove empty rows 

dat1_chr <- subset(dat1, as.character(lapply(dat1$Answer, class)) == "character") # these are the ones that are ready 

dat1_ls <- subset(dat1, as.character(lapply(dat1$Answer, class)) == "list") # these are the ones that need processing
dat1_ls <- subset(dat1_ls, as.character(lapply(dat1_ls$Answer, length)) == 1) # remove empty rows 
dat1_ls <- dat1_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat1_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat1_df <- subset(dat1_ls, as.character(lapply(dat1_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat1_df <- dat1_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat1_df) <- colnames(dat1)
dat1 <- data.frame(rbind(dat1_chr, dat1_df)) # make dat1_df and dat1_chr the new dat1 
dat1 <- dat1 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat1$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat1$External_ID)] # these are the ones 
lost1 <- subset(json_split$class1, External_ID %in% lost1) # full data

dat1_empty <- data.frame(rbind(dat1_empty, lost1)) # bind empty ones 
rm(dat1_df, dat1_ls, lost1, dat1_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat1 <- distinct(dat1, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat1$position <- NULL
colnames(dat1) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_1",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## The column "Class_1" containing the species identification is still a list. This is because sometimes citizen scientists
## assigned more than one species to a label. We assume that the first species given is the most accurate one and only keep that 
## one. While this may not be the most precise way to handle it, we still improve identification accuracy by having multiple rounds
## of identification. Also, we need our Class_1 column as a character and NOT as a list. 

dat1$Class_1 <- sapply(dat1$Class_1,"[[",1) # keep only first element in Class_1 lists

## We do end up with more rows than External ID's. Duplicates happen when there are multiple species groups
## per image. However, most combinations are from e.g. equipment and humans on the same picture. 
# If we e.g., subset it by only mammals, there should be no duplicates. 

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat1$Review, class))) # check if there are different forms of formats... NO :) 

dat1 <- dat1 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

## Sometimes, an image was reviewed by more than 1 expert, and sometimes a reviewer gave the value -1. We do the following: 
# We sum up all values (-1 for disagreement and 1 for agreement) and return the sum of all values where more than one review occurred. 
# All positive values mean a approved review, 0 and -1 mean no approval and will be turned to NA. We write a small function to do the job. 

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat1 <- dat1 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat1 <- dat1 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat1) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)

## This is a bit problematic as the number of levels per image differs a lot and because there are many NA's at different places. 
## Therefore, we parse the meta data in a different df and merge the parsed data back to our dat1 using the External_ID.

meta <- dplyr::select(dat1, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat1 <- Merge(dat1, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat1 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat1 <- Merge(dat1, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat1 by the External ID

dat1$Meta <- NULL # drop redundant meta data
dat1 <- dat1 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat1 <- dat1 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat1 <- dat1[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_1", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]
## Write to csv 
# write.csv(dat1, file ="classifications_1.csv", row.names = FALSE) # write file 

## We will store the parsed classification data in the same structure as the split json files. 
data_parsed <- list(dat1) # store relevant data  
data_empty <- list(dat1_empty) # store empty data

## Clean global environment to speed up the next round.. 
rm(dat1, dat1_empty)
json_split$class1 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (2) Second round of classification ~~~~~~~~~~~~~~~~~~~ 

dat2 <- json_split$class2 # get next round as df 
rownames(dat2) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat2, unlist(lapply(dat2$Object, function(x) length(x))) == 0)) 
dat2_empty <- subset(dat2, unlist(lapply(dat2$Object, function(x) length(x))) == 0) 
dat2_empty$Object <- "empty" 

dat2 <- subset(dat2, unlist(lapply(dat2$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat2) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat2 <- dat2 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat2 <- dat2 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat2$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat2$class_type <- as.character(lapply(dat2$classifications, class)) # 
table(dat2$class_type) # check distribution
dat2 <- subset(dat2, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat2 <- dat2 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat2$class_type <- as.character(lapply(dat2$classifications, class)) # check format 
table(dat2$class_type) # check distribution 
dat2 <- subset(dat2, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat2_ls <- subset(dat2, class_type == "list")  # these need further processing 
dat2_df <- subset(dat2, class_type == "data.frame") # these are ready
dat2_df <- dat2_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat2_df is the first batch of processed classifications. 
dat2 <- dat2_df 
dat2$answer <- dat2$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat2_ls <- dat2_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat2_ls$class_type <- as.character(lapply(dat2_ls$classifications, class)) # check format
table(dat2_ls$class_type) # check format distribution 

dat2_df <- subset(dat2_ls, class_type == "data.frame") # these are the ones that are done
dat2_df <- dat2_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat2_df) <- colnames(dat2)  # make sure column names are equal 
dat2 <- data.frame(rbind(dat2, dat2_df)) # add readily parsed rows to dat2 

dat2_ls <- subset(dat2_ls, class_type == "list") # these are the ones that still require processing
dat2_ls$class_type <- as.character(lapply(dat2_ls$classifications, class))
table(dat2_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat2_ls <- dat2_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat2_ls) <- colnames(dat2)  # make sure column names are equal 
dat2 <- data.frame(rbind(dat2, dat2_ls)) # add readily parsed rows to dat2 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat2$External_ID)) 
lost2 <- id_check[!(id_check %in% dat2$External_ID)] 
lost2 <- subset(json_split$class2, External_ID %in% lost2)
rm(dat2_df, dat2_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat2 <- dat2 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat2 <- distinct(dat2, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat2$class_type <- NULL # ditch non-relevant column 
dat2$position <- NULL # ditch non-relevant column 
colnames(dat2) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat2$Answer, class)))

## First, we ditch rows that have empty answers. 
dat2 <- subset(dat2, as.character(lapply(dat2$Answer, class)) != "NULL") # remove empty rows 

dat2_chr <- subset(dat2, as.character(lapply(dat2$Answer, class)) == "character") # these are the ones that are ready 

dat2_ls <- subset(dat2, as.character(lapply(dat2$Answer, class)) == "list") # these are the ones that need processing
dat2_ls <- subset(dat2_ls, as.character(lapply(dat2_ls$Answer, length)) == 1) # remove empty rows 
dat2_ls <- dat2_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat2_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat2_df <- subset(dat2_ls, as.character(lapply(dat2_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat2_df <- dat2_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat2_df) <- colnames(dat2)
dat2 <- data.frame(rbind(dat2_chr, dat2_df)) # make dat2_df and dat2_chr the new dat2 
dat2 <- dat2 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat2$External_ID)) - nrow(lost2) 
lost2 <- id_check[!(id_check %in% dat2$External_ID)] # these are the ones 
lost2 <- subset(json_split$class2, External_ID %in% lost2) # full data

dat2_empty <- data.frame(rbind(dat2_empty, lost2)) # bind empty ones 
rm(dat2_df, dat2_ls, lost2, dat2_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat2 <- distinct(dat2, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat2$position <- NULL
colnames(dat2) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_2",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat2$Class_2 <- sapply(dat2$Class_2,"[[",1) # keep only first element in Class_2 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat2$Review, class))) # check if there are different forms of formats... NO :) 
dat2 <- dat2 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat2 <- dat2 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat2 <- dat2 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat2) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_2", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat2, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat2 <- Merge(dat2, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat2 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat2 <- Merge(dat2, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat2 by the External ID

dat2$Meta <- NULL # drop redundant meta data
dat2 <- dat2 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat2 <- dat2[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_2", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat2), 2) # relevant data
data_empty <- append(data_empty, list(dat2_empty), 2) # empty data

## Clean global environment to speed up the next round.. 
rm(dat2, dat2_empty)
json_split$class2 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (3) Third round of classification ~~~~~~~~~~~~~~~~~~~ 
dat3 <- json_split$class3 # get next round as df 
rownames(dat3) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat3, unlist(lapply(dat3$Object, function(x) length(x))) == 0)) 
dat3_empty <- subset(dat3, unlist(lapply(dat3$Object, function(x) length(x))) == 0) 
dat3_empty$Object <- "empty" 

dat3 <- subset(dat3, unlist(lapply(dat3$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat3) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat3 <- dat3 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat3 <- dat3 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat3$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat3$class_type <- as.character(lapply(dat3$classifications, class)) # 
table(dat3$class_type) # check distribution
dat3 <- subset(dat3, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat3 <- dat3 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat3$class_type <- as.character(lapply(dat3$classifications, class)) # check format 
table(dat3$class_type) # check distribution 
dat3 <- subset(dat3, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat3_ls <- subset(dat3, class_type == "list")  # these need further processing 
dat3_df <- subset(dat3, class_type == "data.frame") # these are ready
dat3_df <- dat3_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat3_df is the first batch of processed classifications. 
dat3 <- dat3_df 
dat3$answer <- dat3$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat3_ls <- dat3_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat3_ls$class_type <- as.character(lapply(dat3_ls$classifications, class)) # check format
table(dat3_ls$class_type) # check format distribution 

dat3_df <- subset(dat3_ls, class_type == "data.frame") # these are the ones that are done
dat3_df <- dat3_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat3_df) <- colnames(dat3)  # make sure column names are equal 
dat3 <- data.frame(rbind(dat3, dat3_df)) # add readily parsed rows to dat3 

dat3_ls <- subset(dat3_ls, class_type == "list") # these are the ones that still require processing
dat3_ls$class_type <- as.character(lapply(dat3_ls$classifications, class))
table(dat3_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat3_ls <- dat3_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat3_ls) <- colnames(dat3)  # make sure column names are equal 
dat3 <- data.frame(rbind(dat3, dat3_ls)) # add readily parsed rows to dat3 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat3$External_ID)) 
lost3 <- id_check[!(id_check %in% dat3$External_ID)] 
lost3 <- subset(json_split$class3, External_ID %in% lost3)
rm(dat3_df, dat3_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat3 <- dat3 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat3 <- distinct(dat3, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat3$class_type <- NULL # ditch non-relevant column 
dat3$position <- NULL # ditch non-relevant column 
colnames(dat3) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat3$Answer, class)))

## First, we ditch rows that have empty answers. 
dat3 <- subset(dat3, as.character(lapply(dat3$Answer, class)) != "NULL") # remove empty rows 

dat3_chr <- subset(dat3, as.character(lapply(dat3$Answer, class)) == "character") # these are the ones that are ready 

dat3_ls <- subset(dat3, as.character(lapply(dat3$Answer, class)) == "list") # these are the ones that need processing
dat3_ls <- subset(dat3_ls, as.character(lapply(dat3_ls$Answer, length)) == 1) # remove empty rows 
dat3_ls <- dat3_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat3_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat3_df <- subset(dat3_ls, as.character(lapply(dat3_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat3_df <- dat3_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat3_df) <- colnames(dat3)
dat3 <- data.frame(rbind(dat3_chr, dat3_df)) # make dat3_df and dat3_chr the new dat3 
dat3 <- dat3 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat3$External_ID)) - nrow(lost3) 
lost3 <- id_check[!(id_check %in% dat3$External_ID)] # these are the ones 
lost3 <- subset(json_split$class3, External_ID %in% lost3) # full data

dat3_empty <- data.frame(rbind(dat3_empty, lost3)) # bind empty ones 
rm(dat3_df, dat3_ls, lost3, dat3_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat3 <- distinct(dat3, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat3$position <- NULL
colnames(dat3) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_3",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat3$Class_3 <- sapply(dat3$Class_3,"[[",1) # keep only first element in Class_3 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat3$Review, class))) # check if there are different forms of formats... NO :) 
dat3 <- dat3 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat3 <- dat3 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat3 <- dat3 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat3) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_3", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat3, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat3 <- Merge(dat3, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat3 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat3 <- Merge(dat3, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat3 by the External ID

dat3$Meta <- NULL # drop redundant meta data
dat3 <- dat3 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat3 <- dat3 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat3 <- dat3[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_3", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat3), 3) # relevant data
data_empty <- append(data_empty, list(dat3_empty), 3) # empty data

## Clean global environment to speed up the next round.. 
rm(dat3, dat3_empty)
json_split$class3 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (4) Fourth round of classification ~~~~~~~~~~~~~~~~~~~ 
dat4 <- json_split$class4 # get next round as df 
rownames(dat4) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat4, unlist(lapply(dat4$Object, function(x) length(x))) == 0)) 
dat4_empty <- subset(dat4, unlist(lapply(dat4$Object, function(x) length(x))) == 0) 
dat4_empty$Object <- "empty" 

dat4 <- subset(dat4, unlist(lapply(dat4$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat4) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat4 <- dat4 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat4 <- dat4 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat4$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat4$class_type <- as.character(lapply(dat4$classifications, class)) # 
table(dat4$class_type) # check distribution
dat4 <- subset(dat4, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat4 <- dat4 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat4$class_type <- as.character(lapply(dat4$classifications, class)) # check format 
table(dat4$class_type) # check distribution 
dat4 <- subset(dat4, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat4_ls <- subset(dat4, class_type == "list")  # these need further processing 
dat4_df <- subset(dat4, class_type == "data.frame") # these are ready
dat4_df <- dat4_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat4_df is the first batch of processed classifications. 
dat4 <- dat4_df 
dat4$answer <- dat4$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat4_ls <- dat4_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat4_ls$class_type <- as.character(lapply(dat4_ls$classifications, class)) # check format
table(dat4_ls$class_type) # check format distribution 

dat4_df <- subset(dat4_ls, class_type == "data.frame") # these are the ones that are done
dat4_df <- dat4_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat4_df) <- colnames(dat4)  # make sure column names are equal 
dat4 <- data.frame(rbind(dat4, dat4_df)) # add readily parsed rows to dat4 

dat4_ls <- subset(dat4_ls, class_type == "list") # these are the ones that still require processing
dat4_ls$class_type <- as.character(lapply(dat4_ls$classifications, class))
table(dat4_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat4_ls <- dat4_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat4_ls) <- colnames(dat4)  # make sure column names are equal 
dat4 <- data.frame(rbind(dat4, dat4_ls)) # add readily parsed rows to dat4 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat4$External_ID)) 
lost4 <- id_check[!(id_check %in% dat4$External_ID)] 
lost4 <- subset(json_split$class4, External_ID %in% lost4)
rm(dat4_df, dat4_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat4 <- dat4 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat4 <- distinct(dat4, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat4$class_type <- NULL # ditch non-relevant column 
dat4$position <- NULL # ditch non-relevant column 
colnames(dat4) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat4$Answer, class)))

## First, we ditch rows that have empty answers. 
dat4 <- subset(dat4, as.character(lapply(dat4$Answer, class)) != "NULL") # remove empty rows 

dat4_chr <- subset(dat4, as.character(lapply(dat4$Answer, class)) == "character") # these are the ones that are ready 

dat4_ls <- subset(dat4, as.character(lapply(dat4$Answer, class)) == "list") # these are the ones that need processing
dat4_ls <- subset(dat4_ls, as.character(lapply(dat4_ls$Answer, length)) == 1) # remove empty rows 
dat4_ls <- dat4_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat4_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat4_df <- subset(dat4_ls, as.character(lapply(dat4_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat4_df <- dat4_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat4_df) <- colnames(dat4)
dat4 <- data.frame(rbind(dat4_chr, dat4_df)) # make dat4_df and dat4_chr the new dat4 
dat4 <- dat4 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat4$External_ID)) - nrow(lost4) 
lost4 <- id_check[!(id_check %in% dat4$External_ID)] # these are the ones 
lost4 <- subset(json_split$class4, External_ID %in% lost4) # full data

dat4_empty <- data.frame(rbind(dat4_empty, lost4)) # bind empty ones 
rm(dat4_df, dat4_ls, lost4, dat4_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat4 <- distinct(dat4, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat4$position <- NULL
colnames(dat4) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_4",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat4$Class_4 <- sapply(dat4$Class_4,"[[",1) # keep only first element in Class_4 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat4$Review, class))) # check if there are different forms of formats... NO :) 
dat4 <- dat4 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat4 <- dat4 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat4 <- dat4 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat4) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_4", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat4, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat4 <- Merge(dat4, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat4 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat4 <- Merge(dat4, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat4 by the External ID

dat4$Meta <- NULL # drop redundant meta data
dat4 <- dat4 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat4 <- dat4 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat4 <- dat4[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_4", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat4), 4) # relevant data
data_empty <- append(data_empty, list(dat4_empty), 4) # empty data

## Clean global environment to speed up the next round.. 
rm(dat4, dat4_empty)
json_split$class4 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (5) Fifth round of classification ~~~~~~~~~~~~~~~~~~~ 

dat5 <- json_split$class5 # get next round as df 
rownames(dat5) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat5, unlist(lapply(dat5$Object, function(x) length(x))) == 0)) 
dat5_empty <- subset(dat5, unlist(lapply(dat5$Object, function(x) length(x))) == 0) 
dat5_empty$Object <- "empty" 

dat5 <- subset(dat5, unlist(lapply(dat5$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat5) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat5 <- dat5 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat5 <- dat5 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat5$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat5$class_type <- as.character(lapply(dat5$classifications, class)) # 
table(dat5$class_type) # check distribution
dat5 <- subset(dat5, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat5 <- dat5 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat5$class_type <- as.character(lapply(dat5$classifications, class)) # check format 
table(dat5$class_type) # check distribution 
dat5 <- subset(dat5, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat5_ls <- subset(dat5, class_type == "list")  # these need further processing 
dat5_df <- subset(dat5, class_type == "data.frame") # these are ready
dat5_df <- dat5_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat5_df is the first batch of processed classifications. 
dat5 <- dat5_df 
dat5$answer <- dat5$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat5_ls <- dat5_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat5_ls$class_type <- as.character(lapply(dat5_ls$classifications, class)) # check format
table(dat5_ls$class_type) # check format distribution 

dat5_df <- subset(dat5_ls, class_type == "data.frame") # these are the ones that are done
dat5_df <- dat5_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat5_df) <- colnames(dat5)  # make sure column names are equal 
dat5 <- data.frame(rbind(dat5, dat5_df)) # add readily parsed rows to dat5 

dat5_ls <- subset(dat5_ls, class_type == "list") # these are the ones that still require processing
dat5_ls$class_type <- as.character(lapply(dat5_ls$classifications, class))
table(dat5_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat5_ls <- dat5_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat5_ls) <- colnames(dat5)  # make sure column names are equal 
dat5 <- data.frame(rbind(dat5, dat5_ls)) # add readily parsed rows to dat5 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat5$External_ID)) 
lost5 <- id_check[!(id_check %in% dat5$External_ID)] 
lost5 <- subset(json_split$class5, External_ID %in% lost5)
rm(dat5_df, dat5_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat5 <- dat5 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat5 <- distinct(dat5, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat5$class_type <- NULL # ditch non-relevant column 
dat5$position <- NULL # ditch non-relevant column 
colnames(dat5) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat5$Answer, class)))

## First, we ditch rows that have empty answers. 
dat5 <- subset(dat5, as.character(lapply(dat5$Answer, class)) != "NULL") # remove empty rows 

dat5_chr <- subset(dat5, as.character(lapply(dat5$Answer, class)) == "character") # these are the ones that are ready 

dat5_ls <- subset(dat5, as.character(lapply(dat5$Answer, class)) == "list") # these are the ones that need processing
dat5_ls <- subset(dat5_ls, as.character(lapply(dat5_ls$Answer, length)) == 1) # remove empty rows 
dat5_ls <- dat5_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat5_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat5_df <- subset(dat5_ls, as.character(lapply(dat5_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat5_df <- dat5_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat5_df) <- colnames(dat5)
dat5 <- data.frame(rbind(dat5_chr, dat5_df)) # make dat5_df and dat5_chr the new dat5 
dat5 <- dat5 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat5$External_ID)) - nrow(lost5) 
lost5 <- id_check[!(id_check %in% dat5$External_ID)] # these are the ones 
lost5 <- subset(json_split$class5, External_ID %in% lost5) # full data

dat5_empty <- data.frame(rbind(dat5_empty, lost5)) # bind empty ones 
rm(dat5_df, dat5_ls, lost5, dat5_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat5 <- distinct(dat5, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat5$position <- NULL
colnames(dat5) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_5",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat5$Class_5 <- sapply(dat5$Class_5,"[[",1) # keep only first element in Class_5 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat5$Review, class))) # check if there are different forms of formats... NO :) 
dat5 <- dat5 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat5 <- dat5 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat5 <- dat5 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat5) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_5", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat5, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat5 <- Merge(dat5, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat5 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat5 <- Merge(dat5, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat5 by the External ID

dat5$Meta <- NULL # drop redundant meta data
dat5 <- dat5 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat5 <- dat5 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat5 <- dat5[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_5", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat5), 5) # relevant data
data_empty <- append(data_empty, list(dat5_empty), 5) # empty data

## Clean global environment to speed up the next round.. 
rm(dat5, dat5_empty)
json_split$class5 <- NULL # drop redundant data


## ~~~~~~~~~~~~~~~~~~~ (6) Sixth round of classification ~~~~~~~~~~~~~~~~~~~ 

dat6 <- json_split$class6 # get next round as df 
rownames(dat6) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat6, unlist(lapply(dat6$Object, function(x) length(x))) == 0)) 
dat6_empty <- subset(dat6, unlist(lapply(dat6$Object, function(x) length(x))) == 0) 
dat6_empty$Object <- "empty" 

dat6 <- subset(dat6, unlist(lapply(dat6$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat6) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat6 <- dat6 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat6 <- dat6 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat6$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat6$class_type <- as.character(lapply(dat6$classifications, class)) # 
table(dat6$class_type) # check distribution
dat6 <- subset(dat6, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat6 <- dat6 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat6$class_type <- as.character(lapply(dat6$classifications, class)) # check format 
table(dat6$class_type) # check distribution 
dat6 <- subset(dat6, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat6_ls <- subset(dat6, class_type == "list")  # these need further processing 
dat6_df <- subset(dat6, class_type == "data.frame") # these are ready
dat6_df <- dat6_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat6_df is the first batch of processed classifications. 
dat6 <- dat6_df 
dat6$answer <- dat6$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat6_ls <- dat6_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat6_ls$class_type <- as.character(lapply(dat6_ls$classifications, class)) # check format
table(dat6_ls$class_type) # check format distribution 

dat6_df <- subset(dat6_ls, class_type == "data.frame") # these are the ones that are done
dat6_df <- dat6_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat6_df) <- colnames(dat6)  # make sure column names are equal 
dat6 <- data.frame(rbind(dat6, dat6_df)) # add readily parsed rows to dat6 

dat6_ls <- subset(dat6_ls, class_type == "list") # these are the ones that still require processing
dat6_ls$class_type <- as.character(lapply(dat6_ls$classifications, class))
table(dat6_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat6_ls <- dat6_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat6_ls) <- colnames(dat6)  # make sure column names are equal 
dat6 <- data.frame(rbind(dat6, dat6_ls)) # add readily parsed rows to dat6 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat6$External_ID)) 
lost6 <- id_check[!(id_check %in% dat6$External_ID)] 
lost6 <- subset(json_split$class6, External_ID %in% lost6)
rm(dat6_df, dat6_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat6 <- dat6 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat6 <- distinct(dat6, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat6$class_type <- NULL # ditch non-relevant column 
dat6$position <- NULL # ditch non-relevant column 
colnames(dat6) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat6$Answer, class)))

## First, we ditch rows that have empty answers. 
dat6 <- subset(dat6, as.character(lapply(dat6$Answer, class)) != "NULL") # remove empty rows 

dat6_chr <- subset(dat6, as.character(lapply(dat6$Answer, class)) == "character") # these are the ones that are ready 

dat6_ls <- subset(dat6, as.character(lapply(dat6$Answer, class)) == "list") # these are the ones that need processing
dat6_ls <- subset(dat6_ls, as.character(lapply(dat6_ls$Answer, length)) == 1) # remove empty rows 
dat6_ls <- dat6_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat6_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat6_df <- subset(dat6_ls, as.character(lapply(dat6_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat6_df <- dat6_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat6_df) <- colnames(dat6)
dat6 <- data.frame(rbind(dat6_chr, dat6_df)) # make dat6_df and dat6_chr the new dat6 
dat6 <- dat6 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat6$External_ID)) - nrow(lost6) 
lost6 <- id_check[!(id_check %in% dat6$External_ID)] # these are the ones 
lost6 <- subset(json_split$class6, External_ID %in% lost6) # full data

dat6_empty <- data.frame(rbind(dat6_empty, lost6)) # bind empty ones 
rm(dat6_df, dat6_ls, lost6, dat6_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat6 <- distinct(dat6, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat6$position <- NULL
colnames(dat6) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_6",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat6$Class_6 <- sapply(dat6$Class_6,"[[",1) # keep only first element in Class_6 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat6$Review, class))) # check if there are different forms of formats... NO :) 
dat6 <- dat6 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat6 <- dat6 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat6 <- dat6 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat6) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_6", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat6, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat6 <- Merge(dat6, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat6 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat6 <- Merge(dat6, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat6 by the External ID

dat6$Meta <- NULL # drop redundant meta data
dat6 <- dat6 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat6 <- dat6 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat6 <- dat6[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_6", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat6), 6) # relevant data
data_empty <- append(data_empty, list(dat6_empty), 6) # empty data

## Clean global environment to speed up the next round.. 
rm(dat6, dat6_empty)
json_split$class6 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (7) Seventh round of classification ~~~~~~~~~~~~~~~~~~~ 

dat7 <- json_split$class7 # get next round as df 
rownames(dat7) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat7, unlist(lapply(dat7$Object, function(x) length(x))) == 0)) 
dat7_empty <- subset(dat7, unlist(lapply(dat7$Object, function(x) length(x))) == 0) 
dat7_empty$Object <- "empty" 

dat7 <- subset(dat7, unlist(lapply(dat7$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat7) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat7 <- dat7 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat7 <- dat7 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat7$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat7$class_type <- as.character(lapply(dat7$classifications, class)) # 
table(dat7$class_type) # check distribution
dat7 <- subset(dat7, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat7 <- dat7 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat7$class_type <- as.character(lapply(dat7$classifications, class)) # check format 
table(dat7$class_type) # check distribution 
dat7 <- subset(dat7, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat7_ls <- subset(dat7, class_type == "list")  # these need further processing 
dat7_df <- subset(dat7, class_type == "data.frame") # these are ready
dat7_df <- dat7_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat7_df is the first batch of processed classifications. 
dat7 <- dat7_df 
dat7$answer <- dat7$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat7_ls <- dat7_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat7_ls$class_type <- as.character(lapply(dat7_ls$classifications, class)) # check format
table(dat7_ls$class_type) # check format distribution 

dat7_df <- subset(dat7_ls, class_type == "data.frame") # these are the ones that are done
dat7_df <- dat7_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat7_df) <- colnames(dat7)  # make sure column names are equal 
dat7 <- data.frame(rbind(dat7, dat7_df)) # add readily parsed rows to dat7 

dat7_ls <- subset(dat7_ls, class_type == "list") # these are the ones that still require processing
dat7_ls$class_type <- as.character(lapply(dat7_ls$classifications, class))
table(dat7_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat7_ls <- dat7_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat7_ls) <- colnames(dat7)  # make sure column names are equal 
dat7 <- data.frame(rbind(dat7, dat7_ls)) # add readily parsed rows to dat7 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat7$External_ID)) 
lost7 <- id_check[!(id_check %in% dat7$External_ID)] 
lost7 <- subset(json_split$class7, External_ID %in% lost7)
rm(dat7_df, dat7_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat7 <- dat7 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat7 <- distinct(dat7, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat7$class_type <- NULL # ditch non-relevant column 
dat7$position <- NULL # ditch non-relevant column 
colnames(dat7) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat7$Answer, class)))

## First, we ditch rows that have empty answers. 
dat7 <- subset(dat7, as.character(lapply(dat7$Answer, class)) != "NULL") # remove empty rows 

dat7_chr <- subset(dat7, as.character(lapply(dat7$Answer, class)) == "character") # these are the ones that are ready 

dat7_ls <- subset(dat7, as.character(lapply(dat7$Answer, class)) == "list") # these are the ones that need processing
dat7_ls <- subset(dat7_ls, as.character(lapply(dat7_ls$Answer, length)) == 1) # remove empty rows 
dat7_ls <- dat7_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat7_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat7_df <- subset(dat7_ls, as.character(lapply(dat7_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat7_df <- dat7_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat7_df) <- colnames(dat7)
dat7 <- data.frame(rbind(dat7_chr, dat7_df)) # make dat7_df and dat7_chr the new dat7 
dat7 <- dat7 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat7$External_ID)) - nrow(lost7) 
lost7 <- id_check[!(id_check %in% dat7$External_ID)] # these are the ones 
lost7 <- subset(json_split$class7, External_ID %in% lost7) # full data

dat7_empty <- data.frame(rbind(dat7_empty, lost7)) # bind empty ones 
rm(dat7_df, dat7_ls, lost7, dat7_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat7 <- distinct(dat7, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat7$position <- NULL
colnames(dat7) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_7",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat7$Class_7 <- sapply(dat7$Class_7,"[[",1) # keep only first element in Class_7 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat7$Review, class))) # check if there are different forms of formats... NO :) 
dat7 <- dat7 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat7 <- dat7 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat7 <- dat7 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat7) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_7", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat7, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat7 <- Merge(dat7, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat7 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat7 <- Merge(dat7, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat7 by the External ID

dat7$Meta <- NULL # drop redundant meta data
dat7 <- dat7 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat7 <- dat7 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat7 <- dat7[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_7", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat7), 7) # relevant data
data_empty <- append(data_empty, list(dat7_empty), 7) # empty data

## Clean global environment to speed up the next round.. 
rm(dat7, dat7_empty)
json_split$class7 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (8) Eight round of classification ~~~~~~~~~~~~~~~~~~~ 
dat8 <- json_split$class8 # get next round as df 
rownames(dat8) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat8, unlist(lapply(dat8$Object, function(x) length(x))) == 0)) 
dat8_empty <- subset(dat8, unlist(lapply(dat8$Object, function(x) length(x))) == 0) 
dat8_empty$Object <- "empty" 

dat8 <- subset(dat8, unlist(lapply(dat8$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat8) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat8 <- dat8 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat8 <- dat8 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat8$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat8$class_type <- as.character(lapply(dat8$classifications, class)) # 
table(dat8$class_type) # check distribution
dat8 <- subset(dat8, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat8 <- dat8 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat8$class_type <- as.character(lapply(dat8$classifications, class)) # check format 
table(dat8$class_type) # check distribution 
dat8 <- subset(dat8, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat8_ls <- subset(dat8, class_type == "list")  # these need further processing 
dat8_df <- subset(dat8, class_type == "data.frame") # these are ready
dat8_df <- dat8_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat8_df is the first batch of processed classifications. 
dat8 <- dat8_df 
dat8$answer <- dat8$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat8_ls <- dat8_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat8_ls$class_type <- as.character(lapply(dat8_ls$classifications, class)) # check format
table(dat8_ls$class_type) # check format distribution 

dat8_df <- subset(dat8_ls, class_type == "data.frame") # these are the ones that are done
dat8_df <- dat8_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat8_df) <- colnames(dat8)  # make sure column names are equal 
dat8 <- data.frame(rbind(dat8, dat8_df)) # add readily parsed rows to dat8 

dat8_ls <- subset(dat8_ls, class_type == "list") # these are the ones that still require processing
dat8_ls$class_type <- as.character(lapply(dat8_ls$classifications, class))
table(dat8_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat8_ls <- dat8_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat8_ls) <- colnames(dat8)  # make sure column names are equal 
dat8 <- data.frame(rbind(dat8, dat8_ls)) # add readily parsed rows to dat8 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat8$External_ID)) 
lost8 <- id_check[!(id_check %in% dat8$External_ID)] 
lost8 <- subset(json_split$class8, External_ID %in% lost8)
rm(dat8_df, dat8_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat8 <- dat8 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat8 <- distinct(dat8, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat8$class_type <- NULL # ditch non-relevant column 
dat8$position <- NULL # ditch non-relevant column 
colnames(dat8) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat8$Answer, class)))

## First, we ditch rows that have empty answers. 
dat8 <- subset(dat8, as.character(lapply(dat8$Answer, class)) != "NULL") # remove empty rows 

dat8_chr <- subset(dat8, as.character(lapply(dat8$Answer, class)) == "character") # these are the ones that are ready 

dat8_ls <- subset(dat8, as.character(lapply(dat8$Answer, class)) == "list") # these are the ones that need processing
dat8_ls <- subset(dat8_ls, as.character(lapply(dat8_ls$Answer, length)) == 1) # remove empty rows 
dat8_ls <- dat8_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat8_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat8_df <- subset(dat8_ls, as.character(lapply(dat8_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat8_df <- dat8_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat8_df) <- colnames(dat8)
dat8 <- data.frame(rbind(dat8_chr, dat8_df)) # make dat8_df and dat8_chr the new dat8 
dat8 <- dat8 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat8$External_ID)) - nrow(lost8) 
lost8 <- id_check[!(id_check %in% dat8$External_ID)] # these are the ones 
lost8 <- subset(json_split$class8, External_ID %in% lost8) # full data

dat8_empty <- data.frame(rbind(dat8_empty, lost8)) # bind empty ones 
rm(dat8_df, dat8_ls, lost8, dat8_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat8 <- distinct(dat8, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat8$position <- NULL
colnames(dat8) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_8",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat8$Class_8 <- sapply(dat8$Class_8,"[[",1) # keep only first element in Class_8 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat8$Review, class))) # check if there are different forms of formats... NO :) 
dat8 <- dat8 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat8 <- dat8 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat8 <- dat8 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat8) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_8", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat8, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat8 <- Merge(dat8, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat8 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat8 <- Merge(dat8, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat8 by the External ID

dat8$Meta <- NULL # drop redundant meta data
dat8 <- dat8 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat8 <- dat8 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat8 <- dat8[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_8", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat8), 8) # relevant data
data_empty <- append(data_empty, list(dat8_empty), 8) # empty data

## Clean global environment to speed up the next round.. 
rm(dat8, dat8_empty)
json_split$class8 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (9) Ninth round of classification ~~~~~~~~~~~~~~~~~~~ 
dat9 <- json_split$class9 # get next round as df 
rownames(dat9) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

nrow(subset(dat9, unlist(lapply(dat9$Object, function(x) length(x))) == 0)) 
dat9_empty <- subset(dat9, unlist(lapply(dat9$Object, function(x) length(x))) == 0) 
dat9_empty$Object <- "empty" 

dat9 <- subset(dat9, unlist(lapply(dat9$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat9) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat9 <- dat9 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI)

## Un-nest bounding box coordinates & collapse to one variable 
dat9 <- dat9 %>% mutate(bbox = map_chr(bbox, ~ toString(unlist(.x)))) 

## From here on, we need to un-nest classification object to a wide format.
id_check <- unique(dat9$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat9$class_type <- as.character(lapply(dat9$classifications, class)) # 
table(dat9$class_type) # check distribution
dat9 <- subset(dat9, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat9 <- dat9 %>% unnest_longer(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat9$class_type <- as.character(lapply(dat9$classifications, class)) # check format 
table(dat9$class_type) # check distribution 
dat9 <- subset(dat9, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat9_ls <- subset(dat9, class_type == "list")  # these need further processing 
dat9_df <- subset(dat9, class_type == "data.frame") # these are ready
dat9_df <- dat9_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns# expand to columns

## dat9_df is the first batch of processed classifications. 
dat9 <- dat9_df 
dat9$answer <- dat9$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat9_ls <- dat9_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat9_ls$class_type <- as.character(lapply(dat9_ls$classifications, class)) # check format
table(dat9_ls$class_type) # check format distribution 

dat9_df <- subset(dat9_ls, class_type == "data.frame") # these are the ones that are done
dat9_df <- dat9_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat9_df) <- colnames(dat9)  # make sure column names are equal 
dat9 <- data.frame(rbind(dat9, dat9_df)) # add readily parsed rows to dat9 

dat9_ls <- subset(dat9_ls, class_type == "list") # these are the ones that still require processing
dat9_ls$class_type <- as.character(lapply(dat9_ls$classifications, class))
table(dat9_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat9_ls <- dat9_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat9_ls) <- colnames(dat9)  # make sure column names are equal 
dat9 <- data.frame(rbind(dat9, dat9_ls)) # add readily parsed rows to dat9 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat9$External_ID)) 
lost9 <- id_check[!(id_check %in% dat9$External_ID)] 
lost9 <- subset(json_split$class9, External_ID %in% lost9)
rm(dat9_df, dat9_ls) # remove redundant data.

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat9 <- dat9 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat9 <- distinct(dat9, External_ID, title, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat9$class_type <- NULL # ditch non-relevant column 
dat9$position <- NULL # ditch non-relevant column 
colnames(dat9) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Answer",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

## Now we continue to un-nest the following level of the classification object: The answer, i.e., 
## the taxonomic species identification. As the next line shows, we need to follow a similar approach: 
table(as.character(lapply(dat9$Answer, class)))

## First, we ditch rows that have empty answers. 
dat9 <- subset(dat9, as.character(lapply(dat9$Answer, class)) != "NULL") # remove empty rows 

dat9_chr <- subset(dat9, as.character(lapply(dat9$Answer, class)) == "character") # these are the ones that are ready 

dat9_ls <- subset(dat9, as.character(lapply(dat9$Answer, class)) == "list") # these are the ones that need processing
dat9_ls <- subset(dat9_ls, as.character(lapply(dat9_ls$Answer, length)) == 1) # remove empty rows 
dat9_ls <- dat9_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat9_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat9_df <- subset(dat9_ls, as.character(lapply(dat9_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat9_df <- dat9_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat9_df) <- colnames(dat9)
dat9 <- data.frame(rbind(dat9_chr, dat9_df)) # make dat9_df and dat9_chr the new dat9 
dat9 <- dat9 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

# we again check how much External_ID's we lost due to empty classifications.

length(id_check) - length(unique(dat9$External_ID)) - nrow(lost9) 
lost9 <- id_check[!(id_check %in% dat9$External_ID)] # these are the ones 
lost9 <- subset(json_split$class9, External_ID %in% lost9) # full data

dat9_empty <- data.frame(rbind(dat9_empty, lost9)) # bind empty ones 
rm(dat9_df, dat9_ls, lost9, dat9_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat9 <- distinct(dat9, External_ID, Answer, n_indiv, .keep_all = TRUE) # filter only distinct rows 
dat9$position <- NULL
colnames(dat9) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh", "Group", "Class_9",     
                    "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 
dat9$Class_9 <- sapply(dat9$Class_9,"[[",1) # keep only first element in Class_9 lists

## The next step is to un-nest the Review object, e.g., the expert review if it was done. Here, we end up with a 
## score giving the expert review (1 == thumbs up).

table(as.character(lapply(dat9$Review, class))) # check if there are different forms of formats... NO :) 
dat9 <- dat9 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

# Parse review 
sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat9 <- dat9 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat9 <- dat9 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat9) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "Bbox_tlwh","Group",      
                    "Class_9", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

## Lastly, we un-nest the meta data which includes image quality and a number of comments (e.g. top ten, outstanding picture, etc.)
meta <- dplyr::select(dat9, External_ID, Meta) # Extract the nested list with the respective external ID 
table(as.character(lapply(meta$Meta, class))) # we again have to treat lists and df's differently. 

## In this case, if the class is a list, this means the meta object is empty. We can ignore these cases: 
meta <- subset(meta, as.character(lapply(meta$Meta, class)) == "data.frame") # these are the ones that contain data

## First, we deal with those that are already on df level.
meta <- meta %>% unnest_wider(., Meta) %>% # expand first level to columns
  dplyr::select(-featureId, -schemaId, -value) # drop non-relevant columns 
meta <- meta %>% unnest_longer(., answer) # un-nest second level in answer
meta$Quality <- meta$answer$title # get relevant answer value
meta$answer <- NULL # drop redundant answer
meta <- meta %>% unnest_longer(., answers) # un-nest other answers 
meta$title <- NULL # drop non-relevant column
meta$position <- NULL # drop non-relevant columns
meta <- meta %>% unnest_wider(., answers) %>% # un-nest second answers level
  dplyr::select(-featureId, -schemaId, -value, -position) # drop non-relevant columns
meta <- meta %>% unnest_longer(., Quality) # un-nest last level inside Quality
colnames(meta) <- c("External_ID", "Comment", "Quality") # set column names
meta <- meta %>% unnest_longer(., Comment) # un-nest comments if there are multiple per External_ID

## Now we can melt the meta df by the External ID and then step-wise subset it by the desired variables. 
meta <- na.omit(reshape2::melt(meta, id.vars = c("External_ID"))) # Melt by External ID

## First, we deal with the image quality
meta_quality <- distinct(subset(meta, variable == "Quality"), External_ID, .keep_all = TRUE) # Keep only Quality data of distinct External IDs
meta_quality$variable <- NULL # drop non-relevant column 
colnames(meta_quality) <- c("External_ID", "Quality") # set column names
dat9 <- Merge(dat9, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat9 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat9 <- Merge(dat9, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat9 by the External ID

dat9$Meta <- NULL # drop redundant meta data
dat9 <- dat9 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat9 <- dat9 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat9 <- dat9[ ,c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Class_9", "Score",
                 "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment")]

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat9), 9) # relevant data
data_empty <- append(data_empty, list(dat9_empty), 9) # empty data

## Clean global environment to speed up the next round.. 
rm(dat9, dat9_empty)
json_split$class9 <- NULL # drop redundant data

## -------------------- Build Master file --------------------

## Properly name objects in data lists 
names(data_parsed) <- c("class1","class2","class3","class4","class5","class6","class7","class8", "class9")
names(data_empty) <- c("class1","class2","class3","class4","class5","class6","class7","class8", "class9")

## Clean global environment
rm(json_split, id_check, sum_list)

## Finally, we build a master file that contains all the parsed data in a single df. To do so, we need to standardize
## the column names inside the list data_parsed and bind all rows together: 

data_parsed <- lapply(data_parsed, setNames, # standardize column names
                      c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Classification", "Score",
                        "Reviewer", "Station", "Camera", "Bbox_tlwh", "n_indiv", "Quality", "Comment"))

dat.all <- do.call("rbind", data_parsed) # bind data to master file
rownames(dat.all) <- NULL # reset row names

write.csv(dat.all, file = "json_parsed_all_30.10.23.csv", row.names = FALSE) # write file
dir <- getwd() # also save the current workspace 
save.image(paste0(dir, "/finished_json_workspace.RData"))
