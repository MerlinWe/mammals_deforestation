#######################################################################################################
####################################### WildLive! JSON Parsing ########################################
#######################################################################################################

## This Script cleans and processes the raw JSON data retrieved from the WildLive! Project. 
## Written by: Merlin Weiss (2022); merlin.s.weiss@gmail.com

## ~~~~~~~~~ Prepare Work space ~~~~~~~~~~ 

## First: active required package libraries. 

if(!require(Hmisc)){install.packages("Hmisc")}   
if(!require(gdata)){install.packages("gdata")}   
if(!require(data.table)){install.packages("data.table")}
if(!require(plyr)){install.packages("plyr")} 
if(!require(forcats)){install.packages("forcats")}  
if(!require(stringr)){install.packages("stringr")}  
if(!require(tidyr)){install.packages("tidyr")}  
if(!require(purrr)){install.packages("purrr")}  
if(!require(jsonlite)){install.packages("jsonlite")}  
if(!require(dplyr)){install.packages("tidyverse")}        

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
# Briefly write a simple function to remove all columns that only contain NAs
remove_na_columns <- function(json_dat) {
  na_columns <- which(colSums(is.na(json_dat)) == nrow(json_dat))
  json_dat <- json_dat[, -na_columns]
  return(json_dat)}
json_dat <- remove_na_columns(json_dat)

# Adjust column names 
json_dat <- json_dat[ ,-c(23:26)] # remove irrelevant workflow columns
colnames(json_dat) <- c("ID","DataRow_ID", "Link", "Created_By", "Project_Name", "Created_At", "Updated_At",
                        "Seconds_to_Label", "Seconds_to_Review", "Seconds_to_Create", "External_ID", #"Global_Key",
                        "Agreement", "Is_Benchmark", "Benchmark_Agreement", "Dataset_Name", 
                        "Review", "View_Label", "Has_Open_Issues", "Skipped", "Object", "Meta", "Label.relationships")

## Next, some formatting: Make sure R understands 'Created At' as POSIXct (Date format). This should refer to the 
## point in time when the label was created. The format contains weird elements which we need to clean out before we are able to format it as POSIXct. 

json_dat <- as_tibble(json_dat) %>%
  # Transform `Created At` column
  mutate(Created_At = sub("[^0-9.-]", " ", Created_At)) %>%
  mutate(Created_At = gsub(".{5}$", "",    Created_At)) %>%
  mutate(Created_At = as.POSIXct(Created_At, format = "%Y-%m-%d %H:%M:%S")) %>% 
  # Transform `Updated At` column 
  mutate(Updated_At = sub("[^0-9.-]", " ", Updated_At)) %>%
  mutate(Updated_At = gsub(".{5}$", "",    Updated_At)) %>%
  mutate(Updated_At = as.POSIXct(Updated_At, format = "%Y-%m-%d %H:%M:%S")) %>%
  # Drop non-relevant column
  dplyr::select(External_ID, Created_By, Link, Created_At, Updated_At, Agreement, Object, Review, Meta)

## We also extract the station and camera from the External ID and store in in a new column. 
## To do so we create a vector containing all station IDs and use it to match station id inside the External ID. 
# This is needed because the position of this information in the External_ID is not always the same. We get the
# input vector from the camtrap.csv file. 

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

# We can extract the station information from external ID as good as possible, but extracting the camera information is more tricky. 
# For now, we only extract the camera information of images recorded at grid stations that are also JPEG images. Their External_ID 
# should all be in the same structure and we can more or less easily extract the required information. 

# Extract Stations
json_dat <- json_dat %>%
  filter(!grepl("EK", External_ID)) %>%
  mutate(Station = map_chr(External_ID, ~ stations[str_detect(.x, stations)]))

# Extract Cameras 
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
                  index = paste0('Class', 1:length(External_ID)))

table(json_dat$index) # there are 24 levels, thus up to 24 labels for some pictures

## Now we can use that index as a splitting argument to create a list of tibbles, each containing only one classification per image. 
json_dat <- json_dat %>% as_tibble() %>% 
  mutate(index = factor(index, levels = c("Class1", "Class2", "Class3", "Class4", "Class5", "Class6", "Class7", "Class8",
                                          "Class9", "Class10", "Class11", "Class12", "Class13", "Class14", "Class15", "Class16",
                                          "Class17", "Class18", "Class19", "Class20", "Class21", "Class22", "Class23", "Class24")))
json_split <- split(json_dat, json_dat$index, drop = FALSE)
rm(stations, dir, remove_na_columns, cameras, i, matches, pattern, cams) # clean gl. env. 

## ~~~~~~~~~~~~~~~~~~~ (1) First Round of classification ~~~~~~~~~~~~~~~~~~~ 

dat1 <- json_split$Class1 # get first round as df 
rownames(dat1) <- NULL    # reset row names 

## First issue: Count and ditch empty pictures. 

## Now we need to get rid of all empty lists inside the list that contains the bounding box objects. To do so, 
## we count the number of elements within each nested object. Then we remove those that are empty (have a length of zero).

nrow(subset(dat1, unlist(lapply(dat1$Object, function(x) length(x))) == 0)) # empty images 
dat1_empty <- subset(dat1, unlist(lapply(dat1$Object, function(x) length(x))) == 0) # get empty pictures
dat1_empty$Object <- "empty" # tell R these pics are empty

dat1 <- subset(dat1, unlist(lapply(dat1$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat1) <- NULL # reset row names 

## Next, we start to step wise un-pack all the nested information that is inside 'object', 'meta', and 'review'.

## Un-nest first level of the "Object" and drop non-relevant columns 
dat1 <- dat1 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

## From here on, we need to un-nest classification object to a wide format. However, this is somewhat problematic 
## as the nested lists contain varying numbers of nested lists inside other nested lists... We need to have every 
## object as a singular data frame in order to properly widen it while maintaining the respective External ID and other
## meta data. To do so, we first need to expand the rows by the number of nested lists. This is tricky in a tidy context.
## Therefore, one solution is to check for the format after each iteration of unnest_longer() and only expand 
## those rows that already are at df level. Those that are still lists need to be expanded as >long< as it takes 
# until they are on df level. 

id_check <- unique(dat1$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat1$class_type <- as.character(lapply(dat1$classifications, class)) 
table(dat1$class_type) # check distribution
dat1 <- subset(dat1, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat1 <- dat1 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat1$class_type <- as.character(lapply(dat1$classifications, class)) # check format 
table(dat1$class_type) # check distribution 
dat1 <- subset(dat1, class_type != "NULL") # ditch some empty classifications

## Next, we split the data into rows that have the classification at df level (the ones that are "ready"), and
## the ones that are still lists, i.e., those that need further processing. 

dat1_ls <- subset(dat1, class_type == "list")  # these need further processing 

dat1_df <- subset(dat1, class_type == "data.frame") # these are ready
dat1_df <- dat1_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
lost1 <- subset(json_split$Class1, External_ID %in% lost1)
rm(dat1_df, dat1_ls) # remove redundant data

## We will use the External_ID and the title (i.e., the group variable) as parameters to filter unique variables.
## This way, we still keep duplicates of External_ID's if they contain multiple species groups. 

dat1 <- dat1 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat1 <- distinct(dat1, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat1$class_type <- NULL # ditch non-relevant column 
dat1$position <- NULL # ditch non-relevant column 
colnames(dat1) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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
lost1 <- json_split$Class1 %>% subset(External_ID %in% lost1) # full data

dat1_empty <- data.frame(rbind(dat1_empty, lost1)) # bind empty ones 
rm(dat1_df, dat1_ls, lost1, dat1_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat1 <- distinct(dat1, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat1$position <- NULL
colnames(dat1) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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
colnames(dat1) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
dat1 <- dat1 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)
## Write to csv 
# write.csv(dat1, file ="classifications_1.csv", row.names = FALSE) # write file 

## We will store the parsed classification data in the same structure as the split json files. 
data_parsed <- list(dat1) # store relevant data  
data_empty <- list(dat1_empty) # store empty data

## Clean global environment to speed up the next round.. 
rm(dat1, dat1_empty)
json_split$Class1 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (2) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat2 <- json_split$Class2 # get first round as df 
rownames(dat2) <- NULL    # reset row names 

nrow(subset(dat2, unlist(lapply(dat2$Object, function(x) length(x))) == 0)) # empty images 
dat2_empty <- subset(dat2, unlist(lapply(dat2$Object, function(x) length(x))) == 0) # get empty pictures
dat2_empty$Object <- "empty" # tell R these pics are empty

dat2 <- subset(dat2, unlist(lapply(dat2$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat2) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat2 <- dat2 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat2$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat2$class_type <- as.character(lapply(dat2$classifications, class)) 
table(dat2$class_type) # check distribution
dat2 <- subset(dat2, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat2 <- dat2 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat2$class_type <- as.character(lapply(dat2$classifications, class)) # check format 
table(dat2$class_type) # check distribution 
dat2 <- subset(dat2, class_type != "NULL") # ditch some empty classifications

dat2_ls <- subset(dat2, class_type == "list")  # these need further processing 

dat2_df <- subset(dat2, class_type == "data.frame") # these are ready
dat2_df <- dat2_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat2$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat2$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class2, External_ID %in% lost1)
rm(dat2_df, dat2_ls) # remove redundant data

dat2 <- dat2 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat2 <- distinct(dat2, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat2$class_type <- NULL # ditch non-relevant column 
dat2$position <- NULL # ditch non-relevant column 
colnames(dat2) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat2$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat2$External_ID)] # these are the ones 
lost1 <- json_split$Class2 %>% subset(External_ID %in% lost1) # full data

dat2_empty <- data.frame(rbind(dat2_empty, lost1)) # bind empty ones 
rm(dat2_df, dat2_ls, lost1, dat2_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat2 <- distinct(dat2, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat2$position <- NULL
colnames(dat2) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat2$Review, class))) # check if there are different forms of formats... NO :) 

dat2 <- dat2 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat2) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat2 <- dat2 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat2 <- dat2 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat2), 2) # relevant data
data_empty <- append(data_empty, list(dat2_empty), 2) # empty data

## Clean global environment to speed up the next round.. 
rm(dat2, dat2_empty)
json_split$class2 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (3) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat3 <- json_split$Class3 # get first round as df 
rownames(dat3) <- NULL    # reset row names 

nrow(subset(dat3, unlist(lapply(dat3$Object, function(x) length(x))) == 0)) # empty images 
dat3_empty <- subset(dat3, unlist(lapply(dat3$Object, function(x) length(x))) == 0) # get empty pictures
dat3_empty$Object <- "empty" # tell R these pics are empty

dat3 <- subset(dat3, unlist(lapply(dat3$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat3) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat3 <- dat3 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat3$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat3$class_type <- as.character(lapply(dat3$classifications, class)) 
table(dat3$class_type) # check distribution
dat3 <- subset(dat3, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat3 <- dat3 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat3$class_type <- as.character(lapply(dat3$classifications, class)) # check format 
table(dat3$class_type) # check distribution 
dat3 <- subset(dat3, class_type != "NULL") # ditch some empty classifications

dat3_ls <- subset(dat3, class_type == "list")  # these need further processing 

dat3_df <- subset(dat3, class_type == "data.frame") # these are ready
dat3_df <- dat3_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat3$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat3$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class3, External_ID %in% lost1)
rm(dat3_df, dat3_ls) # remove redundant data

dat3 <- dat3 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat3 <- distinct(dat3, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat3$class_type <- NULL # ditch non-relevant column 
dat3$position <- NULL # ditch non-relevant column 
colnames(dat3) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat3$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat3$External_ID)] # these are the ones 
lost1 <- json_split$Class3 %>% subset(External_ID %in% lost1) # full data

dat3_empty <- data.frame(rbind(dat3_empty, lost1)) # bind empty ones 
rm(dat3_df, dat3_ls, lost1, dat3_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat3 <- distinct(dat3, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat3$position <- NULL
colnames(dat3) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat3$Review, class))) # check if there are different forms of formats... NO :) 

dat3 <- dat3 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat3) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat3 <- dat3 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat3), 3) # relevant data
data_empty <- append(data_empty, list(dat3_empty), 3) # empty data

## Clean global environment to speed up the next round.. 
rm(dat3, dat3_empty)
json_split$class3 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (4) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat4 <- json_split$Class4 # get first round as df 
rownames(dat4) <- NULL    # reset row names 

nrow(subset(dat4, unlist(lapply(dat4$Object, function(x) length(x))) == 0)) # empty images 
dat4_empty <- subset(dat4, unlist(lapply(dat4$Object, function(x) length(x))) == 0) # get empty pictures
dat4_empty$Object <- "empty" # tell R these pics are empty

dat4 <- subset(dat4, unlist(lapply(dat4$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat4) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat4 <- dat4 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat4$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat4$class_type <- as.character(lapply(dat4$classifications, class)) 
table(dat4$class_type) # check distribution
dat4 <- subset(dat4, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat4 <- dat4 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat4$class_type <- as.character(lapply(dat4$classifications, class)) # check format 
table(dat4$class_type) # check distribution 
dat4 <- subset(dat4, class_type != "NULL") # ditch some empty classifications

dat4_ls <- subset(dat4, class_type == "list")  # these need further processing 

dat4_df <- subset(dat4, class_type == "data.frame") # these are ready
dat4_df <- dat4_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat4$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat4$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class4, External_ID %in% lost1)
rm(dat4_df, dat4_ls) # remove redundant data

dat4 <- dat4 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat4 <- distinct(dat4, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat4$class_type <- NULL # ditch non-relevant column 
dat4$position <- NULL # ditch non-relevant column 
colnames(dat4) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat4$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat4$External_ID)] # these are the ones 
lost1 <- json_split$Class4 %>% subset(External_ID %in% lost1) # full data

dat4_empty <- data.frame(rbind(dat4_empty, lost1)) # bind empty ones 
rm(dat4_df, dat4_ls, lost1, dat4_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat4 <- distinct(dat4, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat4$position <- NULL
colnames(dat4) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat4$Review, class))) # check if there are different forms of formats... NO :) 

dat4 <- dat4 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat4) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat4 <- dat4 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat4), 4) # relevant data
data_empty <- append(data_empty, list(dat4_empty), 4) # empty data

## Clean global environment to speed up the next round.. 
rm(dat4, dat4_empty)
json_split$class4 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (5) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat5 <- json_split$Class5 # get first round as df 
rownames(dat5) <- NULL    # reset row names 

nrow(subset(dat5, unlist(lapply(dat5$Object, function(x) length(x))) == 0)) # empty images 
dat5_empty <- subset(dat5, unlist(lapply(dat5$Object, function(x) length(x))) == 0) # get empty pictures
dat5_empty$Object <- "empty" # tell R these pics are empty

dat5 <- subset(dat5, unlist(lapply(dat5$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat5) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat5 <- dat5 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat5$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat5$class_type <- as.character(lapply(dat5$classifications, class)) 
table(dat5$class_type) # check distribution
dat5 <- subset(dat5, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat5 <- dat5 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat5$class_type <- as.character(lapply(dat5$classifications, class)) # check format 
table(dat5$class_type) # check distribution 
dat5 <- subset(dat5, class_type != "NULL") # ditch some empty classifications

dat5_ls <- subset(dat5, class_type == "list")  # these need further processing 

dat5_df <- subset(dat5, class_type == "data.frame") # these are ready
dat5_df <- dat5_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat5$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat5$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class5, External_ID %in% lost1)
rm(dat5_df, dat5_ls) # remove redundant data

dat5 <- dat5 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat5 <- distinct(dat5, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat5$class_type <- NULL # ditch non-relevant column 
dat5$position <- NULL # ditch non-relevant column 
colnames(dat5) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat5$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat5$External_ID)] # these are the ones 
lost1 <- json_split$Class5 %>% subset(External_ID %in% lost1) # full data

dat5_empty <- data.frame(rbind(dat5_empty, lost1)) # bind empty ones 
rm(dat5_df, dat5_ls, lost1, dat5_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat5 <- distinct(dat5, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat5$position <- NULL
colnames(dat5) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat5$Review, class))) # check if there are different forms of formats... NO :) 

dat5 <- dat5 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat5) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat5 <- dat5 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat5), 5) # relevant data
data_empty <- append(data_empty, list(dat5_empty), 5) # empty data

## Clean global environment to speed up the next round.. 
rm(dat5, dat5_empty)
json_split$class5 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (6) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat6 <- json_split$Class6 # get first round as df 
rownames(dat6) <- NULL    # reset row names 

nrow(subset(dat6, unlist(lapply(dat6$Object, function(x) length(x))) == 0)) # empty images 
dat6_empty <- subset(dat6, unlist(lapply(dat6$Object, function(x) length(x))) == 0) # get empty pictures
dat6_empty$Object <- "empty" # tell R these pics are empty

dat6 <- subset(dat6, unlist(lapply(dat6$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat6) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat6 <- dat6 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat6$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat6$class_type <- as.character(lapply(dat6$classifications, class)) 
table(dat6$class_type) # check distribution
dat6 <- subset(dat6, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat6 <- dat6 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat6$class_type <- as.character(lapply(dat6$classifications, class)) # check format 
table(dat6$class_type) # check distribution 
dat6 <- subset(dat6, class_type != "NULL") # ditch some empty classifications

dat6_ls <- subset(dat6, class_type == "list")  # these need further processing 

dat6_df <- subset(dat6, class_type == "data.frame") # these are ready
dat6_df <- dat6_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat6$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat6$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class6, External_ID %in% lost1)
rm(dat6_df, dat6_ls) # remove redundant data

dat6 <- dat6 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat6 <- distinct(dat6, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat6$class_type <- NULL # ditch non-relevant column 
dat6$position <- NULL # ditch non-relevant column 
colnames(dat6) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat6$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat6$External_ID)] # these are the ones 
lost1 <- json_split$Class6 %>% subset(External_ID %in% lost1) # full data

dat6_empty <- data.frame(rbind(dat6_empty, lost1)) # bind empty ones 
rm(dat6_df, dat6_ls, lost1, dat6_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat6 <- distinct(dat6, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat6$position <- NULL
colnames(dat6) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat6$Review, class))) # check if there are different forms of formats... NO :) 

dat6 <- dat6 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat6) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat6 <- dat6 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat6), 6) # relevant data
data_empty <- append(data_empty, list(dat6_empty), 6) # empty data

## Clean global environment to speed up the next round.. 
rm(dat6, dat6_empty)
json_split$class6 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (7) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat7 <- json_split$Class7 # get first round as df 
rownames(dat7) <- NULL    # reset row names 

nrow(subset(dat7, unlist(lapply(dat7$Object, function(x) length(x))) == 0)) # empty images 
dat7_empty <- subset(dat7, unlist(lapply(dat7$Object, function(x) length(x))) == 0) # get empty pictures
dat7_empty$Object <- "empty" # tell R these pics are empty

dat7 <- subset(dat7, unlist(lapply(dat7$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat7) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat7 <- dat7 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat7$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat7$class_type <- as.character(lapply(dat7$classifications, class)) 
table(dat7$class_type) # check distribution
dat7 <- subset(dat7, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat7 <- dat7 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat7$class_type <- as.character(lapply(dat7$classifications, class)) # check format 
table(dat7$class_type) # check distribution 
dat7 <- subset(dat7, class_type != "NULL") # ditch some empty classifications

dat7_ls <- subset(dat7, class_type == "list")  # these need further processing 

dat7_df <- subset(dat7, class_type == "data.frame") # these are ready
dat7_df <- dat7_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat7$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat7$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class7, External_ID %in% lost1)
rm(dat7_df, dat7_ls) # remove redundant data

dat7 <- dat7 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat7 <- distinct(dat7, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat7$class_type <- NULL # ditch non-relevant column 
dat7$position <- NULL # ditch non-relevant column 
colnames(dat7) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat7$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat7$External_ID)] # these are the ones 
lost1 <- json_split$Class7 %>% subset(External_ID %in% lost1) # full data

dat7_empty <- data.frame(rbind(dat7_empty, lost1)) # bind empty ones 
rm(dat7_df, dat7_ls, lost1, dat7_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat7 <- distinct(dat7, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat7$position <- NULL
colnames(dat7) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat7$Review, class))) # check if there are different forms of formats... NO :) 

dat7 <- dat7 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat7) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat7 <- dat7 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat7), 7) # relevant data
data_empty <- append(data_empty, list(dat7_empty), 7) # empty data

## Clean global environment to speed up the next round.. 
rm(dat7, dat7_empty)
json_split$class7 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (8) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat8 <- json_split$Class8 # get first round as df 
rownames(dat8) <- NULL    # reset row names 

nrow(subset(dat8, unlist(lapply(dat8$Object, function(x) length(x))) == 0)) # empty images 
dat8_empty <- subset(dat8, unlist(lapply(dat8$Object, function(x) length(x))) == 0) # get empty pictures
dat8_empty$Object <- "empty" # tell R these pics are empty

dat8 <- subset(dat8, unlist(lapply(dat8$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat8) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat8 <- dat8 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat8$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat8$class_type <- as.character(lapply(dat8$classifications, class)) 
table(dat8$class_type) # check distribution
dat8 <- subset(dat8, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat8 <- dat8 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat8$class_type <- as.character(lapply(dat8$classifications, class)) # check format 
table(dat8$class_type) # check distribution 
dat8 <- subset(dat8, class_type != "NULL") # ditch some empty classifications

dat8_ls <- subset(dat8, class_type == "list")  # these need further processing 

dat8_df <- subset(dat8, class_type == "data.frame") # these are ready
dat8_df <- dat8_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat8$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat8$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class8, External_ID %in% lost1)
rm(dat8_df, dat8_ls) # remove redundant data

dat8 <- dat8 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat8 <- distinct(dat8, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat8$class_type <- NULL # ditch non-relevant column 
dat8$position <- NULL # ditch non-relevant column 
colnames(dat8) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat8$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat8$External_ID)] # these are the ones 
lost1 <- json_split$Class8 %>% subset(External_ID %in% lost1) # full data

dat8_empty <- data.frame(rbind(dat8_empty, lost1)) # bind empty ones 
rm(dat8_df, dat8_ls, lost1, dat8_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat8 <- distinct(dat8, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat8$position <- NULL
colnames(dat8) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat8$Review, class))) # check if there are different forms of formats... NO :) 

dat8 <- dat8 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat8) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat8 <- dat8 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat8), 8) # relevant data
data_empty <- append(data_empty, list(dat8_empty), 8) # empty data

## Clean global environment to speed up the next round.. 
rm(dat8, dat8_empty)
json_split$class8 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (9) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat9 <- json_split$Class9 # get first round as df 
rownames(dat9) <- NULL    # reset row names 

nrow(subset(dat9, unlist(lapply(dat9$Object, function(x) length(x))) == 0)) # empty images 
dat9_empty <- subset(dat9, unlist(lapply(dat9$Object, function(x) length(x))) == 0) # get empty pictures
dat9_empty$Object <- "empty" # tell R these pics are empty

dat9 <- subset(dat9, unlist(lapply(dat9$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat9) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat9 <- dat9 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat9$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat9$class_type <- as.character(lapply(dat9$classifications, class)) 
table(dat9$class_type) # check distribution
dat9 <- subset(dat9, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat9 <- dat9 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat9$class_type <- as.character(lapply(dat9$classifications, class)) # check format 
table(dat9$class_type) # check distribution 
dat9 <- subset(dat9, class_type != "NULL") # ditch some empty classifications

dat9_ls <- subset(dat9, class_type == "list")  # these need further processing 

dat9_df <- subset(dat9, class_type == "data.frame") # these are ready
dat9_df <- dat9_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

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
length(id_check) - length(unique(dat9$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat9$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class9, External_ID %in% lost1)
rm(dat9_df, dat9_ls) # remove redundant data

dat9 <- dat9 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat9 <- distinct(dat9, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat9$class_type <- NULL # ditch non-relevant column 
dat9$position <- NULL # ditch non-relevant column 
colnames(dat9) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

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

length(id_check) - length(unique(dat9$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat9$External_ID)] # these are the ones 
lost1 <- json_split$Class9 %>% subset(External_ID %in% lost1) # full data

dat9_empty <- data.frame(rbind(dat9_empty, lost1)) # bind empty ones 
rm(dat9_df, dat9_ls, lost1, dat9_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat9 <- distinct(dat9, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat9$position <- NULL
colnames(dat9) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat9$Review, class))) # check if there are different forms of formats... NO :) 

dat9 <- dat9 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

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
colnames(dat9) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

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
## Sometimes jaguar images contain multiple comments, but we are going to keep them within one cell.
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
dat9 <- dat9 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat9), 9) # relevant data
data_empty <- append(data_empty, list(dat9_empty), 9) # empty data

## Clean global environment to speed up the next round.. 
rm(dat9, dat9_empty)
json_split$class9 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (10) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat10 <- json_split$Class10 # get first round as df 
rownames(dat10) <- NULL    # reset row names 

nrow(subset(dat10, unlist(lapply(dat10$Object, function(x) length(x))) == 0)) # empty images 
dat10_empty <- subset(dat10, unlist(lapply(dat10$Object, function(x) length(x))) == 0) # get empty pictures
dat10_empty$Object <- "empty" # tell R these pics are empty

dat10 <- subset(dat10, unlist(lapply(dat10$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat10) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat10 <- dat10 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat10$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat10$class_type <- as.character(lapply(dat10$classifications, class)) 
table(dat10$class_type) # check distribution
dat10 <- subset(dat10, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat10 <- dat10 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat10$class_type <- as.character(lapply(dat10$classifications, class)) # check format 
table(dat10$class_type) # check distribution 
dat10 <- subset(dat10, class_type != "NULL") # ditch some empty classifications

dat10_ls <- subset(dat10, class_type == "list")  # these need further processing 

dat10_df <- subset(dat10, class_type == "data.frame") # these are ready
dat10_df <- dat10_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat10 <- dat10_df 
dat10$answer <- dat10$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat10_ls <- dat10_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat10_ls$class_type <- as.character(lapply(dat10_ls$classifications, class)) # check format
table(dat10_ls$class_type) # check format distribution 

dat10_df <- subset(dat10_ls, class_type == "data.frame") # these are the ones that are done
dat10_df <- dat10_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat10_df) <- colnames(dat10)  # make sure column names are equal 
dat10 <- data.frame(rbind(dat10, dat10_df)) # add readily parsed rows to dat10 

dat10_ls <- subset(dat10_ls, class_type == "list") # these are the ones that still require processing
dat10_ls$class_type <- as.character(lapply(dat10_ls$classifications, class))
table(dat10_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat10_ls <- dat10_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat10_ls) <- colnames(dat10)  # make sure column names are equal 
dat10 <- data.frame(rbind(dat10, dat10_ls)) # add readily parsed rows to dat10 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat10$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat10$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class10, External_ID %in% lost1)
rm(dat10_df, dat10_ls) # remove redundant data

dat10 <- dat10 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat10 <- distinct(dat10, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat10$class_type <- NULL # ditch non-relevant column 
dat10$position <- NULL # ditch non-relevant column 
colnames(dat10) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat10$Answer, class)))

## First, we ditch rows that have empty answers. 
dat10 <- subset(dat10, as.character(lapply(dat10$Answer, class)) != "NULL") # remove empty rows 

dat10_chr <- subset(dat10, as.character(lapply(dat10$Answer, class)) == "character") # these are the ones that are ready 

dat10_ls <- subset(dat10, as.character(lapply(dat10$Answer, class)) == "list") # these are the ones that need processing
dat10_ls <- subset(dat10_ls, as.character(lapply(dat10_ls$Answer, length)) == 1) # remove empty rows 
dat10_ls <- dat10_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat10_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat10_df <- subset(dat10_ls, as.character(lapply(dat10_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat10_df <- dat10_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat10_df) <- colnames(dat10)
dat10 <- data.frame(rbind(dat10_chr, dat10_df)) # make dat10_df and dat10_chr the new dat10 
dat10 <- dat10 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat10$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat10$External_ID)] # these are the ones 
lost1 <- json_split$Class10 %>% subset(External_ID %in% lost1) # full data

dat10_empty <- data.frame(rbind(dat10_empty, lost1)) # bind empty ones 
rm(dat10_df, dat10_ls, lost1, dat10_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat10 <- distinct(dat10, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat10$position <- NULL
colnames(dat10) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat10$Review, class))) # check if there are different forms of formats... NO :) 

dat10 <- dat10 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat10 <- dat10 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat10 <- dat10 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat10) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat10, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat10 <- Merge(dat10, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat10 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat10 <- Merge(dat10, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat10 by the External ID

dat10$Meta <- NULL # drop redundant meta data
dat10 <- dat10 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat10 <- dat10 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat10 <- dat10 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat10), 10) # relevant data
data_empty <- append(data_empty, list(dat10_empty), 10) # empty data

## Clean global environment to speed up the next round.. 
rm(dat10, dat10_empty)
json_split$class10 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (11) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat11 <- json_split$Class11 # get first round as df 
rownames(dat11) <- NULL    # reset row names 

nrow(subset(dat11, unlist(lapply(dat11$Object, function(x) length(x))) == 0)) # empty images 
dat11_empty <- subset(dat11, unlist(lapply(dat11$Object, function(x) length(x))) == 0) # get empty pictures
dat11_empty$Object <- "empty" # tell R these pics are empty

dat11 <- subset(dat11, unlist(lapply(dat11$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat11) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat11 <- dat11 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat11$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat11$class_type <- as.character(lapply(dat11$classifications, class)) 
table(dat11$class_type) # check distribution
dat11 <- subset(dat11, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat11 <- dat11 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat11$class_type <- as.character(lapply(dat11$classifications, class)) # check format 
table(dat11$class_type) # check distribution 
dat11 <- subset(dat11, class_type != "NULL") # ditch some empty classifications

dat11_ls <- subset(dat11, class_type == "list")  # these need further processing 

dat11_df <- subset(dat11, class_type == "data.frame") # these are ready
dat11_df <- dat11_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat11 <- dat11_df 
dat11$answer <- dat11$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat11_ls <- dat11_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat11_ls$class_type <- as.character(lapply(dat11_ls$classifications, class)) # check format
table(dat11_ls$class_type) # check format distribution 

dat11_df <- subset(dat11_ls, class_type == "data.frame") # these are the ones that are done
dat11_df <- dat11_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat11_df) <- colnames(dat11)  # make sure column names are equal 
dat11 <- data.frame(rbind(dat11, dat11_df)) # add readily parsed rows to dat11 

dat11_ls <- subset(dat11_ls, class_type == "list") # these are the ones that still require processing
dat11_ls$class_type <- as.character(lapply(dat11_ls$classifications, class))
table(dat11_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat11_ls <- dat11_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat11_ls) <- colnames(dat11)  # make sure column names are equal 
dat11 <- data.frame(rbind(dat11, dat11_ls)) # add readily parsed rows to dat11 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat11$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat11$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class11, External_ID %in% lost1)
rm(dat11_df, dat11_ls) # remove redundant data

dat11 <- dat11 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat11 <- distinct(dat11, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat11$class_type <- NULL # ditch non-relevant column 
dat11$position <- NULL # ditch non-relevant column 
colnames(dat11) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat11$Answer, class)))

## First, we ditch rows that have empty answers. 
dat11 <- subset(dat11, as.character(lapply(dat11$Answer, class)) != "NULL") # remove empty rows 

dat11_chr <- subset(dat11, as.character(lapply(dat11$Answer, class)) == "character") # these are the ones that are ready 

dat11_ls <- subset(dat11, as.character(lapply(dat11$Answer, class)) == "list") # these are the ones that need processing
dat11_ls <- subset(dat11_ls, as.character(lapply(dat11_ls$Answer, length)) == 1) # remove empty rows 
dat11_ls <- dat11_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat11_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat11_df <- subset(dat11_ls, as.character(lapply(dat11_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat11_df <- dat11_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat11_df) <- colnames(dat11)
dat11 <- data.frame(rbind(dat11_chr, dat11_df)) # make dat11_df and dat11_chr the new dat11 
dat11 <- dat11 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat11$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat11$External_ID)] # these are the ones 
lost1 <- json_split$Class11 %>% subset(External_ID %in% lost1) # full data

dat11_empty <- data.frame(rbind(dat11_empty, lost1)) # bind empty ones 
rm(dat11_df, dat11_ls, lost1, dat11_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat11 <- distinct(dat11, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat11$position <- NULL
colnames(dat11) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat11$Review, class))) # check if there are different forms of formats... NO :) 

dat11 <- dat11 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat11 <- dat11 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat11 <- dat11 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat11) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat11, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat11 <- Merge(dat11, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat11 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat11 <- Merge(dat11, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat11 by the External ID

dat11$Meta <- NULL # drop redundant meta data
dat11 <- dat11 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat11 <- dat11 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat11 <- dat11 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat11), 11) # relevant data
data_empty <- append(data_empty, list(dat11_empty), 11) # empty data

## Clean global environment to speed up the next round.. 
rm(dat11, dat11_empty)
json_split$class11 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (12) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat12 <- json_split$Class12 # get first round as df 
rownames(dat12) <- NULL    # reset row names 

nrow(subset(dat12, unlist(lapply(dat12$Object, function(x) length(x))) == 0)) # empty images 
dat12_empty <- subset(dat12, unlist(lapply(dat12$Object, function(x) length(x))) == 0) # get empty pictures
dat12_empty$Object <- "empty" # tell R these pics are empty

dat12 <- subset(dat12, unlist(lapply(dat12$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat12) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat12 <- dat12 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat12$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat12$class_type <- as.character(lapply(dat12$classifications, class)) 
table(dat12$class_type) # check distribution
dat12 <- subset(dat12, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat12 <- dat12 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat12$class_type <- as.character(lapply(dat12$classifications, class)) # check format 
table(dat12$class_type) # check distribution 
dat12 <- subset(dat12, class_type != "NULL") # ditch some empty classifications

dat12_ls <- subset(dat12, class_type == "list")  # these need further processing 

dat12_df <- subset(dat12, class_type == "data.frame") # these are ready
dat12_df <- dat12_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat12 <- dat12_df 
dat12$answer <- dat12$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat12_ls <- dat12_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat12_ls$class_type <- as.character(lapply(dat12_ls$classifications, class)) # check format
table(dat12_ls$class_type) # check format distribution 

dat12_df <- subset(dat12_ls, class_type == "data.frame") # these are the ones that are done
dat12_df <- dat12_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat12_df) <- colnames(dat12)  # make sure column names are equal 
dat12 <- data.frame(rbind(dat12, dat12_df)) # add readily parsed rows to dat12 

dat12_ls <- subset(dat12_ls, class_type == "list") # these are the ones that still require processing
dat12_ls$class_type <- as.character(lapply(dat12_ls$classifications, class))
table(dat12_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat12_ls <- dat12_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat12_ls) <- colnames(dat12)  # make sure column names are equal 
dat12 <- data.frame(rbind(dat12, dat12_ls)) # add readily parsed rows to dat12 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat12$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat12$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class12, External_ID %in% lost1)
rm(dat12_df, dat12_ls) # remove redundant data

dat12 <- dat12 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat12 <- distinct(dat12, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat12$class_type <- NULL # ditch non-relevant column 
dat12$position <- NULL # ditch non-relevant column 
colnames(dat12) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat12$Answer, class)))

## First, we ditch rows that have empty answers. 
dat12 <- subset(dat12, as.character(lapply(dat12$Answer, class)) != "NULL") # remove empty rows 

dat12_chr <- subset(dat12, as.character(lapply(dat12$Answer, class)) == "character") # these are the ones that are ready 

dat12_ls <- subset(dat12, as.character(lapply(dat12$Answer, class)) == "list") # these are the ones that need processing
dat12_ls <- subset(dat12_ls, as.character(lapply(dat12_ls$Answer, length)) == 1) # remove empty rows 
dat12_ls <- dat12_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat12_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat12_df <- subset(dat12_ls, as.character(lapply(dat12_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat12_df <- dat12_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat12_df) <- colnames(dat12)
dat12 <- data.frame(rbind(dat12_chr, dat12_df)) # make dat12_df and dat12_chr the new dat12 
dat12 <- dat12 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat12$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat12$External_ID)] # these are the ones 
lost1 <- json_split$Class12 %>% subset(External_ID %in% lost1) # full data

dat12_empty <- data.frame(rbind(dat12_empty, lost1)) # bind empty ones 
rm(dat12_df, dat12_ls, lost1, dat12_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat12 <- distinct(dat12, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat12$position <- NULL
colnames(dat12) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat12$Review, class))) # check if there are different forms of formats... NO :) 

dat12 <- dat12 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat12 <- dat12 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat12 <- dat12 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat12) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat12, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat12 <- Merge(dat12, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat12 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat12 <- Merge(dat12, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat12 by the External ID

dat12$Meta <- NULL # drop redundant meta data
dat12 <- dat12 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat12 <- dat12 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat12 <- dat12 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat12), 12) # relevant data
data_empty <- append(data_empty, list(dat12_empty), 12) # empty data

## Clean global environment to speed up the next round.. 
rm(dat12, dat12_empty)
json_split$class12 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (13) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat13 <- json_split$Class13 # get first round as df 
rownames(dat13) <- NULL    # reset row names 

nrow(subset(dat13, unlist(lapply(dat13$Object, function(x) length(x))) == 0)) # empty images 
dat13_empty <- subset(dat13, unlist(lapply(dat13$Object, function(x) length(x))) == 0) # get empty pictures
dat13_empty$Object <- "empty" # tell R these pics are empty

dat13 <- subset(dat13, unlist(lapply(dat13$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat13) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat13 <- dat13 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat13$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat13$class_type <- as.character(lapply(dat13$classifications, class)) 
table(dat13$class_type) # check distribution
dat13 <- subset(dat13, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat13 <- dat13 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat13$class_type <- as.character(lapply(dat13$classifications, class)) # check format 
table(dat13$class_type) # check distribution 
dat13 <- subset(dat13, class_type != "NULL") # ditch some empty classifications

dat13_ls <- subset(dat13, class_type == "list")  # these need further processing 

dat13_df <- subset(dat13, class_type == "data.frame") # these are ready
dat13_df <- dat13_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat13 <- dat13_df 
dat13$answer <- dat13$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat13_ls <- dat13_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat13_ls$class_type <- as.character(lapply(dat13_ls$classifications, class)) # check format
table(dat13_ls$class_type) # check format distribution 

dat13_df <- subset(dat13_ls, class_type == "data.frame") # these are the ones that are done
dat13_df <- dat13_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat13_df) <- colnames(dat13)  # make sure column names are equal 
dat13 <- data.frame(rbind(dat13, dat13_df)) # add readily parsed rows to dat13 

dat13_ls <- subset(dat13_ls, class_type == "list") # these are the ones that still require processing
dat13_ls$class_type <- as.character(lapply(dat13_ls$classifications, class))
table(dat13_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat13_ls <- dat13_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat13_ls) <- colnames(dat13)  # make sure column names are equal 
dat13 <- data.frame(rbind(dat13, dat13_ls)) # add readily parsed rows to dat13 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat13$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat13$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class13, External_ID %in% lost1)
rm(dat13_df, dat13_ls) # remove redundant data

dat13 <- dat13 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat13 <- distinct(dat13, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat13$class_type <- NULL # ditch non-relevant column 
dat13$position <- NULL # ditch non-relevant column 
colnames(dat13) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat13$Answer, class)))

## First, we ditch rows that have empty answers. 
dat13 <- subset(dat13, as.character(lapply(dat13$Answer, class)) != "NULL") # remove empty rows 

dat13_chr <- subset(dat13, as.character(lapply(dat13$Answer, class)) == "character") # these are the ones that are ready 

dat13_ls <- subset(dat13, as.character(lapply(dat13$Answer, class)) == "list") # these are the ones that need processing
dat13_ls <- subset(dat13_ls, as.character(lapply(dat13_ls$Answer, length)) == 1) # remove empty rows 
dat13_ls <- dat13_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat13_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat13_df <- subset(dat13_ls, as.character(lapply(dat13_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat13_df <- dat13_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat13_df) <- colnames(dat13)
dat13 <- data.frame(rbind(dat13_chr, dat13_df)) # make dat13_df and dat13_chr the new dat13 
dat13 <- dat13 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat13$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat13$External_ID)] # these are the ones 
lost1 <- json_split$Class13 %>% subset(External_ID %in% lost1) # full data

dat13_empty <- data.frame(rbind(dat13_empty, lost1)) # bind empty ones 
rm(dat13_df, dat13_ls, lost1, dat13_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat13 <- distinct(dat13, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat13$position <- NULL
colnames(dat13) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat13$Review, class))) # check if there are different forms of formats... NO :) 

dat13 <- dat13 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat13 <- dat13 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat13 <- dat13 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat13) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat13, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat13 <- Merge(dat13, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat13 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat13 <- Merge(dat13, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat13 by the External ID

dat13$Meta <- NULL # drop redundant meta data
dat13 <- dat13 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat13 <- dat13 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat13 <- dat13 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat13), 13) # relevant data
data_empty <- append(data_empty, list(dat13_empty), 13) # empty data

## Clean global environment to speed up the next round.. 
rm(dat13, dat13_empty)
json_split$class13 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (14) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat14 <- json_split$Class14 # get first round as df 
rownames(dat14) <- NULL    # reset row names 

nrow(subset(dat14, unlist(lapply(dat14$Object, function(x) length(x))) == 0)) # empty images 
dat14_empty <- subset(dat14, unlist(lapply(dat14$Object, function(x) length(x))) == 0) # get empty pictures
dat14_empty$Object <- "empty" # tell R these pics are empty

dat14 <- subset(dat14, unlist(lapply(dat14$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat14) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat14 <- dat14 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat14$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat14$class_type <- as.character(lapply(dat14$classifications, class)) 
table(dat14$class_type) # check distribution
dat14 <- subset(dat14, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat14 <- dat14 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat14$class_type <- as.character(lapply(dat14$classifications, class)) # check format 
table(dat14$class_type) # check distribution 
dat14 <- subset(dat14, class_type != "NULL") # ditch some empty classifications

dat14_ls <- subset(dat14, class_type == "list")  # these need further processing 

dat14_df <- subset(dat14, class_type == "data.frame") # these are ready
dat14_df <- dat14_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat14 <- dat14_df 
dat14$answer <- dat14$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat14_ls <- dat14_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat14_ls$class_type <- as.character(lapply(dat14_ls$classifications, class)) # check format
table(dat14_ls$class_type) # check format distribution 

dat14_df <- subset(dat14_ls, class_type == "data.frame") # these are the ones that are done
dat14_df <- dat14_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat14_df) <- colnames(dat14)  # make sure column names are equal 
dat14 <- data.frame(rbind(dat14, dat14_df)) # add readily parsed rows to dat14 

dat14_ls <- subset(dat14_ls, class_type == "list") # these are the ones that still require processing
dat14_ls$class_type <- as.character(lapply(dat14_ls$classifications, class))
table(dat14_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat14_ls <- dat14_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat14_ls) <- colnames(dat14)  # make sure column names are equal 
dat14 <- data.frame(rbind(dat14, dat14_ls)) # add readily parsed rows to dat14 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat14$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat14$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class14, External_ID %in% lost1)
rm(dat14_df, dat14_ls) # remove redundant data

dat14 <- dat14 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat14 <- distinct(dat14, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat14$class_type <- NULL # ditch non-relevant column 
dat14$position <- NULL # ditch non-relevant column 
colnames(dat14) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat14$Answer, class)))

## First, we ditch rows that have empty answers. 
dat14 <- subset(dat14, as.character(lapply(dat14$Answer, class)) != "NULL") # remove empty rows 

dat14_chr <- subset(dat14, as.character(lapply(dat14$Answer, class)) == "character") # these are the ones that are ready 

dat14_ls <- subset(dat14, as.character(lapply(dat14$Answer, class)) == "list") # these are the ones that need processing
dat14_ls <- subset(dat14_ls, as.character(lapply(dat14_ls$Answer, length)) == 1) # remove empty rows 
dat14_ls <- dat14_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat14_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat14_df <- subset(dat14_ls, as.character(lapply(dat14_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat14_df <- dat14_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat14_df) <- colnames(dat14)
dat14 <- data.frame(rbind(dat14_chr, dat14_df)) # make dat14_df and dat14_chr the new dat14 
dat14 <- dat14 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat14$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat14$External_ID)] # these are the ones 
lost1 <- json_split$Class14 %>% subset(External_ID %in% lost1) # full data

dat14_empty <- data.frame(rbind(dat14_empty, lost1)) # bind empty ones 
rm(dat14_df, dat14_ls, lost1, dat14_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat14 <- distinct(dat14, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat14$position <- NULL
colnames(dat14) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat14$Review, class))) # check if there are different forms of formats... NO :) 

dat14 <- dat14 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat14 <- dat14 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat14 <- dat14 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat14) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat14, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat14 <- Merge(dat14, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat14 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat14 <- Merge(dat14, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat14 by the External ID

dat14$Meta <- NULL # drop redundant meta data
dat14 <- dat14 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat14 <- dat14 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat14 <- dat14 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat14), 14) # relevant data
data_empty <- append(data_empty, list(dat14_empty), 14) # empty data

## Clean global environment to speed up the next round.. 
rm(dat14, dat14_empty)
json_split$class14 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (15) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat15 <- json_split$Class15 # get first round as df 
rownames(dat15) <- NULL    # reset row names 

nrow(subset(dat15, unlist(lapply(dat15$Object, function(x) length(x))) == 0)) # empty images 
dat15_empty <- subset(dat15, unlist(lapply(dat15$Object, function(x) length(x))) == 0) # get empty pictures
dat15_empty$Object <- "empty" # tell R these pics are empty

dat15 <- subset(dat15, unlist(lapply(dat15$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat15) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat15 <- dat15 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat15$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat15$class_type <- as.character(lapply(dat15$classifications, class)) 
table(dat15$class_type) # check distribution
dat15 <- subset(dat15, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat15 <- dat15 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat15$class_type <- as.character(lapply(dat15$classifications, class)) # check format 
table(dat15$class_type) # check distribution 
dat15 <- subset(dat15, class_type != "NULL") # ditch some empty classifications

dat15_ls <- subset(dat15, class_type == "list")  # these need further processing 

dat15_df <- subset(dat15, class_type == "data.frame") # these are ready
dat15_df <- dat15_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat15 <- dat15_df 
dat15$answer <- dat15$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat15_ls <- dat15_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat15_ls$class_type <- as.character(lapply(dat15_ls$classifications, class)) # check format
table(dat15_ls$class_type) # check format distribution 

dat15_df <- subset(dat15_ls, class_type == "data.frame") # these are the ones that are done
dat15_df <- dat15_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat15_df) <- colnames(dat15)  # make sure column names are equal 
dat15 <- data.frame(rbind(dat15, dat15_df)) # add readily parsed rows to dat15 

dat15_ls <- subset(dat15_ls, class_type == "list") # these are the ones that still require processing
dat15_ls$class_type <- as.character(lapply(dat15_ls$classifications, class))
table(dat15_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat15_ls <- dat15_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat15_ls) <- colnames(dat15)  # make sure column names are equal 
dat15 <- data.frame(rbind(dat15, dat15_ls)) # add readily parsed rows to dat15 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat15$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat15$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class15, External_ID %in% lost1)
rm(dat15_df, dat15_ls) # remove redundant data

dat15 <- dat15 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat15 <- distinct(dat15, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat15$class_type <- NULL # ditch non-relevant column 
dat15$position <- NULL # ditch non-relevant column 
colnames(dat15) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat15$Answer, class)))

## First, we ditch rows that have empty answers. 
dat15 <- subset(dat15, as.character(lapply(dat15$Answer, class)) != "NULL") # remove empty rows 

dat15_chr <- subset(dat15, as.character(lapply(dat15$Answer, class)) == "character") # these are the ones that are ready 

dat15_ls <- subset(dat15, as.character(lapply(dat15$Answer, class)) == "list") # these are the ones that need processing
dat15_ls <- subset(dat15_ls, as.character(lapply(dat15_ls$Answer, length)) == 1) # remove empty rows 
dat15_ls <- dat15_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat15_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat15_df <- subset(dat15_ls, as.character(lapply(dat15_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat15_df <- dat15_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat15_df) <- colnames(dat15)
dat15 <- data.frame(rbind(dat15_chr, dat15_df)) # make dat15_df and dat15_chr the new dat15 
dat15 <- dat15 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat15$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat15$External_ID)] # these are the ones 
lost1 <- json_split$Class15 %>% subset(External_ID %in% lost1) # full data

dat15_empty <- data.frame(rbind(dat15_empty, lost1)) # bind empty ones 
rm(dat15_df, dat15_ls, lost1, dat15_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat15 <- distinct(dat15, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat15$position <- NULL
colnames(dat15) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat15$Review, class))) # check if there are different forms of formats... NO :) 

dat15 <- dat15 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat15 <- dat15 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat15 <- dat15 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat15) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat15, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat15 <- Merge(dat15, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat15 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat15 <- Merge(dat15, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat15 by the External ID

dat15$Meta <- NULL # drop redundant meta data
dat15 <- dat15 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat15 <- dat15 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat15 <- dat15 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat15), 15) # relevant data
data_empty <- append(data_empty, list(dat15_empty), 15) # empty data

## Clean global environment to speed up the next round.. 
rm(dat15, dat15_empty)
json_split$class15 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (16) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat16 <- json_split$Class16 # get first round as df 
rownames(dat16) <- NULL    # reset row names 

nrow(subset(dat16, unlist(lapply(dat16$Object, function(x) length(x))) == 0)) # empty images 
dat16_empty <- subset(dat16, unlist(lapply(dat16$Object, function(x) length(x))) == 0) # get empty pictures
dat16_empty$Object <- "empty" # tell R these pics are empty

dat16 <- subset(dat16, unlist(lapply(dat16$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat16) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat16 <- dat16 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat16$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat16$class_type <- as.character(lapply(dat16$classifications, class)) 
table(dat16$class_type) # check distribution
dat16 <- subset(dat16, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat16 <- dat16 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat16$class_type <- as.character(lapply(dat16$classifications, class)) # check format 
table(dat16$class_type) # check distribution 
dat16 <- subset(dat16, class_type != "NULL") # ditch some empty classifications

dat16_ls <- subset(dat16, class_type == "list")  # these need further processing 

dat16_df <- subset(dat16, class_type == "data.frame") # these are ready
dat16_df <- dat16_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat16 <- dat16_df 
dat16$answer <- dat16$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat16_ls <- dat16_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat16_ls$class_type <- as.character(lapply(dat16_ls$classifications, class)) # check format
table(dat16_ls$class_type) # check format distribution 

dat16_df <- subset(dat16_ls, class_type == "data.frame") # these are the ones that are done
dat16_df <- dat16_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat16_df) <- colnames(dat16)  # make sure column names are equal 
dat16 <- data.frame(rbind(dat16, dat16_df)) # add readily parsed rows to dat16 

dat16_ls <- subset(dat16_ls, class_type == "list") # these are the ones that still require processing
dat16_ls$class_type <- as.character(lapply(dat16_ls$classifications, class))
table(dat16_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat16_ls <- dat16_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat16_ls) <- colnames(dat16)  # make sure column names are equal 
dat16 <- data.frame(rbind(dat16, dat16_ls)) # add readily parsed rows to dat16 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat16$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat16$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class16, External_ID %in% lost1)
rm(dat16_df, dat16_ls) # remove redundant data

dat16 <- dat16 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat16 <- distinct(dat16, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat16$class_type <- NULL # ditch non-relevant column 
dat16$position <- NULL # ditch non-relevant column 
colnames(dat16) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat16$Answer, class)))

## First, we ditch rows that have empty answers. 
dat16 <- subset(dat16, as.character(lapply(dat16$Answer, class)) != "NULL") # remove empty rows 

dat16_chr <- subset(dat16, as.character(lapply(dat16$Answer, class)) == "character") # these are the ones that are ready 

dat16_ls <- subset(dat16, as.character(lapply(dat16$Answer, class)) == "list") # these are the ones that need processing
dat16_ls <- subset(dat16_ls, as.character(lapply(dat16_ls$Answer, length)) == 1) # remove empty rows 
dat16_ls <- dat16_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat16_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat16_df <- subset(dat16_ls, as.character(lapply(dat16_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat16_df <- dat16_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat16_df) <- colnames(dat16)
dat16 <- data.frame(rbind(dat16_chr, dat16_df)) # make dat16_df and dat16_chr the new dat16 
dat16 <- dat16 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat16$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat16$External_ID)] # these are the ones 
lost1 <- json_split$Class16 %>% subset(External_ID %in% lost1) # full data

dat16_empty <- data.frame(rbind(dat16_empty, lost1)) # bind empty ones 
rm(dat16_df, dat16_ls, lost1, dat16_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat16 <- distinct(dat16, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat16$position <- NULL
colnames(dat16) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat16$Review, class))) # check if there are different forms of formats... NO :) 

dat16 <- dat16 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat16 <- dat16 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat16 <- dat16 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat16) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat16, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat16 <- Merge(dat16, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat16 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat16 <- Merge(dat16, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat16 by the External ID

dat16$Meta <- NULL # drop redundant meta data
dat16 <- dat16 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat16 <- dat16 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat16 <- dat16 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat16), 16) # relevant data
data_empty <- append(data_empty, list(dat16_empty), 16) # empty data

## Clean global environment to speed up the next round.. 
rm(dat16, dat16_empty)
json_split$class16 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (17) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat17 <- json_split$Class17 # get first round as df 
rownames(dat17) <- NULL    # reset row names 

nrow(subset(dat17, unlist(lapply(dat17$Object, function(x) length(x))) == 0)) # empty images 
dat17_empty <- subset(dat17, unlist(lapply(dat17$Object, function(x) length(x))) == 0) # get empty pictures
dat17_empty$Object <- "empty" # tell R these pics are empty

dat17 <- subset(dat17, unlist(lapply(dat17$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat17) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat17 <- dat17 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat17$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat17$class_type <- as.character(lapply(dat17$classifications, class)) 
table(dat17$class_type) # check distribution
dat17 <- subset(dat17, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat17 <- dat17 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat17$class_type <- as.character(lapply(dat17$classifications, class)) # check format 
table(dat17$class_type) # check distribution 
dat17 <- subset(dat17, class_type != "NULL") # ditch some empty classifications

dat17_ls <- subset(dat17, class_type == "list")  # these need further processing 

dat17_df <- subset(dat17, class_type == "data.frame") # these are ready
dat17_df <- dat17_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat17 <- dat17_df 
dat17$answer <- dat17$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat17_ls <- dat17_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat17_ls$class_type <- as.character(lapply(dat17_ls$classifications, class)) # check format
table(dat17_ls$class_type) # check format distribution 

dat17_df <- subset(dat17_ls, class_type == "data.frame") # these are the ones that are done
dat17_df <- dat17_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat17_df) <- colnames(dat17)  # make sure column names are equal 
dat17 <- data.frame(rbind(dat17, dat17_df)) # add readily parsed rows to dat17 

dat17_ls <- subset(dat17_ls, class_type == "list") # these are the ones that still require processing
dat17_ls$class_type <- as.character(lapply(dat17_ls$classifications, class))
table(dat17_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat17_ls <- dat17_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat17_ls) <- colnames(dat17)  # make sure column names are equal 
dat17 <- data.frame(rbind(dat17, dat17_ls)) # add readily parsed rows to dat17 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat17$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat17$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class17, External_ID %in% lost1)
rm(dat17_df, dat17_ls) # remove redundant data

dat17 <- dat17 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat17 <- distinct(dat17, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat17$class_type <- NULL # ditch non-relevant column 
dat17$position <- NULL # ditch non-relevant column 
colnames(dat17) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat17$Answer, class)))

## First, we ditch rows that have empty answers. 
dat17 <- subset(dat17, as.character(lapply(dat17$Answer, class)) != "NULL") # remove empty rows 

dat17_chr <- subset(dat17, as.character(lapply(dat17$Answer, class)) == "character") # these are the ones that are ready 

dat17_ls <- subset(dat17, as.character(lapply(dat17$Answer, class)) == "list") # these are the ones that need processing
dat17_ls <- subset(dat17_ls, as.character(lapply(dat17_ls$Answer, length)) == 1) # remove empty rows 
dat17_ls <- dat17_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat17_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat17_df <- subset(dat17_ls, as.character(lapply(dat17_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat17_df <- dat17_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat17_df) <- colnames(dat17)
dat17 <- data.frame(rbind(dat17_chr, dat17_df)) # make dat17_df and dat17_chr the new dat17 
dat17 <- dat17 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat17$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat17$External_ID)] # these are the ones 
lost1 <- json_split$Class17 %>% subset(External_ID %in% lost1) # full data

dat17_empty <- data.frame(rbind(dat17_empty, lost1)) # bind empty ones 
rm(dat17_df, dat17_ls, lost1, dat17_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat17 <- distinct(dat17, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat17$position <- NULL
colnames(dat17) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat17$Review, class))) # check if there are different forms of formats... NO :) 

dat17 <- dat17 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat17 <- dat17 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat17 <- dat17 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat17) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat17, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat17 <- Merge(dat17, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat17 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat17 <- Merge(dat17, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat17 by the External ID

dat17$Meta <- NULL # drop redundant meta data
dat17 <- dat17 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat17 <- dat17 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat17 <- dat17 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat17), 17) # relevant data
data_empty <- append(data_empty, list(dat17_empty), 17) # empty data

## Clean global environment to speed up the next round.. 
rm(dat17, dat17_empty)
json_split$class17 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (18) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat18 <- json_split$Class18 # get first round as df 
rownames(dat18) <- NULL    # reset row names 

nrow(subset(dat18, unlist(lapply(dat18$Object, function(x) length(x))) == 0)) # empty images 
dat18_empty <- subset(dat18, unlist(lapply(dat18$Object, function(x) length(x))) == 0) # get empty pictures
dat18_empty$Object <- "empty" # tell R these pics are empty

dat18 <- subset(dat18, unlist(lapply(dat18$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat18) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat18 <- dat18 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat18$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat18$class_type <- as.character(lapply(dat18$classifications, class)) 
table(dat18$class_type) # check distribution
dat18 <- subset(dat18, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat18 <- dat18 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat18$class_type <- as.character(lapply(dat18$classifications, class)) # check format 
table(dat18$class_type) # check distribution 
dat18 <- subset(dat18, class_type != "NULL") # ditch some empty classifications

dat18_ls <- subset(dat18, class_type == "list")  # these need further processing 

dat18_df <- subset(dat18, class_type == "data.frame") # these are ready
dat18_df <- dat18_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat18 <- dat18_df 
dat18$answer <- dat18$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat18_ls <- dat18_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat18_ls$class_type <- as.character(lapply(dat18_ls$classifications, class)) # check format
table(dat18_ls$class_type) # check format distribution 

dat18_df <- subset(dat18_ls, class_type == "data.frame") # these are the ones that are done
dat18_df <- dat18_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat18_df) <- colnames(dat18)  # make sure column names are equal 
dat18 <- data.frame(rbind(dat18, dat18_df)) # add readily parsed rows to dat18 

dat18_ls <- subset(dat18_ls, class_type == "list") # these are the ones that still require processing
dat18_ls$class_type <- as.character(lapply(dat18_ls$classifications, class))
table(dat18_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat18_ls <- dat18_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat18_ls) <- colnames(dat18)  # make sure column names are equal 
dat18 <- data.frame(rbind(dat18, dat18_ls)) # add readily parsed rows to dat18 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat18$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat18$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class18, External_ID %in% lost1)
rm(dat18_df, dat18_ls) # remove redundant data

dat18 <- dat18 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat18 <- distinct(dat18, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat18$class_type <- NULL # ditch non-relevant column 
dat18$position <- NULL # ditch non-relevant column 
colnames(dat18) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat18$Answer, class)))

## First, we ditch rows that have empty answers. 
dat18 <- subset(dat18, as.character(lapply(dat18$Answer, class)) != "NULL") # remove empty rows 

dat18_chr <- subset(dat18, as.character(lapply(dat18$Answer, class)) == "character") # these are the ones that are ready 

dat18_ls <- subset(dat18, as.character(lapply(dat18$Answer, class)) == "list") # these are the ones that need processing
dat18_ls <- subset(dat18_ls, as.character(lapply(dat18_ls$Answer, length)) == 1) # remove empty rows 
dat18_ls <- dat18_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat18_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat18_df <- subset(dat18_ls, as.character(lapply(dat18_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat18_df <- dat18_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat18_df) <- colnames(dat18)
dat18 <- data.frame(rbind(dat18_chr, dat18_df)) # make dat18_df and dat18_chr the new dat18 
dat18 <- dat18 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat18$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat18$External_ID)] # these are the ones 
lost1 <- json_split$Class18 %>% subset(External_ID %in% lost1) # full data

dat18_empty <- data.frame(rbind(dat18_empty, lost1)) # bind empty ones 
rm(dat18_df, dat18_ls, lost1, dat18_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat18 <- distinct(dat18, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat18$position <- NULL
colnames(dat18) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat18$Review, class))) # check if there are different forms of formats... NO :) 

dat18 <- dat18 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat18 <- dat18 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat18 <- dat18 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat18) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat18, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat18 <- Merge(dat18, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat18 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat18 <- Merge(dat18, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat18 by the External ID

dat18$Meta <- NULL # drop redundant meta data
dat18 <- dat18 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat18 <- dat18 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat18 <- dat18 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat18), 18) # relevant data
data_empty <- append(data_empty, list(dat18_empty), 18) # empty data

## Clean global environment to speed up the next round.. 
rm(dat18, dat18_empty)
json_split$class18 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (19) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat19 <- json_split$Class19 # get first round as df 
rownames(dat19) <- NULL    # reset row names 

nrow(subset(dat19, unlist(lapply(dat19$Object, function(x) length(x))) == 0)) # empty images 
dat19_empty <- subset(dat19, unlist(lapply(dat19$Object, function(x) length(x))) == 0) # get empty pictures
dat19_empty$Object <- "empty" # tell R these pics are empty

dat19 <- subset(dat19, unlist(lapply(dat19$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat19) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat19 <- dat19 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat19$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat19$class_type <- as.character(lapply(dat19$classifications, class)) 
table(dat19$class_type) # check distribution
dat19 <- subset(dat19, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat19 <- dat19 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat19$class_type <- as.character(lapply(dat19$classifications, class)) # check format 
table(dat19$class_type) # check distribution 
dat19 <- subset(dat19, class_type != "NULL") # ditch some empty classifications

dat19_ls <- subset(dat19, class_type == "list")  # these need further processing 

dat19_df <- subset(dat19, class_type == "data.frame") # these are ready
dat19_df <- dat19_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat19 <- dat19_df 
dat19$answer <- dat19$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat19_ls <- dat19_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat19_ls$class_type <- as.character(lapply(dat19_ls$classifications, class)) # check format
table(dat19_ls$class_type) # check format distribution 

dat19_df <- subset(dat19_ls, class_type == "data.frame") # these are the ones that are done
dat19_df <- dat19_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat19_df) <- colnames(dat19)  # make sure column names are equal 
dat19 <- data.frame(rbind(dat19, dat19_df)) # add readily parsed rows to dat19 

dat19_ls <- subset(dat19_ls, class_type == "list") # these are the ones that still require processing
dat19_ls$class_type <- as.character(lapply(dat19_ls$classifications, class))
table(dat19_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat19_ls <- dat19_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat19_ls) <- colnames(dat19)  # make sure column names are equal 
dat19 <- data.frame(rbind(dat19, dat19_ls)) # add readily parsed rows to dat19 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat19$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat19$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class19, External_ID %in% lost1)
rm(dat19_df, dat19_ls) # remove redundant data

dat19 <- dat19 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat19 <- distinct(dat19, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat19$class_type <- NULL # ditch non-relevant column 
dat19$position <- NULL # ditch non-relevant column 
colnames(dat19) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat19$Answer, class)))

## First, we ditch rows that have empty answers. 
dat19 <- subset(dat19, as.character(lapply(dat19$Answer, class)) != "NULL") # remove empty rows 

dat19_chr <- subset(dat19, as.character(lapply(dat19$Answer, class)) == "character") # these are the ones that are ready 

dat19_ls <- subset(dat19, as.character(lapply(dat19$Answer, class)) == "list") # these are the ones that need processing
dat19_ls <- subset(dat19_ls, as.character(lapply(dat19_ls$Answer, length)) == 1) # remove empty rows 
dat19_ls <- dat19_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat19_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat19_df <- subset(dat19_ls, as.character(lapply(dat19_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat19_df <- dat19_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat19_df) <- colnames(dat19)
dat19 <- data.frame(rbind(dat19_chr, dat19_df)) # make dat19_df and dat19_chr the new dat19 
dat19 <- dat19 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat19$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat19$External_ID)] # these are the ones 
lost1 <- json_split$Class19 %>% subset(External_ID %in% lost1) # full data

dat19_empty <- data.frame(rbind(dat19_empty, lost1)) # bind empty ones 
rm(dat19_df, dat19_ls, lost1, dat19_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat19 <- distinct(dat19, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat19$position <- NULL
colnames(dat19) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat19$Review, class))) # check if there are different forms of formats... NO :) 

dat19 <- dat19 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat19 <- dat19 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat19 <- dat19 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat19) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat19, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat19 <- Merge(dat19, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat19 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat19 <- Merge(dat19, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat19 by the External ID

dat19$Meta <- NULL # drop redundant meta data
dat19 <- dat19 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat19 <- dat19 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat19 <- dat19 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat19), 19) # relevant data
data_empty <- append(data_empty, list(dat19_empty), 19) # empty data

## Clean global environment to speed up the next round.. 
rm(dat19, dat19_empty)
json_split$class19 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (20) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat20 <- json_split$Class20 # get first round as df 
rownames(dat20) <- NULL    # reset row names 

nrow(subset(dat20, unlist(lapply(dat20$Object, function(x) length(x))) == 0)) # empty images 
dat20_empty <- subset(dat20, unlist(lapply(dat20$Object, function(x) length(x))) == 0) # get empty pictures
dat20_empty$Object <- "empty" # tell R these pics are empty

dat20 <- subset(dat20, unlist(lapply(dat20$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat20) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat20 <- dat20 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat20$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat20$class_type <- as.character(lapply(dat20$classifications, class)) 
table(dat20$class_type) # check distribution
dat20 <- subset(dat20, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat20 <- dat20 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat20$class_type <- as.character(lapply(dat20$classifications, class)) # check format 
table(dat20$class_type) # check distribution 
dat20 <- subset(dat20, class_type != "NULL") # ditch some empty classifications

dat20_ls <- subset(dat20, class_type == "list")  # these need further processing 

dat20_df <- subset(dat20, class_type == "data.frame") # these are ready
dat20_df <- dat20_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat20 <- dat20_df 
dat20$answer <- dat20$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat20_ls <- dat20_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat20_ls$class_type <- as.character(lapply(dat20_ls$classifications, class)) # check format
table(dat20_ls$class_type) # check format distribution 

dat20_df <- subset(dat20_ls, class_type == "data.frame") # these are the ones that are done
dat20_df <- dat20_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat20_df) <- colnames(dat20)  # make sure column names are equal 
dat20 <- data.frame(rbind(dat20, dat20_df)) # add readily parsed rows to dat20 

dat20_ls <- subset(dat20_ls, class_type == "list") # these are the ones that still require processing
dat20_ls$class_type <- as.character(lapply(dat20_ls$classifications, class))
table(dat20_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat20_ls <- dat20_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat20_ls) <- colnames(dat20)  # make sure column names are equal 
dat20 <- data.frame(rbind(dat20, dat20_ls)) # add readily parsed rows to dat20 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat20$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat20$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class20, External_ID %in% lost1)
rm(dat20_df, dat20_ls) # remove redundant data

dat20 <- dat20 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat20 <- distinct(dat20, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat20$class_type <- NULL # ditch non-relevant column 
dat20$position <- NULL # ditch non-relevant column 
colnames(dat20) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat20$Answer, class)))

## First, we ditch rows that have empty answers. 
dat20 <- subset(dat20, as.character(lapply(dat20$Answer, class)) != "NULL") # remove empty rows 

dat20_chr <- subset(dat20, as.character(lapply(dat20$Answer, class)) == "character") # these are the ones that are ready 

dat20_ls <- subset(dat20, as.character(lapply(dat20$Answer, class)) == "list") # these are the ones that need processing
dat20_ls <- subset(dat20_ls, as.character(lapply(dat20_ls$Answer, length)) == 1) # remove empty rows 
dat20_ls <- dat20_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat20_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat20_df <- subset(dat20_ls, as.character(lapply(dat20_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat20_df <- dat20_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat20_df) <- colnames(dat20)
dat20 <- data.frame(rbind(dat20_chr, dat20_df)) # make dat20_df and dat20_chr the new dat20 
dat20 <- dat20 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat20$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat20$External_ID)] # these are the ones 
lost1 <- json_split$Class20 %>% subset(External_ID %in% lost1) # full data

dat20_empty <- data.frame(rbind(dat20_empty, lost1)) # bind empty ones 
rm(dat20_df, dat20_ls, lost1, dat20_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat20 <- distinct(dat20, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat20$position <- NULL
colnames(dat20) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat20$Review, class))) # check if there are different forms of formats... NO :) 

dat20 <- dat20 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat20 <- dat20 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat20 <- dat20 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat20) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat20, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat20 <- Merge(dat20, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat20 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat20 <- Merge(dat20, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat20 by the External ID

dat20$Meta <- NULL # drop redundant meta data
dat20 <- dat20 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat20 <- dat20 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat20 <- dat20 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat20), 20) # relevant data
data_empty <- append(data_empty, list(dat20_empty), 20) # empty data

## Clean global environment to speed up the next round.. 
rm(dat20, dat20_empty)
json_split$class20 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (21) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat21 <- json_split$Class21 # get first round as df 
rownames(dat21) <- NULL    # reset row names 

nrow(subset(dat21, unlist(lapply(dat21$Object, function(x) length(x))) == 0)) # empty images 
dat21_empty <- subset(dat21, unlist(lapply(dat21$Object, function(x) length(x))) == 0) # get empty pictures
dat21_empty$Object <- "empty" # tell R these pics are empty

dat21 <- subset(dat21, unlist(lapply(dat21$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat21) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat21 <- dat21 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat21$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat21$class_type <- as.character(lapply(dat21$classifications, class)) 
table(dat21$class_type) # check distribution
dat21 <- subset(dat21, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat21 <- dat21 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat21$class_type <- as.character(lapply(dat21$classifications, class)) # check format 
table(dat21$class_type) # check distribution 
dat21 <- subset(dat21, class_type != "NULL") # ditch some empty classifications

dat21_ls <- subset(dat21, class_type == "list")  # these need further processing 

dat21_df <- subset(dat21, class_type == "data.frame") # these are ready
dat21_df <- dat21_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat21 <- dat21_df 
dat21$answer <- dat21$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat21_ls <- dat21_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat21_ls$class_type <- as.character(lapply(dat21_ls$classifications, class)) # check format
table(dat21_ls$class_type) # check format distribution 

dat21_df <- subset(dat21_ls, class_type == "data.frame") # these are the ones that are done
dat21_df <- dat21_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat21_df) <- colnames(dat21)  # make sure column names are equal 
dat21 <- data.frame(rbind(dat21, dat21_df)) # add readily parsed rows to dat21 

dat21_ls <- subset(dat21_ls, class_type == "list") # these are the ones that still require processing
dat21_ls$class_type <- as.character(lapply(dat21_ls$classifications, class))
table(dat21_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat21_ls <- dat21_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat21_ls) <- colnames(dat21)  # make sure column names are equal 
dat21 <- data.frame(rbind(dat21, dat21_ls)) # add readily parsed rows to dat21 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat21$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat21$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class21, External_ID %in% lost1)
rm(dat21_df, dat21_ls) # remove redundant data

dat21 <- dat21 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat21 <- distinct(dat21, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat21$class_type <- NULL # ditch non-relevant column 
dat21$position <- NULL # ditch non-relevant column 
colnames(dat21) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat21$Answer, class)))

## First, we ditch rows that have empty answers. 
dat21 <- subset(dat21, as.character(lapply(dat21$Answer, class)) != "NULL") # remove empty rows 

dat21_chr <- subset(dat21, as.character(lapply(dat21$Answer, class)) == "character") # these are the ones that are ready 

dat21_ls <- subset(dat21, as.character(lapply(dat21$Answer, class)) == "list") # these are the ones that need processing
dat21_ls <- subset(dat21_ls, as.character(lapply(dat21_ls$Answer, length)) == 1) # remove empty rows 
dat21_ls <- dat21_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat21_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat21_df <- subset(dat21_ls, as.character(lapply(dat21_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat21_df <- dat21_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat21_df) <- colnames(dat21)
dat21 <- data.frame(rbind(dat21_chr, dat21_df)) # make dat21_df and dat21_chr the new dat21 
dat21 <- dat21 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat21$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat21$External_ID)] # these are the ones 
lost1 <- json_split$Class21 %>% subset(External_ID %in% lost1) # full data

dat21_empty <- data.frame(rbind(dat21_empty, lost1)) # bind empty ones 
rm(dat21_df, dat21_ls, lost1, dat21_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat21 <- distinct(dat21, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat21$position <- NULL
colnames(dat21) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat21$Review, class))) # check if there are different forms of formats... NO :) 

dat21 <- dat21 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat21 <- dat21 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat21 <- dat21 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat21) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat21, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat21 <- Merge(dat21, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat21 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat21 <- Merge(dat21, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat21 by the External ID

dat21$Meta <- NULL # drop redundant meta data
dat21 <- dat21 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat21 <- dat21 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat21 <- dat21 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat21), 21) # relevant data
data_empty <- append(data_empty, list(dat21_empty), 21) # empty data

## Clean global environment to speed up the next round.. 
rm(dat21, dat21_empty)
json_split$class21 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (22) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat22 <- json_split$Class22 # get first round as df 
rownames(dat22) <- NULL    # reset row names 

nrow(subset(dat22, unlist(lapply(dat22$Object, function(x) length(x))) == 0)) # empty images 
dat22_empty <- subset(dat22, unlist(lapply(dat22$Object, function(x) length(x))) == 0) # get empty pictures
dat22_empty$Object <- "empty" # tell R these pics are empty

dat22 <- subset(dat22, unlist(lapply(dat22$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat22) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat22 <- dat22 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat22$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat22$class_type <- as.character(lapply(dat22$classifications, class)) 
table(dat22$class_type) # check distribution
dat22 <- subset(dat22, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat22 <- dat22 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat22$class_type <- as.character(lapply(dat22$classifications, class)) # check format 
table(dat22$class_type) # check distribution 
dat22 <- subset(dat22, class_type != "NULL") # ditch some empty classifications

dat22_ls <- subset(dat22, class_type == "list")  # these need further processing 

dat22_df <- subset(dat22, class_type == "data.frame") # these are ready
dat22_df <- dat22_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat22 <- dat22_df 
dat22$answer <- dat22$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat22_ls <- dat22_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat22_ls$class_type <- as.character(lapply(dat22_ls$classifications, class)) # check format
table(dat22_ls$class_type) # check format distribution 

dat22_df <- subset(dat22_ls, class_type == "data.frame") # these are the ones that are done
dat22_df <- dat22_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat22_df) <- colnames(dat22)  # make sure column names are equal 
dat22 <- data.frame(rbind(dat22, dat22_df)) # add readily parsed rows to dat22 

dat22_ls <- subset(dat22_ls, class_type == "list") # these are the ones that still require processing
dat22_ls$class_type <- as.character(lapply(dat22_ls$classifications, class))
table(dat22_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat22_ls <- dat22_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat22_ls) <- colnames(dat22)  # make sure column names are equal 
dat22 <- data.frame(rbind(dat22, dat22_ls)) # add readily parsed rows to dat22 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat22$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat22$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class22, External_ID %in% lost1)
rm(dat22_df, dat22_ls) # remove redundant data

dat22 <- dat22 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat22 <- distinct(dat22, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat22$class_type <- NULL # ditch non-relevant column 
dat22$position <- NULL # ditch non-relevant column 
colnames(dat22) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat22$Answer, class)))

## First, we ditch rows that have empty answers. 
dat22 <- subset(dat22, as.character(lapply(dat22$Answer, class)) != "NULL") # remove empty rows 

dat22_chr <- subset(dat22, as.character(lapply(dat22$Answer, class)) == "character") # these are the ones that are ready 

dat22_ls <- subset(dat22, as.character(lapply(dat22$Answer, class)) == "list") # these are the ones that need processing
dat22_ls <- subset(dat22_ls, as.character(lapply(dat22_ls$Answer, length)) == 1) # remove empty rows 
dat22_ls <- dat22_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat22_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat22_df <- subset(dat22_ls, as.character(lapply(dat22_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat22_df <- dat22_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat22_df) <- colnames(dat22)
dat22 <- data.frame(rbind(dat22_chr, dat22_df)) # make dat22_df and dat22_chr the new dat22 
dat22 <- dat22 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat22$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat22$External_ID)] # these are the ones 
lost1 <- json_split$Class22 %>% subset(External_ID %in% lost1) # full data

dat22_empty <- data.frame(rbind(dat22_empty, lost1)) # bind empty ones 
rm(dat22_df, dat22_ls, lost1, dat22_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat22 <- distinct(dat22, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat22$position <- NULL
colnames(dat22) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat22$Review, class))) # check if there are different forms of formats... NO :) 

dat22 <- dat22 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat22 <- dat22 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat22 <- dat22 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat22) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat22, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat22 <- Merge(dat22, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat22 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat22 <- Merge(dat22, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat22 by the External ID

dat22$Meta <- NULL # drop redundant meta data
dat22 <- dat22 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat22 <- dat22 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat22 <- dat22 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat22), 22) # relevant data
data_empty <- append(data_empty, list(dat22_empty), 22) # empty data

## Clean global environment to speed up the next round.. 
rm(dat22, dat22_empty)
json_split$class22 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (23) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat23 <- json_split$Class23 # get first round as df 
rownames(dat23) <- NULL    # reset row names 

nrow(subset(dat23, unlist(lapply(dat23$Object, function(x) length(x))) == 0)) # empty images 
dat23_empty <- subset(dat23, unlist(lapply(dat23$Object, function(x) length(x))) == 0) # get empty pictures
dat23_empty$Object <- "empty" # tell R these pics are empty

dat23 <- subset(dat23, unlist(lapply(dat23$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat23) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat23 <- dat23 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat23$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat23$class_type <- as.character(lapply(dat23$classifications, class)) 
table(dat23$class_type) # check distribution
dat23 <- subset(dat23, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat23 <- dat23 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat23$class_type <- as.character(lapply(dat23$classifications, class)) # check format 
table(dat23$class_type) # check distribution 
dat23 <- subset(dat23, class_type != "NULL") # ditch some empty classifications

dat23_ls <- subset(dat23, class_type == "list")  # these need further processing 

dat23_df <- subset(dat23, class_type == "data.frame") # these are ready
dat23_df <- dat23_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat23 <- dat23_df 
dat23$answer <- dat23$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat23_ls <- dat23_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat23_ls$class_type <- as.character(lapply(dat23_ls$classifications, class)) # check format
table(dat23_ls$class_type) # check format distribution 

dat23_df <- subset(dat23_ls, class_type == "data.frame") # these are the ones that are done
dat23_df <- dat23_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat23_df) <- colnames(dat23)  # make sure column names are equal 
dat23 <- data.frame(rbind(dat23, dat23_df)) # add readily parsed rows to dat23 

dat23_ls <- subset(dat23_ls, class_type == "list") # these are the ones that still require processing
dat23_ls$class_type <- as.character(lapply(dat23_ls$classifications, class))
table(dat23_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat23_ls <- dat23_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat23_ls) <- colnames(dat23)  # make sure column names are equal 
dat23 <- data.frame(rbind(dat23, dat23_ls)) # add readily parsed rows to dat23 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat23$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat23$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class23, External_ID %in% lost1)
rm(dat23_df, dat23_ls) # remove redundant data

dat23 <- dat23 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat23 <- distinct(dat23, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat23$class_type <- NULL # ditch non-relevant column 
dat23$position <- NULL # ditch non-relevant column 
colnames(dat23) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat23$Answer, class)))

## First, we ditch rows that have empty answers. 
dat23 <- subset(dat23, as.character(lapply(dat23$Answer, class)) != "NULL") # remove empty rows 

dat23_chr <- subset(dat23, as.character(lapply(dat23$Answer, class)) == "character") # these are the ones that are ready 

dat23_ls <- subset(dat23, as.character(lapply(dat23$Answer, class)) == "list") # these are the ones that need processing
dat23_ls <- subset(dat23_ls, as.character(lapply(dat23_ls$Answer, length)) == 1) # remove empty rows 
dat23_ls <- dat23_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat23_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat23_df <- subset(dat23_ls, as.character(lapply(dat23_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat23_df <- dat23_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat23_df) <- colnames(dat23)
dat23 <- data.frame(rbind(dat23_chr, dat23_df)) # make dat23_df and dat23_chr the new dat23 
dat23 <- dat23 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat23$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat23$External_ID)] # these are the ones 
lost1 <- json_split$Class23 %>% subset(External_ID %in% lost1) # full data

dat23_empty <- data.frame(rbind(dat23_empty, lost1)) # bind empty ones 
rm(dat23_df, dat23_ls, lost1, dat23_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat23 <- distinct(dat23, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat23$position <- NULL
colnames(dat23) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat23$Review, class))) # check if there are different forms of formats... NO :) 

dat23 <- dat23 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat23 <- dat23 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat23 <- dat23 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat23) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat23, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat23 <- Merge(dat23, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat23 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat23 <- Merge(dat23, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat23 by the External ID

dat23$Meta <- NULL # drop redundant meta data
dat23 <- dat23 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat23 <- dat23 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat23 <- dat23 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat23), 23) # relevant data
data_empty <- append(data_empty, list(dat23_empty), 23) # empty data

## Clean global environment to speed up the next round.. 
rm(dat23, dat23_empty)
json_split$class23 <- NULL # drop redundant data

## ~~~~~~~~~~~~~~~~~~~ (24) Next round of classification ~~~~~~~~~~~~~~~~~~~ 

dat24 <- json_split$Class24 # get first round as df 
rownames(dat24) <- NULL    # reset row names 

nrow(subset(dat24, unlist(lapply(dat24$Object, function(x) length(x))) == 0)) # empty images 
dat24_empty <- subset(dat24, unlist(lapply(dat24$Object, function(x) length(x))) == 0) # get empty pictures
dat24_empty$Object <- "empty" # tell R these pics are empty

dat24 <- subset(dat24, unlist(lapply(dat24$Object, function(x) length(x))) != 0) # Now, keep only relevant pictures
rownames(dat24) <- NULL # reset row names 

## Un-nest first level of the "Object" and drop non-relevant columns 
dat24 <- dat24 %>% unnest_wider(., Object, names_repair = 'unique', strict = FALSE) %>%
  dplyr::select(-featureId, -schemaId, -color, -title, -value, -instanceURI) %>%
  unnest(bbox)

id_check <- unique(dat24$External_ID) # we keep a list of all External_IDs to see how many we loose in the process

## We check the format before any un-nesting to see if some rows are already at the appropriate level. 
dat24$class_type <- as.character(lapply(dat24$classifications, class)) 
table(dat24$class_type) # check distribution
dat24 <- subset(dat24, class_type != "NULL") # ditch some empty classifications

## First iteration: Un-nest the first level of classifications
dat24 <- dat24 %>% unnest(., classifications)

## Now we check the format and  split the data by the format type (list and df) to un-nest them separately.
dat24$class_type <- as.character(lapply(dat24$classifications, class)) # check format 
table(dat24$class_type) # check distribution 
dat24 <- subset(dat24, class_type != "NULL") # ditch some empty classifications

dat24_ls <- subset(dat24, class_type == "list")  # these need further processing 

dat24_df <- subset(dat24, class_type == "data.frame") # these are ready
dat24_df <- dat24_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns expand to columns

dat24 <- dat24_df 
dat24$answer <- dat24$answer$value # since the answer is already parsed we can already add it

## Now we start the second iteration of un-nesting classifications
dat24_ls <- dat24_ls %>% unnest_longer(., classifications) # un-nest to the next level  
dat24_ls$class_type <- as.character(lapply(dat24_ls$classifications, class)) # check format
table(dat24_ls$class_type) # check format distribution 

dat24_df <- subset(dat24_ls, class_type == "data.frame") # these are the ones that are done
dat24_df <- dat24_df %>% unnest_wider(., classifications, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat24_df) <- colnames(dat24)  # make sure column names are equal 
dat24 <- data.frame(rbind(dat24, dat24_df)) # add readily parsed rows to dat24 

dat24_ls <- subset(dat24_ls, class_type == "list") # these are the ones that still require processing
dat24_ls$class_type <- as.character(lapply(dat24_ls$classifications, class))
table(dat24_ls$class_type) # check numbers

## At this point we are left with only lists. Those we can expand and add it to the rest. 
dat24_ls <- dat24_ls %>% unnest_wider(., classifications, strict = FALSE)  %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns

colnames(dat24_ls) <- colnames(dat24)  # make sure column names are equal 
dat24 <- data.frame(rbind(dat24, dat24_ls)) # add readily parsed rows to dat24 

## Excellent. Now we check how much External_ID's we lost due to empty classifications.
length(id_check) - length(unique(dat24$External_ID)) # 371 images
lost1 <- id_check[!(id_check %in% dat24$External_ID)] # these are the ones 
lost1 <- subset(json_split$Class24, External_ID %in% lost1)
rm(dat24_df, dat24_ls) # remove redundant data

dat24 <- dat24 %>% add_count(External_ID, title, name = "n_indiv") # get variable giving the individual count
dat24 <- distinct(dat24, External_ID, title, n_indiv, top, left, height, width, .keep_all = TRUE) # filter only distinct rows 
dat24$class_type <- NULL # ditch non-relevant column 
dat24$position <- NULL # ditch non-relevant column 
colnames(dat24) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height", 
                    "Group", "Answer", "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat24$Answer, class)))

## First, we ditch rows that have empty answers. 
dat24 <- subset(dat24, as.character(lapply(dat24$Answer, class)) != "NULL") # remove empty rows 

dat24_chr <- subset(dat24, as.character(lapply(dat24$Answer, class)) == "character") # these are the ones that are ready 

dat24_ls <- subset(dat24, as.character(lapply(dat24$Answer, class)) == "list") # these are the ones that need processing
dat24_ls <- subset(dat24_ls, as.character(lapply(dat24_ls$Answer, length)) == 1) # remove empty rows 
dat24_ls <- dat24_ls %>% unnest_longer(., Answer) # First we get rid of the first list level, this should not change the row length  
table(as.character(lapply(dat24_ls$Answer, class))) # check if there are different forms of formats 

## Next, we prepare rows that are already at df level. 
dat24_df <- subset(dat24_ls, as.character(lapply(dat24_ls$Answer, class)) == "data.frame") # these are the ones that are ready
dat24_df <- dat24_df %>% unnest_wider(., Answer, strict = FALSE) %>% # expand to columns
  dplyr::select(-featureId, -schemaId, -value, -position) # ditch non-relevant columns
colnames(dat24_df) <- colnames(dat24)
dat24 <- data.frame(rbind(dat24_chr, dat24_df)) # make dat24_df and dat24_chr the new dat24 
dat24 <- dat24 %>% unnest_longer(., Answer) # unnest rows with multiple species identifications

length(id_check) - length(unique(dat24$External_ID)) - nrow(lost1) # 103 images 
lost1 <- id_check[!(id_check %in% dat24$External_ID)] # these are the ones 
lost1 <- json_split$Class24 %>% subset(External_ID %in% lost1) # full data

dat24_empty <- data.frame(rbind(dat24_empty, lost1)) # bind empty ones 
rm(dat24_df, dat24_ls, lost1, dat24_chr) # clean global environment

## Finally, we make sure we have no unwanted replicates using the same distinct() syntax as previously: 
dat24 <- distinct(dat24, External_ID, Answer, n_indiv, top, left, width, height, .keep_all = TRUE) # filter only distinct rows 
dat24$position <- NULL
colnames(dat24) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "widhth", "height",
                    "Group", "Class_1",  "Review", "Meta", "Station", "Camera", "Index", "n_indiv") # set column names to avoid problems 

table(as.character(lapply(dat24$Review, class))) # check if there are different forms of formats... NO :) 

dat24 <- dat24 %>% unnest_wider(., Review, strict = FALSE) %>% # expand to columns 
  dplyr::select(-id, -createdAt, -labelId) # drop non-relevant columns

sum_list <- function(lst) {
  if (is.null(lst)) {
    NA
  } else {
    sum(lst, na.rm = TRUE)
  }
}

dat24 <- dat24 %>% mutate(score = map_dbl(score, sum_list)) # then we apply to to our score column 
dat24 <- dat24 %>% mutate(score = ifelse(score >= 1, 1, NA)) # set values to only 1 or NA

## Set column names to avoid problems 
colnames(dat24) <- c("External_ID", "Created_By", "Link", "Created_At", "Updated_At", "Agreement", "top", "left", "width", "height",
                    "Group", "Class_1", "Score" , "Reviewer", "Meta", "Station", "Camera", "Index", "n_indiv")

meta <- dplyr::select(dat24, External_ID, Meta) # Extract the nested list with the respective external ID 
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
dat24 <- Merge(dat24, meta_quality, all = TRUE, id = ~ External_ID) # Merge Quality data to dat24 by the External ID

## Second, we deal with the image comments
meta_comment <- distinct(subset(meta, variable == "Comment"), External_ID, .keep_all = TRUE) # Keep only Comments data of distinct External IDs
meta_comment$variable <- NULL # drop non-relevant column 
colnames(meta_comment) <- c("External_ID", "Comment") # set column names
dat24 <- Merge(dat24, meta_comment, all = TRUE, id = ~ External_ID) # Merge Comment data to dat24 by the External ID

dat24$Meta <- NULL # drop redundant meta data
dat24 <- dat24 %>% mutate(Reviewer = map_chr(Reviewer, ~ toString(unlist(.x)))) # make sure the reviewer column is also a character
dat24 <- dat24 %>% mutate(Reviewer = ifelse(Reviewer == "", NA, Reviewer)) # make sure the rest is NA
rm(meta, meta_comment, meta_quality) # clean global environment

## Only keep relevant columns for final data set
dat24 <- dat24 %>% select(External_ID, Link, Index, Created_By, Created_At, Updated_At, Group, Class_1, Score,
                        Reviewer, Station, Camera, n_indiv, Quality, Comment, top, left, width, height)

## Add parsed data to our data lists 
data_parsed <- append(data_parsed, list(dat24), 24) # relevant data
data_empty <- append(data_empty, list(dat24_empty), 24) # empty data

## Clean global environment to speed up the next round.. 
rm(dat24, dat24_empty)
json_split$class24 <- NULL # drop redundant data

## -------------------- Build Master file --------------------

## Properly name objects in data lists 
names(data_parsed) <- c("Class1","Class2","Class3","Class4","Class5","Class6","Class7","Class8", "Class9", "Class10", "Class11", "Class12","Class13","Class14","Class15","Class16","Class17","Class18","Class19","Class20","Class21","Class22","Class23","Class24")  
names(data_empty) <- c("Class1","Class2","Class3","Class4","Class5","Class6","Class7","Class8", "Class9", "Class10", "Class11", "Class12","Class13","Class14","Class15","Class16","Class17","Class18","Class19","Class20","Class21","Class22","Class23","Class24")

## Clean global environment
rm(json_split, id_check, sum_list)

## Finally, we build a master file that contains all the parsed data in a single df. To do so, we need to standardize
## the column names inside the list data_parsed and bind all rows together: 

data_parsed <- lapply(data_parsed, setNames, # standardize column names
                      c("External_ID", "Link", "Index", "Created_By", "Created_At", "Updated_At", "Group", "Classification", "Score",
                        "Reviewer", "Station", "Camera", "top", "left", "width", "height", "n_indiv", "Quality", "Comment"))

dat.all <- do.call("rbind", data_parsed) # bind data to master file
rownames(dat.all) <- NULL # reset row names

write.csv(dat.all, file = "json_parsed_all_30.12.23.csv", row.names = FALSE) # write file
dir <- getwd() # also save the current workspace 
save.image(paste0(dir, "/finished_json_workspace.RData"))
