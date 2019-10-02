# script for concatenating randsample data and creating image key

library(tidyverse)

# read the data into tables
t1 <- read_csv("data/IN2018_V06_012_RANDSPL.CSV")
t2 <- read_csv("data/IN2018_V06_013_RANDSPL.CSV")
t3 <- read_csv("data/IN2018_V06_014_RANDSPL.CSV")
t4 <- read_csv("data/IN2018_V06_015_RANDSPL.CSV")
t5 <- read_csv("data/IN2018_V06_020_RANDSPL.CSV")
t6 <- read_csv("data/IN2018_V06_021_RANDSPL.CSV")
t7 <- read_csv("data/IN2018_V06_022_RANDSPL.CSV")

#bind all tables together
RandSmpl <-  bind_rows(t1,t2,t3,t4,t5,t6,t7)

#clean up temp files
rm(t1,t2,t3,t4,t5,t6,t7)

#check out data and variable formats
glimpse(RandSmpl)

# create the image_key - a unique identifier for each image using the operation number (last 3 digits from operation_id) and image number (last 4 digits of filename excuding .JPG)
# create RanSel_key the random selection number within a transect(deployment) using peration number (last 3 digits from operation_id) and random selection number (first 3 digits 
# of dest_file, excluding the first letter

RandSmpl <- RandSmpl  %>% 
  mutate(RanSelNo = str_sub(dest_file, start=2, end=4),
         OpsNo = str_sub(operation_id, start=12, end=14),
         ImageNo = str_sub(dest_file, start=48, end=51),      # separate the components out
         image_key = paste(OpsNo,"_",ImageNo, sep=""),        #create the image_key
         ranSel_key = paste(OpsNo,"_",RanSelNo, sep=""))     #create the ransom selection key

write_csv(RandSmpl,"C:/Data/2019_DataSchoolFOCUS/MyData/IN2018_V06_stills/IN2018_V06_stills/results/randSmpl.csv")
