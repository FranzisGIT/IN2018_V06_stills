# script for tidying up data output from SEAGUS TM percent cover annotations
# raw data file: concatenated outputs from TM in tab delimeted .txt file copied from:  
#"\\gpfs2-hba.san.csiro.au\OA_SEAMOUNTS_SOURCE\IN2018_V06\IMP\Coral_scoring"
# back up on GIT hub repository 

library(tidyverse)
TM_RAWconcat <- read_tsv("data/Concat_20190930.TXT", skip = 4)  # fist 4 rows are not needed

# select the columns that have recorded data from the file
TM_data1 <-TM_RAWconcat %>% 
  select(Filename, Image_row='Image row', Image_col='Image col', OpCode, PC_radius='Radius %', L1_CAT, L2_CAT, L2_Code, NOTES)

glimpse(TM_data1)

# create the image_key - a unique identifier for each image using the operation number (last 3 digits from OpCode) and image number (last 4 digits of filename excuding .JPG)
# create RanSel_key the random selection number within a transect(deployment) using peration number (last 3 digits from OpCode) and random selection number (first 3 digits 
# of filename, excluding the first letter

TM_data <- TM_data1  %>% 
  mutate(RanSelNo = str_sub(Filename, start=2, end=4),
         OpsNo = str_sub(OpCode, start=12, end=14),
         ImageNo = str_sub(Filename, start=48, end=51),      # separate the components out
         image_key = paste(OpsNo,"_",ImageNo, sep=""),        #create the image_key
         ranSel_key = paste(OpsNo,"_",RanSelNo, sep=""))     #create the ransom selection key
  

# filter out the 'overall image scores that should all have a NOTE saying 'user defined' 
# STILL TO DO need to check if there are records where NOTE was omitted...

OverviewScores <-  TM_data %>% 
  filter(!is.na(NOTES))    
        
# filter out the Percent cover point annotations including blanks
PC_cover_Anno <- TM_data %>% 
  filter(str_detect(L2_Code, "SU_*")| 
         str_detect(L2_Code, "SC_*") | 
         str_detect(L2_Code, "NS") | 
         is.na(L2_Code)
          ) %>% 
   group_by(image_key, L2_Code) %>% 
   summarise(count = n())

