# tidying up VME fauna anotations extract from ORACLE BHIMAGE -- extract created using VARS_2018-StillsAnnoExtracts.sql
# 
library(tidyverse)
VMEanno_raw <- read_csv("data/IN2018_V06_STILLS_VME_20190717.csv", na = c("(null)", "NA"))

# tidy up the CNT column first make No VME fauna a zero and Hydrocorals 1 (presence) then
# convert numeric values to numbers in new variable so only the counts are stored  
Temp1 <- VMEanno_raw %>% 
      mutate(Count = case_when(CONCEPT=="No-VMEfauna" ~ 0, 
                          CONCEPT=="Hydrocorals"  ~ 1,
                          TRUE ~ as.numeric(CNT)))



# create ID variables for linking these data to PC cover and stills tibbles

VMEanno_IDs <- Temp1  %>% 
  mutate(ImageName = str_sub(IMAGE_URL, start=85, end=139),   # identify image filename (without .jpg)
         ImageNo = str_sub(ImageName, start=48, end=51),      # pick out image number
         RanSelNo = str_sub(ImageName, start=2, end=4),       # pick out random selection number
         OpsNo = str_sub(SURVEY_OPS, start=12, end=14),       # this picks the operation number out
         image_key = paste(OpsNo,"_",ImageNo, sep=""),        #create the image_key
         ranSel_key = paste(OpsNo,"_",RanSelNo, sep=""))    #create the random selection key

# separate out comments from counts, look at the concepts that were recorded

Concepts <- VMEanno_IDs %>% 
  group_by(CONCEPT) %>% 
  summarise(n())

# other unquantified concepts that were scored
VMEanno_other <- VMEanno_IDs %>% 
  filter(CONCEPT=="fishing gear" |
           CONCEPT=="gear marks" |
           CONCEPT=="camera off bottom" |
           CONCEPT=="Rock" |
           CONCEPT=="rubbish")

# comments and or no counts recorded
VMEanno_comms <- VMEanno_IDs %>% 
  filter(is.na(Count),
         !(CONCEPT=="fishing gear" |
             CONCEPT=="gear marks" |
             CONCEPT=="camera off bottom" |
             CONCEPT=="Rock" |
             CONCEPT=="rubbish"))

VMEanno_data <- VMEanno_IDs %>% 
  filter(!is.na(Count)) %>% 
  select(SURVEY_OPS,
         image_key,
         ranSel_key,
         CONCEPT,
         Count,
         )


