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

# actual numeric data with only the necesary variables in the Tibble...
VMEanno_data <- VMEanno_IDs %>% 
  filter(!is.na(Count)) %>% 
  select(SURVEY_OPS,
         image_key,
         ranSel_key,
         CONCEPT,
         Count,
         )

# for data school the data is located on CSIRO network at: 
#\fstas1-hba.nexus.csiro.au\CMAR-SHARE\Public\AlthausF\FA_DataSchool_FOCUS-Rawdata" 

AllSTills <- read_csv("data/IN2018_V06_AllStills.csv")

#check number of rows in percent cover data - ensure number rows stays the same with join below
nrow(PC_cover_Anno)

# make depth numeric

AllSTills <- AllSTills %>% 
  mutate(depth=as.numeric(Z)) %>% 
  select(-c(Z))



# BITS THAT MIGHT BE USEFUL...
# join image details to the PC cover data, overwriting the previous version

VMEanno_data1 <- left_join(VMEanno_data, AllSTills, by=c("image_key"="KEY"))

# check the data for annotations that are not part of the data annotation plan (there is a number entry in Selection round and SelNo - NS replaced)
glimpse(VMEanno_data1)

check1 <- VMEanno_data1 %>% 
  group_by(`Selection round (1 orig sel, 2 replacement)`) %>% 
  summarise(n())

check2 <- VMEanno_data1 %>% 
  filter(is.na(`SelNo-NS replaced`))

# CHECK: look at the check1 & check2 file and make sure these are not part of the selection
# if sure, remove the data entries where `Selection round (1 orig sel, 2 replacement)` or `SelNo-NS eplaced` is NA  
VMEanno_data1 <-  VMEanno_data1 %>% 
  filter(!is.na(`Selection round (1 orig sel, 2 replacement)`), 
         !is.na(`SelNo-NS replaced`),
         `Selection round (1 orig sel, 2 replacement)`<100)

# rerun check1 & check2 to ensure the data was deleted.

# write out data to results for new scipt
write_csv(VMEanno_data1, "Results/VMEanno_data.csv")

# 