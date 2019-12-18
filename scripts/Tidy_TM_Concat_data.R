# script for tidying up data output from SEAGIS TM percent cover annotations
# raw data file: concatenated outputs from TM in tab delimeted .txt file copied from:  
#"\\gpfs2-hba.san.csiro.au\OA_SEAMOUNTS_SOURCE\IN2018_V06\IMP\Coral_scoring"

# for data school the data is located on CSIRO network at: 
#\fstas1-hba.nexus.csiro.au\CMAR-SHARE\Public\AlthausF\FA_DataSchool_FOCUS-Rawdata" 

library(tidyverse)
TM_RAWconcat <- read_tsv("data/Concat_20191218.TXT", skip = 4)  # fist 4 rows are not needed

# select the columns that have recorded data from the file
TM_data1 <-TM_RAWconcat %>% 
  select(Filename, Image_row='Image row', Image_col='Image col', OpCode, PC_radius='Radius %', L1_CAT, L2_CAT, L2_Code, NOTES)

glimpse(TM_data1)

# create the image_key - a unique identifier for each image using the operation number (last 3 digits from OpCode (ignoring _NEW)) and image number (last 4 digits of filename excuding .JPG)
# create RanSel_key the random selection number within a transect(deployment) using peration number (last 3 digits from OpCode) and random selection number (first 3 digits 
# of filename, excluding the first letter
# exclude records where no annotations were made

TM_data <- TM_data1  %>% 
  mutate(RanSelNo = str_sub(Filename, start=2, end=4),
         OpsNo = str_sub(OpCode, start=12, end=14),          # this picks the operation number out ignoring tailing '_NEW'
         ImageNo = str_sub(Filename, start=48, end=51),      # separate the components out
         image_key = paste(OpsNo,"_",ImageNo, sep=""),        #create the image_key
         ranSel_key = paste(OpsNo,"_",RanSelNo, sep="")) %>%   #create the random selection key
  filter(!is.na(L1_CAT))
  
  
# check for duplication of images scored in original and 'NEW' TM to weed out double scoring - write out the result and get it fixrd at data entry level.
dataTemp <- TM_data %>% 
  group_by(OpCode, image_key) %>% 
  summarise(count = n())

duplicates <- dataTemp %>% 
  ungroup() %>% 
  group_by(image_key) %>% 
  summarise(countSum = sum(count), NoOps = n()) %>% 
  filter(NoOps!=1)
write_csv(duplicates, "Results/duplicateScored.csv")

# once the duplicate scores are weeded out (no data in above tibble) continue data cleaning

# filter out the 'overall image scores that should all have a NOTE saying 'user defined' 
# also select the entries where NOTE was omitted (forgotten), but the L2 code is of overview type
# add impacts groupings and classes for L2_codes and replace wordy L2_code with a shorter Letter coding 

OverviewScores <-  TM_data %>% 
  filter(!is.na(NOTES)|
           (str_detect(L2_Code, "SU-*", negate = TRUE) & 
           str_detect(L2_Code, "SC-*", negate = TRUE) & 
           str_detect(L2_Code, "NS", negate = TRUE))  ) %>% 
  mutate(ImpGroup = case_when(
      L2_Code == "N/A" ~"0",
      L2_Code == "High - reef" ~"8",
      L2_Code == "Low - sediment filled" ~"0",
      L2_Code == "Isolated fragments/ clumps" ~"4",
      L2_Code == "MedAb - with clumps - dead" ~"2",
      L2_Code == "HighAb - with patch - live" ~"6",
      L2_Code == "HighAb - with other" ~"3",
      L2_Code == "Low - with patch - live" ~"7",
      L2_Code == "HighAb - with clumps - dead" ~"2",
      L2_Code == "High - shaved" ~"1",
      L2_Code == "Low - with clumps - dead" ~"2",
      L2_Code == "LowAb - with clumps" ~"2",
      L2_Code == "Shaved" ~"1",
      L2_Code == "MedAb" ~"8",
      L2_Code == "LowAb - no clumps" ~"0",
      L2_Code == "MedAb - with patch - dead" ~"6",
      L2_Code == "Low - shaved" ~"1",
      L2_Code == "Low - with patch - dead" ~"7",
      L2_Code == "MedAb - with patch - live" ~"6",
      L2_Code == "HighAb - with patch - dead" ~"5",
      L2_Code == "MedAb - with clumps - live" ~"2",
      L2_Code == "HighAb - no other" ~"3",
      L2_Code == "HighAb - with clumps - live" ~"2",
      L2_Code == "Low - with clumps - live" ~"2"   
      )) %>% 
  mutate(ImpGroup = as.numeric(ImpGroup)) %>% 
  mutate(ImpClass = case_when(
    L2_Code == "N/A" ~"0",
    L2_Code == "High - reef" ~"8M",
    L2_Code == "Low - sediment filled" ~"0M",
    L2_Code == "Isolated fragments/ clumps" ~"4R",
    L2_Code == "MedAb - with clumps - dead" ~"2R_M",
    L2_Code == "HighAb - with patch - live" ~"6R",
    L2_Code == "HighAb - with other" ~"3R",
    L2_Code == "Low - with patch - live" ~"7M",
    L2_Code == "HighAb - with clumps - dead" ~"2R_H",
    L2_Code == "High - shaved" ~"1M",
    L2_Code == "Low - with clumps - dead" ~"2M",
    L2_Code == "LowAb - with clumps" ~"2R_L",
    L2_Code == "Shaved" ~"1R",
    L2_Code == "MedAb" ~"8R",
    L2_Code == "LowAb - no clumps" ~"0R",
    L2_Code == "MedAb - with patch - dead" ~"6R",
    L2_Code == "Low - shaved" ~"1M",
    L2_Code == "Low - with patch - dead" ~"7M",
    L2_Code == "MedAb - with patch - live" ~"6R",
    L2_Code == "HighAb - with patch - dead" ~"5R",
    L2_Code == "MedAb - with clumps - live" ~"2R_M",
    L2_Code == "HighAb - no other" ~"3R",
    L2_Code == "HighAb - with clumps - live" ~"2R_H",
    L2_Code == "Low - with clumps - live" ~"2M"   
  )) %>% 
  mutate(L2_Code = case_when(
    L2_Code == "N/A" ~"OTHER",
    L2_Code == "High - reef" ~"MHReef",
    L2_Code == "Low - sediment filled" ~"MLSed",
    L2_Code == "Isolated fragments/ clumps" ~"RIC",
    L2_Code == "MedAb - with clumps - dead" ~"RmCD",
    L2_Code == "HighAb - with patch - live" ~"RhPL",
    L2_Code == "HighAb - with other" ~"RhO",
    L2_Code == "Low - with patch - live" ~"MLPL",
    L2_Code == "HighAb - with clumps - dead" ~"RhCD",
    L2_Code == "High - shaved" ~"MHS",
    L2_Code == "Low - with clumps - dead" ~"MLCD",
    L2_Code == "LowAb - with clumps" ~"RlC",
    L2_Code == "Shaved" ~"RS",
    L2_Code == "MedAb" ~"Rm",
    L2_Code == "LowAb - no clumps" ~"RlNC",
    L2_Code == "MedAb - with patch - dead" ~"RmPD",
    L2_Code == "Low - shaved" ~"MLS",
    L2_Code == "Low - with patch - dead" ~"MLPD",
    L2_Code == "MedAb - with patch - live" ~"RmPL",
    L2_Code == "HighAb - with patch - dead" ~"RhPD",
    L2_Code == "MedAb - with clumps - live" ~"RmCL",
    L2_Code == "HighAb - no other" ~"RhNOth",
    L2_Code == "HighAb - with clumps - live" ~"RhCL",
    L2_Code == "Low - with clumps - live" ~"MLCL"
  ))

# identify overview scores used 
OV_cat <- OverviewScores %>% 
  distinct(L1_CAT, L2_CAT,L2_Code, ImpGroup, ImpClass)
    
# filter out the Percent cover point annotations including blanks
PC_cover_Anno1 <- TM_data %>% 
  filter(str_detect(L2_Code, "SU-*")| 
         str_detect(L2_Code, "SC-*") | 
         str_detect(L2_Code, "NS") | 
         is.na(L2_Code)
          ) %>% 
   group_by(OpCode, image_key, L2_CAT, L2_Code) %>% 
   summarise(count = n())

# extract the number of annotated points per image
PtsPerImage <- PC_cover_Anno1 %>%
  group_by(image_key) %>% 
  summarise(PpI = sum(count))

# join the number of points per image onto the data and calculate a percentage for each score
PC_cover_Anno <- right_join(PC_cover_Anno1,PtsPerImage, by=c("image_key"="image_key")) %>% 
  arrange(image_key) %>% 
  mutate(PC_cover=100*count/PpI)


# read in additional information for each image - location name, geolocation, quadrat sizes, etc

# for data school the data is located on CSIRO network at: 
#\fstas1-hba.nexus.csiro.au\CMAR-SHARE\Public\AlthausF\FA_DataSchool_FOCUS-Rawdata" 

AllSTills <- read_csv("data/IN2018_V06_AllStills.csv", col_types = "ccccccdddccdddcdd")

#check number of rows in percent cover data - ensure number rows stays the same with join below
nrow(PC_cover_Anno)

# make depth numeric

AllSTills <- AllSTills %>% 
  mutate(depth=as.numeric(Z)) %>% 
  select(-c(Z))

# join image details to the PC cover data, overwriting the previous version

PC_cover <- left_join(PC_cover_Anno, AllSTills, by=c("image_key"="KEY"))

# check the data for annotations that are not part of the data annotation plan (there is a number entry in Selection round and SelNo - NS replaced)
glimpse(PC_cover)

check1 <- PC_cover %>% 
  group_by(`Selection round (1 orig sel, 2 replacement)`) %>% 
  summarise(n())
 
check2 <- PC_cover %>% 
  filter(is.na(`SelNo-NS replaced`))


# CHECK: look at the check1 & check2 file and make sure these are not part of the selection
# if sure, remove the data entries where `Selection round (1 orig sel, 2 replacement)` or `SelNo-NS eplaced` is NA  
PC_cover <-  PC_cover %>% 
  filter(!is.na(`Selection round (1 orig sel, 2 replacement)`), !is.na(`SelNo-NS replaced`))

# rerun check1 & check2 to ensure the data was deleted.


# checking distribution of scored images over random sample selection along each transect

randSelPoints_byOps <-  ggplot(PC_cover,
       aes(x=`SelNo-NS replaced`,
           y=`SelNo-NS replaced`
       ))+
  geom_point(size=0.5)+
  facet_wrap(~SVY_OPS)+  
  theme(axis.text.x=element_text(size=6), 
        axis.text.y=element_text(size=6),
        strip.text =element_text(size=6))
  
ggsave("figures/randSelPoints_byOps.jpg", 
       plot=randSelPoints_byOps, 
       width=24, 
       height=15,
       units= "cm",
       dpi=600)

randSelPoints_byOps


# spread the data into a by image matrix formatand adding the 'overview' annotation then export to .csv for taking into QGIS maps
glimpse(PC_cover)

PC_cover <- ungroup(PC_cover)   # ensuring that no groupings are left over from previous checks need to reassign!


PC_cover %>% 
  select(image_key, L2_Code, PC_cover) %>% 
  spread(L2_Code, PC_cover)


# check out overview scores and get them ready to re-attach to the 'by image data'
glimpse(OverviewScores)
OV1 <- OverviewScores %>% 
  select(image_key, L1_CAT, L2_CAT, L2_Code, ImpGroup, ImpClass) %>% 
  rename(OV_group=L1_CAT, OV_CAT=L2_CAT, OV_CD=L2_Code) 


# check it only removes 3 records then re-run spread but other way round...; 
#join location info to the by image matrix and export for QGIS mapping\
# join overview score to the data matrix
PCcoverbyImage <- PC_cover %>% 
  select(image_key, L2_Code,PC_cover) %>% 
  spread(L2_Code, PC_cover) %>% 
  replace_na(list('SC-ENLP'=0,
                  'SU-ENLP'=0,
                  'SC-SOL'=0,
                  'SU-SOL'=0,
                  'SC-MAD'=0,
                  'SU-MAD'=0,
                  'SU-BCOR'=0,
                  'SU-BBAR'=0,
                  'SU-BOTH'=0,
                  'SU-ROK'=0,
                  'SU-BOL'=0,
                  'SU-COB'=0,
                  'SU-CONBIO',
                  'SU-PEBGRAV'=0,
                  'SU-SAMU'=0,
                  'NS' = 0)) %>% 
  left_join(AllSTills, by=c("image_key"="KEY")) %>% 
  left_join(OV1, by=c("image_key"="image_key"))

write_csv(PCcoverbyImage, "Results/PCcoverbyImage.csv")

# add the overview score to the column data and write it out
PC_cover <- PC_cover %>% 
  left_join(OV1, by=c("image_key"="image_key"))
write_csv(PC_cover, "Results/PCcover.csv")


# NOW THE DATA IS CLEANED UP: start new script using output from here
# data exploration in script: PC_coverExplore.R


