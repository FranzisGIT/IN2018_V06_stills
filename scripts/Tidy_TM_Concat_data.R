# script for tidying up data output from SEAGIS TM percent cover annotations
# raw data file: concatenated outputs from TM in tab delimeted .txt file copied from:  
#"\\gpfs2-hba.san.csiro.au\OA_SEAMOUNTS_SOURCE\IN2018_V06\IMP\Coral_scoring"

# for data school the data is located on CSIRO network at: 
#\fstas1-hba.nexus.csiro.au\CMAR-SHARE\Public\AlthausF\FA_DataSchool_FOCUS-Rawdata" 

library(tidyverse)
TM_RAWconcat <- read_tsv("data/Concat_20191011.TXT", skip = 4)  # fist 4 rows are not needed

# select the columns that have recorded data from the file
TM_data1 <-TM_RAWconcat %>% 
  select(Filename, Image_row='Image row', Image_col='Image col', OpCode, PC_radius='Radius %', L1_CAT, L2_CAT, L2_Code, NOTES)

glimpse(TM_data1)

# create the image_key - a unique identifier for each image using the operation number (last 3 digits from OpCode (ignoring _NEW)) and image number (last 4 digits of filename excuding .JPG)
# create RanSel_key the random selection number within a transect(deployment) using peration number (last 3 digits from OpCode) and random selection number (first 3 digits 
# of filename, excluding the first letter

TM_data <- TM_data1  %>% 
  mutate(RanSelNo = str_sub(Filename, start=2, end=4),
         OpsNo = str_sub(OpCode, start=12, end=14),          # this picks the operation number out ignoring tailing '_NEW'
         ImageNo = str_sub(Filename, start=48, end=51),      # separate the components out
         image_key = paste(OpsNo,"_",ImageNo, sep=""),        #create the image_key
         ranSel_key = paste(OpsNo,"_",RanSelNo, sep=""))     #create the ransom selection key
  

# filter out the 'overall image scores that should all have a NOTE saying 'user defined' 
# also select the entries where NOTE was omitted (forgotten), but the L2 code is of overview type

OverviewScores <-  TM_data %>% 
  filter(!is.na(NOTES)|
           (str_detect(L2_Code, "SU_*", negate = TRUE) & 
           str_detect(L2_Code, "SC_*", negate = TRUE) & 
           str_detect(L2_Code, "NS", negate = TRUE))  )   
        
# filter out the Percent cover point annotations including blanks
PC_cover_Anno1 <- TM_data %>% 
  filter(str_detect(L2_Code, "SU_*")| 
         str_detect(L2_Code, "SC_*") | 
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

AllSTills <- read_csv("data/IN2018_V06_AllStills.csv")

#check number of rows in percent cover data - ensure number rows stays the same with join below
nrow(PC_cover_Anno)

# join image details to the PC cover data, overwriting the previous version

PC_cover_Anno <- left_join(PC_cover_Anno, AllSTills, by=c("image_key"="KEY"))

# check the data for annotations that are not part of the data annotation plan (there is a number entry in Selection round and SelNo - NS replaced)
glimpse(PC_cover_Anno)

check1 <- PC_cover_Anno %>% 
  group_by(`Selection round (1 orig sel, 2 replacement)`) %>% 
  summarise(n())
 
check2 <- PC_cover_Anno %>% 
  filter(is.na(`SelNo-NS eplaced`))

# CHECK: look at the check1 & check2 file and make sure these are not part of the selection
# if sure, remove the data entries where `Selection round (1 orig sel, 2 replacement)` or `SelNo-NS eplaced` is NA  
PC_cover_Anno <-  PC_cover_Anno %>% 
  filter(!is.na(`Selection round (1 orig sel, 2 replacement)`), !is.na(`SelNo-NS eplaced`))

# rerun check1 & check2 to ensure the data was deleted.

# checking distribution of scored images over random sample selection along each transect

randSelPoints_byOps <-  ggplot(PC_cover_Anno,
       aes(x=`SelNo-NS eplaced`,
           y=`SelNo-NS eplaced`
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



# NOW THE DATA IS CLEANED UP


# just playing with ggplot - mapping the data in space 
ggplot(PC_cover_Anno,
       mapping= aes(x=KF_USBL_LON,
                    y=KF_USBL_LAT,
                    colour=L2_Code)
)+
  geom_point()



# intend to put Australia map (shape file in maps folder) as a backgrop
#for opening up shape files I need the packages rgdal & ggmap - once installed I don't need the 
# install statement anymore thus

#install.packages("rgdal")
#install.packages("ggmaps")

#library(rgdal)     # R wrapper around GDAL/OGR
#library(ggmaps)    # for fortifying shapefiles   # error message package ‘ggmaps’ is not available (for R version 3.6.1)
#library(ggplot2)   # for general plotting

# commented out because ggplot is not supported - too hard basket for now

# check out the substrate codes that were annotated
PC_cover_Anno %>% 
  group_by(L2_Code) %>% 
  summarise(meanPCcover= mean(PC_cover), n())

# create a vector with the sequence of the substrate types for ordering them in a meaningful way
SubstSeq <- c('SC-ENLP',
              'SU-ENLP',
              'SC-SOL',
              'SU-SOL',
              'SC-MAD',
              'SU-MAD',
              'SU-BCOR',
              'SU-BBAR',
              'SU-BOTH',
              'SU-ROK',
              'SU-BOL',
              'SU-COB',
              'SU-CONBIO',
              'SU-PEBGRAV',
              'SU-SAMU',
              'NS')

# summarising the average % of substrate types by transect (OpCode) or by seamount (MapLoc) and checking
# checking out varaious ways of visualising the data

byOps <- PC_cover_Anno %>% 
  group_by(MapLoc,OpCode,L2_Code) %>% 
  summarise(meanPCcover= mean(PC_cover), 
            meanDpth=mean(Z))

# try bar graph - not very infomative            
ggplot(byOps,
       mapping= aes(x=OpCode,
                    y=meanPCcover,
                    colour=L2_Code)
       )+
  geom_col()

# point scatterplot
ggplot(byOps,
       mapping= aes(x=factor(L2_Code, level =SubstSeq),
                    y=meanDpth,
                    size=meanPCcover)
      )+
  geom_point(alpha=0.2)+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")

# create the same plot without summarising the data first to look at the dept 
#distribution of the substrate types

ggplot(PC_cover_Anno,
       mapping= aes(x=factor(L2_Code, level =SubstSeq),              #call the pre existing vector
                    y=Z)
  )+
  geom_point()+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")

subst_depthDist <- ggplot(PC_cover_Anno,
       mapping= aes(x=factor(L2_Code, level =SubstSeq),              
                    y=Z,
                    size=PC_cover)
  )+
  geom_point(alpha=0.2)+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")
ggsave("figures/subst_depthDist.jpg", 
       plot=subst_depthDist, 
       dpi=600)

# error message about 240 missing vales - need to check where these are why they are missing...
# run plot without PC_cover
ggplot(PC_cover_Anno,
          mapping= aes(x=factor(L2_Code, level =SubstSeq),              
                       y=Z,
                       )
  )+
  geom_point(alpha=0.2)+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")

# still 240 missing - missing depths? 
PC_cover_Anno %>% 
  filter(is.na(Z))

#FLAG: -- for finalising the data need to update depths in data extract IN2018_V06_AllStills.csv then rerun checks

# creating pie graphs for the distributuion of %cover by substrate types for each location 
ggplot(PC_cover_Anno,
          mapping= aes(x="", 
                       y=PC_cover,               
                       fill=factor(L2_Code, level =SubstSeq)
                          )
  )+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0) + 
  facet_wrap(~MapLoc)
  #geom_text(aes(label = paste0(round(PC_cover*100), "%")), 
   #         position = position_stack(vjust = 0.5))+    # labels too crowded - turn them off

# did not work very well - not sur why it has partially empty graphs - try and look at a couple locations separately

Fang <- PC_cover_Anno %>% 
  filter(MapLoc=="Fang")
ggplot(Fang,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)
 
Pedra <- PC_cover_Anno %>% 
  filter(MapLoc=="Pedra")
ggplot(Pedra,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)

z16 <- PC_cover_Anno %>% 
  filter(MapLoc=="z16")
ggplot(z16,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)

Hill_U <- PC_cover_Anno %>% 
  filter(MapLoc=="Hill U")
ggplot(Hill_U,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)

MainMatt <- PC_cover_Anno %>% 
  filter(MapLoc=="Main Matt")
ggplot(MainMatt,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)

