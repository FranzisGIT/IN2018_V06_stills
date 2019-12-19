# data exploration of Percent Cover for substrate types; input filrs from cleaning up 
# the TM output data (script: Tidy_TM_Concat_data.R)

library(tidyverse)

PCcoverbyImage <- read_csv("Results/PCcoverbyImage.csv")
PC_cover <- read_csv("Results/PCcover.csv")

VMEanno_DensQ <- read_csv("Results/VMEanno_DensQ.csv")
VMEanno_PCcoral <- read_csv("Results/VMEanno_PCcoral.csv")
VMEannoMatrix <- read_csv("Results/VMEannoMatrix.csv")

# just playing with ggplot - mapping the data in space 

ggplot(PC_cover,
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
PC_cover %>% 
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

byOps <- PC_cover %>% 
  group_by(MapLoc,OpCode,L2_Code) %>% 
  summarise(meanPCcover= mean(PC_cover), 
            meanDpth=mean(depth))

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

ggplot(PC_cover,
       mapping= aes(x=factor(L2_Code, level =SubstSeq),              #call the pre existing vector
                    y=depth)
)+
  geom_point()+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")

subst_depthDist <- ggplot(PC_cover,
                          mapping= aes(x=factor(L2_Code, level =SubstSeq),              
                                       y=depth,
                                       size=PC_cover)
)+
  geom_point(alpha=0.2)+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")

subst_depthDist
ggsave("figures/subst_depthDist.jpg", 
       plot=subst_depthDist, 
       dpi=600)

# error message about 240 missing vales - need to check where these are why they are missing...
# run plot without PC_cover
ggplot(PC_cover,
       mapping= aes(x=factor(L2_Code, level =SubstSeq),              
                    y=depth,
       )
)+
  geom_point(alpha=0.2)+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  labs(x="substrate type", y="depth")

# still 240 missing - missing depths? 
PC_cover %>% 
  filter(is.na(depth))

#FLAG: -- for finalising the data need to update depths in data extract IN2018_V06_AllStills.csv then rerun checks

# creating pie graphs for the distributuion of %cover by substrate types for each location 
ggplot(PC_cover,
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

Fang <- PC_cover %>% 
  filter(MapLoc=="Fang") %>% 
  ggplot(Fang,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
    geom_bar(stat="identity", width=1)+
    coord_polar("y", start=0)
Fang

Pedra <- PC_cover %>% 
  filter(MapLoc=="Pedra") %>% 
  ggplot(Pedra,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
    geom_bar(stat="identity", width=1)+
    coord_polar("y", start=0)
Pedra

z16 <- PC_cover %>% 
  filter(MapLoc=="z16") %>% 
  ggplot(z16,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
   geom_bar(stat="identity", width=1)+
   coord_polar("y", start=0)

Hill_U <- PC_cover %>% 
  filter(MapLoc=="Hill U") %>% 
  ggplot(Hill_U,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
   geom_bar(stat="identity", width=1)+
   coord_polar("y", start=0)
Hill_U

MainMatt <- PC_cover %>% 
  filter(MapLoc=="Main Matt") %>% 
  ggplot(MainMatt,
       mapping= aes(x="", 
                    y=PC_cover,               
                    fill=factor(L2_Code, level =SubstSeq)
       ))+
    geom_bar(stat="identity", width=1)+
   coord_polar("y", start=0)
MainMatt

library(cowplot)
comboPlot1 <- plot_grid(Pedra,z16, Hill_U, MainMatt, Fang,ncol = 2)
comboPlot1

## these are all interesting but not especially useful

# looking at the distribution of density and number of taxa over the whole data set
VME_TotDens <- VMEanno_DensQ %>% 
  group_by(image_key, SVY_OPS, MapLoc, depth) %>% 
  summarise(TotDens=sum(Dens),
            noTaxa=sum(NoTypes))
VME_TotDens %>% 
  ggplot(aes(x=TotDens))+
  geom_histogram()

VME_TotDens %>% 
  ggplot(aes(x=noTaxa))+
  geom_histogram()  

# looking at some summary stats
VMEandSubst <- VMEannoMatrix %>% 
  select(image_key,
         `Black & Octocorals`,
         `Brisingid`,
         `D.horridus`,
         `Enallopsammia`,
         `Hydrocorals`,
         `Hydrocorals: Branching`,
         `Irregular urchins`,
         `Madrepora`,
         `No-VMEfauna`,
         `Regular urchins`,
         `S.variabilis`,
         `Sponges`,
         `Stalked crinoids`,
         `Stony corals`,
         `True anemones: Fourlobed`,
         `Unstalked crinoids`,
         PC_Sub_CoralReef,
         PC_EnallopMatrix,
         PC_SolMatrix) %>% 
  left_join(PCcoverbyImage, by=c("image_key"="image_key"))

Txx <- VMEannoMatrix %>% 
  select(image_key,
         `Black & Octocorals`,
         `Brisingid`,
         `D.horridus`,
         `Enallopsammia`,
         `Hydrocorals`,
         `Hydrocorals: Branching`,
         `Irregular urchins`,
         `Madrepora`,
         `No-VMEfauna`,
         `Regular urchins`,
         `S.variabilis`,
         `Sponges`,
         `Stalked crinoids`,
         `Stony corals`,
         `True anemones: Fourlobed`,
         `Unstalked crinoids`,
         PC_Sub_CoralReef,
         PC_EnallopMatrix,
         PC_SolMatrix) %>% 
  left_join(VMEonly_TotDens, by=c("image_key"="image_key")) %>% 
  left_join(VME_TotDens, by=c("image_key"="image_key")) %>% 
  select(-MapLoc.y, -depth.y, -SVY_OPS.y)
  
PCcoverbyImage %>% 
  select(image_key,
         'SC-ENLP',
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
         'NS') %>% 
  left_join(TVME, by=c("image_key"="image_key"))


# data exploration for overview scoring

PCcoverbyImage %>% 
  ggplot(aes(x = depth,
             y = `SU-BCOR`,
             colour = OV_CAT))+
  geom_point(alpha=0.2)+
  facet_wrap(~MapLoc)

PCcoverbyImage %>% 
  ggplot(aes(x = depth,
             y = (`SU-SOL`+ `SC-SOL`),
             colour = OV_CAT))+
  geom_point(alpha=0.2)+
  facet_wrap(~MapLoc)

temp1 <- PC_cover %>% 
  left_join(OV1, by=c("image_key"="image_key"))
 temp1

 PC_cover %>% 
   group_by(L2_Code) %>% 
   ggplot(aes(x = `OV_CAT`,
              y = mean(PC_cover),
              colour = L2_Code)) +
     geom_col() +
   theme(axis.text.x.bottom = element_text(angle = 90))
 
 PCcoverbyImage %>%
   filter(MapLoc=="z16") %>% 
   ggplot(aes(x = depth,
              y = (`SU-SOL`+ `SC-SOL`),
              colour = OV_CAT))+
   geom_point()+
   facet_wrap(~MapLoc)
 
 PC_cover %>% 
   ggplot(aes(x = `OV_CAT`
             )) +
   geom_bar() +
   theme(axis.text.x.bottom = element_text(angle = 90))
 
AllSTills %>% 
  filter(`Selection round (1 orig sel, 2 replacement)` == 1 |
           `Selection round (1 orig sel, 2 replacement)` == 2 |
           `Selection round (1 orig sel, 2 replacement)` == 3) %>% 
  group_by(RAN_SEL_STAT) %>% 
  summarise(cntImages=n())


nTarget <- Target %>% 
  ungroup %>% 
  summarise(sum(`Target RANSMPL (1/10)`))
nTarget


VMEanno_data %>% 
  group_by(image_key) %>% 
  summarise(no_anno=n()) %>% 
  ungroup() %>% 
  summarize(noIm=n())


VME_AnnoAll %>% 
  ggplot(mapping = aes(x= (`SC-SOL`+`SU-SOL`), 
                       y= PC_SolMatrix,
                    ))+
  geom_point()  


PC_cover %>%
  filter(`OV_CAT` != `No rubble or matrix` &
           `OV_CAT` != `High - reef` &
           `OV_CAT` != `Low - sediment fille`) %>% 
  group_by(MapLoc, `OV_CAT`) %>% 
  ggplot(aes(x = MapLoc,
             y = mean(PC_cover),
             colour = `OV_CAT`)) +
  geom_col() +
  theme(axis.text.x.bottom = element_text(angle = 90))


OV1 %>% 
  left_join(AllSTills, by=c("image_key"="KEY")) %>% 
  write_csv("Results/ImapactsTemp.csv")

PCcoverbyImage %>% 
  ggplot(aes(x = depth,
             y = (`SU-SOL`+ `SC-SOL`+`SU-BCOR`),
             colour = ImpClass))+
  geom_point(alpha=0.2)+
  facet_wrap(~MapLoc) 

colpal <-  c("grey",
            "blue",
             "cyan",
             "purple",
             "Orchid",
             "green",
             "orange",
             "red",
             "brown",
             "black")
ggplot(PC_cover,
       mapping = aes(x=factor(ImpClass),              #call the pre existing vector
                    y=depth,
                    colour = factor(ImpGroup)))+
  geom_point(alpha=0.2)+
  scale_y_reverse() +                            # reverse y-axis because it represents ocean depth 
  theme(axis.text.x = element_text(angle = 90))+   # rotate the label on x-axis
  scale_color_manual(values = colpal)+
  labs(x="Impactclass", y="depth")+
  facet_wrap(~MapLoc)


