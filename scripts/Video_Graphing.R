# separate script to look at the video data recorded at 1s intervalls and check out plotting options
# fill-down in ops: https://tidyr.tidyverse.org/reference/fill.html

library(tidyverse)
Video1s <- read_csv("data/IN2018_V06_VIDEOANNO20190717.csv", col_types = "dddTcdcccdcccc")
glimpse(Video1s)

# just looking at ops 113

Op113Vid <- Video1s %>% 
  filter(OPS == 113) %>% 
  fill(HABITAT, SUBSTRATE, .direction = c("down")) 

PCcoverbyImage %>% 
  filter(MapLoc == "z16") %>% 
  group_by(SVY_OPS) %>% 
  summarise(nrows=n())
  



Op113Vid %>%  
  ggplot(mapping=aes(x = REC_TIMESTAMP,
                     y = DPTH_M,
                     colour = SUBSTRATE) )+
           geom_point()+
            scale_y_reverse()
Op113
  
z16 <- Video1s %>% 
  filter(OPS == 113|
           OPS == 114| 
           OPS == 21|
           OPS == 41|
           OPS == 62| 
           OPS == 63|
           OPS == 125) %>% 
  fill(HABITAT, SUBSTRATE, .direction = c("down")) %>% 
  ggplot(mapping=aes(x = REC_TIMESTAMP,
                     y = DPTH_M,
                     colour = SUBSTRATE) )+
  geom_point()+
  scale_y_reverse()+ 
  facet_wrap(~OPS)
z16
