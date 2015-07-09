library(networkD3)

head(TAGDP_public)

?networkD3
names(TAGDP_public)


Nodes <- with(TAGDP_public, 
              data_frame(name = unique(c(Region, TA, RGDP_industry, NGDP_industry)))
)

Nodes$ID <- 0:(nrow(Nodes) -1)

tmp1 <- TAGDP_public %>%
  filter(Year == max(Year)) %>%
  group_by(Region, TA) %>%
  summarise(GDP = sum(GDP)) %>%
  rename(from = Region,
         to = TA)

tmp2 <- TAGDP_public %>%
  filter(Year == max(Year)) %>%
  group_by(TA, RGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  rename(from = TA,
         to = RGDP_industry)

Links <- rbind(tmp1, tmp2) %>%
  

the_data <- TAGDP_public