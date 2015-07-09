
load("data/TAGDP_public.rda")
library(dplyr)
library(tidyr)
library(cluster)



shares <- TAGDP_public %>%
  group_by(Year, TA, RGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  ungroup() %>%
  group_by(Year, TA) %>%
  mutate(share = GDP / sum(GDP)) %>%
  select(-GDP) 



shares_av <- shares %>%
  ungroup() %>%
  group_by(TA, RGDP_industry) %>%
  summarise(share = mean(share)) %>%
  spread(RGDP_industry, share)

TA_short <- gsub(" District", "", shares_av$TA)
TA_short <- gsub(" City", "", TA_short)
row.names(shares_av) <- TA_short

mod <- princomp(shares_av [ , -1])
biplot(mod, xlabs = TA_short)



shares_daisy <- daisy(shares_av[ ,-1])

shares_diana <- diana(shares_av)
plot(shares_diana, which.plots = 2, main ="", sub = "")

