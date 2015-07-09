### creates an overview plot showing TAGDP average annual growth rate & tendencies
## Peter Ellis, MBIE Sector Performance
# 2015-05-28 

# call to core functionality & the data
  library(dplyr)
  library(tidyr)
  library(mbie)
  library(gridExtra)
  library(extrafont)
  library(Cairo)

# the data
  load("data/TAGDP_public.rda")

## this function is just a stub, need a more general function to deal with other geographical nuances
   source("R/organise_geographies.R")

## calculate the totals & generate the plots
totals <- TAGDP_public %>%
  mutate(TA = gsub(" District", "", TA),
         TA = gsub(" City", "", TA)) %>%
  group_by(TA) %>%
  summarise(Growth = CAGR(sum(GDP[Year == 2012]) / sum(GDP[Year == 2000]), period = 12) / 100,
            GDP = sum(GDP[Year == 2012])) %>%
  arrange(Growth) %>%
  mutate(TA = factor(TA, levels = TA))

totals$TA <- organise_tas(totals)
totals$TA <- factor(totals$TA, levels = rev(levels(totals$TA)))  ## order needs to be reversed for this plot

p1 <- totals %>%
  ggplot(aes(x = TA, size = GDP, colour = Growth, y = Growth)) +
  coord_flip() +
  theme_minimal(10, base_family = "Calibri") +
  geom_point() +
  geom_point(colour = "black", shape = 1) +
  geom_smooth(aes(group=1), colour=alpha("grey", 0.05), show_guide=FALSE) +
  theme(legend.position = "bottom") +
  scale_size("Gross Domestic Product in 2012 ($m)", labels = comma, range = c(2, 9)) +
  scale_x_discrete("") +
  scale_y_continuous("\nAverage annual growth 2000 - 2012", label = percent) +
  scale_colour_gradientn(colours = mbie.cols(c("Orange", "Blue")), guide = "none")

lines <- TAGDP_public %>%
  mutate(TA = gsub(" District", "", TA),
         TA = gsub(" City", "", TA)) %>%
  group_by(TA, Year) %>%
  summarise(GDP = sum(GDP)) %>%
  ungroup() %>%
  left_join(totals[, c("TA", "Growth")])%>%
  mutate(TA = factor(TA, levels = levels(totals$TA)[nrow(totals):1])) 


p2 <- lines %>%
  ggplot(aes(x = Year, y = GDP, colour = Growth)) +
  geom_line() +
  theme_nothing() +
  facet_wrap(~TA, scales = "free_y", ncol = 1) +
  scale_colour_gradientn(colours = mbie.cols(c("Orange", "Blue")), guide = "none") +
  theme(
    panel.margin = unit(-0.5, "lines"),
    strip.text = element_blank()
    ) 

## setting the viewports
vp1 <- viewport(0.45, 0.5, width = 0.9, height = 1)
vp2 <- viewport(0.93, 0.574, width = 0.1695, height = 0.878)


png("exploratory_output/dotplot_growth.png", 3500, 5000, res = 600)
  print(p1, vp = vp1)
  print(p2, vp = vp2)
 dev.off()
