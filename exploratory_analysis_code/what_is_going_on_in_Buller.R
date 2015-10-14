##
##    Name:       what_is_going_on_in_Buller.R
##
##    Objective:  Exploration into the GDP figure for the Buller region to verify the relative
##                increase in LEED earnings indexed to 2000.  Includes a comparative plot of raw 
##                data (scaled to $1 m), excluding "Owner-Occupied Property Operation" and "GST 
##                on Production, Import Duties and Other Taxes".
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-28
##

## Call to key packages
library(dplyr)
library(mbie)
library(Cairo)

## Subset the TAGDP_public object for Buller District
buller <-  TAGDP_public %>%
  filter(TA == "Buller District") %>%
  mutate(ind = wrap(RGDP_industry, 20))
  

## Output the plots to a single .pdf
CairoPDF("exploratory_output/buller_leed.pdf"  , 11, 8)
print(
  buller %>%
    group_by(ind, Year) %>%
    summarise(Earnings_original = sum(Earnings_original),
              Earnings_commuting_corrected = sum(Earnings_commuting_corrected)) %>%
    ungroup() %>%
    ggplot(aes(x=Year)) +
    stat_index1(aes(y = Earnings_original)) +
    labs(x="", y = "LEED Earnings, Indexed (2000 = 100)") +
    ggtitle("Buller original LEED data") +
    facet_wrap(~ind)
)

print(
  buller %>%
    filter(! RGDP_industry %in% c("Owner-Occupied Property Operation", 
                                                     "GST on Production, Import Duties and Other Taxes")) %>%
    group_by(ind, Year) %>%
    summarise(Earnings_original = sum(Earnings_original),
              Earnings_commuting_corrected = sum(Earnings_commuting_corrected)) %>%
    ungroup() %>%
    ggplot(aes(x=Year)) +
    geom_line(aes(y = Earnings_original / 10 ^ 6)) +
    labs(x="") +
    scale_y_continuous("LEED Earnings, ($m) (excluding OOD and GST)", label = dollar) +
    ggtitle("Buller original LEED data") +
    facet_wrap(~ind)
)

dev.off()
