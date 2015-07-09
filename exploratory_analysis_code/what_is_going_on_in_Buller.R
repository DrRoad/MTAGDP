library(dplyr)
library(mbie)
library(Cairo)

buller <-  TAGDP_public %>%
  filter(TA == "Buller District") %>%
  mutate(ind = wrap(RGDP_industry, 20))
  

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
