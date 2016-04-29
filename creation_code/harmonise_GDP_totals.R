##
##  Programme:  harmonise_GDP_totals.R
##
##  Objective:  Similar to the LEED tables, differences in industry levels between the different
##               sources of GDP data need to be adjusted in order to be consistent between them.
##
##  Approach:   Summarise the Year totals for Regional and National GDP and adjust them to add up.
##               Adjusted "population" objects are retained for future computations & testing of MTAGDP
##               results.
##
##  Author:     Peter Ellis, Sector Performance, MBIE
##
##  Date:       2015-05-21
##

##  Notes:      The GDP totals mistmatch only by a few millions, so necessary to do this in the
##              last stage of raking

#------------ summarise the year totals ----------------#

  YearTotalsRGDP <- rgdp_pop_pub %>%
                    group_by(Year) %>%
                    summarise(RGDP = sum(Freq, na.rm=TRUE)) %>%
                    data.frame()

  YearTotalsRGDP_det <- rgdp_pop_pub_det %>%
                    group_by(Year) %>%
                    summarise(RGDP_det = sum(Freq, na.rm=TRUE)) %>%
                    data.frame()

  YearTotalsNGDP <- ngdp_pop %>%
                    group_by(Year) %>%
                    summarise(NGDP = sum(Freq, na.rm=TRUE)) %>%
                    data.frame()

#-------------adjust NGDP to add up to RGDP-------------
  combined <- merge(YearTotalsRGDP, YearTotalsNGDP, by="Year") %>%
              mutate(Adjust = RGDP / NGDP)

  ngdp_pop <- merge(ngdp_pop, combined[ , c("Year", "Adjust")]) %>%
              mutate(Freq = Freq * Adjust) %>%
              select(-Adjust)

#-------------adjuste RGDP_det (with the region/industry combined variable) to add up to RGDP-------------
  combined <- merge(YearTotalsRGDP, YearTotalsRGDP_det, by="Year") %>%
              mutate(Adjust = RGDP / RGDP_det)

  rgdp_pop_pub_det <- merge(rgdp_pop_pub_det, combined[ , c("Year", "Adjust")]) %>%
              mutate(Freq = Freq * Adjust) %>%
              select(-Adjust)

