##
##    Name:       modify_tagdp.R
##
##    Objective:  The result of the Iterational Proportional Fitting ('raking') from grunt needs
##                some modification for calculating additional measures of per capita GDP and 
##                adjustments according to industry-specific inflation.  Some additional columns
##                are added to assist in the graphical outputs.
##
##                This script is designed to pick up from the end of grunt.R, where users can
##                work with the output independent of the core calculation of MTAGDP.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2014-08-10
##


##     
##  incorporating deflator values in TAGDP object     
## 

   # source the deflator concordances & deflator values
     concord_defl <- read.csv("data_raw/concordances/deflatorIndustries.csv", header=TRUE)
     
     TAGDP_defl <- left_join(TAGDP_grunt, concord_defl, by = "LEED4Industry") %>%
                   left_join(deflators[, c("Year", "deflatorIndustry", "ind.deflator")],
                            by=c("Year", "deflatorIndustry")) %>%
                   mutate(GDP_real = GDP * ind.deflator)   %>%
                   data.frame()

   if(nrow(TAGDP_defl) != nrow(TAGDP_grunt) | sum(is.na(TAGDP_defl$GDP_real)) > 0){
     print(TAGDP_defl %>% filter(is.na(GDP_real)) %>% select(LEED4Industry, deflatorIndustry) %>% unique())
     stop("something went wrong with the merge with the deflators")

  }

##
##  create object for analytical use & output generation
##

   # first collapse the industry classes and modified TAs
     TAGDP_public <- TAGDP_defl %>%
                      group_by(Year, NGDP_industry, RGDP_industry, TA, Region) %>%
                        summarise(
                          Employees                    = sum(Employees),
                          Earnings_original            = sum(Earnings),
                          Earnings_commuting_corrected = sum(Earnings_commuting_corrected),
                          GDP                          = sum(GDP),
                          GDP_real                     = sum(GDP_real)
                          ) %>%
                       ungroup()

   # calculate GDP and real GDP per capita for final TAGDP object
     
       TAGDP_public <- left_join(TAGDP_public, ta_pops, by=c("Year", "TA"))

       TAGDP_public <- TAGDP_public %>%
                        mutate(GDP_perCapita      = GDP * 1000000 / Population,
                               GDP_real_perCapita = GDP_real * 1000000 / Population) %>%
                        select(-Population) %>%
                        data.frame()       


   if(sum(TAGDP_public$GDP) != sum(TAGDP_grunt$GDP)){
     stop("TAGDP_public has a different sum(GDP) to TAGDP_grunt.  Not right.")
  }
   
   ## save final TAGDP object for exploratory data analyses
   save(TAGDP_public, file = "data/TAGDP_public.rda")                  
   
   ## csv dump for web consumption                 
   write.csv(TAGDP_public, file="data/TAGDP_public.csv", row.names=FALSE)
   


