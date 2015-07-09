##
##    Name:       assess_deflator.R
##
##    Objective:  Provide an assessment of the influence of the deflator values on the
##                TAMGDP estimates for individual TAs and by industry.  This script
##                creates a .png to visualise the impact of the deflators through time.
##
##    Authors:    Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-06-12
##

# call to the sorting functions
  source("R/organise_geographies.R")
  source("R/organise_industries.R")

# call to the core data object
  load("data/TAGDP_public.rda")

# organise the geographies
  TAGDP_public$Region        <- organise_rcs(TAGDP_public)
  TAGDP_public$TA            <- organise_tas(TAGDP_public)
  TAGDP_public$NGDP_industry <- organise_ngdps(TAGDP_public)

# filter out the GST and OWPOs to calculate the difference
  TAGDP_public.d <- TAGDP_public %>%
                     filter(!NGDP_industry %in% c("GST on Production, Import Duties and Other Taxes",
                                                 "Owner-Occupied Property Operation (National Accounts Only)")) %>%
                     filter(!Year == 2012) %>%     ## this should be 1 ##
                     group_by(Year, NGDP_industry, TA) %>%
                     mutate(diff = GDP - GDP_real) %>%
                     data.frame()

## plotting TAs by industries
   png("testing_outputs/assess_deflator_TAbyIndustry.png", 8000, 5000, res=600)
     p3 <- ggplot(TAGDP_public.d, aes(Year, diff))
     p3 <- p3 + geom_line(aes(colour=factor(TA)), alpha=0.7)
     p3 <- p3 + facet_wrap(~NGDP_industry, scales="free")
     print(p3)

   dev.off()

## plotting industries by TAs
   png("testing_outputs/assess_deflator_IndustrybyTAs.png", 9000, 5000, res=600)
     p3 <- ggplot(TAGDP_public.d, aes(Year, diff))
     p3 <- p3 + geom_line(aes(colour=factor(NGDP_industry)), alpha=0.7)
     p3 <- p3 + facet_wrap(~TA, scales="free")
     print(p3)

   dev.off()

