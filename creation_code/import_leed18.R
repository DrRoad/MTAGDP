##
##    Programme:  import_leed18.R
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.  These data are used as sample margins for 'raking' to the
##                employee numbers (i.e. weights) from the Business Demography Statistics.
##
##    Approach:   This programme Imports and concords a csv downloaded from
##                NZ.Stat LEED Table 18:"LEED measures, by industry (based on ANZSIC06) and region"
##                Settings on NZ.Stat are "select all" quarters, select "Total Earnings" as
##                the only measure, and only the bottom hierarchy of the Industry dimension,
##                and the bottom level of the hierarchy of the region dimensions.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##                  
##    Date:       2014-08-10
##

##    Notes:      1. Changed to direct call to TRED [FS: 2016-04-04]
##                2. Looks like there are aggregated regions in the LEED18Regions, so need to check
##                   that this was not done to 'confidentialise' information.  Total number of Regions
##                   is the same to previous LEED18, so assume for now that this is not excluding data.
##


  # Import leed18 Total earnings
    leed18 <- ImportTS2(TRED, Dataset = 'Table 18: LEED measures, by industry (based on ANZSIC06) and region',
                              stringsAsFactors=FALSE,
                              where = "Unit = 'Total earnings'") %>%
              mutate(TimePeriod = as.character(TimePeriod)) %>%                               
              dplyr::filter(TimePeriod %in% c("2000-03-31","2001-03-31","2002-03-31","2003-03-31","2004-03-31",
                                              "2005-03-31","2006-03-31","2007-03-31","2008-03-31","2009-03-31",
                                              "2010-03-31","2011-03-31","2012-03-31","2013-03-31")) %>%    
              rename(LEED18Industry = CV1, LEED18Region = CV2) %>%
              group_by(LEED18Industry, LEED18Region, TimePeriod) %>%
              summarise(TotalEarnings = sum(Value))

   ##
   ##    Getting ready for using it for poststratification weighting
   ##       fiddle with the leed18 object to make it the right shape for acting as population
   ##       in a post stratification.

      leed18_pop <- leed18 %>%
                    dplyr::filter(!LEED18Industry %in% c("Not elsewhere included",
                                                         "All industries",
                                                         "Financial, insurance, rental, hiring, and real estate services",
                                                         "Government, arts and recreation, and other services",
                                                         "Mining, electricity, gas, water, and waste services; and construction",
                                                         "Professional, scientific, technical services, administrative, and support services"),
                             !LEED18Region   %in% c("Total region",
                                                    "Gisborne, Hawke's Bay",         ## looks like they may have aggregated
                                                    "Taranaki, Manawatu-Wanganui")) %>%                         
                    mutate(Year = year(TimePeriod),
                           Freq = TotalEarnings) %>%
                    select(Year, LEED18Region, LEED18Industry, Freq) %>%
                    filter(Year %in% InYears)


 ## ----------------------------------------------------- ##
 ##               previous call to leed18                 ##
 ## ----------------------------------------------------- ##

     # leed18_orig <- read.csv("data_raw/TABLECODE7018_Data_bfc13c90-b579-4dd5-a8f1-e0031010b6d7.csv",
                      # stringsAsFactors = FALSE)
     # leed18 <- leed18_orig

   ##
   ##    Aggregate to YE March
   ##   
      # leed18$Year <- 2000 + as.numeric(substring(leed18$Quarter, 5, 6))

      # fix the problems with the 1990s appearing as 2099 instead of 1999:
      # leed18$Year <- with(leed18, ifelse(Year > 2080, Year - 100, Year))

      # leed18$YEMar <- with(leed18, ifelse(substring(leed18$Quarter, 1, 3) == "Mar", Year, Year + 1))

      # convert Value to numeric
      # leed18$Value <- as.numeric(leed18$Value)

      # leed18_pop               <- subset(leed18, LEED18Industry != "Not elsewhere included")
      # leed18_pop$Freq          <- leed18_pop$TotalEarnings
      # leed18_pop$Year          <- leed18_pop$YEMar
      # leed18_pop$YEMar         <- NULL
      # leed18_pop$TotalEarnings <- NULL

      # leed18_pop <- subset(leed18_pop, Year %in% InYears)
