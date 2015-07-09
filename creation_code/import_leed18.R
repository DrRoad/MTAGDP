##
##    Programme:  import_leed4.R
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.  This programme Imports and concords a csv downloaded from
##                NZ.Stat LEED Table 18:"LEED measures, by industry (based on ANZSIC06) and region"
##                Settings on NZ.Stat are "select all" quarters, select "Total Earnings" as
##                the only measure, and only the bottom hierarchy of the Industry dimension,
##                and the bottom level of the hierarchy of the region dimesnions.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##                This one's all Peter
##
##                Peter Ellis 10 August 2014

   leed18_orig <- read.csv("data_raw/TABLECODE7018_Data_bfc13c90-b579-4dd5-a8f1-e0031010b6d7.csv",
                      stringsAsFactors = FALSE)

   leed18 <- leed18_orig

   ##
   ##    Aggregate to YE March
   ##   
      leed18$Year <- 2000 + as.numeric(substring(leed18$Quarter, 5, 6))

      # fix the problems with the 1990s appearing as 2099 instead of 1999:
      leed18$Year <- with(leed18, ifelse(Year > 2080, Year - 100, Year))

      leed18$YEMar <- with(leed18, ifelse(substring(leed18$Quarter, 1, 3) == "Mar", Year, Year + 1))

      # convert Value to numeric
      leed18$Value <- as.numeric(leed18$Value)

      leed18 <- leed18 %>%
        group_by(Industry, Region, YEMar) %>%
        summarise(TotalEarnings = sum(Value)) %>%
        rename(LEED18Industry = Industry,
               LEED18Region = Region)

   ##
   ##    Getting ready for using it for poststratification weighting
   ##       fiddle with the leed18 object to make it the right shape for acting as population
   ##       in a post stratification.
   ## 
      leed18_pop <- subset(leed18, LEED18Industry != "Not elsewhere included")
      leed18_pop$Freq <- leed18_pop$TotalEarnings
      leed18_pop$Year <- leed18_pop$YEMar
      leed18_pop$YEMar <- NULL
      leed18_pop$TotalEarnings <- NULL

      leed18_pop <- subset(leed18_pop, Year %in% InYears)

