##
##    Programme:  import_leed37.R
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.  This programme Imports and concords a csv downloaded from
##                NZ.Stat LEED table 37.  Select All Regions, just the lowest level of the industry hierarchy
##                all quarter.
##
##    Authors:    Peter Ellis, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2014-08-10
##

   ##
   ##    Import the raw table from Statistics New Zealand
   ##

   leed37 <- read.csv("data_raw/TABLECODE7037_Data_2fbd4042-a117-470a-a484-5c38c1033011.csv",
                      stringsAsFactors = FALSE)
   names(leed37)[names(leed37) == "Territorial.authority"] <- "LEED_TA"

   ##
   ##    Prior to Quarter Dec-10, there are no values for "Auckland" so we need to create them.
   ##
      OldAuckland <- c("Auckland city", "Rodney district", "Manukau city", "Papakura district", 
                       "North Shore city", "Franklin district", "Waitakere city")

      leed37$LEED_TA <- as.character(rename.levels(factor(leed37$LEED_TA),
                                                    orig = OldAuckland,
                                                    new  = rep("Auckland", length(OldAuckland))))
   ##
   ##    Remove double counting
   ##
      leed37 <- subset(leed37, LEED_TA != "Total territorial authority")
      leed37 <- leed37 %>%
          group_by(Quarter, LEED_TA) %>%
          summarise(TotalEarnings = sum(Value))

      if(length(unique(leed37$LEED_TA)) != 66)
      {
        stop("Wrong number of territorial authorities - not 66")
      }
      
   ##
   ##    Merge with the correctly spelled TAs (eg "District" not " district")
   ##
      leed37 <- merge(leed37, 
                      leedTA_to_SNZTA, 
                      by = "LEED_TA", 
                      all.x=TRUE)
   ##
   ##    Do a cross join with the lower level TA and make an obect
   ##
      tmp <- merge(leed37, TA_to_multiple_regions, by = "SNZ_TA", all.x= TRUE, all.y=TRUE)
      tmp$TotalEarnings <- with(tmp, TotalEarnings * Proportion)

      if(sum(tmp$TotalEarnings) != sum(leed37$TotalEarnings))
      {
        stop("Something went wrong apportioning earnings to the sub-TAs (one region per sub-TA)")
      }

      leed37 <- tmp
      rm(tmp)
      
   ##
   ##    Aggregate to YE March
   ##

      leed37$Year <- 2000 + as.numeric(substring(leed37$Quarter, 5, 6))

      # fix the problems with the 1990s appearing as 2099 instead of 1999:
      leed37$Year <- with(leed37, ifelse(Year > 2080, Year - 100, Year))

      leed37$YEMar <- with(leed37, ifelse(substring(leed37$Quarter, 1, 3) == "Mar", Year, Year + 1))

      leed37 <- leed37 %>%
          group_by(TA_Region_modified, Region, SNZ_TA, YEMar) %>%
          summarise(TotalEarnings = sum(TotalEarnings))      
                      
   ##
   ##    Get ready to use as population in raking a survey object
   ##
      leed37_pop <- leed37[ , c("YEMar", "TA_Region_modified", "TotalEarnings")]

      names(leed37_pop)[names(leed37_pop) == "TotalEarnings"] <- "Freq"
      names(leed37_pop)[names(leed37_pop) == "YEMar"] <- "Year"

      leed37_pop <- subset(leed37_pop, Year %in% InYears)
