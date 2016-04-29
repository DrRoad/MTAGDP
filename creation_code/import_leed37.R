##
##    Programme:  import_leed37.R
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.  These data are used as sample margins for 'raking' to the
##                employee numbers (i.e. weights) from the Business Demography Statistics.
##
##    Approach:  This programme Imports and concords a csv downloaded from
##                NZ.Stat LEED table 37.  Select All Regions, just the lowest level of the industry hierarchy
##                all quarter.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##                  
##    Date:       2014-08-10
##

##
##    Notes:      1. Changed to import LEED tables from TRED [FS: 2016-04-04]
##                2. Need to fix the filter on the TimePeriod dates.  Does not filter based on string [FS]
##

   ##
   ##    Import the raw table from Statistics New Zealand
   ##
       leed37 <- ImportTS2(TRED, Dataset = 'Table 37: LEED measures, by territorial authority', stringsAsFactors=FALSE,
                                 where   = "Unit = 'Total earnings'")
                             
 ## ---------------------------------------------------------------------------------------- ##  
 ##   hard coded the selection for the YEMar                                                 ##
 ##   as.Date(paste0(seq(from = 2000, to = 2014),"-03-31")) embedded in filter not working   ##  [FS 2016-04-04]
 ## ---------------------------------------------------------------------------------------- ## 
      # leed37raw <- leed37  ## just for testing

       leed37 <- leed37 %>%
                 dplyr::mutate(TimePeriod = as.character(TimePeriod)) %>%
                 dplyr::filter(TimePeriod %in% c("2000-03-31","2001-03-31","2002-03-31","2003-03-31","2004-03-31",
                                                 "2005-03-31","2006-03-31","2007-03-31","2008-03-31","2009-03-31",
                                                 "2010-03-31","2011-03-31","2012-03-31","2013-03-31")) %>%
                 dplyr::rename(LEED_TA = CV1)      

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
      leed37 <- leed37 %>%
                filter(LEED_TA != "Total territorial authority") %>%
                dplyr::group_by(TimePeriod, LEED_TA) %>%
                dplyr::summarise(TotalEarnings = sum(Value))

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
   ##    Do a cross join with the lower level TA and make an object
   ##
      tmp               <- merge(leed37, TA_to_multiple_regions, by = c("SNZ_TA"), all.x= TRUE, all.y=TRUE)
      tmp$TotalEarnings <- with(tmp, TotalEarnings * Proportion)

      if(sum(tmp$TotalEarnings) != sum(leed37$TotalEarnings))
      {
        stop("Something went wrong apportioning earnings to the sub-TAs (one region per sub-TA)")
      }

      leed37 <- tmp
      rm(tmp)
      
   ##
   ##    Summarise data
   ##
        leed37 <- leed37 %>%
                  dplyr::group_by(TA_Region_modified, Region, SNZ_TA, TimePeriod) %>%
                  dplyr::summarise(TotalEarnings = sum(TotalEarnings)) %>%
                  data.frame() 
                      
   ##
   ##    Get ready to use as population in raking a survey object
   ##
      leed37_pop <- leed37 %>%
                    dplyr::mutate(Year = year(TimePeriod)) %>%
                    dplyr::select(Year, TA_Region_modified, TotalEarnings) %>%
                    dplyr::rename(Freq = TotalEarnings) %>%
                    dplyr::filter(Year %in% InYears)


## -------------------------------------------------------------------- ##
##                     from previous import_leed37                      ##
## -------------------------------------------------------------------- ##
      
   # leed37 <- read.csv("data_raw/TABLECODE7037_Data_2fbd4042-a117-470a-a484-5c38c1033011.csv",
                      # stringsAsFactors = FALSE)
   # names(leed37)[names(leed37) == "Territorial.authority"] <- "LEED_TA"

   ##
   ##    Aggregate to YE March
   ##

       # leed37$Year <- 2000 + as.numeric(substring(leed37$Quarter, 5, 6))

      # fix the problems with the 1990s appearing as 2099 instead of 1999:
        # leed37$Year <- with(leed37, ifelse(Year > 2080, Year - 100, Year))

        # leed37$YEMar <- with(leed37, ifelse(substring(leed37$Quarter, 1, 3) == "Mar", Year, Year + 1))
      # names(leed37_pop)[names(leed37_pop) == "TotalEarnings"] <- "Freq"
      # names(leed37_pop)[names(leed37_pop) == "YEMar"] <- "Year"

      
