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
##   Peer-Reviewed by: Senay Yasar Saglam, Sector Performance, MBIE
##   Review Period: 2016-05-16 to 2016-05-23
##
##    Notes:      1. Changed to import LEED tables from TRED [FS: 2016-04-04]
##                2. Need to fix the filter on the TimePeriod dates.  Does not filter based on string [FS]
##                3. The data is quarterly.
##

   ##
   ##    Import the raw table from Statistics New Zealand
   ##
       leed37 <- ImportTS2(TRED, Dataset = 'Table 37: LEED measures, by territorial authority', stringsAsFactors=FALSE,
                                 where   = "Unit = 'Total earnings'") %>%
                  dplyr::mutate(Year = year(TimePeriod)) %>%
                  dplyr::mutate(YEMar = ifelse(month(TimePeriod) == 3, Year, Year + 1)) %>%
                  dplyr::rename(LEED_TA = CV1) %>%             
                  dplyr::filter(YEMar >= startYear)
    
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
                #dplyr::group_by(Year, LEED_TA) %>% ### Modified
                group_by(YEMar, LEED_TA) %>% ### Modified
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
              
       ## Removed the check whether TotalEarnings stayed same once we bring in the TA_to_multiple_regions. 
       ## If the sum of proportions for a given TA is equal to 1, then we do not need to make this check. 
        
       leed37 <- merge(leed37, TA_to_multiple_regions, by = c("SNZ_TA"), all.x= TRUE, all.y=TRUE)
       leed37$TotalEarnings <- with(leed37, TotalEarnings*Proportion)
      
   ##
   ##    Summarise data
   ##
        leed37 <- leed37 %>%
                  #dplyr::group_by(TA_Region_modified, Region, SNZ_TA, Year) %>%
                  dplyr::group_by(TA_Region_modified, Region, SNZ_TA, YEMar) %>%
                  dplyr::summarise(TotalEarnings = sum(TotalEarnings)) %>%
                  data.frame() 
                      
   ##
   ##    Get ready to use as population in raking a survey object
   ##
      leed37_pop <- leed37 %>%
                    #dplyr::mutate(Year = year(TimePeriod)) %>%
                    dplyr::select(YEMar, TA_Region_modified, TotalEarnings) %>%
                    dplyr::rename(Freq = TotalEarnings)    %>%
                    dplyr::filter(YEMar %in% InYears) %>%
                    #dplyr::filter(Year %in% InYears) %>%
                    rename(Year = YEMar)
       
   # ============check whether we are missing any years ========================#
       
       Leed37Years <-  sort( unique(leed37_pop$Year) )
       DiffYears <- setdiff(InYears, Leed37Years) # The returned value for setdiff changes by the order of the arguments
       # DiffYears will specify the years that are required for the analysis but are not 
       # missing from the Leed37 data.
       
       if(length(DiffYears) > 0){
         cat("The analysis cannot be performed. The data for the specified time period is not available in the Leed37 table. The missing years are: ")
         cat(DiffYears,sep="\n")
         stop("The analysis is stopped")
       } else {
        rm(DiffYears, Leed37Years)
       }
