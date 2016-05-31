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
##   Peer-Reviewed by: Senay Yasar Saglam, Sector Performance, MBIE
##   Review Period: 2016-05-16 to 2016-05-23

##    Notes:      1. Changed to direct call to TRED [FS: 2016-04-04]
##                2. Looks like there are aggregated regions in the LEED18Regions, so need to check
##                   that this was not done to 'confidentialise' information.  Total number of Regions
##                   is the same to previous LEED18, so assume for now that this is not excluding data.
##                3. The data in TRED is quarterly.
##


  # Import leed18 Total earnings
      leed18 <- ImportTS2(TRED, Dataset = 'Table 18: LEED measures, by industry (based on ANZSIC06) and region',
                    stringsAsFactors=FALSE,
                    where = "Unit = 'Total earnings'") %>%
                dplyr::mutate(Year = year(TimePeriod)) %>%
                dplyr::mutate(YEMar = ifelse(month(TimePeriod) == 3, Year, Year + 1)) %>%                      
                rename(LEED18Industry = CV1, LEED18Region = CV2) %>%
                group_by(LEED18Industry, LEED18Region, YEMar) %>%
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
                    mutate(Freq = TotalEarnings) %>%
                    select(YEMar, LEED18Region, LEED18Industry, Freq)  %>%
                    filter(YEMar %in% InYears) %>%
                    rename(Year = YEMar)
      
      # ============check whether we are missing any years ========================#
      
      Leed18Years <-  sort( unique(leed18_pop$Year) )
      DiffYears <- setdiff(InYears, Leed18Years) # The returned value for setdiff changes by the order of the arguments
      # DiffYears will specify the years that are required for the analysis but are not 
      # missing from the Leed18 data.
      
      if(length(DiffYears) > 0){
        cat("The analysis cannot be performed. The data for the specified time period is not available in the Leed18 table. The missing years are: ")
        cat(DiffYears,sep="\n")
        stop("The analysis is stopped")
      } else {
        rm(DiffYears, Leed18Years)
      }