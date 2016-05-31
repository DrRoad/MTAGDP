##
##    Programme:  import_leed4.R
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.   These data are used as sample margins for 'raking' to the
##                employee numbers (i.e. weights) from the Business Demography Statistics.
##
##    Approach:   This programme reads data from the LEED table 4 and aggregates the values to year end
##                March.  'Year' is used again to match other data throughout the routine.
##
##    Authors:    Peter Ellis, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##                
##    Date:       2014-08-10
##
##    Peer-Reviewed by: Senay Yasar Saglam, Sector Performance, MBIE
##    Review Period: 2016-05-16 to 2016-05-25
##
##    Notes:      1. Changed to import of table from TRED [FS:  2016-04-04]
##                2. The data in TRED is quarterly.

  ## Important - This is table 4 with all quarters, all of the lowest level in the industry
  ##             hierarchy (but not other levels), and only one measure

    leed4 <- ImportTS2(TRED, Dataset = 'Table 4: LEED measures, by industry (based on ANZSIC06)', stringsAsFactors=FALSE,
                             where = "Unit = 'Total earnings'") %>%
             dplyr::mutate(Year = year(TimePeriod)) %>%
             dplyr::mutate(YEMar = ifelse(month(TimePeriod) == 3, Year, Year + 1)) %>%
             rename(LEED4Industry = CV1) %>%
             group_by(LEED4Industry, YEMar) %>%
             summarise(TotalEarnings = sum(Value))
             

  # as the TRED import has numerous different levels of ANZSIC06 classes, these need to be filtered for the correct level
    allLEED4industries <- unique(industries$LEED4Industry) 


    leed4 <- leed4 %>%
             filter(LEED4Industry %in% allLEED4industries)

  # ========== set up to use as population frame for a survey object =========== #             
    leed4_pop <- leed4 %>%
                 filter(LEED4Industry != "Not elsewhere included") %>%
                 rename(Freq=TotalEarnings) %>%
                 filter(YEMar %in% InYears) %>%
                 rename(Year = YEMar)
    
 # ============check whether we are missing any years ========================#
    
    Leed4Years <-  sort( unique(leed4_pop$Year) )
    DiffYears <- setdiff(InYears, Leed4Years) # The returned value for setdiff changes by the order of the arguments
    # DiffYears will specify the years that are required for the analysis but are not 
    # missing from the Leed4 data.
    
    if(length(DiffYears) > 0){
      cat("The analysis cannot be performed. The data for the specified time period is not available in the Leed4 table. The missing years are: ")
      cat(DiffYears,sep="\n")
      stop("The analysis is stopped")
    } else {
      rm(DiffYears, Leed4Years)
    }