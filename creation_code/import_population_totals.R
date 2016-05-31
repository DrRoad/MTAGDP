##
##   Name:       import_population_totals.R
##
##   Objective:  Script performs extraction of population data from TRED (if working within an MBIE)
##               environment and filters to match the years for MTAGDP.
##
##                For the public version of this project, a load for extracted data objects is provided
##
##   Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##   Date:       2015-05-18
##

##   Notes:      Extended the dates to include figures for the mtagdp_forecasts  (FS: 2015-12-23)
##

   # Check on available population data in TRED
   #   tmp <- sqlQuery(TRED, "select Dataset_ID, Dataset from timeseries.dataset")
   #   tmp <- tmp[grep("population", tmp$Dataset, ignore.case = TRUE), ]

   ta_pops <- ImportTS2(TRED, "Estimated Resident Population for Territorial Authority Areas, at 30 June(1996+) (Annual-Jun)",
                        stringsAsFactors = FALSE) %>%
              mutate(Year = year(TimePeriod)) %>%
              rename(TA = CV1, 
                     Population = Value) %>%
              select(Year, TA, Population) %>%
#               filter(!TA %in% c("Chatham Islands Territory"),
#                                 Year %in% 2000:2015) %>%
              filter(!TA %in% c("Chatham Islands Territory"),
                     Year %in% startYear:forecastYear) %>%
              mutate(TA = ifelse(TA == "New Zealand", "All New Zealand", TA))
   
   
   PopulationYears<-  sort( unique(ta_pops$Year) )
   UserYears<- startYear:forecastYear
   DiffYears_Population<- setdiff(UserYears, PopulationYears) # The returned value for setdiff changes by the order of the arguments
   # DiffYears will specify the years that are required for the analysis but are not 
   # missing from the population data.
   
   if(length(DiffYears_Population) > 0){
     
     cat("The analysis cannot be performed. The data for the specified time period is not available in the population table. The missing years are: ")
     cat(DiffYears_Population,sep="\n")
     stop("The analysis is stopped")
     
   } else {
     save(ta_pops, file="data/ta_pops.rda")
     rm(UserYears, PopulationYears,DiffYears_Population)
   }
