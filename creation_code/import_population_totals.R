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


   tmp <- sqlQuery(TRED, "select Dataset_ID, Dataset from timeseries.dataset")
   tmp <- tmp[grep("population", tmp$Dataset, ignore.case = TRUE), ]

   ta_pops <- ImportTS2(TRED, "Estimated Resident Population for Territorial Authority Areas, at 30 June(1996+) (Annual-Jun)",
                        stringsAsFactors = FALSE) %>%
              mutate(Year = as.numeric(substring(TimePeriod, 1, 4))) %>%
              rename(TA = CV1, 
                     Population = Value) %>%
              select(Year, TA, Population) %>%
              filter(!TA %in% c("Chatham Islands Territory", "New Zealand"),
                                Year %in% 2000:2012)

  




