##
##    Programme:  import_gdp_deflator_totals.R
##
##    Objective:  Import the published GDP deflators from Statistics New Zealand from the nominal
##                'SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 industry groups (Annual-Mar)' and
##                chain 'SNE - Series, GDP(P), Chain volume, Actual, ANZSIC06 industry groups (Annual-Mar)'
##                tables.
##       
##                The approach here is to calculate the difference between the Nominal measures and Chain
##                volumes to adjust subnational GDP figures according for individual industries.  These
##                adjustments use the change from year 2012 to year 2013 as the reference point (i.e. latest available year for industry-level GDP).
##
##    Authors:    Franz Smith, Sector Performance, Ministry of Business, Innovation & Employment
##                  
##    Date:       2015-05-21
##
##    Reviewer:   Senay Yasar Saglam
##    Review Date: 2016-05-25
##
##    Note:       Should parameterise the year for future releases to track updates in the GDP series (MTAGDP II).
##

##
##  1. Import GDP deflators for creating inflation-adjusted estimates
##
 
   nominal <- ImportTS2(TRED,  "SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 industry groups (Annual-Mar)",
                                stringsAsFactors = FALSE) %>%
                     filter(CV1 == "Gross Domestic Product - production measure") %>%
                     mutate(Year = year(TimePeriod)) %>%
                     rename(deflatorIndustry = CV2, nGDP = Value) %>%
                     select(Year, deflatorIndustry, nGDP) %>%
                     # filter(!deflatorIndustry %in% c("Total All Industries")) %>%
                     #filter(Year > 1999)
                      filter(Year >= startYear & Year <= endYear) # Starting year of the time period being analysed
                     
   chain    <- ImportTS2(TRED,  "Series, GDP(P), Chain volume, Actual, ANZSIC06 industry groups (Annual-Mar)",
                                 stringsAsFactors = FALSE) %>%
                      filter(CV1 == "Gross Domestic Product - production measure") %>%
                      mutate(Year = year(TimePeriod)) %>%
                      rename(deflatorIndustry = CV2, cGDP = Value) %>%
                      select(Year, deflatorIndustry, cGDP) %>%
                      filter(!deflatorIndustry %in% c("Unallocated")) %>%
                      #filter(Year > 1999)    ## save this just in case we want to explore relative to current values
                      filter(Year >= startYear & Year <= endYear) # Retriving the data for the time period specified for the analysis
##
## 2. Create industry-level deflators aby comparing the published nominal and chain volume GDP by industry (national)
##                
   
   deflators <- left_join(chain, nominal, by = c("Year", "deflatorIndustry")) %>%
                          mutate(ind.deflator = (cGDP / nGDP) / (cGDP[Year == deflationYear] / nGDP[Year == deflationYear])) %>%
     
                          # we need somehow to deflate the GST; we use the Total All Industries to do this
                          mutate(deflatorIndustry = gsub(
                                                        "Total All Industries",
                                                        "GST on Production, Import Duties and Other Taxes",
                                 deflatorIndustry, fixed = TRUE))
   
   
   
 
#   deflators <- left_join(subset(chain, Year < 2014), subset(nominal, Year < 2014), by = c("Year", "deflatorIndustry")) %>%
#                      mutate(ind.deflator = (cGDP / nGDP) / (cGDP[Year == 2013] / nGDP[Year == 2013])) %>%
#                      
#                      # we need somehow to deflate the GST; we use the Total All Industries to do this
#                      mutate(deflatorIndustry = gsub(
#                        "Total All Industries",
#                        "GST on Production, Import Duties and Other Taxes",
#                        deflatorIndustry, fixed = TRUE))

   DeflationYears <-  sort( unique(deflators$Year) )
   UserYears <- startYear:endYear
   DiffYears <- setdiff(UserYears, DeflationYears) # The returned value for setdiff changes by the order of the arguments
   # DiffYears will specify the years that are required for the analysis but are not 
   # missing from the population data.
   
   if(length(DiffYears) > 0){
     
     cat("The analysis cannot be performed. The data for the specified time period is not available in the nominal/chain GDP table. The missing years are: ")
     cat(DiffYears,sep="\n")
     stop("The analysis is stopped")
     
   } else {
     rm(UserYears, DeflationYears,DiffYears)
   }