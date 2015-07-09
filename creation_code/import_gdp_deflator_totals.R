##
##    Programme:  import_gdp_deflator_totals.R
##
##    Objective:  Import the published GDP deflators from Statistics New Zealand from the nominal
##                'SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 industry groups (Annual-Mar)' and
##                chain 'SNE - Series, GDP(P), Chain volume, Actual, ANZSIC06 industry groups (Annual-Mar)'
##                tables.
##       
##                Calculation of the difference between the tables uses year 2012 as the reference point
##                (i.e. latest available year for industry-level GDP).
##
##    Authors:    Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-21
##


# Imports GDP deflators for creating inflation-adjusted estimates


## industry-level deflators; created by comparing the published nominal and chain volume GDP by industry (national)
 
   nominal <- ImportTS2(TRED,  "SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 industry groups (Annual-Mar)",
                                stringsAsFactors = FALSE) %>%
                     filter(CV1 == "Gross Domestic Product - production measure") %>%
                     mutate(Year = as.numeric(substring(TimePeriod, 1, 4))) %>%
                     rename(deflatorIndustry = CV2) %>%
                     select(Year, deflatorIndustry, Value) %>%
                     # filter(!deflatorIndustry %in% c("Total All Industries")) %>%
                     filter(Year > 1999)
                     
   chain    <- ImportTS2(TRED,  "SNE - Series, GDP(P), Chain volume, Actual, ANZSIC06 industry groups (Annual-Mar)",
                                 stringsAsFactors = FALSE) %>%
                      filter(CV1 == "Gross Domestic Product - production measure") %>%
                      mutate(Year = as.numeric(substring(TimePeriod, 1, 4))) %>%
                      rename(deflatorIndustry = CV2) %>%
                      select(Year, deflatorIndustry, Value) %>%
                      filter(!deflatorIndustry %in% c("Unallocated")) %>%
                      filter(Year > 1999)
                      # filter(Year < 2013) %>%    ## save this just in case we want to explore relative to current values
                      
 
  deflators <- left_join(subset(chain, Year < 2013), subset(nominal, Year < 2013), by = c("Year", "deflatorIndustry")) %>%
                     mutate(ind.deflator = (Value.x / Value.y) / (Value.x[Year == 2012] / Value.y[Year == 2012])) %>%
                     
                     # we need somehow to deflate the GST; we use the Total All Industries to do this
                     mutate(deflatorIndustry = gsub(
                       "Total All Industries",
                       "GST on Production, Import Duties and Other Taxes",
                       deflatorIndustry, fixed = TRUE))

  