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
##                adjustments use year 2012 as the reference point (i.e. latest available year for industry-level GDP).
##
##    Authors:    Franz Smith, Sector Performance, Ministry of Business, Innovation & Employment
##                  
##    Date:       2015-05-21
##

##
##    Note:       Should parameterise the year for future releases to track updates in the GDP series (MTAGDP II).
##

##
##  1. Import GDP deflators for creating inflation-adjusted estimates
##
 
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
##
## 2. Create industry-level deflators aby comparing the published nominal and chain volume GDP by industry (national)
##                     
 
  deflators <- left_join(subset(chain, Year < 2013), subset(nominal, Year < 2013), by = c("Year", "deflatorIndustry")) %>%
                     mutate(ind.deflator = (Value.x / Value.y) / (Value.x[Year == 2012] / Value.y[Year == 2012])) %>%
                     
                     # we need somehow to deflate the GST; we use the Total All Industries to do this
                     mutate(deflatorIndustry = gsub(
                       "Total All Industries",
                       "GST on Production, Import Duties and Other Taxes",
                       deflatorIndustry, fixed = TRUE))

  