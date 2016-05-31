##
##    Programme:  import_concordances.R
##
##    Objective:  Statistics New Zealand publish the LEED data, RGDP and NGDP on different classifications.
##                This programme reads in concordances which have been derived outside the processing system
##                to enable comparisons between data sources.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance, Ministry of Business, Innovation &  
##                  Employment
##

# This file aligns the Territorial Authorities to multiple Regional Councils, where there is not a 1:1 correspondence
   TA_to_multiple_regions <- read.csv("data_raw/concordances/TA_to_multiple_regions.csv",
                              stringsAsFactors = FALSE)
 
# This set of concordances align the Territorial Authorities in LEED to the Statistics New Zealand TAs                             
   leedTA_to_SNZTA        <- read.csv("data_raw/concordances/leedTA_to_SNZTA.csv",                 
                              stringsAsFactors = FALSE)

# This is the main table of the industry classes across the different LEED and GDP tables                              
   industries             <- read.csv("data_raw/concordances/industries.csv",                      
                              stringsAsFactors = FALSE, na.strings="..")

# This table of concordances aligns to region x industry specific classes - focused on LEED table 18 
# In the csv file, the variable "RegionGDP" is renamed to be "RGDP_Region
   
   regions_concs          <- read.csv("data_raw/concordances/region_to_leed18_andRGDP_region.csv", 
                              stringsAsFactors = FALSE)
