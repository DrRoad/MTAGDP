##
##    Name:       import_RGDP.r
##
##    Objective:  Script extracts Statistics New Zealand's public release of Regional GDP 
##                 from TRED and reads the custom finer-detailed industry Regional GDP provided
##                 by SNZ for this project.
##
##                For the public version of this project, a load for extracted data objects is provided
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2014-08-10
##

# We need three versions of the RGDP data, with different industry classifications.

##
##-----1. Download the public version from TRED, with 2014 industries (the most consistent classification)-------------
##

  # Import data
    rgdp_pop_pub <- ImportTS2(TRED, "Gross domestic product, by region and industry (Annual-Mar)", 
                         stringsAsFactors = FALSE) %>%
                    rename(RGDP_Region     = CV2,
                           RGDP_industry = CV3,
                           Freq          = Value) %>%
						        #mutate(Year = as.numeric(substring(TimePeriod , 1, 4))) %>%
                    mutate(Year = year(TimePeriod)) %>%
                    select(Year, RGDP_Region, RGDP_industry, Freq) 

    rgdp_pop_pub <- rgdp_pop_pub %>%
                    dplyr::filter(Year %in% InYears)

## ================== Checking whether we have data for the time period user wants

    RGDPYears <- unique(rgdp_pop_pub$Year)
    DiffYears <- setdiff(InYears, RGDPYears)
    if(length(DiffYears) > 0){
      cat("The analysis cannot be performed. The data for the specified time period is not available in the RGDP Public data. The missing years are: ")
      cat(DiffYears,sep="\n")
      stop("The analysis is stopped")
    } else{
      rm(RGDPYears, DiffYears)
    }

##  2. Remove the totals
##

# We use the 2014 industry classification because it is consistently there (although we need to make up prof etc services)
# eg remove Primary and Other Manufacturing and just use 'manufacturing', which is present for all Regions.  
# The more fine grained distinction will come in at the stage of raking to rgdp_pop_custom, which has more fine grained

     rgdp_pop_pub <- rgdp_pop_pub %>% 
                group_by(Year, RGDP_Region) %>%
                mutate(Freq = ifelse(RGDP_industry == "Professional, Scientific, Technical, Administrative and Support Services",
                                Freq[RGDP_industry == "Professional, Scientific and Support Services"] +
                                Freq[RGDP_industry == "Administrative and Support Services"],
                                Freq)
    ) %>%
      filter(!RGDP_industry %in% c("Gross Domestic Product", 
                                   "Total All Industries",
                                   "Forestry, Fishing, and Mining",
                                   "Electricity, Gas, Water and Waste Services",
                                   "Primary Manufacturing",
                                   "Other Manufacturing",
                                   "Professional, Scientific and Support Services",
                                   "Administrative and Support Services",
                                   "Electricity, Gas, Water, and Waste services"),
             !RGDP_Region %in% c("New Zealand", "Total North Island", "Total South Island"),
             !is.na(Freq))

# save a copy for the forecasting
  # save(rgdp_pop_pub, file="data_intermediate/rgdp_pop_pub.rda")         

#----------------Public version - with the best industry classification available--------------

# Because the RGDP in 2015 was published with a ragged classification, we need to make a horrible
# composite of region and industry combined classification.  eg most regions have manufacturing broken
# into primary and other, but Northland and Southland do not.  So, we need to have 'Gisborne manufacturing' 
# and 'Southland manufacturing' as classification levels, and 'Auckland primary manufacturing' etc.

# note that the concordance of manufacturing to other / primary is particularly tricky and I couldn't find
# a definition anywhere so had to deduce it by comparing the custom data cut to the published figures.
# CC1, CC3, CC5, CC6 are primary; CC2, CC4, CC7, CC8, CC9 are other (otherwise can't reconcile the two).
# All this then gets sourced from the data_raw/concordances/industries.csv file.

    rgdp_pop_pub_det <- ImportTS2(TRED, "Gross domestic product, by region and industry (Annual-Mar)", 
                             stringsAsFactors = FALSE) %>%
           rename(RGDP_Region  = CV2,
                        ind  = CV3,   # short name for this column as we need to use it lots
                        Freq = Value) %>%
           mutate(Year = year(TimePeriod)) %>%
           select(Year, RGDP_Region, ind, Freq) %>%
           filter(!RGDP_Region %in% c("New Zealand", "Total North Island", "Total South Island")) %>%
  
           # No 'Manufacturing' please unless Northland or Southland:
           filter(!(ind == "Manufacturing" & ! (RGDP_Region %in% c("Northland", "Southland")))) %>%
  
           # No 'FFMEGWWS' unless West Coast or Marlborough:
           filter(!(ind == "Forestry, Fishing, Mining, Electricity, Gas, Water and Waste Services" &
                      !(RGDP_Region %in% c("West Coast", "Marlborough")))) %>%
           mutate(RegionIndustryRGDP15 = ifelse(ind %in% 
                                                  c("Agriculture", 
                                                    "Construction",
                                                    "Wholesale Trade", 
                                                    "Retail Trade",
                                                    "Accommodation and Food Services",
                                                    "Transport, Postal and Warehousing",
                                                    "Information Media, Telecommunications and Other Services",
                                                    "Financial and Insurance Services",
                                                    "Rental, Hiring and Real Estate Services",
                                                    "Owner-Occupied Property Operation",
                                                    "Professional, Scientific and Support Services",
                                                    "Administrative and Support Services",
                                                    "Public Administration and Safety",
                                                    "Education and Training",
                                                    "Health Care and Social Assistance",
                                                    "GST on Production, Import Duties and Other Taxes"),
                                                ind, 
                                                paste(RGDP_Region, ind))) %>% 
           group_by(Year, RGDP_Region) %>%
           filter(!(ind %in% c("Gross Domestic Product", "Total All Industries"))) %>%
           filter(!is.na(Freq)) %>%
           select(-ind)
           
 
        rgdp_pop_pub_det <- rgdp_pop_pub_det %>%
                            dplyr::filter(Year %in% InYears)
  ##

      ## Checking the time period user specified
      RGDP_det_Years <- unique(rgdp_pop_pub_det$Year)
      DiffYears <- setdiff(InYears, RGDP_det_Years)

      if(length(DiffYears) > 0){
          cat("The analysis cannot be performed. The data for the specified time period is not available in the RGDP table. The missing years are: ")
          cat(DiffYears,sep="\n")
          stop("The analysis is stopped")
        } else{
          rm(DiffYears, RGDP_det_Years)
        }


        if(sum(rgdp_pop_pub_det$Freq) - sum(rgdp_pop_pub$Freq) > 0.0001 * sum(rgdp_pop_pub_det$Freq)){
           stop("The RGDP public population figures based on industry classifications before and after 2015 are too different .")
        }

##
## 3.----------------Import the custom data----------------------------------
##

    rgdp_file        <- "data_raw/MBIE-Regional-GDP-industry-breakdowns-2000_2006-and-2007_2013.xlsx"
    rgdp_custom_orig <- read.xlsx(rgdp_file, 
                           sheet = c("2007_13"),
                           startRow    = 1,
                           check.names = FALSE) %>%
                  rbind(read.xlsx(rgdp_file,
                           sheet       = c("2000_06"),
                           startRow    = 1,
                           check.names = FALSE)) %>%                      
                  gather(Region, GDP, -Year, -Industry.code) %>%
                # clean up:
                  mutate(GDP       = as.numeric(GDP),
                         Region    = gsub(".region", "", Region),
                         Region    = gsub("\\.", " ", Region)) %>%
                # give names consistent with concordances
                  rename(RGDPRef_custom = Industry.code,
                         RGDP_Region    = Region,
                         Freq           = GDP) %>%
                # knock out the totals
                  dplyr::filter(!RGDPRef_custom  %in% c("B01")) %>%  
                  dplyr::group_by(Year, RGDPRef_custom, RGDP_Region) %>%
                  dplyr::summarise(Freq = sum(Freq))

  rgdp_custom_orig <- rgdp_custom_orig %>%
                      filter(Year %in% InYears)

  RGDP_custom_orig_Years <- unique(rgdp_custom_orig$Year)
  DiffYears_RGDP_custom_orig<- setdiff(InYears, RGDP_custom_orig_Years)

  if(length(DiffYears_RGDP_custom_orig) > 0){
    cat("The analysis cannot be performed. The data for the specified time period is not available in the RGDP custom data. The missing years are: ")
    cat(DiffYears_RGDP_custom_orig,sep="\n")
    stop("The analysis is stopped")
  } else {
    rm(DiffYears_RGDP_custom_orig, RGDP_custom_orig_Years)
  }

  


if(sum(rgdp_pop_pub_det$Freq) - sum(rgdp_pop_pub$Freq) > 0.0001 * sum(rgdp_pop_pub_det$Freq)){
  stop("The RGDP public population figures based on industry classifications before and after 2015 are too different .")
}

 ## ---------------------------------------------------------------------- ##
 ##                  from previous custom cut of rGDP                      ## 
 ## ---------------------------------------------------------------------- ##

# rgdp_custom_orig <- readWorksheetFromFile("data_raw\\MBIE_CONFIDENTIAL_RegionalGDP_matrix30industries_15regions_2000_2012.xlsx", 
                      # sheet = c('Backdated RGDP for MBIE in thou'),
                      # startRow = 1,
                      # check.names=FALSE) %>%
  # gather(Region, GDP, -Year, -Industry) %>%
  # # clean up:
  # mutate(GDP      = gsub(",", "", GDP),
         # GDP      = as.numeric(GDP) / 1000,
         # Region   = gsub("  region", "", Region),
         # Region   = gsub(" region", "", Region),
         # Industry = gsub("Import duties", "IMP", Industry),
         # Industry = gsub("Import", "IMP", Industry),
         # Industry = gsub("Import duties", "IMP", Industry),
         # Industry = gsub("IMP", "GST", Industry)) %>%
  # # give names consistent with concordances
  # rename(RGDPRef_custom = Industry,
         # RegionGDP      = Region,
         # Freq           = GDP) %>%
  # # knock out the totals
  # filter(!RGDPRef_custom  %in% c("TOT", "Total GDP")) %>%
  # group_by(Year, RGDPRef_custom, RegionGDP) %>%
  # summarise(Freq = sum(Freq))                      
                      
