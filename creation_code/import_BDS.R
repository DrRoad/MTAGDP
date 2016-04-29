##
##    Programme:  import_BDS.r
##
##    Objective:  The Business Demography Statistics (BDS) provide the low level regional estimates 
##                for estimating where economic activity is taking place.  BDS data are used to estimate
##                the relative size of the production occuring at low industry definitions and within
##                the different Territorial Authorities.
##                
##                Employee numbers are used as weights for the survey design when raking to the LEED
##                tables (i.e. total Earnings).
##       
##                This programme reads the data in from NZ.Stats from the BDS.  Concordance between
##                Regional Councils and Territorial Authorities are also performed on the data.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment 
##                  
##    Date:       2015-05-21
##

##  
##    Notes:      1. There appears to be 'confidentialised' cells in the new BDS import from Stats NZ,
##                   so may need to impute missing values (raking) to higher-level totals (e.g. Region) [FS 2016-04-05]
##

   ##
   ##    Data comes from Detailed industry by region-ANZSIC06 2000 to 2012.  All dimensions are selected
   ##       except only the employee count values, not enterprise counts

 ## ----------------------------------------------------------------------- ##
 ##     commented out until the 'confidential' cells can be dealt with      ##
 ## ----------------------------------------------------------------------- ##
    # bds <- ImportTS2(TRED, Dataset = 'Geographic units by region and industry 2000-14', stringsAsFactors=FALSE,
                           # where   = "Unit = 'Employee Count' and TimePeriod > '1999-12-31'")

    # BDS <- bds %>%
           # filter(TimePeriod < "2014-01-31") %>%
           # rename(ANZSIC06 = CV1, Area = CV2)
                   
 ## ---------------------------------------------------------------------- ##
 ##                        using last year's BDS                           ##
 ## ---------------------------------------------------------------------- ##

      BDS.1 <- read.csv("data_raw/TABLECODE7601_Data_c51b48ec-65bb-4ece-880a-fa575544cf03.csv",
                      stringsAsFactors = FALSE)

      BDS.2 <- read.csv("data_raw/TABLECODE7601_Data_b6e1d6e3-0e61-4a18-95c2-46eda83122ef.csv",
                      stringsAsFactors = FALSE)                

      BDS <- rbind(BDS.2, BDS.1)
           
   ##
   ##    LEED data are aggregations of specific ANZISC 3 digit industries.  This section makes
   ##       industry codes which will be used later to map to LEED.
   ##
      AllBDSIndustries <- unique(BDS$ANZSIC06)
      Level3Industries <- AllBDSIndustries[substring(AllBDSIndustries, 5, 5) == " " & 
                                             substring(AllBDSIndustries, 4, 4) != " "]

   ## Some of the industry levels need to be combined
      MustMerge <- list(
            c("B101", "B109"),
            c("B060", "B070", "B080"), 
            c("C113", "C115", "C119"),
            c("C117", "C118"),
            c("C121", "C122" ),
            c("C151", "C152" ),
            c("C211", "C212", "C213", "C214" ),
            c("C191", "C192"),
            c("C243", "C244"),
            c("C162", "C185", "C259"),
            c("D261", "D262", "D263", "D264", "D270"),
            c("I461", "I462"),
            c("I471", "I472", "I481", "I482", "I490"),
            c("I510", "I529"),
            c("J541", "J542"),
            c("J561", "J562"),
            c("J601", "J602"),
            c("J580", "J570", "J591", "J592"),
            c("K621", "K623"), 
            c("L662", "L663"),
            c("O754", "O760", "O771", "O772"),  ## includes "Government Representation"
            c("P821", "P822"))
      
    # filter by L3 industries & create LEEDCode
      BDS <- BDS %>%
             filter( ANZSIC06 %in% Level3Industries ) %>%
             mutate( LEEDCode = factor( substring( ANZSIC06, 1, 4) ) )
     
      for (i in 1:length(MustMerge)){
        BDS$LEEDCode <- rename.levels(BDS$LEEDCode, 
                                       orig = MustMerge[[i]], 
                                       new=rep( paste0( MustMerge[[i]], collapse="" ), 
                                                length( MustMerge[[i]] )
                                                )
                                      )
        }

      
      # we don't need ANZSIC06 any more, so aggregate up to the industries we need

        BDS <- BDS %>%
               # mutate(Year = year(Year)) %>%
               dplyr::group_by(Year, LEEDCode, Area) %>%
               dplyr::summarise(Value = sum(Value)) %>%
               left_join(unique(industries[, c("LEED4Industry",         ## only use the necessary columns
                                               "LEED18Industry",
                                               "RGDPIndustry_custom",
                                               "RGDPIndustry_2015",
                                               "RGDP_industry",
                                               "NGDP_industry",
                                               "LEEDCode")]))     
               
      if(length(unique(subset(BDS, is.na(LEED4Industry))$LEEDCode)) > 0){
        stop("Some BDS industries failed to concord to the LEED level")
      }
      
   ##
   ##    Splitting the regional data into lower level TA areas needs
   ##       the Region => TA concordance applied to the regional data
   ##
      #BDS <- subset(BDS, Area %in% unique(leedTA_to_SNZTA$SNZ_TA) )
      BDS <- BDS %>%
        filter( Area %in% unique(leedTA_to_SNZTA$SNZ_TA) )

      # do a cross join with the lower level TA 
      tmp <- BDS %>%
             full_join(TA_to_multiple_regions,
             by = c('Area'='SNZ_TA')
             ) %>%
        mutate( Value = Value * Proportion )
      ##
     
      ##
      ##    Check some totals - everything still balancing?
      ##

         BDS_TA_Totals <- with(BDS,
                            aggregate(list(Value = Value),
                                      list(Area  = Area),
                                      sum, 
                                      na.rm = TRUE)
                                      )
         tmp_TA_Totals <- with(tmp,
                            aggregate(list(Value = Value),
                                      list(Area  = Area),
                                      sum, 
                                      na.rm = TRUE)
                                      )

         check <- merge(BDS_TA_Totals,
                        tmp_TA_Totals,
                        by  = c("Area"),
                        all = TRUE)
         check$Difference <- with(check, (Value.x - Value.y))

         if(sum(check$Difference) != 0){
           stop("Something went wrong in allocating employees to the broken down sub-TAs")
         }
   ##
   ##    Lock in this dataframe and free up some space
   ##
      BDS <- tmp
      rm(tmp)

   ##
   ##    Add on the LEED18 region variable
   ##
      ( unique(BDS$Region) %in% unique(regions_concs$Region) )
      BDS <- left_join(BDS, regions_concs) %>%
             rename(TA = Area)

   ##
   ## creates a combined region-industry classification that makes the optimal use of the published
   ## regional GDP data.
 
     BDS <- BDS %>%
        mutate(
          i = ifelse(RGDPIndustry_2015 %in% c("Forestry, Fishing, and Mining", 
                                              "Electricity, Gas, Water, and Waste services") &
                     RegionGDP %in% c("Marlborough", "West Coast"),
                                      "Forestry, Fishing, Mining, Electricity, Gas, Water and Waste Services",
                     RGDPIndustry_2015),
          i = ifelse(i %in% c("Other Manufacturing", "Primary Manufacturing") &
                       RegionGDP %in% c("Northland", "Southland"),
                                        "Manufacturing",
                     i),
          RegionIndustryRGDP15 = ifelse(i %in% 
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
                                                 "Health Care and Social Assistance",
                                                 "GST on Production, Import Duties and Other Taxes"),
                                             i, 
                                             paste(RegionGDP, i)))  %>%
        select(-i)

      #==================together====================
      BDS <- BDS %>%
               group_by(Year, 
                        LEED4Industry, LEED18Industry, RGDPIndustry_custom, NGDP_industry, RegionIndustryRGDP15, RGDP_industry,
                        TA, TA_Region_modified, Region, LEED18Region, RegionGDP) %>%
               summarise(Employees = sum(Value)) %>%
               filter(LEED18Industry != "Not elsewhere included") %>%
               data.frame()
     