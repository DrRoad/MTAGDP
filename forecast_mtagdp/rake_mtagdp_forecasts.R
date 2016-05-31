##
##  Programme:  rake_mtagdp_forecasts.R
##
##  Objective:  Use the forecast data from the {gts} analysis (forecast_mtagdp.R)
##              to rake to the published Regional GDP figures
##
##  Approach:   Output from the gts are extracted and combined and then joined with the mtagdp
##              data object.  This aligns the different regional concordances for summarising
##              the data and aligning to the regional GDP figures.
##
##  Author:     Franz Smith, Sector Performance, Ministry of Business, Innovation and Employment
##
##  Date:       2015-12-10
##
##  Reviewed and refactored Peter Ellis 2015-12-17

##  Notes:   load("/Users/franzinho/Desktop/forecast_mtagdp/data_intermediate/rgdp_pub.rda")

     # make sure the functionality for pulling on TRED is present
       library(RODBC)
       library(mbieDBmisc)

       TRED    <- odbcConnect("TRED_Prod")
##
##  0. Import of the published RGDP values for raking
##

    rgdp_pop <- ImportTS2(TRED, "Gross domestic product, by region and industry (Annual-Mar)", 
                         stringsAsFactors = FALSE) %>%
                     rename(RGDP_Region = CV2,
                            RGDP_industry = CV3,
                            Freq = Value) %>%
                     mutate(Year = year(TimePeriod)) %>%
                     select(Year, RGDP_Region, RGDP_industry, Freq) %>%
                     filter(RGDP_industry %in% c("Gross Domestic Product"),
                           !RGDP_Region %in% c("New Zealand", "Total South Island", "Total North Island")) %>%
                     select(-RGDP_industry)

 #      load("data_intermediate/mtagdp_gts.rda")
 #      load("data_intermediate/mtagdp_forecast.rda")

##
##  1. Extract the 'TA x Industry' level from the gts output & aggregate to yearly figures
##
    # extract the bottom level of the time series objects we made earlier
       bottom_timeseries  <- aggts(mtagdp_gts, levels = c(1)) # actuals
       bottom_forecast    <- aggts(mtagdp_forecast, levels = c(1)) # forecasts

     # combine the objects into a single time series
       bottom_combined <- ts(rbind(bottom_timeseries, bottom_forecast), start=c(startYear, 1), frequency=12)
	   #bottom_combined <- ts(rbind(bottom_timeseries, bottom_forecast), start=c(2000, 1), frequency=12)

     # aggregate the 'monthly' time series for total gdp to yearly
       bottom_combined    <- ta(bottom_combined, conversion="sum", to = "annual")
                
       bottom_combined.df <- data.frame(matrix(bottom_combined, ncol = ncol(bottom_combined))) 

       names(bottom_combined.df) <- colnames(bottom_combined)
     
     # clean up some of the names
       bottom_combined.df %<>%
                      mutate(Year = startYear:(endYear+2)) %>%
					  gather(TA_Region_modified, Value, -Year) %>%
                      mutate(TA_Region_modified = gsub("\\.", " ", TA_Region_modified),
                             TA_Region_modified = gsub("Queenstown Lakes",  "Queenstown-Lakes",  TA_Region_modified),
                             TA_Region_modified = gsub("Thames Coromandel", "Thames-Coromandel", TA_Region_modified),
                             TA_Region_modified = gsub("Matamata Piako",    "Matamata-Piako",    TA_Region_modified),                             
                             TA_Region_modified = gsub("Hawke s",           "Hawke's",           TA_Region_modified))              

##
##  2. Summarise mtagdp object and bind to the {gts} output & shape Regional 
##
    # combine mtagdp summary with the forecast data 
      ta_summary <- TAGDP_defl %>%
                    group_by(Year, TA_Region_modified, TA, Region, RGDP_Region) %>%
                    summarise(GDP = sum(GDP)) %>%
                    data.frame()

      data_to_grunt <- bottom_combined.df %>%                       
                       left_join(ta_summary, by = c("Year", "TA_Region_modified")) %>%
                       tidyr::fill(TA_Region_modified, TA, Region, RGDP_Region) %>%
                       data.frame()
                       
    ## test merge worked ok.  The Actuals from the time series should be the same as in
    ## the original TAGDP_defl object.
        outby <- abs(with(data_to_grunt, sum(round(GDP, 0) - round(Value, 0), na.rm = TRUE)))
        if(outby > 0){
            print(outby)
            print(tail(data_to_grunt))
            stop("GDP from mtagdp should match actuals in the forecast object; something wrong with the merging")
         }

##
##  3. Prepare for the iterative proportional fitting
##
    # create tghe design object
      mtagdp_svy <- svydesign(~1, data = data_to_grunt, weights = ~Value)

    # set the tolerance value similar to grunt.R
      tolerance <- 0.01 / sum(rgdp_pop$Freq)         ## get as close as 0.1 GDP units as possible

    # Unlike grunt.r, no need for a loop as only one set of population totals to rake to.
      mtagdp_svy <- rake(mtagdp_svy,
                     sample.margins     = list(~Year + RGDP_Region),
                     population.margins = list(rgdp_pop),
                     control            = list(maxit = 100, epsilon = tolerance, verbose = FALSE))
          
            
    # collate the final result of the raking as GDP
      data_to_grunt$GDP_rake <- weights(mtagdp_svy) 

    # create a copy for further modifcations with population & inflation-adjustment information
      forecast_grunt <- data_to_grunt
     
 ## test that the actuals didn't get changed during raking
	outby <- with(filter(forecast_grunt, Year <= endYear), sum(abs(GDP_rake - GDP), na.rm = TRUE))
    
    if(outby > 0.01){
      message(paste("Raking corrected the total GDP by a total of", outby, 
                    "to match published region totals"))
      # note that this is expected behaviour because the published MTAGDP differs
      # somewhat from RGDP due to also being forced to match NGDP.  As we don't
      # match NGDP, and don't have to worry about industries, the MTAGDP totals
      # will differ very slightly from MTAGDP by industry.  This can be put down
      # to various rounding and estimation errors and is not to be worried about.
      
      forecast_grunt %>%
        ungroup() %>%
        mutate(diff = GDP_rake - GDP) %>%
        filter(diff > 0.1 & Year <= endYear) %>%
        arrange(-diff) %>%
        print()
      }
    
    # create a final object for publication
      mtagdp_totals <- forecast_grunt %>%
                         group_by(Year, TA, Region) %>%
                         summarise(GDP = sum(GDP_rake)) %>%
                         mutate(notes = ifelse(Year <= endYear , "", "provisional")) %>%
	                     ungroup()
    
    write.csv(mtagdp_totals, file = "data/mtagdp_totals.csv", row.names = FALSE)

