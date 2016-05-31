##
##  Programme:  forecast_mtagdp.R
##
##  Objective:  Estimate final years of the MTAGDP series (i.e. from 2012 to 2014).
##
##  Approach:   Use the modelled TA-level GDP as the basis for time-series analysis to predict
##              the final 2 years.  Hierarchical (or Grouped) Time Series is used as a means to
##              reconcile the timeseries (and forecasts) at each level of the groupings, in this
##              case Regions x Territorial Authorities x Industries.
##
##              The overall approach is described in detail at:
##                Hyndman, R.J., R.A. Ahmed, G. Athanasopoulos and H.L. Shang (2011) Optimal 
##                combination forecasts for hierarchical time series. Computational Statistics
##                and Data Analysis, 55(9), 2579–2589. http://robjhyndman.com/papers/hierarchical/
##              and
##                http://robjhyndman.com/hyndsight/gts/
##
##              As the series for which to forecast are extremely short (i.e. 12 data points),
##              the data are disaggregated to monthly time steps prior to running the {gts} routines.
##              This step makes for more plausible forecasts from the short series, and appears to 
##              make a difference in matching the published Statistics NZ RGDP (e.g. Taranaki, 
##              Wellington).
##
##
##  Author:     Franz Smith, Sector Performance, Ministry of Business, Innovation and Employment
##
##  Date:       2015-09-17
##
##  Reviewed and refactored Peter Ellis 2015-12-17
#
#              Raking of these results & validation tests provided in separate scripts.
##

##
##  1. Load core functionality, data & set parameters (required if starting from pre-cooked objects)
##
    # clean up
#       rm(list=ls())
   
    # functionality first
       library(mbie)
       library(dplyr)
       library(tidyr)
       library(data.table)
       library(lubridate)
       library(hts)
       library(tempdisagg)
       library(Cairo)
       library(extrafont)
       library(RColorBrewer)
       library(survey)
       library(mbieDBmisc)
             
    # these help manage geographical nuances
      source("R/organise_geographies.R")
	  
    # call to data, created separately in the main MTAGDP process
      # load("data_intermediate/TAGDP_defl.rda")  ## used for modelling the modified TA level GDP

##
##  2. Modify data objects for analysis
##

   # create a wide matrix with the region x TA x industry classes as columns
     mtagdp_wide <- TAGDP_defl %>%
                    group_by(Year, TA_Region_modified) %>%
                    summarise(GDP = sum(GDP, na.rm=TRUE)) %>%
                    spread(TA_Region_modified, GDP) 

     mtagdp_wide[is.na(mtagdp_wide)] <- 0  ## can safely assume these are zeros

    # turn ta x industry combinations into a timeseries matrix
         mtagdp_tsmatrix <- mtagdp_wide
    #        for(i in 1:ncol(mtagdp_wide)) {
    #           mtagdp_tsmatrix[, i] <- ts(mtagdp_tsmatrix[, i], start=c(2000, 1), frequency = 1)
    #       }
		   for(i in 1:ncol(mtagdp_wide)) {
               mtagdp_tsmatrix[, i] <- ts(mtagdp_tsmatrix[, i], start=c(startYear, 1), frequency = 1)
           }

         mtagdp_tsmatrix <- data.frame(mtagdp_tsmatrix)
         mtagdp_tsmatrix <- data.matrix(mtagdp_tsmatrix[, 2:ncol(mtagdp_tsmatrix)])
         mtagdp_tsmatrix <- ts(mtagdp_tsmatrix[, 1:ncol(mtagdp_tsmatrix)], start=c(startYear, 1), frequency = 1)
	#	 mtagdp_tsmatrix <- ts(mtagdp_tsmatrix[, 1:ncol(mtagdp_tsmatrix)], start=c(2000, 1), frequency = 1)

##
##  3. Disaggregate the time series & bind results into a matrix & perform {gts}
##         
      # disaggregate the series to monthly time steps   ## N.B. also tested with "quarterly", but monthly
                                                        #    performs much better
         m   <- NULL
         for(i in 1:ncol(mtagdp_tsmatrix)) {
              m[[i]] <- td(mtagdp_tsmatrix[, i] ~ 1, to = "monthly", method = "denton-cholette")
                          
           }

        tmp <- m  # create a copy for converting to time series
          for(i in 1:length(tmp)) {
             tmp[[i]] <- ts(tmp[[i]]$values, start=c(startYear, 1), frequency=12)
          }
		#  for(i in 1:length(tmp)) {
        #     tmp[[i]] <- ts(tmp[[i]]$values, start=c(2000, 1), frequency=12)
        #  }
    
        tmp <- data.matrix(ts(tmp))
       
      # create a matrix from the list
        tmp <- ts(matrix(unlist(tmp), ncol = length(tmp)), start=c(startYear, 1), frequency=12)       
		#tmp <- ts(matrix(unlist(tmp), ncol = length(tmp)), start=c(2000, 1), frequency=12)       

      # attach the group names (TAs)
        colnames(tmp) <- colnames(mtagdp_tsmatrix)

      # create the group time series object             ## N.B. 'characters' specify the groupings 4 region
        mtagdp_gts <- gts(tmp)


      # forecast
        mtagdp_forecast <- forecast(mtagdp_gts, 
                             h           = 24,           ## note the time steps reflect the 'monthly' series
                             fmethod     = "arima", 
                             method      = "comb", 
                             keep.fitted = TRUE,         ## required for the accuracy.gts{hts} functions
                             parallel    = TRUE,         ##  on the MBIE machines
                             num.cores   = 6,
                             keep.resid  = TRUE)

##
##  4. Plot the results & save expensive objects for later processing
##
   # plot to pdf (or screen)
     CairoPDF("testing_outputs/mtagdp_forecast.pdf", 7, 7)
       plot(mtagdp_forecast)
     dev.off()

   # save expensive data objects
     save(mtagdp_gts,      file="data_intermediate/mtagdp_gts.rda")
     save(mtagdp_forecast, file="data_intermediate/mtagdp_forecast.rda")  ## quite expensive to create

