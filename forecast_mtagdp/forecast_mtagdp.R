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
     mtagdp.w <- TAGDP_defl %>%
                 group_by(Year, TA_Region_modified) %>%
                 summarise(GDP = sum(GDP, na.rm=TRUE)) %>%
                 spread(TA_Region_modified, GDP) 

     mtagdp.w[is.na(mtagdp.w) ] <- 0  ## can safely assume these are zeros

    # turn ta x industry combinations into a timeseries matrix
         mtagdp.ts <- mtagdp.w
         for(i in 1:ncol(mtagdp.w)) {
             mtagdp.ts[, i] <- ts(mtagdp.ts[, i], start=c(2000, 1), frequency = 1)
           }

         mtagdp.ts <- data.frame(mtagdp.ts)
         mtagdp.tsm  <- data.matrix(mtagdp.ts[, 2:ncol(mtagdp.ts)])
         mtagdp.tsm  <- ts(mtagdp.tsm[, 1:ncol(mtagdp.tsm)], start=c(2000, 1), frequency = 1)

##
##  3. Disaggregate the time series & bind results into a matrix & perform {gts}
##         
      # disaggregate the series to monthly time steps   ## N.B. also tested with "quarterly", but monthly
                                                        #    performs much better
         m   <- NULL
         for(i in 1:ncol(mtagdp.tsm)) {
              m[[i]]   <- td(mtagdp.tsm[, i] ~ 1, to = "monthly", method = "denton-cholette")
                          
           }

        tmp <- m  # create a copy for converting to time series
        for(i in 1:length(tmp)) {
	         tmp[[i]] <- ts(tmp[[i]]$values, start=c(2000, 1), frequency=12)
           }
    
        tmp.dm <- data.matrix(ts(tmp))
       
      # create a matrix from the list
        tmp.ul <- ts(matrix(unlist(tmp.dm), ncol = length(tmp)), start=c(2000, 1), frequency=12)       

      # attach the group names (TAs)
        colnames(tmp.ul) <- colnames(mtagdp.tsm)

     # create the group time series object       ## N.B. 'characters' specify the groupings 4 region; 4 ta; 4 industry
        tmp.gts <- gts(tmp.ul)


      # forecast
        tmp.gts.fc <- forecast(tmp.gts, 
                        h           = 24,           ## note the time steps reflect the 'monthly' series
                        fmethod     = "arima", 
                        method      = "comb", 
                        keep.fitted = TRUE,         ## required for the accuracy.gts{hts} functions
                        parallel    = TRUE,       ##  on the MBIE machines
                        num.cores   = 6,          ## seems to be a problem with the multicore function/firewall)
                        keep.resid  = TRUE)

##
##  4. Plot the results & save expensive objects for later processing
##
   # plot to pdf (or screen)
     CairoPDF("testing_outputs/ngdp_industries_gts.pdf", 7, 7)
       plot(tmp.gts.fc)
     dev.off()

   # save expensive data objects
     save(tmp.gts,    file="data_intermediate/tmp.gts.rda")
     save(tmp.gts.fc, file="data_intermediate/tmp.gts.fc.rda")  ## quite expensive to create if multicore not resolved

