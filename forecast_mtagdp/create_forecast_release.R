##
##  Programme:  create_forecast_release.R
##
##  Objective:  Create MTAGDP forecast series for public release.
##
##  Approach:   Bind the forecast MTAGDP object with additional columns for perCapita, realGDP,
##              and realGDP_perCapita columns.  Existing real_GDP are summarised for the period
##              with industry-level detail and deflate the 'between years', using  the implicit 
##              price deflators from published table:
##              "Series, Rolling Annuals - IPDs, Actual, Total (Annual-Mar)"
##
##              perCapita information come from the same population series used in base series of TAGDP
##
##
##  Author:     Franz Smith, Sector Performance, Ministry of Business, Innovation and Employment
##
##  Date:       2015-12-23
##
##

##
##  1. Call to data objects & implicit price deflators
##
    # load("data_intermediate/TAGDP_defl.rda")
    # load("data/mtagdp_totals.rda")
    # load("data/ta_pops.rda")

    rolling_annuals <- ImportTS2(TRED, "Series, Rolling Annuals - IPDs, Actual, Total (Annual-Mar)") %>%
                         mutate(Year = year(TimePeriod)) %>%
                         filter(CV1  == "Gross Domestic Product - expenditure measure",
                                #Year %in% c(2012, 2013, 2014, 2015)
								Year %in% (endYear-1):forecastYear # 4 year period
								) %>%
                         #mutate(deflator2012 = Value[Year == 2012] / Value) %>%              
                         mutate(deflatorEndYear = Value[Year == endYear] / Value) %>%              
                         select(Year, deflatorEndYear) %>%       
                         data.frame()                     
        
##
##  2. Sum the real GDP figures for years with industry detail and deflate the 'between' years
##     

    existing_real <- TAGDP_defl %>%
                     group_by(Year, TA, Region) %>%
                     summarise(GDP      = sum(GDP,      na.rm=TRUE),
                               GDP_real = sum(GDP_real, na.rm=TRUE)) %>%
                     mutate(notes = "estimate") %>%
                     data.frame()
                     
    forecast_real <- mtagdp_totals %>%
                     #filter(Year %in% c(2014, 2015)) %>%
					 filter(Year %in% (forecastYear-1):forecastYear) %>%
                     left_join(rolling_annuals, by = c("Year")) %>%
                     group_by(Year, TA, Region, notes) %>%
                     summarise(GDP      = sum(GDP, na.rm=TRUE),
                               GDP_real = sum(GDP, na.rm=TRUE) * deflatorEndYear) %>%
                     data.frame()

##
##  3. Combine the existing and forecast objects, include the population measures & bind for public release
##                     
    mtagdp_totals <- rbind(existing_real, forecast_real) %>%
                     left_join(ta_pops, by = c("Year", "TA")) %>%
                     mutate(GDP_perCapita      = GDP * 1000000 / Population,
                            GDP_real_perCapita = GDP_real * 1000000 / Population) %>%
                     select(Year, TA, Region, GDP, GDP_real, GDP_perCapita, GDP_real_perCapita, notes) %>%
                     data.frame()
                                                                                              
    if(sum(mtagdp_totals$GDP[mtagdp_totals$Year <= endYear]) != sum(TAGDP_defl$GDP)){
		stop("mtagdp_totals has a different sum(GDP) to TAGDP_defl.  Not right.")
	 }
		
##
## 4.  Save final MTAGDP object for exploratory data analyses & dump to .csv
##

       save(mtagdp_totals, file = "data/mtagdp_totals.rda")                  
   
     # csv dump for web consumption                 
       write.csv(mtagdp_totals, file = "data/mtagdp_totals_public.csv", row.names=FALSE)
  
