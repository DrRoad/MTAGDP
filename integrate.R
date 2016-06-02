##
##   Project Name:  Modelled Territorial Authority Gross Domestic Product (MTAGDP) 
##                   
##   Objective:     Use official published coarse GDP statistics (i.e. National and Regional Council  
##                  geographies and industry classifications) from Statistics New Zealand to calculate
##                  finer-scale GDP estimates for Territorial Authorities and at finer industry 
##                  levels (e.g. consistent with published National GDP).  
##
##   Approach:      Calculations for MTAGDP are based on Iterative Proportional Fitting (IPF), 
##                  also known as 'raking', which adjusts a data table to match marginal totals of 
##                  columns and rows.  The steps in this process are:
##                    1. Import Business Demography Statistics at the TA level x detailed Industry 
##                         employee numbers, and break down the TAs to TA_region_modified by proportions
##                    2. Weight up to "Linked Employer-Employee Data" (LEED18) interaction of Region 
##                         and high level Industry categories, to provide TotalEarnings
##                    3. Weight up by raking to marginal totals of LEED 37 (TA) and LEED 4 (detailed 
##                         Industry) 
##                    4. Iteratively repeat steps 2 and 3 till change each time is minimal
##                         (alternative - do #2 and #3 together, if rake() is up to it)
##                    5. Weight the results to GDP so it is ValueAdded.  This is done in several steps.
##
##                       First values are weighted to a custom version of the Regional GDP (RGDP) 
##                         provided by Stats NZ.  As this has confidentialised cells, they have had 
##                         their NA values replaced with values that are consistent with marginal 
##                         National GDP industry totals and the published RGDP region totals; this 
##                         imputation-like procedure was also done with raking.
## 
##                       Second we rake weights simultaneously to Industry x Region x Year totals from 
##                         RGDP, and Industry x Year totals from NGDP.
## 
##                  The end result should have columns for Year, ValueAdded, TotalEarnings, 
##                  TA_region_modified, TA (which can be created from TA_region_modified), Region
##                  (which can also be created from TA_region_modified), and industry at the level of 
##                  the LEED table 4 (i.e. 6 digit ANZSIC06).  This data object is then summarised
##                  by TA, and per capita & inflation-adjusted measures are also added to the final object. 
##
##                  Testing results for individual regions, industries, and comparisons of
##                  earnings and employees are used as a validation step for the results prior to 
##                  producing dissemination materials. 
##
##                  Preparation of data for the deployment of an interactive web tool (based on shiny)
##                  are also conducted in the project workflow.
##
##
##   Authors:       Peter Ellis, Franz Smith, Sector Performance, Ministry of Business, Innovation &  
##                  Employment
##
##   Date:          2014-08-10 to 2015-06-20
##   Peer-Reviewed by: Senay Yasar Saglam
##   Review Period: 2016-05-16 to 2016-05-23
##   
##
## --------- 1. Clear the decks, and initialise the R environment for running the job----------------
##

    # clear the workspace & load the MBIE R profile 
      rm(list=ls()[ls() != "creds"])
      
      source("P:/r/common.Rprofile")
    
    ##  Load core packages for the project
      library(Cairo)
      library(ggplot2)
      library(extrafont)
      library(knitr)
      library(mbie)
      library(survey)
      library(mbieDBmisc) 
      library(lubridate)
      library(dplyr)
      library(tidyr)
      library(readr)
      library(magrittr)
      library(RODBC)
      library(sqldf)
      library(stringr)
      library(XLConnect)
      library(reshape2)
      library(RColorBrewer)
      library(riverplot)   ## used in the creation of the sankeys
      library(Hmisc)
      library(qgraph)      ## used for creating the commuter diagram
      library(igraph)
      library(shinyapps)
      library(mbiemaps)
      library(riverplot)
      library(tm)
      library(wordcloud)
      library(xtable)
      library(openxlsx)
      library(tcltk) # Used for creating a dialog box for entering time period for analysis 

      # Proxy password to get through MBIE firewall.  Placed here so you can run all of integrate.r without it stopping 
      # and asking for your password when it gets to the deploy app stage.
      if(!exists("creds") & Sys.getenv("USERDNSDOMAIN") == "WD.GOVT.NZ"){
        creds <- AskCreds(Title = "User Log In Name and Password", startuid = "", returnValOnCancel = "ID_CANCEL")   
        options(RCurlOptions = list(proxy = 'http://proxybcw.wd.govt.nz:8080',
                                    proxyusername = creds$uid, 
                                    proxypassword = creds$pwd))
      }
      
      
      ## Get the information on which time period user wants to analyze
      source("creation_code/getAnalysisYears.R")

      userInput <-  getTimePeriod(vars=c('startYear', 'endYear'), 
                            labels=c('Enter the start year for the analysis after 1999 (inclusive): ', 'Enter the end year for the analysis (inclusive): '),
                            fun=c(checkStart, checkEnd))

      startYear <- userInput$startYear
      endYear   <- userInput$endYear
      deflationYear <- endYear
      forecastYear <- endYear+2

    # call to additional functions  & themes
        source("R/themes.r")    
        source("R/organise_geographies.R")
        source("R/organise_industries.R")

      # set the working directory and themes
        project_dir <- getwd()
        theme_set(theme_bw(base_family = "Calibri"))      

      # set resolution for png plots too detailed to use PDFs
         DPI <- 700
        
    ##
    ##  Connect to our databases
    ##
    
      TRED    <- odbcConnect("TRED_Prod")
    
##
##-----2. Import concordances, business demography, LEED tables, and regional and national GDP------------
##
   ##
   ##  Read in concordances needed to connect the different data sources
   ##
       source("creation_code/import_concordances.R")
      
     # calculate commuting corrections from 2013 Census data for .csv called in grunt
       source("creation_code/create_commuting_corrections.R")
     
     # import population data for the calculation of GDP per capita
       source("creation_code/import_population_totals.R") 
       
     # import the deflator values (total, chain, nominal) & save .rda's to data_intermediate  
       source("creation_code/import_gdp_deflator_totals.R")

   ##
   ##  Import Business Demography Statistics (N.B. takes a while to import)
   ##
       source("creation_code/import_BDS.R")
      
     # create the object for selecting the years for the series  
       InYears <- startYear:endYear


   ##  Read in the LEED tables.  

      source("creation_code/import_leed4.R")
      source("creation_code/import_leed18.R")
      source("creation_code/import_leed37.R")

   ##
   ##  Force totals to match (they don't in the original due to rounding etc)
   ##  
      source("creation_code/harmonise_leed_totals.R")

   ##
   ##  Read in the published RGDP measures, and the National GDP measures.
   ##      Align the RGDP measures to the National GDP measures.
   ##  
      source("creation_code/import_RGDP.R")
           
      source("creation_code/import_NGDP.R")

      # we need the NGDP and RGDP totals to match before we do the imputation...
      source("creation_code/harmonise_GDP_totals.R")
      
      # ... as the imputation involves working out the size of holes that need to match
      source("creation_code/impute_rgdp_custom.R")
      
       
##
## ------------ 3. Run the Iterative Proportional Fitting (i.e. "raking") & save the results.------------
##    
   ## The programme "Grunt" does all the heavy lifting. 
        source("creation_code/grunt.R")

   ## Reshape, merge with deflators and population totals for per capita, etc
        source("creation_code/modify_tagdp.R")

   ## Save versions of TAGDP for forecasting & public dissemination
        source("creation_code/save_tagdp.R")   
   
##
## --------------- 4. Perform trouble-shooting & testing of results------------------------
##
    ## Create colour code breakdown for the plots
       if((endYear-startYear)%%2==0){
           breakYears <- seq(from=startYear, to=endYear, by=2)
             }else{
        
          breakYears <- c(seq(from=startYear, to=(endYear-1), by=2 ),endYear)
         }

      # Comparison of original populations for incompatibilities
      # This script prints stuff to the screen.  The thing to look at is the "DiffPercent" column,
      # which is the difference in percent between two population totals we are weighting to,
      # when they are aggregated up to a compatible classification.  Less than 1% is ok.
      source("testing_code/troubleshoot_incompatible_population_data.R")
            
      #  Comparison of marginal totals of the result with the original populations
      #  This script prints stuff to the screen but also generates plots in test_outputs/ that
      #  you should examine.  We are looking for there to be no difference between the final
      #  resulting GDP and the published marginal totals.  The plots are set to show the percentage
      #  difference, so you're looking for them all to be near zero percent. Up to 1 % is fine.
      source("testing_code/compare_marginal_totals.R")
    
   #  Check example of Wellington
      source("testing_code/testing_wellington_ratios.r")
      

    # what was the net impact of the commuting correction on earnings
      source("testing_code/net_impact_commuting_correction.R")

##
##------------------  5. Prepare graphic outputs & shinyapp deployment--------------------------------------
##
   
   ##
   ## Graphical output for the website & visuals for the reports 
   ##
    # Selection of industries and geographic entities for testing
      source("dissemination_code/plot_topTAs_by_industry.R")
      source("dissemination_code/plot_industries_by_TA.R")
      source("dissemination_code/plot_industries_by_region.R")

      source("dissemination_code/plot_commuting_patterns.r")
   
  
##
## ----------- 6. Forecast MTAGDP for 'between year' totals ----------------------
##
   # performs the grouped time series forecasts for 'between years' (at the moment, 2013-2014)
     source("forecast_mtagdp/forecast_mtagdp.R")
      
   # rake the values to match published Regional GDP marginal totals   
     source("forecast_mtagdp/rake_mtagdp_forecasts.R")
      
   # perform checks and visual inspections of output   
     source("forecast_mtagdp/validate_mtagdp_forecasts.R")
      
   # prepare & save data file for public release
     source("forecast_mtagdp/create_forecast_release.R")


##
## Prepare data and deploy Shinyapp
## 
   source("dissemination_code/prepare_basic_TA_commentary.R")
   source("dissemination_code/save_shiny_data.R")
   source("dissemination_code/deploy_shinyapp.R") # deploys "mtagdp_test" by default; edit the script if you also 
                                                  #   want to deploy "mtagdp"


##
## ----------------- 7. Conduct exploratory analyses and map visuals----------------------
##

   # create TAGDP map visuals for national view of industries & cagr stats for regional clusters
   # source("exploratory_analysis_code/create_tagdp_maps.r")

   # perform cluster analysis on proportional shares
     source("exploratory_analysis_code/cluster_analysis.r")

   # check out the Buller LEED data
     source("exploratory_analysis_code/what_is_going_on_in_Buller.R")


   # A dot plot summarise economic growth
     source("exploratory_analysis_code/dotplot_orderedRibbon.R")
   

##
##  8. Creation of reports & supporting documentation
##

    # create figures for the summary & methodology reports
     source("knitr/create_figures.R")

    # set directory for knitr
      setwd(paste0(project_dir, "/knitr"))
        
    # document describing the methodology & technical details
      knit2pdf("methodology.Rnw", compiler = 'xelatex', quiet=FALSE)

    # summary document describing the output data & results
      knit2pdf("summary.Rnw", compiler = 'xelatex', quiet=FALSE, clean = FALSE)  
                                                                   
    # some FAQs to accompany the data & website
      knit2pdf("tagdp_faqs.rnw", compiler = 'xelatex', quiet=FALSE)  
        
    # Getting back to the project directory     
      setwd(project_dir)


    # Make a public copy of the source material.  Assumes existence on the f: drive of the necessary folders etc (created once-off by hand)
      source("dissemination_code/create_public_repo_snapshot.R")

    # meta analysis of this repository - how many lines of code etc
      source("exploratory_analysis_code/meta_analysis.R")
        