##
##  Programme:  create_TAMGDP_public.R
##
##  Objective:  Create a project folder with embedded source code and data files for a public
##              version of TAMGDP.
##               
##  Approach:   Identifies the working (i.e. MBIE) version of TAMGDP and identifies the destination
##              (i.e. "project") location and creates the file hierarchy, core functions, scripts for
##              calculating, testing, and dissemination outputs.  The file/folder compilation also
##              includes necessary components for the shinyapp deployment.
##
##  Author:     Franz Smith, Sector Performance, MBIE
##
##  Date:       2015-06-24
##
##
##  Notes:      The 'integrate.R' script that is transferred from 'public/' differs from the MBIE
##              project version, as it only includes the files & routines necessary for the public
##              deployment in github.
##

##
## 1. Create the folder structure & insert core functionality
##

  # set current working directory
    # setwd("/Users/franzinho/Desktop/TAGDP_creation")     ## just for testing purposes
    working <- getwd()

  # establish the target & create the project folder
    # project <- "/Users/franzinho/Desktop/TAMGDP_public"  ## just for testing purposes
    project <- "F:/TAMGDP_public"

    dir.create(paste0(project))
  
  # create & place functions folder
    dir.create(paste0(project, "/R"), recursive=TRUE)

    file.copy(paste0(working, "/R/functions.R"),            paste0(project, "/R/"))
    file.copy(paste0(working, "/R/themes.r"),               paste0(project, "/R/"))
    file.copy(paste0(working, "/R/organise_geographies.R"), paste0(project, "/R/"))
    file.copy(paste0(working, "/R/organise_industries.R"),  paste0(project, "/R/"))

  # make a copy of the public 'integrate.R' script
    file.copy(paste0(working, "/public/integrate.R"),       paste0(project))

##
## 2. Set up for creation code
##
  
  # create the folder to hold the code
    dir.create(paste0(project, "/creation_code"))

  # concordances
    file.copy(paste0(working, "/creation_code/import_concordances.R"),          paste0(project, "/creation_code/"))  

  # data import files
    file.copy(paste0(working, "/creation_code/import_BDS.R"),                   paste0(project, "/creation_code/"))
    file.copy(paste0(working, "/creation_code/import_leed4.R"),                 paste0(project, "/creation_code/"))
    file.copy(paste0(working, "/creation_code/import_leed18.R"),                paste0(project, "/creation_code/"))
    file.copy(paste0(working, "/creation_code/import_leed37.R"),                paste0(project, "/creation_code/"))
    file.copy(paste0(working, "/creation_code/import_RGDP.R"),                  paste0(project, "/creation_code/"))    
    file.copy(paste0(working, "/creation_code/impute_rgdp_custom.R"),           paste0(project, "/creation_code/"))    
    file.copy(paste0(working, "/creation_code/import_NGDP.R"),                  paste0(project, "/creation_code/"))

  # scripts to harmonise tables
    file.copy(paste0(working, "/creation_code/harmonise_leed_totals.R"),        paste0(project, "/creation_code/"))    
    file.copy(paste0(working, "/creation_code/harmonise_GDP_totals.R"),         paste0(project, "/creation_code/"))        
 
  # import script the commuter data & set function for making the correction
    file.copy(paste0(working, "/creation_code/create_commuting_corrections.R"), paste0(project, "/creation_code/"))      
 
  # import scripts for additional data sources for modifying TAMGDP
    file.copy(paste0(working, "/creation_code/import_gdp_deflator_totals.R"),   paste0(project, "/creation_code/"))  
    file.copy(paste0(working, "/creation_code/import_population_totals.R"),     paste0(project, "/creation_code/"))   
 
  # script for TAMGDP calculations & modifications
    file.copy(paste0(working, "/creation_code/grunt.R"),                        paste0(project, "/creation_code/"))     
    file.copy(paste0(working, "/creation_code/modify_tagdp.R"),                 paste0(project, "/creation_code/")) 


 
##
## 3. Set up for testing code
##

  # create the folder to hold the code
    dir.create(paste0(project, "/testing_code"))

  # scripts for testing the outputs
    file.copy(paste0(working, "/testing_code/compare_marginal_totals.R"),                   paste0(project, "/testing_code/"))     
    file.copy(paste0(working, "/testing_code/troubleshoot_incompatible_population_data.R"), paste0(project, "/testing_code/"))    
    file.copy(paste0(working, "/testing_code/net_impact_commuting_correction.R"),           paste0(project, "/testing_code/"))     
    file.copy(paste0(working, "/testing_code/assess_deflator.R"),                           paste0(project, "/testing_code/"))   

  # create the folder to hold the outputs
    dir.create(paste0(project, "/testing_outputs"))

##
## 4. Set up for dissemination code
##

  # create the folder to hold the code
    dir.create(paste0(project, "/dissemination_code"))

  # scripts for creating the graphic outputs
    file.copy(paste0(working, "/dissemination_code/plot_sankey_charts.R"),      paste0(project, "/dissemination_code/"))    
    file.copy(paste0(working, "/dissemination_code/plot_industries_by_TA.R"),   paste0(project, "/dissemination_code/"))
    file.copy(paste0(working, "/dissemination_code/plot_topTAs_by_industry.R"), paste0(project, "/dissemination_code/"))
         
  # create the folder to hold the outputs
    dir.create(paste0(project, "/dissemination_outputs"))

##
## 5. Set up for data containers
##

  # create the folder for the raw data
    dir.create(paste0(project, "/data_raw"))
    
  # create a folder to hold extracts from tred
    dir.create(paste0(project, "/data_raw/tred")) 
    
  # create the folder for intermediate data objects
    dir.create(paste0(project, "/data_intermediate"))

  # create the folder for final working data objects
    dir.create(paste0(project, "/data"))

  # tables for BDS
    file.copy(paste0(working, "/data_raw/TABLECODE7601_Data_c51b48ec-65bb-4ece-880a-fa575544cf03.csv"),                paste0(project, "/data_raw/"))   
    file.copy(paste0(working, "/data_raw/TABLECODE7601_Data_b6e1d6e3-0e61-4a18-95c2-46eda83122ef.csv"),                paste0(project, "/data_raw/"))
    file.copy(paste0(working, "/data_raw/TABLECODE7601_FootnotesLegend_c51b48ec-65bb-4ece-880a-fa575544cf03.csv"),     paste0(project, "/data_raw/"))   
    file.copy(paste0(working, "/data_raw/TABLECODE7601_FootnotesLegend_b6e1d6e3-0e61-4a18-95c2-46eda83122ef.csv"),     paste0(project, "/data_raw/"))
    
  # files for LEED tables      
    file.copy(paste0(working, "/data_raw/TABLECODE7004_Data_e7610f76-5579-49b3-b63e-9d13a339bd93.csv"),                paste0(project, "/data_raw/"))  ## 4   
    file.copy(paste0(working, "/data_raw/TABLECODE7018_Data_bfc13c90-b579-4dd5-a8f1-e0031010b6d7.csv"),                paste0(project, "/data_raw/"))  ## 18  
    file.copy(paste0(working, "/data_raw/TABLECODE7037_Data_2fbd4042-a117-470a-a484-5c38c1033011.csv"),                paste0(project, "/data_raw/"))  ## 37       
    file.copy(paste0(working, "/data_raw/TABLECODE7004_FootnotesLegend_e7610f76-5579-49b3-b63e-9d13a339bd93.csv"),     paste0(project, "/data_raw/"))  ## 4   
    file.copy(paste0(working, "/data_raw/TABLECODE7018_FootnotesLegend_bfc13c90-b579-4dd5-a8f1-e0031010b6d7.csv"),     paste0(project, "/data_raw/"))  ## 18  
    file.copy(paste0(working, "/data_raw/TABLECODE7037_FootnotesLegend_2fbd4042-a117-470a-a484-5c38c1033011.csv"),     paste0(project, "/data_raw/"))  ## 37   

  # files for Regional and National GDP
    file.copy(paste0(working, "/data_raw/tred/rgdp_pop_pub.rda"),                                                      paste0(project, "/data_raw/tred/"))   
    file.copy(paste0(working, "/data_raw/tred/rgdp_pop_pub_det.rda"),                                                  paste0(project, "/data_raw/tred/"))  
    file.copy(paste0(working, "/data_raw/MBIE_CONFIDENTIAL_RegionalGDP_matrix30industries_15regions_2000_2012.xlsx"),  paste0(project, "/data_raw/"))  
    file.copy(paste0(working, "/data_raw/tred/ngdp.rda"),                                                              paste0(project, "/data_raw/tred/"))  

  # file with the commuter data
    file.copy(paste0(working, "/data_raw/2013-usual-residence-by-workplace-address-territorial-authority.csv"),        paste0(project, "/data_raw/"))  

  # tred extacts for population, gst measures, and deflators
    file.copy(paste0(working, "/data/ta_pops.rda"),                                                                    paste0(project, "/data/"))
    file.copy(paste0(working, "/data_raw/tred/GST_Duties_Tax.rda"),                                                    paste0(project, "/data_raw/tred/"))
    file.copy(paste0(working, "/data_intermediate/nominal.rda"),                                                       paste0(project, "/data_intermediate/"))
    file.copy(paste0(working, "/data_intermediate/chain.rda"),                                                         paste0(project, "/data_intermediate/"))
    file.copy(paste0(working, "/data/deflators.rda"),                                                                  paste0(project, "/data/"))

  # create embedded folder to house the concordances  
    dir.create(paste0(project, "/data_raw/concordances"), recursive=TRUE)

  # individual files for concordances
    # industries
    file.copy(paste0(working, "/data_raw/concordances/industries.csv"),                       paste0(project, "/data_raw/concordances/")) 
    file.copy(paste0(working, "/data_raw/concordances/deflatorIndustries.csv"),               paste0(project, "/data_raw/concordances/"))
    
    # regions 
    file.copy(paste0(working, "/data_raw/concordances/leedTA_to_SNZTA.csv"),                  paste0(project, "/data_raw/concordances/")) 
    file.copy(paste0(working, "/data_raw/concordances/region_to_leed18_andRGDP_region.csv"),  paste0(project, "/data_raw/concordances/"))
    file.copy(paste0(working, "/data_raw/concordances/RegionIndustryRGDP15.csv"),             paste0(project, "/data_raw/concordances/")) 
    file.copy(paste0(working, "/data_raw/concordances/TA_to_multiple_regions.csv"),           paste0(project, "/data_raw/concordances/"))     

##
## 6. Set up for shiny
##

  # files necessary for the shiny deployment   ## parked in dissemination_code
    file.copy(paste0(working, "/dissemination_code/save_shiny_data.R"),      paste0(project, "/dissemination_code/"))   
    file.copy(paste0(working, "/dissemination_code/deploy_shinyapp.R"),      paste0(project, "/dissemination_code/"))   

  # this is a full deployment of the shiny folder
    dir.create(paste0(project, "/shiny"))
    file.copy(paste0(working, "/shiny"),                                     paste0(project), recursive=TRUE)
