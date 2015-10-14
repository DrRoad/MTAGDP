##
##    Programme:  grunt.r
##
##    Objective:  This is the stage of the programme where all the magic happens.  Everything up until
##                here is about getting the data in the right shape and totally to the right amount -
##                matching the employee and earnings information from the BDS and LEED tables, correcting
##                for the "commuter effect", and matching published & custom regional, and National GDP.
##
##    Approach:   The {survey} package's rake function is used to align the different measures of employees,
##                earnings and GDP across the tables.  The measures of economic activity are derived from
##                the BDS employment data, which has had different industry and geographic classifications
##                appended to it.  Using these different region and industry classifications, the BDS data
##                are "scaled" up and down to LEED estimates of regional earnings.  The LEED tables estimate
##                the value of a Compensation of Employment proxy for each industry and each region.  "Earnings"
##                is a closer measure to value added than earnings numbers, and the process of raking LEED over 
##                the BDS makes the resultant weights (i.e. earnings) a closer poximity to GDP in the second
##                stage of raking.
##
##                The correct_commuting() function is used to modify the earnings values accoring to the
##                relative amount of "commuters" (i.e. that have different home and business addresses) in
##                each TA.  The modified values are then raked against official published National and Regional
##                GDP from Statistics New Zealand and a custom version provided by SNZ for the purposes of 
##                this project.  This process is looped 15 times to help achieve convergence on the marginal
##                totals.
##
##                The final data object is retained for additional modifications with population and inflation-
##                adjustment information.
##
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##   Date:       2015-05-21
##

##
## 1. Set up data objects and the survey object for initial stages of raking on employees & earnings
##
   # create earnings object from BDS data
     EarningsDetailed <- BDS

   # This is treated as a survey design, with the estimates of employees as weights
     EarningsDetailed_svy <- svydesign(~1, data=EarningsDetailed, weights = ~Employees)

#----------------Weight up to LEED total earnings--------------------

# We use the rake function, designed to bring up survey weights to meet marginal population totals,
# to convert the number of employees to add up to the marginal totals of total earnings for the 3 LEED tables.
# In the command below, epsilon is the maximum acceptable change in a total before convergence is declared.
# this defaults to one, for survey purposes; but as we are doing this with billions of dollars we set it higher

   for(i in 1:5){
      EarningsDetailed_svy <- rake(EarningsDetailed_svy, 
                                sample.margins = list (~Year + LEED18Industry + LEED18Region),
                                population.margins = list(leed18_pop),
                                control = list(maxit = 25, epsilon = 100, verbose=FALSE))
  
      EarningsDetailed_svy  <- rake(EarningsDetailed_svy, 
                                sample.margins = list (~Year + TA_Region_modified),
                                population.margins = list(leed37_pop),
                                control = list(maxit = 25, epsilon = 100, verbose=FALSE))
  
      EarningsDetailed_svy <- rake(EarningsDetailed_svy, 
                               sample.margins = list (~Year + LEED4Industry),
                               population.margins = list(leed4_pop),
                               control = list(maxit = 25, epsilon = 100, verbose=FALSE))
   }

   # collate the resultant matching weights as earnings
     EarningsDetailed$Earnings <- weights(EarningsDetailed_svy)

#-----------------Add in OOD and GST------------------------------

# We now want to rake up these estimated Earnings to the RGDP and NGDP figures.  
#
# As there is no "Owner-Occupied Property Operation" item in either the BDS 
# or the LEED, we need something that's proportionate to it within regions, then
# the weighting will take care of it.  Total Earnings being proportionate to OOD, within
# the constraint of the overall region, is a reasonable assumption, so we do that

   OOD <- EarningsDetailed %>% group_by(Year, TA, TA_Region_modified, RegionGDP, Region, LEED18Region) %>%
                               summarise(
                                 Earnings = sum(Earnings),
                                 Employees = 0,
                                 RGDP_industry = "Owner-Occupied Property Operation",
                                 RGDPIndustry_custom = "owner-occupied property operation (national accounts only)",
                                 RegionIndustryRGDP15 = "Owner-Occupied Property Operation",
                                 LEED4Industry = "Owner-occupied property operation (national accounts only)",
                                 LEED18Industry = "owner-occupied property operation (national accounts only)",
                                 NGDP_industry = "Owner-Occupied Property Operation (National Accounts Only)"
                             )

  ## Same trick with GST, import duties and taxes - these are a "fake" industry
            
   GST_Duties_Tax <- OOD %>%
                     mutate(
                       RGDP_industry = "GST on Production, Import Duties and Other Taxes" ,
                       RGDPIndustry_custom = "GST on Production, Import Duties and Other Taxes" ,
                       RegionIndustryRGDP15 = "GST on Production, Import Duties and Other Taxes" ,
                       LEED4Industry = "GST on Production, Import Duties and Other Taxes" ,
                       LEED18Industry = "GST on Production, Import Duties and Other Taxes" ,
                       NGDP_industry = "GST on Production, Import Duties and Other Taxes" 
                     )

  # We now create a data frame that will become MTAGDP, using our Earnings data as the kernel
    TAGDP <- rbind(EarningsDetailed, OOD)
    TAGDP <- rbind(TAGDP, GST_Duties_Tax)
    TAGDP <- TAGDP %>%  
             filter(Year %in% unique(rgdp_pop_custom$Year)) %>%
             # add on the code of the custom data
             left_join(unique(industries[ , c("RGDPRef_custom", "RGDPIndustry_custom")]))

##
## 2. Adjust for commuting effects
##

  # As the LEED data gave us earnings by place of residence, not of work, we use data available from
  #  the 2013 Census to adjust the earnings according to the relative proportion of commuters for each
  #  TA.  The function iterates over the top 4 TA destinations (from proportion of commuters) to best
  #  approximate the shift in earnings.  

    for(i in 1:4){
	
          TAGDP <- correct_commuting(TAGDP, paste0("data_intermediate/commuting_corrections_", i, ".csv"))
     }

##
## 3. Next round of raking to GDP measures
##

  # first create a survey object using the commuting_corrected earnings as the weights
    TAGDP_svy <- svydesign(~1, data=TAGDP, weights = ~Earnings_commuting_corrected)

  # set the tolerance value for how much to iterate when raking to GDP.
    tolerance <- 0.01 / sum(ngdp_pop$Freq) # ie insist on getting as close as 0.1 GDP units

  #----------to custom RGDP--------------
  # We do this first as a one-off because it is basically incompatible with the published data
  # hence if we include it in iterations there is no convergence.

    TAGDP$GDP <- 1
  
  #----------------------Rake to the various GDP measures---------------------
  # This step iterates manually rgdp v. ngdp to help with harmonising the weights and isolating
  #   any errors.
 
    for(i in 1:15){
      cat("Beginning of loop\n")    
      TAGDP_svy <- rake(TAGDP_svy, 
                        sample.margins = list (~Year + RGDPIndustry_custom + RegionGDP),
                        population.margins = list(rgdp_pop_custom[!is.na(rgdp_pop_custom$RGDPIndustry_custom), 
                                                                  c("Year", "RGDPIndustry_custom", "RegionGDP", "Freq")]),
                        control = list(maxit = 100, epsilon = tolerance, verbose=FALSE))
          cat(sum((TAGDP$GDP - weights(TAGDP_svy)) ^ 2), "change after weighting to the custom data on loop ", i, "\n")
          TAGDP$GDP <- weights(TAGDP_svy) 
  
      # First to the national gdp published industry figures
        TAGDP_svy <- rake(TAGDP_svy, 
                       sample.margins = list (~Year + NGDP_industry),
                       population.margins = list(ngdp_pop),
                       control = list(maxit = 100, epsilon = tolerance, verbose=FALSE))  
      # how are we going: troubleshooting
        cat(sum((TAGDP$GDP - weights(TAGDP_svy)) ^ 2), "change after weighting to NGDP on loop ", i, "\n")
        TAGDP$GDP <- weights(TAGDP_svy) 
        
      # Then to the two iterations of RGDP.  All these should be very consistent
        TAGDP_svy <- rake(TAGDP_svy, 
                      sample.margins = list (~Year + RegionGDP + RegionIndustryRGDP15),
                      population.margins = list(rgdp_pop_pub_det),
                      control = list(maxit = 100, epsilon = tolerance, verbose=FALSE))  
      # how are we going: troubleshooting
        cat(sum((TAGDP$GDP - weights(TAGDP_svy)) ^ 2), "change after weighting to RGDP detailed on loop ", i, "\n")
        TAGDP$GDP <- weights(TAGDP_svy) 
    
         TAGDP_svy <- rake(TAGDP_svy, 
                        sample.margins = list (~Year + RegionGDP + RGDP_industry),
                        population.margins = list(rgdp_pop_pub),
                       control = list(maxit = 100, epsilon = tolerance, verbose=FALSE))  
      # how are we going: troubleshooting
        cat(sum((TAGDP$GDP - weights(TAGDP_svy)) ^ 2), "change after weighting to RGDP high level on loop ", i, "\n")
       TAGDP$GDP <- weights(TAGDP_svy) 
      
     }
  
    # collate the final result of the raking as GDP
      TAGDP$GDP <- weights(TAGDP_svy) 

    # create a copy for further modifcations with population & inflation-adjustment information
      TAGDP_grunt <- TAGDP
     
      rm(TAGDP) # to prevent it being used later and causing confusion

     ##
     ## create a short TA name for graphical output
     ##
     
     TAGDP_grunt$TA_short <- gsub(" District", "", TAGDP_grunt$TA)
     TAGDP_grunt$TA_short <- gsub(" City", "",     TAGDP_grunt$TA_short)
     
     
     
  