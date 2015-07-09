##
##   Programme:  create_commuting_corrections.R
##
##   Objective:  In the calculation of TAGDP, information on earnings comes from the place of usual 
##               residence (i.e. not necessarily where the person works) which creates a potential 
##               error in appropriating GDP in cases where individuals are commuting across Territorial
##               Authority boundaries.  To correct for this, script takes Statistics New Zealand 
##               Census 2013 data on the 'usual residence' and 'workplace address' to identify the 
##               greatest unidirectional flow & collates the information in a .csv file.  This 
##               correction is later called by 'grunt.R' in adjusting the earnings prior to raking
##                to RGDP and NGDP.
##
##   Approach:   Import the raw matrix, identify the 'NA' cells, and filter for regions "Not Further 
##               Defined".  Data are then placed on a relative scale to the total number of commuters
##               in each Territorial Authority (excepting the NA cells).  The maximum relative 
##               extra-Territorial Authority travel is then identified and combined with the "to" identifier.
##               A function iterates 4 times through the commuter data and writes the top 4 commuting
##               destinations for each TA.
##
##               A function is used to correct the earnings according to the commuter correction files,
##               by working out the proportion of earnings (i.e. "bonuses") that should be shfited and 
##               added to the earnings of the "recipient" TA.  A running tally of the accumulated change
##               is printed during the iterations to gauge the relative % change for each correction step.
##
##
##   Authors:     Franz Smith & Peter Ellis, Sector Performance, MBIE
##
##   Date:       2015-05-21
##

##
##   Notes:      At the moment, the commuting population proportions are scaled INCLUDING the non-commuters
##               (i.e. people that have their usual residence and workplace address in the same
##               Territorial Authority).
##

##
##  1. Import the commuting data & get it into shape
##
      
      # call to the commuting matrix, ignoring the final row (with the population totals)
        commuting_data <- read.csv("data_raw/2013-usual-residence-by-workplace-address-territorial-authority.csv", 
                                    header=TRUE, na.string="..C", nrows = 69)
      
      # shape the data into an edgelist
        commuting_edgelist <- commuting_data %>% 
                              gather(Usual.residence, commuters) %>%
                              data.frame()
                              
      # clean up some of the accessory TAs and "Regions not further defined"                
        commuting_edgelist <- commuting_edgelist %>% 
		                        filter(!Usual.residence   %in% c("Area Outside Territorial Authority",
		                                                         "Total, usual residents",
		                                                         "Chatham Islands Territory")) %>%
		                        filter(!Usual.residence.1 %in% c("No.Fixed.Workplace.Address",
		                                                         "New.Zealand.Not.Further.Defined",
		                                                         "Area.Outside.Territorial.Authority",
		                                                         "Total.workplace.address",
		                                                         "Northland.Region.Not.Further.Defined",
		                                                         "Auckland.Region.Not.Further.Defined",
		                                                         "Waikato.Region.Not.Further.Defined",
		                                                         "Bay.of.Plenty.Region.Not.Further.Defined",
		                                                         "Gisborne.Region.Not.Further.Defined",
		                                                         "Hawke.s.Bay.Region.Not.Further.Defined",
		                                                         "Taranaki.Region.Not.Further.Defined",
		                                                         "Manawatu.Wanganui.Region.Not.Further.Defined",
		                                                         "Wellington.Region.Not.Further.Defined",
		                                                         "West.Coast.Region.Not.Further.Defined",
		                                                         "Canterbury.Region.Not.Further.Defined",
		                                                         "Otago.Region.Not.Further.Defined",
		                                                         "Southland.Region.Not.Further.Defined",
		                                                         "Tasman.Region.Not.Further.Defined",
		                                                         "Nelson.Region.Not.Further.Defined",
		                                                         "Marlborough.Region.Not.Further.Defined",
		                                                         "Chatham.Islands.Territory")) %>%
		                        filter(!is.na(commuters)) %>%                                              
		                        data.frame()

      # subsitute the dots for spaces  
        commuting_edgelist$Usual.residence.1 <- gsub("\\.","\\ ", commuting_edgelist$Usual.residence.1)

	  # replacing with conventional edgelist names
	    colnames(commuting_edgelist) <- c("Source", "Target", "Weight")
	    commuting_edgelist <- commuting_edgelist[,c("Source","Target","Weight")]
        
        # Fix a few Targets that went wrong in the import
        commuting_edgelist <- commuting_edgelist %>%
          mutate(Target = gsub("Thames Coromandel", "Thames-Coromandel", Target),
                 Target = gsub("Matamata Piako", "Matamata-Piako", Target),
                 Target = gsub("Queenstown Lakes", "Queenstown-Lakes", Target),
                 Target = gsub("Hawke s Bay", "Hawke's Bay", Target))
        
        commuting_edgelist2 <-  commuting_edgelist %>%
          group_by(Source) %>%
          mutate(Proportion = Weight / sum(Weight)) %>%
          filter(Source != Target) %>%
          arrange(-Proportion)

##        
## 2. Identify the ith biggest commuting shift.  We do the first four biggest places people commute to.
##     After 4 the percentages are down to < 2%, and some source TAs have no fifth destination eg Stratford.
##
        
       for(i in 1:4){
         commuting_edglist_ith_biggest <- commuting_edgelist2 %>%
           summarise(to = Target[i],
                     commuting_correction = Proportion[i]) %>% # ie the proportin of original earnings to give away
           ungroup() %>%
           rename(TA = Source)
  
         write.csv(commuting_edglist_ith_biggest, 
                   file = paste0("data_intermediate/commuting_corrections_", i, ".csv"),
                   row.names=FALSE)
       }

##
## 3. Define the function for calculating the commuter adjustments for MTAGDP
##
      correct_commuting <- function(TAGDP.c, filename){
  
        # filename needs to be a csv with columns 
        #   TA = donor TA
        #   commuting_correction = amount of earnings that *goes* from the donor TA to the TA in 'to 
        #               (note this is the inverse of how it was until 30 May 2015)
        #   to = TA that will receive (commuting_correction) * TA's earnings
  
        # TAGDP.c <- TAGDP; filename = "data_raw/commuting_corrections_2.csv" # for debugging 
  
        commuting_corrections <- read.csv(filename, stringsAsFactors = FALSE)
    
        if(is.null(TAGDP.c$Earnings_commuting_corrected)){
          print("This is the first round of commuting correction, so I'm starting with Earnings")
          TAGDP.c$Earnings_latest <- TAGDP.c$Earnings
         } else {
             print("This is not the first round of commuting correction, so I'm starting with Earnings_commuting_corrected")
             TAGDP.c$Earnings_latest <- TAGDP.c$Earnings_commuting_corrected
        }
   
        # First we scale down the latest version of the earnings of commuters
          TAGDP.c <- left_join(TAGDP.c, commuting_corrections, by = "TA") %>%
                     mutate(subtract = Earnings * commuting_correction, # note that this is now the proportion to subtract, not to leave
                     Earnings_latest = Earnings_latest - subtract)
  
        # Then we work out how much extra should be going to each city - column "to" -
        # to add up to what we just took away from various commuting suburbs / districts
           bonuses <- TAGDP.c %>% 
                      group_by(to, Year) %>%                               ## ie group by recipient TA
                      summarise(bonus = sum(subtract, na.rm = TRUE)) %>%   ## what's the dollar ammount it gets extra in this industry/year
                      rename(TA = to)
  
        # An annoying complication is that some people commute to TAs that are split over multiple regions...
        # So we need to calculate the *addition* of funds in terms of ratios too, of original earnings
           originals <- TAGDP.c %>%
                        group_by(TA, Year) %>%
                        summarise(Earnings = sum(Earnings))
  
           combined <- left_join(originals, bonuses) %>%
                       mutate(bonus = ifelse(is.na(bonus), 0, bonus)) %>%
                       mutate(bonus_ratio = bonus / Earnings,
                       bonus_ratio = ifelse(Earnings == 0, 0, bonus_ratio)) %>%
                       select(-Earnings)
        
           TAGDP.c <- TAGDP.c %>%
                      left_join(combined, by = c("TA", "Year")) %>%
                      mutate(
                        Earnings_latest = Earnings_latest + bonus_ratio * Earnings) ## add on the bonus it gets from commuters - 
                                                                                    #  a proportion of the *original* earnings amount
  
           # rename and drop unwanted variables
             TAGDP.c <- TAGDP.c %>%
                        mutate(Earnings_commuting_corrected = Earnings_latest) %>%
                        select(-(Earnings_latest:bonus_ratio))
  
           # None of the above should change total earnings other than rounding error so we do a check
             Diff <- (sum(TAGDP.c$Earnings_commuting_corrected) - sum(TAGDP.c$Earnings)) / sum(TAGDP.c$Earnings) * 100
                      print(paste("Cumulative impact of commuting correction so far on total earnings is", round(Diff, 2), "% (should be zero).")) 

                   if(abs(Diff) > 0.5){
                      stop("Commuting correction changed the total earnings by more than 0.5%")
                   }
  
           # Give a running tally of how much we're changing things
             Diff2 <- TAGDP.c %>%
                      filter(Earnings > 0) %>%
                      ungroup() %>%
                      summarise(value = mean(abs(Earnings_commuting_corrected - Earnings) / Earnings * 100))
                      print(paste("The average change so far between Earnings and Earnings_commuting_corrected is",
                      round(Diff2$value, 2), "per cent"))
                
        return(TAGDP.c)
      }

        
