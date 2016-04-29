##
##    Name:       save_shiny_data.R
##
##    Objective:  This script prepares copies of the data that are used in the deployment
##                of the shinyapp.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-14
##
##    Modified December 2015 to add the two forecast total years

##
## ===============Facts====================
##

#-------------------------
  tmp <- TAGDP_public %>%
         group_by(NGDP_industry, Year, TA) %>%
         summarise(
           GDP = sum(GDP),
           GDP_real = sum(GDP_real)
         ) %>%
         rename(Industry = NGDP_industry) %>%
         mutate(Type = "NGDP") 

  averages <- tmp %>%
              group_by(Year, Industry)  %>%
              summarise(GDP_tot = sum(GDP)) %>%
              select(-Year)

  tagdp2 <- left_join(tmp, averages) %>%
            arrange(Year, GDP_tot) %>%
            ungroup()

#-------------
  tmp <- TAGDP_public %>%
         group_by(RGDP_industry, Year, TA) %>%
         summarise(
          GDP = sum(GDP),
          GDP_real = sum(GDP_real)
          ) %>%
          rename(Industry = RGDP_industry) %>%
          mutate(Type = "RGDP") 

  averages <- tmp %>%
              group_by(Year, Industry)  %>%
              summarise(GDP_tot = sum(GDP)) %>%
              select(-Year)

  tagdp3 <- left_join(tmp, averages) %>%
            arrange(Year, GDP_tot) %>%
            ungroup()

   tagdp_shiny <- rbind(tagdp2, tagdp3)

AllNZTotals <- tagdp_shiny %>%
  group_by(Industry, Year, Type) %>%
  summarise(GDP = sum(GDP),
            GDP_real = sum(GDP_real),
            GDP_tot = mean(GDP_tot)) %>%
  mutate(TA = "All New Zealand")
  
tagdp_shiny <- rbind(tagdp_shiny, AllNZTotals)

#-------------------
  AllNZTotals2 <- mtagdp_totals %>%
    group_by(Year) %>%
    summarise(GDP = sum(GDP), GDP_real = sum(GDP_real)) %>%
    mutate(TA = "All New Zealand")

  TheTotals <- mtagdp_totals %>%
    group_by(Year, TA) %>%
    summarise(GDP = sum(GDP), 
              GDP_real = sum(GDP_real)) %>%
    rbind(AllNZTotals2) %>%
    arrange(Year, TA) %>%
    ungroup()
##
## =====================Dimensions=================
##

## We need the data frame we usually use to map TAs as it has the centres of each TA
# first check if mbiemaps package is available or load local copy
if(("mbiemaps" %in% installed.packages()[, "Package"])) {
	library(mbiemaps)
   data(ta_simpl_gg)
} else {
	load("data/ta_simpl_gg.rda")
}

# some environments have a corrupt version of ta_simpl_gg with bad FULLNAMEs so we need to check and fix
  ta_simpl_gg <- ta_simpl_gg %>%
                 mutate(FULLNAME = gsub(" District District", " District", FULLNAME))

  TAs_ordered <- ta_simpl_gg %>%
                 select(FULLNAME, lat.centre) %>%
                 distinct() %>%
                 arrange(-lat.centre)
  TAs <- c(TAs_ordered$FULLNAME, "All New Zealand")

  TAs_short <- gsub(" District", "", TAs)
  TAs_short <- gsub(" City", "", TAs_short)
  TAs_short <- data_frame(TA = TAs_short, Sequence = (1:length(TAs_short)))

  Industries_NGDP <- unique(tagdp2$Industry)
  Industries_RGDP <- organise_rgdps(tagdp3, class_name = "Industry") %>% unique() %>% levels()

  Types <- data.frame(code = unique(tagdp_shiny$Type)[2:1],     # want RGDP as the first so reverse alphabetical order
                     label = c("High level (recommended)", "Detailed (caution)"),
                     stringsAsFactors = FALSE)

##
## ===============colours===================
##
  set.seed(123)
  ta_cols <- data_frame(TA = c(TAs, "Black"), TA_short = c(TAs_short$TA, "Black"),
                        Col = c(sample(colorRampPalette(mbie.cols())(length(TAs))), "Black"))

ta_cols[ta_cols$TA == "All New Zealand", "Col"] <- "Black"

##
## =====================save===============
##
  save(TAs, TAs_short, Industries_NGDP, Industries_RGDP, Types, file = "shiny/dimensions.rda")
  save(ta_pops, file = "shiny/ta_pops.rda")     
  save(tagdp_shiny, TheTotals, file = "shiny/TAGDP_data.rda")
  save(ta_cols, file = "shiny/ta_cols.rda")
  save(Commentary, file = "shiny/Commentary.rda")

