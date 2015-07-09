##
##   Programme:  taGDP_lowIncomes.R
##
##   Objective:  Quick statistics from Minister's request on regional economic performance. Key interest
##               on stats for lower income regions (e.g. Far North, Opotiki, Horowhenua, et cetera)
##
##   Approach:   Take the latest TAGDP_public object, subset for the "lower income" Territorial
##               Authorities, compute some averages, and totals for national comparisons & create
##               comparative plots and export .cvs files for perusal. 
##
##
##   Author:     Franz Smith, Contractor, Sector Performance, MBIE
##
##   Date:       2015-06-02
##

##
##  1. Set up
##
   # call to functionality
     library(mbie)
     library(plyr)
     library(tidyr)
     library(extrafont)
     library(Cairo)
 
   # call to core data objects
     load("data/TAGDP_public.rda")  ## latest TAGDP object
     load("data/ta_pops.rda")      ## population data for Territorial Authorities

##
##  2. Combine population data & TAGDP
## 
     tagdp.comb <- TAGDP_public %>%
                   group_by(Year, TA) %>%
                   summarise(GDP_total       = sum(GDP),
                             Earnings_total  = sum(Earnings_commuting_corrected),
                             Employees_total = sum(Employees)) %>%
                   data.frame()

     tagdp.comb <- left_join(tagdp.comb, ta_pops, by=c("Year", "TA"))
     tagdp.comb <- tagdp.comb %>%
                    mutate(GDP_perCapita        = GDP_total/Population,
                           Earnings_perCapita   = Earnings_total/Population,
                           Earnings_perEmployee = Earnings_total/Employees_total) %>%
                   data.frame()       

   ## subset the lower income regions
      lower <- subset(tagdp.comb, TA %in% c("Far North District",
                                            "Opotiki District",
                                            "Kawerau District",
                                            "Gisborne District",
                                            "Horowhenua District",
                                            "Wanganui District",
                                            "Ruapehu District"))


   ## calculate the national averages
       # tagdp_ave <- tagdp.comb %>%
                    # group_by(Year) %>%
                    # summarise(GDP_perCapita_ave        = mean(GDP_perCapita),
                              # Earnings_perCapita_ave   = mean(Earnings_perCapita),
                              # Earnings_perEmployee_ave = mean(Earnings_perEmployee)) %>%
       #             data.frame()          

       tagdp_ave <- tagdp.comb %>%
                    group_by(Year) %>%
                    summarise(GDP_total            = mean(GDP_total),
                              GDP_perCapita        = mean(GDP_perCapita)) %>%
                    data.frame() 
    
       tagdp_ave$TA <- c("National Average")

   ## combine the objects
      lower <- rbind(lower[,c("Year","TA","GDP_total","GDP_perCapita")], tagdp_ave)

   ## put into a format for easier plotting
      lower.m <- lower %>%
                 gather(Variable, GDP, -Year, -TA) %>%
                 data.frame()

   ## order the regions
      lower.m$TA <- factor(lower.m$TA, levels=c("National Average",
                                                "Far North District", 
                                                "Opotiki District",
                                                "Kawerau District",
                                                "Gisborne District",
                                                "Ruapehu District",
                                                "Wanganui District",
                                                "Horowhenua District"
                                               ))

##
##  3. Create the outputs
##
   # the plot
     CairoPDF("exploratory_output/taGDP_lowIncomes.pdf", 7, 7)
        p <- ggplot(lower.m, aes(Year, GDP))
        p <- p + geom_line(aes(colour=factor(TA)), alpha=0.7)
        p <- p + facet_wrap(~Variable, scales="free")
       print(p)
     dev.off()


    # create the csvs for total GDP and per Capita
      lower_gdp.total <- lower %>%
                         select(-GDP_perCapita) %>%
                         spread(Year, GDP_total) %>%
                         data.frame()

       write.csv(lower_gdp.total, file="data_intermediate/lower_gdp.total.csv", row.names=FALSE)


       lower_gdp.perCapita <- lower %>%
                              select(-GDP_total) %>%
                              spread(Year, GDP_perCapita) %>%
                              data.frame()

       write.csv(lower_gdp.perCapita, file="data_intermediate/lower_gdp.perCapita.csv", row.names=FALSE)


















lower.aves <- left_join(lower, tagdp_ave, by=c("Year"))
head(lower.aves)



##
   # setting up some parameters for plotting
      base_size <- 9
      TheFont <- "Calibri"
   
   # identify the most recent data
        LastYear <- max(tagdp.comb$Year)

        tagdp.comb_sum <- tagdp.comb %>%
          group_by(Year, TA) %>%
          summarise(
             Latest_GDPperCapita = tagdp.comb[Year == LastYear],
             cagr                = CAGR(ratio = tagdp.comb[Year == LastYear] / tagdp.comb[Year == 2009],
                                    period = LastYear - 2009)                                                                                                                                                                                
             ) %>%
          data.frame()

   ## Merge the taGDP with the TA map
        combined    <- merge(ta_simpl_gg, TAGDP_sum, all.x=TRUE, by.x="FULLNAME", by.y="TA")
        latest_only <- unique(combined [ , c("NAME", "long.centre", "lat.centre", "Latest_TAGDP", "RGDP_industry")])








 

