##
##    Name:       plot_topTAs_by_industry.R
##
##    Objective:  This script plots Territorial Authority rankings for each
##                industry.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-18
##

##
##  1. Create industry list for looping
##
       allindustries <- unique(TAGDP_public$NGDP_industry)
       allindustries <- allindustries[order(allindustries)]
       allindustries <- allindustries [!is.na(allindustries)] # remove OOD for these plots - no LEED4 category

##
##  2. Output the figures to a single pdf
##
       CairoPDF("dissemination_outputs/topTAs_by_industry.pdf", 8, 8)
        for (i in 1: length(allindustries)){
         tmp <- TAGDP_public %>% filter(NGDP_industry == allindustries[i]) %>%
           group_by(Year, TA) %>%
           summarise(Employees = sum(Employees),
                     Earnings = sum(Earnings_commuting_corrected),
                     GDP = sum(GDP)) %>%
           filter(GDP >0) %>%
           ungroup()
    
         tmpAverage <- tmp %>%
           group_by(TA) %>%
           summarise(GDP = mean(GDP)) %>%
           data.frame()
  
         tmp$TA <- factor(tmp$TA, levels=tmpAverage$TA[order(tmpAverage$GDP)])
  
         biggest <- tmpAverage[order(-tmpAverage$GDP), "TA"][1:20]
         tmp <- subset(tmp, TA %in% biggest)
  
         print(
           ggplot(tmp, aes(y=TA, x=GDP, colour = as.numeric(Year))) +
           geom_point() +
           scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                  breaks = seq(from = 2000, to =2012, length.out =4)) +
           scale_x_continuous("\nContribution to GDP ($m)", label=dollar) +
           labs(y="") +
           ggtitle(paste(allindustries[i], "\nTop 20 Territorial Authorities"))
         ) 
  
       }
       dev.off()

