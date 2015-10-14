##
##    Name:       plot_industries_by_TA.R
##
##    Objective:  This script plots the industries by individual Territorial Authorities
##                to compare the industry rankings across regions.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-18
##

##
##  1. Make a list of the Territorial Authorities for the loop
##

       allTAs <- unique(TAGDP_public$TA)
       allTAs <- allTAs[order(allTAs)]

##
##  2. Create a single PDF to contain graphs from individual TAs 
##
       CairoPDF("dissemination_outputs/topIndustries_by_TA.pdf", 8, 8)
        for (i in 1: length(allTAs)){
          tmp <- TAGDP_public %>%
                 filter(TA == allTAs[i]) %>%
                 group_by(Year, NGDP_industry) %>%
                 summarise(GDP = sum(GDP )) %>%
                 filter(GDP > 0) %>%
                 ungroup()
  
          tmpAverage <- tmp %>%
                        group_by(NGDP_industry) %>%
                        summarise(GDP = mean(GDP)) %>%
                        data.frame()
  
        tmp$NGDP_industry <- factor(tmp$NGDP_industry, levels=tmpAverage$NGDP_industry[order(tmpAverage$GDP)])
  
        biggest <- tmpAverage[order(-tmpAverage$GDP), "NGDP_industry"][1:20]
        tmp <- subset(tmp, NGDP_industry %in% biggest)
  
        print(
           ggplot(tmp, aes(y=NGDP_industry, x=GDP, colour=as.numeric(Year))) +
             geom_point() +
             scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                    breaks = seq(from = 2000, to = 2012, length.out = 4)) +
             scale_x_continuous("\nContribution to GDP ($m)", label = dollar) +
             labs(y ="") +
             theme( plot.title  = element_text(size = 18)) +
             ggtitle(paste(allTAs[i], "- top 20 industries"))
         ) 
  
       }
       dev.off()

