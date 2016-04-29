##
##    Name:       plot_industries_by_region.R
##
##    Objective:  This script plots the industries by individual Regional Councils to
##                compare the industry rankings across regions of New Zealand.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-18
##

##
##  1. Create a list of the regions for the loop
##
       allRegions <- unique(TAGDP_public$Region)
       allRegions <- allRegions[order(allRegions)]

##
##  2. Output all regions to a single PDF
##
      CairoPDF("dissemination_outputs/top_industries_by_region.pdf", 8, 8)
         for (i in 1: length(allRegions)){
            tmp <- TAGDP_public %>%
                   filter(Region == allRegions[i]) %>%
                   group_by(Year, NGDP_industry) %>%
                   summarise(GDP = sum(GDP)) %>%
                   filter(GDP > 0) %>%
                   ungroup()
    
        tmpAverage <- tmp %>%
                      group_by(NGDP_industry) %>%
                      summarise(GDP = mean(GDP)) %>%
                      data.frame()
  
        tmp$NGDP_industry <- factor(tmp$NGDP_industry, levels = tmpAverage$NGDP_industry[order(tmpAverage$GDP)])
  
        biggest <- tmpAverage[order(-tmpAverage$GDP), "NGDP_industry"][1:20]
        tmp     <- subset(tmp, NGDP_industry %in% biggest)
  
        print(
           ggplot(tmp, aes(y=NGDP_industry, x=GDP, colour=as.numeric(Year))) +
             geom_point() +
             scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                  breaks = seq(from = 2000, to = 2012, length.out = 4)) +
             scale_x_continuous("\nContribution to GDP ($m)", label = dollar) +
             labs(y = "") +
             ggtitle(paste(allRegions[i], "- top 20 industries"))
          ) 
  
        }
      dev.off()

