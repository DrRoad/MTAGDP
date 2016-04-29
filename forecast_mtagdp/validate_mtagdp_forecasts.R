##
##  Programme:  validate_mtagdp_forecasts.R
##
##  Objective:  Create visuals & diagnostics for the mtagdp forecasts
##
##  Approach:   Output from the forecasts/raking are compared with the published figures and other
##              sanity checks.
##
##  Author:     Franz Smith, Sector Performance, Ministry of Business, Innovation and Employment
##
##  Date:       2015-12-10
##
##  Reviewed and refactored Peter Ellis 2015-12-17


##  Notes:      3. accuracy.gts{hts} routines also to include 


##
##  1. Create the visuals
##
   # first look at the ta-level predictions
     p2 <- ggplot(mtagdp_totals, aes(Year, GDP)) +
           geom_line(aes(colour = TA, linetype = notes), alpha = 0.7) +
            geom_point(size = 1, aes(colour=TA, shape = notes)) +
           facet_wrap(~Region, scales = "free") +
          theme_bw() +
          ggtitle("MTAGDP predictions") +
          ylab("GDP") +
          theme(plot.title = element_text(size=16, vjust=1)) +
          theme(axis.text.x=element_text(angle=90)) +
          theme(legend.position = "none") +
          scale_x_continuous(breaks = c(2000, 2004, 2008, 2012))       
    
   CairoPDF("testing_outputs/mtagdp_forecasts.pdf", 7, 7)
     print(p2)
   dev.off()  

     
##
##  3. Processing the published values & forecast output
##
# this compares the forecast_grunt$GDP_raked to the published population totals
     
     p4 <- forecast_grunt %>%
       group_by(Year, RegionGDP) %>%
       summarise(GDP_rake = sum(GDP_rake)) %>%
       left_join(rgdp_pop) %>%
       mutate(Diff = Freq - GDP_rake) %>%
       ggplot(aes(x = Year, y = Diff)) +
       geom_line() +
       geom_point() +
       facet_wrap(~RegionGDP) +
       theme_bw() +
       ggtitle("Discrepancy in final regional totals to published RGD\nby RGDP region")
     
     
     
     CairoPDF("testing_outputs/compare_rake_gts.vs.publ.pdf", 7, 7)
       print(p4)
     dev.off()  

