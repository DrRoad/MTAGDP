
# ##
##  Programme:  create_figures.R
##
##  Objective:  Create figures for Summary and Methodology Reports for MTAGDP project.
##               
##  Approach:   Takes excerpts from outputs from testing_code and dissemination_code
##              to create graphics to include in the '/figures' folder later called by 
##              {knitr} and the compilation of the documents
##
##  Author:     Franz Smith, Sector Performance, MBIE
##
##  Date:       2015-06-26
##
##
##

##
## 1. Testing Code outputs
##

  # From compare_marginal_totals.R
# comparison with marginal totals of published Regional GDP
#  CairoPDF("figures/compare_marginal_totals.rgdp.pdf", 16, 11)
   CairoPNG("figures/compare_marginal_totals.rgdp.png", 6000, 6000, dpi=600)
     print(ggplot(comps0, aes(x=RGDP_industry, y = OutPerc / 100, colour = Year)) +
       geom_point() +
       facet_wrap(~RegionGDP) +
       coord_flip() +
       scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                               breaks = seq(from = 2000, to =2012, length.out =4)) +
       scale_y_continuous("Percentage out", label=percent, limits = c(-0.00001, 0.00001)) +
       theme(legend.position = c(0.9, 0.13)) +
       ggtitle("Differences from the RGDP marginal totals - published aggregates v1"))

    dev.off()

# comparison with marginal totals of published National GDP
#  CairoPDF("figures/compare_marginal_totals.ngdp.pdf", 16, 11)
   CairoPNG("figures/compare_marginal_totals.ngdp.png", 6000, 6000, dpi=600)
     print(ggplot(comps2, aes(x=NGDP_industry, y = OutPerc / 100, colour = Year)) +
        geom_point() +
        coord_flip() +
        scale_y_continuous("Percentage out", label=percent) +
        scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                               breaks = seq(from = 2000, to =2012, length.out =4)) +
        ggtitle("Differences from the NGDP marginal totals"))

      dev.off()

##
## 2. Dissemination Code outputs
##

  # from plot_industries_by_TA.R
    allTAs <- unique(TAGDP_public$TA)
    allTAs <- allTAs[order(allTAs)]
   
    CairoPDF("figures/OneTA.pdf", 8, 8)
       tmp <- TAGDP_public %>%
         filter(TA == allTAs[66]) %>%
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
                                  breaks = seq(from = 2000, to =2012, length.out =4)) +
           scale_x_continuous("\nContribution to GDP ($m)", label=dollar) +
           labs(y="") +
           theme( plot.title  = element_text(size = 18)) +
           ggtitle(paste(allTAs[66], "- top 20 industries"))
       ) 
  
     
     dev.off()
  
  ## from plot_TAs_by_industry.R
     allindustries <- unique(TAGDP_public$NGDP_industry)
     allindustries <- allindustries[order(allindustries)]
     allindustries <- allindustries [!is.na(allindustries)] # remove OOD for these plots - no LEED4 category
  
  
     CairoPDF("figures/OneDetailedIndustry.pdf", 8, 8)
      
       tmp <- TAGDP_public %>% filter(NGDP_industry == allindustries[56]) %>%
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
           ggtitle(paste(allindustries[56], "\nTop 20 Territorial Authorities"))
        ) 
  
     
     dev.off()  
    
  
  ## example sankey chart
  
   Pal1 <- brewer.pal(9, "Pastel1")
   Pal2 <- brewer.pal(8, "Pastel2")
  
     CairoPDF("figures/sankey_southlandConstruction.pdf",11, 9)
          par(family = "Calibri")

         the_region <- "Southland"
         the_ind <- "Construction"
    
         # Get the data we need for just this particular subset of industry and region
         sql <- paste0('select * from TAGDP_public where 
                 RGDP_industry = "', the_ind, '" and
                 Region = "', the_region, '"')
                     
         tagdp <- sqldf(sql)
    
    tmp <- tagdp %>%
      filter(Year == max(Year)) %>%
      group_by(Region, RGDP_industry, NGDP_industry, TA) %>%
      summarise(GDP = sum(GDP)) %>%
      ungroup()
    
    # First big node - the particular Region - Industry combination
    AllTot <- paste(unique(tmp$RGDP_industry), sum(tmp$GDP) %>% FormatDollars("m")) 
    
    # Second node - break down to TA level
    TATots <- tmp %>%
      group_by(TA) %>%
      summarise(GDP = sum(GDP)) %>%
      mutate(TA_Lab = paste(TA,  GDP %>% FormatDollars("m")))
    
    # Third node - break down to NGDP industry x TA
    TAIndTots <- tmp %>%
      select(-Region, -RGDP_industry) %>%
      mutate(col = Pal1[as.numeric(as.factor(NGDP_industry))]) %>%
      mutate(TAIndLab = paste(NGDP_industry, GDP %>% FormatDollars("m", 1))) %>%
      mutate(N1 = TA %>% factor() %>% rename.levels(orig = TATots$TA, new = TATots$TA_Lab) %>% as.character()) %>%
      rename(N2 = TAIndLab,
             Value = GDP) %>%
      filter(Value > 0) %>%
      select(N1, N2, Value, col) %>%
      arrange(N1, col)
    
    # wrapping parameter
    w <- 25
    
    # set the height of the second layer in the chart
    lo <- nrow(TATots)
    if(lo ==1){
      y2 <- 1.5
    } else {
      y2 <- seq(from = 0.5, to = 2.5, length.out = lo)  
    }
    
    if(nrow(TAIndTots) > nrow(TATots)){
      nodes <- data.frame(ID = c(AllTot, TATots$TA_Lab, TAIndTots$N2) %>% wrap(w), 
                          x = c(1, rep(2, nrow(TATots)), rep(3, nrow(TAIndTots))), stringsAsFactors = FALSE,
                          col = c(Pal2[1],
                                  Pal2[2:(nrow(TATots) + 1)],
                                  TAIndTots$col),
                          y= c(1.5, y2, (1:nrow(TAIndTots)) / nrow(TAIndTots) * 3))
           
      edges <- TATots %>%
        select(TA_Lab, GDP) %>%
        mutate(N1 = AllTot) %>%
        rename(N2 = TA_Lab, Value = GDP) %>%
        select(N1, N2, Value) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        rbind(TAIndTots[ , c("N1", "N2", "Value")]) %>%
        mutate(N1 = N1 %>% wrap(w),
               N2 = N2 %>% wrap(w))
      
    } else {
        
        # This segment draws the plot whenwe don't want the industries ie there's 
        # only one NGDP industry per RGDP industry.  So we don't use TAIndTots
        nodes <- data.frame(ID = c(AllTot, TATots$TA_Lab) %>% wrap(w), 
                            x = c(1, rep(2, nrow(TATots))), stringsAsFactors = FALSE,
                            col = c(Pal2[1],
                                    Pal2[2:(nrow(TATots) + 1)]),
                            y= c(1.5, y2))
             
        edges <- TATots %>%
          select(TA_Lab, GDP) %>%
          mutate(N1 = AllTot) %>%
          rename(N2 = TA_Lab, Value = GDP) %>%
          select(N1, N2, Value) %>%
          data.frame(stringsAsFactors = FALSE) %>%
          mutate(N1 = N1 %>% wrap(w),
                 N2 = N2 %>% wrap(w))
      
    }
    
    r <- makeRiver( nodes, edges)
    
    st <- default.style()
    st$srt <- "0"
     
    if (nrow(TATots) > 1 | nrow(TAIndTots) != nrow(TATots)){
      # we don't want a plot at all if there's only one node going to one node - looks silly.
      
      plot(r, default_style = st, plot_area = 0.9)
      grid.text(paste(the_region, the_ind), 0.5, 0.95,
                gp = gpar(fontfamily = "Calibri", fontface = "bold", fontsize = 17))
    }
    
    

dev.off()

##
## 3. Exploratory Maps & Miscellaneous Visualisations
##

  data(ta_simpl_gg)
  
  LastYear <- max(TAGDP_public$Year)
  
        TAGDP_sum <- TAGDP_public %>%
          group_by(TA, RGDP_industry, Year)  %>%
          summarise(GDP = sum(GDP)) %>%
          group_by(TA, RGDP_industry) %>%
          summarise(
             Latest_TAGDP = GDP[Year == LastYear],
             cagr               = CAGR(ratio = GDP[Year == LastYear] / GDP[Year == 2009],
                                    period = LastYear - 2009)                                                                                                                                                                                
             ) %>%
        # filter(RGDP_industry == "Rental, Hiring and Real Estate Services") %>%
          filter(RGDP_industry == "Wholesale Trade") %>%
          ungroup()

   ## Merge the taGDP with the TA map
        combined    <- merge(ta_simpl_gg, TAGDP_sum, all.x = TRUE, by.x = "FULLNAME", by.y = "TA")
        latest_only <- unique(combined [ , c("NAME", "long.centre", "lat.centre", "Latest_TAGDP", "RGDP_industry")])

    
   ## Sort the industries for organising the pdf output
        rgdp.ind <- sort(unique(TAGDP_grunt$RGDP_industry))

        CairoPDF("figures/map_selected_industry.pdf", 7, 7)
           
              TAGDP.Map <- ggplot() +
                  geom_polygon(data = combined, aes(x=long, y=lat, group=group, fill = cagr / 100), 
                         colour="grey75", size = 0.2) + # size refers to linewidth
                   geom_point(data = latest_only, aes(x=long.centre, y = lat.centre, size = Latest_TAGDP), shape = 1) +
                          theme_nothing(9, base_family = "Calibri") +
                          scale_size(paste0("MTAGDP ", LastYear, ", $m"), label=comma, range = c(1, 10)) +
                          scale_fill_gradientn(paste0("Average growth 2009 to ", LastYear) %>% wrap(18), 
                              colours=brewer.pal(9, "Spectral"),
                              label = percent, 
                              limits = c(-1, 1) * max(abs(combined$cagr) / 100)) + # to make the scale symettrical around zero
                          coord_map() +
                          ggtitle("Estimated growth and absolute size of GDP for Wholesale Trade")
              print(TAGDP.Map)
                
         dev.off()
  
##
## 4. Net impact of commuting correction
##


#CairoPDF("figures/net_impact_commuting_correction.pdf", 11, 8)
CairoPNG("figures/net_impact_commuting_correction.png", 6000, 6000, dpi=600)
  print(TAGDP_grunt %>%
        group_by(TA) %>%
        summarise(Original            = sum(Earnings),
                  Commuting_corrected = sum(Earnings_commuting_corrected),
                  Ratio               = Commuting_corrected / Original) %>%
        arrange(Ratio) %>%
       mutate(TA = factor(TA, levels = TA)) %>%
       ggplot(aes(y = TA, x = Ratio)) +
             geom_point() +
             ggtitle("Net impact of commuting correction")
   )
dev.off()
  
  
  
  


