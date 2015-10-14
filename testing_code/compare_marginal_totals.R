##
##    Name:       compare_marginal_totals.R
##
##    Objective:  Compare the marginal totals from the different data sources to the results
##                of the MTAGDP estimates - including National, Regional GDP and LEED tables.
##                The script produces output that is printed to the screen & combines visual 
##                outputs into a single PDF.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2014-10-06
##

#====================Compare to the marginal totals of the published GDP numbers==========

# high level
  comps0 <- TAGDP_grunt %>%
            group_by(RegionGDP, RGDP_industry, Year) %>%
            summarise(GDP=sum(GDP)) %>%
            left_join(rgdp_pop_pub) %>%
            ungroup() %>%
            mutate(OutPerc = ((GDP - Freq) / Freq) * 100)

# with the funny breakdown
  comps1 <- TAGDP_grunt %>%
            group_by(RegionGDP, RegionIndustryRGDP15, Year) %>%
            summarise(GDP=sum(GDP)) %>%
            left_join(rgdp_pop_pub_det) %>%
            mutate(OutPerc = ((GDP - Freq) / Freq) * 100) %>%
            ungroup()

# national
  comps2 <- TAGDP_grunt %>%
            group_by(NGDP_industry, Year) %>%
            summarise(GDP=sum(GDP)) %>%
            left_join(ngdp_pop) %>%
            mutate(OutPerc = ((Freq - GDP) / Freq) * 100) %>%
            ungroup()

  comps0 %>%
            arrange(-abs(OutPerc)) %>%
            head(10) %>%
            print() # ok

  comps1 %>%
            arrange(-abs(OutPerc)) %>%
            head(10) %>%
            print() # ok

  comps2 %>%
            arrange(-abs(OutPerc)) %>%
            head(10) %>%
            print() # ok

#======================Compare to marginal totals of custom RGDP provided by SNZ===================
# Note that we are comparing to the object rgdp_custom_orig, which has the original pre-imputation numbers

# This one doesn't come out very well but the problems are mainly where the amount is actually very very small.

  comps3 <- TAGDP_grunt %>%
            group_by(RegionGDP, RGDPRef_custom, Year) %>%
            summarise(GDP_final = sum(GDP)) %>%
            left_join(rgdp_custom_orig) %>%
            mutate(GDP_custom = Freq,
                   OutPerc = ((GDP_final - GDP_custom) / GDP_custom) * 100) %>%
            select(-Freq) %>%
            ungroup()
  
  comps3 %>%
            arrange(-abs(OutPerc)) %>%
            head(30) %>%
            data.frame() %>%
            print()

  comps3 %>%
            filter(GDP_custom >2) %>%
            arrange(-abs(OutPerc)) %>%
            head(30) %>%
            data.frame() %>%
            print()

#===========================Compare to LEED totals===============

#------------LEED4-----------------
  comps4 <- EarningsDetailed %>%
            group_by(LEED4Industry, Year) %>%
            summarise(Earnings = round(sum(Earnings))) %>%
            left_join(leed4_pop) %>%
            mutate(leed4_pop = Freq,
                   OutPerc = ((Earnings - leed4_pop) / leed4_pop) * 100) %>%
            select(-Freq) %>%
            ungroup()

  comps4 %>%
            arrange(-abs(OutPerc)) %>%
            head(30) %>%
            data.frame() %>%
            print()

#------------LEED18-----------------
  comps5 <- EarningsDetailed %>%
            group_by(LEED18Industry, LEED18Region, Year) %>%
            summarise(Earnings = round(sum(Earnings))) %>%
            left_join(leed18_pop) %>%
            mutate(leed18_pop = Freq,
                   OutPerc = ((Earnings - leed18_pop) / leed18_pop) * 100) %>%
            select(-Freq) %>%
            ungroup()

  comps5 %>%
            arrange(-abs(OutPerc)) %>%
            head(50) %>%
            data.frame() %>%
            print()


#------------LEED37-----------------
  comps6 <- EarningsDetailed %>%
            group_by(TA_Region_modified, Year) %>%
            summarise(Earnings = round(sum(Earnings))) %>%
            left_join(leed37_pop) %>%
            mutate(leed37_pop = Freq,
                   OutPerc = ((Earnings - leed37_pop) / leed37_pop) * 100) %>%
            select(-Freq) %>%
            ungroup()

  comps6 %>%
            arrange(-abs(OutPerc)) %>%
            head(30) %>%
            data.frame() %>%
            print()
  
# organise the NGDP industries
  comps2$NGDP_industry <- organise_ngdps(comps2)
  comps2$NGDP_industry <- factor(comps2$NGDP_industry, levels = rev(levels(comps2$NGDP_industry))) ## order needs to be reversed

# organise the RGDP industries
  comps0$RGDP_industry <- organise_rgdps(comps0)
  comps0$RGDP_industry <- factor(comps0$RGDP_industry, 
                                             levels = rev(levels(comps0$RGDP_industry))) ## reverse order

# organise the LEED4 industries
  comps4$LEED4Industry <- organise_leed4s(comps4)
  comps4$LEED4Industry <- factor(comps4$LEED4Industry, levels = rev(levels(comps4$LEED4Industry))) ## reverse for plotting

# create the graphical output
 CairoPDF("testing_outputs/compare_marginal_totals.pdf", 16, 11)
  print(ggplot(comps2, aes(x=NGDP_industry, y = OutPerc / 100, colour = Year)) +
          geom_point() +
          coord_flip() +
          scale_y_continuous("Percentage out", label=percent) +
          scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                 breaks = seq(from = 2000, to =2012, length.out =4)) +
          ggtitle("Differences from the NGDP marginal totals"))


## the only RGDP_industry categories that have been retained are 'owner-occupied property operation' and 'GST on production..'
  print(ggplot(comps0, aes(x=RGDP_industry, y = OutPerc / 100, colour = Year)) +
    geom_point() +
    facet_wrap(~RegionGDP) +
    coord_flip() +
      scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                             breaks = seq(from = 2000, to =2012, length.out =4)) +
    scale_y_continuous("Percentage out", label=percent) +
    theme(legend.position = c(0.9, 0.13)) +
    ggtitle("Differences from the RGDP marginal totals - published aggregates v1"))

  print(ggplot(comps1, aes(x=RegionIndustryRGDP15, y = OutPerc / 100, colour = Year)) +
          geom_point() +
          facet_wrap(~RegionGDP, nrow = 1) +
          coord_flip() +
          scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                 breaks = seq(from = 2000, to =2012, length.out =4)) +
          scale_y_continuous("Percentage out", label=percent) +
          ggtitle("Differences from the RGDP (detailed) marginal totals - published aggregates v2"))

  print(ggplot(comps1, aes(x = Freq, y = GDP)) +
          geom_abline(xintercept = 0, slop1 = 1, colour = "grey50") +
          facet_wrap(~RegionGDP) +
          geom_point() +
          scale_x_log10() + scale_y_log10() +
          ggtitle("Differences from the RGDP (detailed) marginal totals - published aggregates"))
      
  print(ggplot(comps3, aes(x=RGDPRef_custom, y = OutPerc / 100, colour = Year)) +
          geom_point() +
          facet_wrap(~RegionGDP) +
          coord_flip() +
          scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                 breaks = seq(from = 2000, to =2012, length.out =4)) +
          scale_y_continuous("Percentage out", label=percent) +
          theme(legend.position = c(0.9, 0.13)) +
          ggtitle("Differences from the RGDP marginal totals - custom detail (excluding imputed)"))

  print(comps3 %>%
      ggplot(aes(x = GDP_custom, y = GDP_final)) +
      facet_wrap(~RGDPRef_custom)   +
      geom_abline(xintercept = 0, slop1 = 1, colour = "grey50") +
      geom_point() +
        labs(x = "Original custom data provided by SNZ", y = "Final GDP result") +
        scale_x_log10() + scale_y_log10() +
      ggtitle("Differences from the RGDP marginal totals - custom detail (excluding imputed)"))



  print(ggplot(comps4, aes(x=LEED4Industry, y = OutPerc / 100, colour = Year)) +
          geom_point() +
          coord_flip() +
          scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                 breaks = seq(from = 2000, to =2012, length.out =4)) +
          scale_y_continuous("Percentage out", label=percent) +
          ggtitle("Differences from the LEED4 marginal totals"))

  print(ggplot(comps5, aes(x=LEED18Industry, y = OutPerc / 100, colour = Year )) +
          geom_point() +
          facet_wrap(~LEED18Region) +
          coord_flip() +
          scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                 breaks = seq(from = 2000, to =2012, length.out =4)) +
          scale_y_continuous("Percentage out", label=percent) +
          theme(legend.position = c(0.75, 0.13)) +
          ggtitle("Differences from the LEED18 marginal totals"))

  print(ggplot(comps6, aes(x=TA_Region_modified, y = OutPerc / 100, colour = Year)) +
          geom_point() +
          coord_flip() +
          scale_colour_gradientn("Year Ending March", colours = brewer.pal(10, "Spectral"), 
                                 breaks = seq(from = 2000, to =2012, length.out =4)) +
          scale_y_continuous("Percentage out", label=percent) +
          ggtitle("Differences from the LEED37 marginal totals"))
        
  dev.off()
  
