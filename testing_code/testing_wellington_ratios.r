##
##    Name:       testing_wellington_ratios.R
##
##    Objective:  Provides a visual assessment of the ratio of GDP to earnings from the LEED tables
##                after the  'commuting correction' and to the original BDS numbers.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2014-10-06
##

  
# have a look at Wellington 
   tagdp_well <- TAGDP_grunt %>%
                 filter(Region == 'Wellington') %>%
                 group_by(RGDPIndustry_custom, TA) %>%
                 summarise(Employees = sum(Employees),
                           Earnings  = sum(Earnings_commuting_corrected),
                           GDP       = sum(GDP)) %>%
                 mutate(EmpRatio  = GDP / Employees * 1000,
                        EarnRatio = GDP / Earnings  * 10 ^ 6)
  
   png("testing_outputs/wellington_emp_ratio.png", 9000, 6000, res = 600)
     print(tagdp_well %>%
          filter(!is.na(EmpRatio)) %>% # exclude OOD
          ggplot(aes(y=TA, x = EmpRatio, colour = TA)) +
          geom_point(size = 5, shape=1) +
          facet_wrap(~RGDPIndustry_custom, scales = "fixed") +
          scale_colour_brewer(palette = "Set1", guide = "none") +
          labs(x = "Ratio of GDP to original BDS employee numbers"))
   dev.off()

  png("testing_outputs/wellington_earn_ratio.png", 9000, 6000, res = 600)
  print(tagdp_well %>%
    filter(!is.na(EarnRatio)) %>% # exclude OOD
    ggplot(aes(y=TA, x = EarnRatio, colour = TA)) +
    geom_point(size = 5, shape=1) +
    facet_wrap(~RGDPIndustry_custom, scales = "free_x") +
    scale_colour_brewer(palette = "Set1", guide = "none") +
    labs(x = "Ratio of GDP to LEED earnings after 'commuting correction'"))
  dev.off()

  ## placeholder for summaryReport media
  CairoPDF("figures/wellington_earn_ratio.pdf", 9, 6)
     print(tagdp_well %>%
           filter(!is.na(EarnRatio)) %>% # exclude OOD
           ggplot(aes(y=TA, x = EarnRatio, colour = TA)) +
           geom_point(size = 5, shape=1) +
           facet_wrap(~RGDPIndustry_custom, scales = "free_x") +
           scale_colour_brewer(palette = "Set1", guide = "none") +
           labs(x = "Ratio of GDP to LEED earnings after 'commuting correction'"))
  dev.off()
  
   