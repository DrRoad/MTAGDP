# Generates commentary for use in the Shiny app.  For each TA it identifies the most distinctive industry,
# the largest in absolute terms, the fastest growing industry, and the three TAs that are closest to it
# in a notional five dimensional space created by the first five principal components of the TA x industry
# matrix.   The selected industries are done both for RGDP_industry and NGDP_industry.

#====================RGDP level==================
Props_nat <- TAGDP_public %>%
  group_by(RGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  mutate(GDPProp_nat = GDP / sum(GDP)) %>%
  select(-GDP)

CompAdv <- TAGDP_public %>%
  filter(Year == max(Year)) %>%
  group_by(TA, RGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  group_by(TA) %>%
  mutate(GDPProp = GDP / sum(GDP)) %>%
  left_join(Props_nat, by = "RGDP_industry") %>%
  mutate(CompAdv = GDPProp / GDPProp_nat) %>%
  filter(RGDP_industry != "GST on Production, Import Duties and Other Taxes")

Biggest <- CompAdv %>%
  group_by(TA) %>%
  filter(CompAdv == max(CompAdv)) %>%
  mutate(Commentary1 = paste0(
    "<p>Using this high level industry classification, ", TA, " is most <em>distinctive</em> for '", RGDP_industry, "', which at an estimated ",
    FormatDollars(GDP, digits = 0, endmark = "m"), " in ", max(TAGDP_public$Year), 
    " is ", round(GDPProp * 100), "% of GDP, compared to ",
    round(GDPProp_nat * 100), "% for New Zealand as a whole.</p>"
    ))

Biggest_abs <- CompAdv %>%
  group_by(TA) %>%
  filter(GDP == max(GDP)) %>%
  rename(BigAbsInd = RGDP_industry) %>%
  left_join(Biggest[ , c("TA", "RGDP_industry", "Commentary1")]) %>%
  mutate(Commentary2 = paste0("<p>'", BigAbsInd, "' is also the largest industry in <em>absolute</em> terms.</p>"),
         Commentary2 = ifelse(BigAbsInd == RGDP_industry,
                              Commentary2,
                              paste0(
                                "<p>The largest industry in <em>absolute</em> terms is '", BigAbsInd, "' with estimated total value-add of ",
                                FormatDollars(GDP, digits = 0, endmark = "m"), ", making it ",
                                round(GDPProp * 100), "% of GDP.</p>"
                                ))
    )

Fastest <- TAGDP_public %>%
  group_by(TA, RGDP_industry) %>%
  summarise(cagr5year = CAGR(ratio = sum(GDP[Year == max(Year)]) / sum(GDP[Year == max(Year - 5)]),
                             period = 5, digits = 3)
  ) %>%
  filter(cagr5year != Inf) %>%
  filter(cagr5year == max(cagr5year, na.rm = TRUE)) %>%
  mutate(cagr5year = round(cagr5year, 1)) %>%
  left_join(CompAdv[ , c("TA", "RGDP_industry", "GDP")], by = c("TA", "RGDP_industry")) %>%
  rename(FastestIndustry = RGDP_industry,
         FastestIndustryGDP = GDP)

Commentary_rgdp_df <- Biggest_abs %>%
  left_join(Fastest, by = "TA") %>%
  mutate(
    Commentary3 = paste0(
    "<p>The <em>fastest growing</em> industry in ", TA, " is '",
    FastestIndustry, "' which grew ",
    round(cagr5year), "% per year for five years to reach ",
    FormatDollars(FastestIndustryGDP, endmark = "m"), " in ", max(TAGDP_public$Year), ".</p>"
    ),
    Commentary_rgdp = paste(Commentary1, Commentary2, Commentary3, sep = "  ")) 

#=========NGDP level============
Props_nat <- TAGDP_public %>%
  group_by(NGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  mutate(GDPProp_nat = GDP / sum(GDP)) %>%
  select(-GDP)

CompAdv <- TAGDP_public %>%
  filter(Year == max(Year)) %>%
  group_by(TA, NGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  group_by(TA) %>%
  mutate(GDPProp = GDP / sum(GDP)) %>%
  left_join(Props_nat, by = "NGDP_industry") %>%
  mutate(CompAdv = GDPProp / GDPProp_nat) %>%
  filter(NGDP_industry != "GST on Production, Import Duties and Other Taxes")

Biggest <- CompAdv %>%
  group_by(TA) %>%
  filter(CompAdv == max(CompAdv)) %>%
  mutate(Commentary1 = paste0(
    "<p>Using this granular industry classification, ", TA, " is most <em>distinctive</em> for '", NGDP_industry, "', which at an estimated ",
    FormatDollars(GDP, digits = 0, endmark = "m"), " in ", max(TAGDP_public$Year), 
    " is ", round(GDPProp * 100), "% of GDP, compared to ",
    round(GDPProp_nat * 100), "% for New Zealand as a whole.</p>"
  ))

Biggest_abs <- CompAdv %>%
  group_by(TA) %>%
  filter(GDP == max(GDP)) %>%
  rename(BigAbsInd = NGDP_industry) %>%
  left_join(Biggest[ , c("TA", "NGDP_industry", "Commentary1")]) %>%
  mutate(Commentary2 = paste0("<p>'", BigAbsInd, "' is also the largest industry in <em>absolute</em> terms.</p>"),
         Commentary2 = ifelse(BigAbsInd == NGDP_industry,
                              Commentary2,
                              paste0(
                                "<p>The largest industry in <em>absolute</em> terms is '", BigAbsInd, "' with estimated total value-add of ",
                                FormatDollars(GDP, digits = 0, endmark = "m"), ", making it ",
                                round(GDPProp * 100), "% of GDP.</p>"
                              ))
  )

Fastest <- TAGDP_public %>%
  group_by(TA, NGDP_industry) %>%
  summarise(cagr5year = CAGR(ratio = sum(GDP[Year == max(Year)]) / sum(GDP[Year == max(Year - 5)]),
                             period = 5, digits = 3)
  ) %>%
  filter(cagr5year != Inf) %>%
  filter(cagr5year == max(cagr5year, na.rm = TRUE)) %>%
  mutate(cagr5year = round(cagr5year, 1)) %>%
  left_join(CompAdv[ , c("TA", "NGDP_industry", "GDP")], by = c("TA", "NGDP_industry")) %>%
  rename(FastestIndustry = NGDP_industry,
         FastestIndustryGDP = GDP)

Commentary_ngdp_df <- Biggest_abs %>%
  left_join(Fastest, by = "TA") %>%
  mutate(
    Commentary3 = paste0(
      "<p>The <em>fastest growing</em> industry in ", TA, " is '",
      FastestIndustry, "' which grew ",
      round(cagr5year), "% per year for five years to reach ",
      FormatDollars(FastestIndustryGDP, endmark = "m"), " in ", max(TAGDP_public$Year), ".</p>
      <p>Estimated GDP using this detailed industry classification needs to be treated with particular caution.</p>"
    ),
    Commentary_ngdp = paste(Commentary1, Commentary2, Commentary3, sep = "  ")) 


#==================="closest" district/city=======================
industry_shares <- TAGDP_public %>%
  filter(Year == max(Year)) %>%
  group_by(TA, NGDP_industry) %>%
  summarise(GDP = sum(GDP)) %>%
  group_by(TA) %>%
  mutate(GDP = GDP / sum(GDP)) %>%
  spread(NGDP_industry, GDP, fill = 0)



mod <- princomp(industry_shares[, -1])
industry_reduced <- data.frame(mod$scores[ , 1:5])
row.names(industry_reduced) <- industry_shares$TA

closest <- dist(industry_reduced) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(From = row.names(.)) %>%
  gather(To, Distance, -From) %>%
  group_by(From) %>%
  arrange(Distance) %>%
  filter(
    From != To,
    Distance <= Distance[4])

commentary_closest <- data_frame(TA = unique(closest$From))
for(i in 1:nrow(commentary_closest)){
  tmp <- closest %>%  
    filter(From == commentary_closest[i, "TA"]) %>%
    mutate(To = as.character(To))
  commentary_closest[i, "closest"] <- paste0(
    "<p>The authorities with an industry profile closest to ",
    commentary_closest[i, "TA"],
    " are ", tmp[1, "To"], ", ", tmp[2, "To"], " and ", tmp[3, "To"], ".</p>"
    )
}



#===================Combine the various bits of commentary========================
Commentary <- Commentary_rgdp_df %>%
  left_join(Commentary_ngdp_df[ , c("TA", "Commentary_ngdp")]) %>%
  left_join(commentary_closest)



