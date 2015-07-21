CAGR <- function (ratio, period, digits = 1) {
  round((exp(log(ratio)/period) - 1) * 100, digits)
}

head(TAGDP_public)

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
  mutate(CompAdv = GDPProp / GDPProp_nat)

Biggest <- CompAdv %>%
  group_by(TA) %>%
  filter(CompAdv == max(CompAdv)) %>%
  mutate(Commentary1 = paste0(
    "<p>", TA, " is most distinctive for '", RGDP_industry, "', which at ",
    FormatDollars(GDP, digits = 0, endmark = "m"), " in ", max(TAGDP_public$Year), 
    " is ", round(GDPProp * 100), "% of GDP, compared to ",
    round(GDPProp_nat * 100), "% for New Zealand as a whole.</p>"
    ))

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

Commentary <- Biggest %>%
  left_join(Fastest, by = "TA") %>%
  mutate(
    Commentary2 = paste0(
    "<p>The fastest growing industry in ", TA, " is '",
    FastestIndustry, "' which grew ",
    round(cagr5year), "% per year for five years to reach ",
    FormatDollars(FastestIndustryGDP, endmark = "m"), " in ", max(TAGDP_public$Year), ".</p>"
    ),
    CommentaryBoth = paste(Commentary1, Commentary2, sep = "  ")) 
