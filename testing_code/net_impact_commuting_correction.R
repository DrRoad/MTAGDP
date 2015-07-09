# This script gives a visual summary of what happened in all that commuter correction of the earnings.
# We expect to see dormitory suburbs with ratios less than 1, and urban centres with ratios above 1.

CairoPDF("testing_outputs/net_impact_commuting_correction.pdf", 11, 8)
print(TAGDP_grunt %>%
  group_by(TA) %>%
  summarise(Original = sum(Earnings),
            Commuting_corrected = sum(Earnings_commuting_corrected),
            Ratio = Commuting_corrected / Original) %>%
  arrange(Ratio) %>%
  mutate(TA = factor(TA, levels = TA)) %>%
  ggplot(aes(y = TA, x = Ratio)) +
    geom_point() +
    ggtitle("Net impact of commuting correction")
)
dev.off()
