

head(leed37)
head(leed37_past)


evaluate_leed37 <- leed37_past %>%
                   left_join(leed37)

head(evaluate_leed37, 180)                   
tail(evaluate_leed37, 80)                   
                   
                   