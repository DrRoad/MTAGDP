
#### Importing last years data
leed37_prev <- read.csv("data_raw/TABLECODE7037_Data_2fbd4042-a117-470a-a484-5c38c1033011.csv",
                        stringsAsFactors = FALSE) 
names(leed37_prev)[names(leed37_prev) == "Territorial.authority"] <- "LEED_TA"
leed37_prev <- leed37_prev %>%
  rename(ValuePrev = Value) %>%
  select(-Flags, -Measure)

Qtr_prev <- unique(leed37_prev$Quarter)


#### Importing this years data
leed37_tmp <- read.csv("data_raw/TABLECODE7037_Data_e5992295-96ec-494b-a801-5d6bf7131077.csv",
                   stringsAsFactors = FALSE) 
names(leed37_tmp)[names(leed37_tmp) == "Territorial.authority"] <- "LEED_TA"

leed37_tmp <- leed37_tmp %>%
              filter(Quarter %in% Qtr_prev) %>%
              select(-Measure, -Flags)

#### Comparing the values 
leed37diff <- leed37_prev %>%
              left_join(leed37_tmp) %>%
              mutate(Diff = Value - ValuePrev) %>%
              mutate(DiffPercent = (Diff/ValuePrev)*100) 

leed37ValueDiff <- leed37diff %>%
                  filter(!Diff==0)

leed37ValueDiffByTA <- leed37ValueDiff %>%
                        group_by(LEED_TA) %>%
                        summarise(TotalDiff = sum(Diff))

leed37ValueDiffByQTR <- leed37ValueDiff %>%
  group_by(Quarter) %>%
  summarise(TotalDiff = sum(Diff))
