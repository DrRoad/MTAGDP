##
##    Name:       troubleshoot_incompatible_population_data.R
##
##    Objective:  After calculating MTAGDP, tests for differences in the marginal totals can be
##                a useful indicator of the accuracy in the modelled estimates.  As there are a
##                number of different versions of GDP and industry tables in the LEED, there are
##                individual tests to validate between these different sources used in the stages
##                of calculating MTAGDP
##
##   Approach:    Comparisons between tables are made and individual tests are printed to the
##                 screen.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2014-08-10
##

print("Printing the most problematic conflicts between different sets of marginal population totals")

#=======GDP===========

#----------------public RGDP v NGDP-------------
# ok, what if the RGDP figures are fundamentally not compatible with the NGDP ones for some year - 
# industry combinations.  How to check this?

# I make a little concordance of the two
tmp_conc <- industries %>%
  select(RGDP_industry, NGDP_industry) %>%
  unique() 

# make a national GDP object with RGDP categories
comparison <- ngdp_pop %>%
  left_join(tmp_conc) %>%
  group_by(RGDP_industry, Year) %>%
  summarise(NGDP = sum(Freq)) %>%
  left_join(rgdp_pop_pub) %>%
  rename(RGDP = Freq) %>%
  ungroup() %>%
  group_by(RGDP_industry, Year) %>%
  summarise(NGDP = mean(NGDP),
            RGDP = sum(RGDP)) %>%
  mutate(Difference = RGDP - NGDP,
         DiffPercent = Difference / NGDP * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(30) %>%
  print()

# Everything is less than 1% different.

#---------------------------The custom RGDP v. the public RGDP industry margins-------------------

tmp_conc <- industries %>%
  select(RGDP_industry, RGDPIndustry_custom) %>%
  unique() 

comparison <- rgdp_pop_custom %>%
  left_join(tmp_conc) %>%
  group_by(RGDP_industry, Year) %>%
  summarise(Custom = sum(Freq)) %>%
  left_join(rgdp_pop_pub) %>%
  rename(RGDP = Freq) %>%
  ungroup() %>%
  group_by(RGDP_industry, Year) %>%
  summarise(Custom = mean(Custom),
            RGDP = sum(RGDP)) %>%
  mutate(Difference = RGDP - Custom,
         DiffPercent = Difference / Custom * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(30) %>%
  print() # OK


#-------------------------The custom RGDP v. the public RGDP industry x Region margins----------------------
tmp_conc <- industries %>%
  select(RGDP_industry, RGDPIndustry_custom) %>%
  unique() 

comparison <- rgdp_pop_custom %>%
  left_join(tmp_conc) %>%
  group_by(RGDP_industry, RegionGDP, Year) %>%
  summarise(Custom = sum(Freq)) %>%
  left_join(rgdp_pop_pub) %>%
  rename(RGDP = Freq) %>%
  ungroup() %>%
  group_by(RGDP_industry, RegionGDP, Year) %>%
  summarise(Custom = mean(Custom),
            RGDP = sum(RGDP)) %>%
  mutate(Difference = RGDP - Custom,
         DiffPercent = Difference / Custom * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(30) %>%
  print() # Not OK


#---------The high level public RGDP v low level public RGDP-----------
tmp_conc <- read.csv("data_raw/concordances/RegionIndustryRGDP15.csv")

comparison <- rgdp_pop_pub_det %>%
  left_join(tmp_conc) %>%
  group_by(RGDP_industry, RegionGDP, Year) %>%
  summarise(Detailed = sum(Freq)) %>%
  left_join(rgdp_pop_pub) %>%
  rename(HighLevel = Freq) %>%
  mutate(Difference = Detailed - HighLevel,
         DiffPercent = Difference / HighLevel * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(30) %>%
  print()


#---------------------------The custom RGDP v. the public NGDP-------
tmp_conc <- industries %>%
  select(NGDP_industry, RGDPIndustry_custom) %>%
  unique() 

# make a national GDP object with RGDP categories
comparison <- ngdp_pop %>%
  left_join(tmp_conc) %>%
  group_by(RGDPIndustry_custom, Year) %>%
  summarise(NGDP = sum(Freq)) %>%
  left_join(rgdp_pop_custom) %>%
  rename(rgdp_custom = Freq) %>%
  ungroup() %>%
  group_by(RGDPIndustry_custom, Year) %>%
  summarise(rgdp_custom = sum(rgdp_custom),
            NGDP = mean(NGDP)) %>%
  mutate(Difference = NGDP - rgdp_custom,
         DiffPercent = Difference / rgdp_custom * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(30) %>%
  print()

# OK 

#========================LEED totals=======================
# These are less important because in the end we're interested in GDP.  But it's important to 
# work out why things look so different in some industries in the different LEED tables. 
# This is mostly about error checking

# The big thing was Public administration and safety - $2b more in LEED4 than LEED18. This turned
# out to be an error in the data download - Central Govt etc had been clicked twice.  A fix has
# been put in the script for now; down the track we'll want to do it all properly from the database
# once it can speak properly to NZ.Stat data.

tmp_conc <- industries %>%
  select(LEED4Industry, LEED18Industry) %>%
  unique() 

# make a national GDP object with RGDP categories
comparison <- leed4_pop %>%
  left_join(tmp_conc) %>%
  group_by(LEED18Industry, Year) %>%
  summarise(LEED4 = sum(Freq)) %>%
  left_join(leed18_pop) %>%
  rename(LEED18 = Freq) %>%
  ungroup() %>%
  group_by(LEED18Industry, Year) %>%
  summarise(LEED18 = sum(LEED18), # LEED18 is the one with regions so need to add them up
            LEED4 = mean(LEED4)) %>%
  mutate(Difference = LEED18 - LEED4,
         DiffPercent = Difference / LEED4 * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(10) %>%
  print()


#----------------
tmp_conc <- TAGDP_grunt %>%
  select(LEED18Region, TA_Region_modified) %>%
  unique() 

# make a national GDP object with RGDP categories
comparison <- leed37_pop %>%
  left_join(tmp_conc) %>%
  group_by(LEED18Region, Year) %>%
  summarise(LEED37 = sum(Freq)) %>% 
  left_join(leed18_pop) %>% 
  rename(LEED18 = Freq) %>%
  ungroup() %>%
  group_by(LEED18Region, Year) %>%
  summarise(LEED37 = mean(LEED37), # LEED37 is the one with only one entry per TA_Region_modified
            LEED18 = sum(LEED18)) %>%
  mutate(Difference = LEED37 - LEED18,
         DiffPercent = Difference / LEED18 * 100) %>%
  ungroup() %>%
  arrange(-abs(DiffPercent))

comparison %>%
  data.frame() %>%
  head(100) %>%
  print()

# Bay of Plenty consistently 8% higher in LEED18; Otago 5% higher; Waikato and Hawke's Bay 3% lower
# The fundamental problem is probably from how we allocate some TA earnings over regions, which must
# be different to how SNZ do it.


