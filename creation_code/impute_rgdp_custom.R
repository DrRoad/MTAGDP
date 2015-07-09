##
##  Programme:  impute_rgdp_custom.R
##
##  Objective:  As the custom Regional GDP data provided by Statistics New Zealand contianed a
##              number of 'confidentialised' cells, this script imputes the missing values based
##              on public versions of Regional and National GDP.
##
##  Approach:   Iteratively Proportional Fitting (IPF, or 'raking') was used to estimate values for
##              the missing cells by using the marginal totals from public Regional and National GDP
##              aligned for each industry.  As there are a small range of solutions based on these
##              constraints, this method provides a reasonable estimate for the missing cells.
##               Adjusted "population" objects are retained for future computations & testing of MTAGDP
##               results.
##
##  Author:     Peter Ellis, Sector Performance, MBIE
##
##  Date:       2015-04-23
##

##
##  1. ================Define holes==============
##

#------------For Regions-------------
# Total by Region from the public figures
tmp <- rgdp_pop_pub %>%
  group_by(Year, RegionGDP, RGDP_industry) %>%
  summarise(Freq = sum(Freq))

tmp_conc <- unique(industries[ , c("RGDP_industry", "RGDPRef_custom")])

rgdp_custom_mod <- rgdp_custom_orig %>%
  left_join(tmp_conc)

# combine that with the total by region from the custom data and calculate differences
reg_holes <- rgdp_custom_mod  %>%
  group_by(Year, RegionGDP, RGDP_industry) %>%
  summarise(Custom = sum(Freq, na.rm = TRUE)) %>%
  left_join(tmp) %>%
  mutate(Missing = Freq - Custom) 

if(sum(reg_holes$Freq) != sum(rgdp_pop_pub$Freq)){
  stop("something went wrong in the regional imputation")
}

#---------------For Industries-------------
# Total by custom RGDP industry by year in the published NGDP
tmp1 <- ngdp_pop %>%
  left_join(unique(industries[ , c("NGDP_industry", "RGDPRef_custom")])) %>%
  group_by(Year, RGDPRef_custom) %>%
  summarise(Freq = sum(Freq))

# Total by custom RGDP industry by year in the custom RGDP
tmp2 <- rgdp_custom_mod %>%
      group_by(Year, RGDPRef_custom) %>%
      summarise(Custom=sum(Freq, na.rm = TRUE))

# Combine them and calculate the differences
ind_holes <- tmp2 %>%
  left_join(tmp1) %>%
  mutate(Missing = Freq - Custom) 

if(sum(ind_holes$Freq) != sum(ngdp_pop$Freq)){
  stop("something went wrong in the industry imputation")
}


if(!(nrow(tmp1) == nrow(tmp2) ) & nrow(tmp1) == nrow(ind_holes)){
  stop("something went wrong with the concordance in imputing industry 'holes'")
}


##
## 2.-----------Identify problematic areas - where the 'missing' is negative---------------
##
cat("Printing out the problematic areas where the custom data already exceeds the permissable:\n")

ind_holes %>%
  filter(Missing < -1) %>%
  arrange(Missing) %>%
  print()

write.csv(ind_holes %>% ungroup() %>% filter(Missing < -1) %>% arrange(Missing),
          file = "data/industry_problems_RGDPCustomTooBigForNGDPPublished.csv", row.names = FALSE)

reg_holes %>%
  filter(Missing < -1) %>%
  arrange(Missing) %>%
  print()

##
## 3. ------------------Check the totals match-------------
##
ind_holes <- ind_holes %>%
  mutate(Missing = ifelse(Missing < 0, 0, Missing))

reg_holes <- reg_holes %>%
  mutate(Missing = ifelse(Missing < 0, 0, Missing))


YearTotalsInd <- ind_holes %>%
  group_by(Year) %>%
  summarise(MissingInd = sum(Missing, na.rm=TRUE)) 

YearTotalsReg <- reg_holes %>%
  group_by(Year) %>%
  summarise(MissingReg = sum(Missing, na.rm=TRUE)) 

# worth checking this next object looks ok - Adjust should be very close to 1
combined <- merge(YearTotalsInd, YearTotalsReg, by="Year") %>%
  mutate(Adjust = MissingReg / MissingInd)


# put the objects into the shape they need to be to be population totals
# for raking
ind_holes <- ind_holes %>%
  select(Year, RGDPRef_custom, Missing) %>%
  left_join(combined[ , c("Year", "Adjust")]) %>%
  mutate(Missing = Missing * Adjust) %>%
  select(-Adjust) %>%
  rename(Freq = Missing)

reg_holes <- reg_holes %>%
  select(Year, RegionGDP, RGDP_industry, Missing) %>%
  rename(Freq = Missing)

##
## 4. -----------Restrict the population objects to just those with missing values-----------
##

the_missing_dims <- unique(rgdp_custom_mod[!complete.cases(rgdp_custom_mod), 
                                            c("Year", "RegionGDP", "RGDPRef_custom", "RGDP_industry")])

ind_holes <- ind_holes %>%
  right_join(the_missing_dims[ , c("Year", "RGDPRef_custom")]) %>%
  unique() %>%
  mutate(Freq = ifelse(Freq < 1, 1, Freq))

reg_holes <- reg_holes %>%
  right_join(the_missing_dims[ , c("Year", "RegionGDP", "RGDP_industry")]) %>%
  unique()

##
## 5.--------------------Create a solution that fills all the holes----------------
##

missing_fill <- rgdp_custom_mod[!complete.cases(rgdp_custom_mod), ] %>%
  mutate(Freq = 1) %>%
  arrange(RegionGDP)

missing_fill_svy <- svydesign(~1, data = missing_fill, weights = ~Freq)

missing_fill_svy <- rake(missing_fill_svy, 
                  sample.margins = list (~Year + RegionGDP + RGDP_industry, 
                                         ~Year + RGDPRef_custom),
                  population.margins = list(reg_holes, 
                                            ind_holes),
                  control = list(maxit = 100, epsilon = 1, verbose=FALSE))

missing_fill$Freq <- weights(missing_fill_svy)

rgdp_pop_custom <- rbind(rgdp_custom_mod[complete.cases(rgdp_custom_mod), ], missing_fill) %>%
  ungroup() 

tmp <- left_join(rgdp_pop_custom, unique(industries[ , c("RGDPRef_custom", "RGDPIndustry_custom")]))
if(nrow(tmp) != nrow(rgdp_pop_custom)){
  stop("something went wrong when adding in the labels of RGDPIndustry_custom")
} else {
  rgdp_pop_custom <- tmp
  rm(tmp)
}

##
## 5. Final scaling of missing values
##

#-------------------------
# note - at this point rgdp_pop_custom$Freq adds up to slightly more than the other 3 GDP pop objects
#        because of all the small amounts where the missing data was less than zero.  So we scale it down
#        very slightly to help the convergence later on.

rgdp_pop_custom_svy <- svydesign(~1, weights = ~Freq, data = rgdp_pop_custom)
rgdp_pop_custom_svy <- rake(rgdp_pop_custom_svy,
                            sample.margins = list(~Year + RegionGDP + RGDP_industry),
                            population.margins = list(rgdp_pop_pub))

rgdp_pop_custom$Freq <- weights(rgdp_pop_custom_svy)
     
