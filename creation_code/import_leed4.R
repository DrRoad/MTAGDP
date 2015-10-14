##
##    Programme:  import_leed4.R
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.   These data are used as sample margins for 'raking' to the
##                employee numbers (i.e. weights) from the Business Demography Statistics.
##
##    Approach:   This programme reads data from the LEED table 4 and aggregates the values to year end
##                March.  'Year' is used again to match other data throughout the routine.
##
##    Authors:    Peter Ellis, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##                
##    Date:       2014-08-10
##

# important - This is table 4 with all quarters, all of the lowest level in the industry
#             hierarchy (but not other levels), and only one measure


  leed4 <- read.csv("data_raw/TABLECODE7004_Data_e7610f76-5579-49b3-b63e-9d13a339bd93.csv",
                    stringsAsFactors = FALSE)
  
# WARNING - a problem with the manual download means the Central Government Administration was in twice
  leed4 <- leed4 %>%
           mutate(Value = ifelse(Industry == "Central government administration", Value / 2, Value))

#============aggregate to YE March=================

  leed4$Year <- 2000 + as.numeric(substring(leed4$Quarter, 5, 6))

# fix the problems with the 1990s appearing as 2099 instead of 1999:
  leed4$Year <- with(leed4, ifelse(Year > 2080, Year - 100, Year))

  leed4$YEMar <- with(leed4, ifelse(substring(leed4$Quarter, 1, 3) == "Mar", Year, Year + 1))

  leed4 <- leed4 %>%
           group_by(Industry, YEMar) %>%
           summarise(TotalEarnings = sum(as.numeric(Value))) %>%
           rename(LEED4Industry = Industry)

#===========set up to use as population frame for a survey object===========
  leed4_pop <- leed4 %>%
               filter(LEED4Industry != "Not elsewhere included") %>%
               rename(Freq = TotalEarnings,
                      Year = YEMar) %>%
               filter(Year %in% InYears)
