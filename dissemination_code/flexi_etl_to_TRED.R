# upload the data via "flexi ETL" to the PlayPen and hence to TRED database, TimeSeries schema
# Peter Ellis, 11 March 2016


library(dplyr)
library(tidyr)
library(mbieDBmisc)

load("data/mtagdp_totals.rda")
load("data/TAGDP_public.rda")

# Use PlayPen_Test and TRED_Test (WIN1110) for any dev, practice or checks
PlayPen <- odbcConnect("PlayPen_Prod")
TRED <- odbcConnect("TRED_Prod")
# PlayPen <- odbcConnect("PlayPen_Test")
# TRED <- odbcConnect("TRED_Test")



#===================GDP by industry======================
tagdp_flexi1 <- TAGDP_public %>%
  select(-Employees, -Earnings_original, -Earnings_commuting_corrected, -GDP_perCapita, -GDP_real_perCapita) %>%
  gather(Variable, Value, -(Year:Region)) %>%
  mutate(Year = as.Date(paste0(Year, "-03-31")))

DataSeries <- list(
  'GDP by Industry and Territorial Authority'  = 
    data.frame(Dataseries_Name = "GDP by Industry and Territorial Authority", 
               Prefix          =       "MTAGDP",
               Group           = 'Modelled Territorial Authority Gross Domestic Product',
               Classifications = c("NGDP_industry", "RGDP_industry", "TA", "Region", "Variable"),
               Time_Variable   = "Year",
               Value           = "Value",
               Unit_of_Measure = "$",
               Magnitude       = "Millions"
    )
)

Upload_Status_1 <- Flexi_ETL(Source_Data = tagdp_flexi1,
                             DataSeries  = DataSeries,
                             PlayPen = PlayPen,
                             TRED = TRED)    


#=======================GDP per capita==========================
tagdp_flexi2 <- TAGDP_public %>%
  select(-Employees, -Earnings_original, -Earnings_commuting_corrected, -GDP, -GDP_real) %>%
  gather(Variable, Value, -(Year:Region)) %>%
  mutate(Year = as.Date(paste0(Year, "-03-31")))

DataSeries <- list(
  'GDP Per Capita by Industry and Territorial Authority'  = 
    data.frame(Dataseries_Name = "GDP Per Capita by Industry and Territorial Authority", 
               Prefix          =       "MTAGDP",
               Group           = 'Modelled Territorial Authority Gross Domestic Product',
               Classifications = c("NGDP_industry", "RGDP_industry", "TA", "Region", "Variable"),
               Time_Variable   = "Year",
               Value           = "Value",
               Unit_of_Measure = "$",
               Magnitude       = "Units"
    )
)

Upload_Status_2 <- Flexi_ETL(Source_Data = tagdp_flexi2,
                             DataSeries  = DataSeries,
                             PlayPen = PlayPen,
                             TRED = TRED)    

#=======================Totals==========================
tagdp_flexi3 <- mtagdp_totals %>%
  select(-GDP_perCapita, -GDP_real_perCapita) %>%
  gather(Variable, Value, -(Year:Region), -notes) %>%
  mutate(Year = as.Date(paste0(Year, "-03-31")))

DataSeries <- list(
  'GDP by Territorial Authority'  = 
    data.frame(Dataseries_Name = "GDP by Territorial Authority", 
               Prefix          =       "MTAGDP",
               Group           = 'Modelled Territorial Authority Gross Domestic Product',
               Classifications = c("TA", "Region", "Variable", "notes"),
               Time_Variable   = "Year",
               Value           = "Value",
               Unit_of_Measure = "$",
               Magnitude       = "Millions"
    )
)

Upload_Status_3 <- Flexi_ETL(Source_Data = tagdp_flexi3,
                             DataSeries  = DataSeries,
                             PlayPen = PlayPen,
                             TRED = TRED)    

#=======================Totals per capita==========================
tagdp_flexi4 <- mtagdp_totals %>%
  select(-GDP, -GDP_real) %>%
  gather(Variable, Value, -(Year:Region), -notes) %>%
  mutate(Year = as.Date(paste0(Year, "-03-31")))

DataSeries <- list(
  'GDP Per Capita by Territorial Authority'  = 
    data.frame(Dataseries_Name = "GDP Per Capita by Territorial Authority", 
               Prefix          =       "MTAGDP",
               Group           = 'Modelled Territorial Authority Gross Domestic Product',
               Classifications = c("TA", "Region", "Variable", "notes"),
               Time_Variable   = "Year",
               Value           = "Value",
               Unit_of_Measure = "$",
               Magnitude       = "Millions"
    )
)

Upload_Status_4 <- Flexi_ETL(Source_Data = tagdp_flexi4,
                             DataSeries  = DataSeries,
                             PlayPen = PlayPen,
                             TRED = TRED)    