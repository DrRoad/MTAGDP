###########################################################################
##    Name:       regression_cagr_on_industry_composition.R
##
##    Objective:  Do a regression analysis on 12 years (2000-2012) GDP growth rate 
##                vs initial industry shares (at RGDP level) to investigate the 
##                relationship between them and see if there is anything leaps out.
##    
##    refer to:   https://github.com/nz-mbie/TAGDP/issues/17
##
##    Authors:    Eric Wu, Sector Performance,   
##                Ministry of Business, Innovation & Employment
##
##    Date:       2015-7-27
##
###########################################################################

library(mbie)
library(mbieDBmisc)
library(dplyr)
library(tidyr)
library(lubridate)
library(Cairo)
library(extrafont)

      TRED    <- odbcConnect("TRED_Prod")

###########################################################################
## anaysis 1
## Import data: regional GDP by industry
## cagr_00_to_12 is renamed as "cagr_analysisPeriod
## rgdp_cagr_00_to_12 is renamed as "rgdp_cagr_analysisPeriod"
rgdp <- 
  ImportTS2(TRED, "Gross domestic product, by region and industry (Annual-Mar)") %>%
  mutate(Year = year(TimePeriod)) %>%
  ## select only useful cols; assign meaningful names to the columns
  select(Year, Value, Area = CV2, Industry = CV3) 

## individual industry
rgdp_individual_industry <-
  rgdp %>% filter(Industry != "Gross Domestic Product")

## Gross Domestic Product
rgdp_total_all_industry <-
  rgdp %>% filter(Industry == "Gross Domestic Product")

## Industry share
rgdp_industry_share <-
  rgdp_individual_industry %>% select(Year, Area, Industry, GDP = Value) %>%
  left_join(rgdp_total_all_industry %>% select(Year, Area, Totat_GDP = Value), 
            by = c("Year" = "Year", "Area" = "Area")) %>%
  mutate(GDP_industry_share = GDP/Totat_GDP*100)

## CAGR
rgdp_cagr_analysisPeriod <-
  rgdp %>%
  #filter(Year %in% c(2000,2012)) %>%
  filter(Year %in% c(startYear,endYear)) %>%
  spread(Year, Value) %>%
  ## cagr 00 to 12
  #mutate(cagr_analysisPeriod = CAGR(.$"2012"/.$"2000", 2012-2000))
  mutate(cagr_analysisPeriod = CAGR(.$as.character(endYear)/.$as.character(startYear), endYear-startYear))
## join cagr and industry share
rgdp_cagr_analysisPeriod_share00 <-
  rgdp_cagr_analysisPeriod %>% filter(Industry != "Gross Domestic Product") %>% 
  select(Area, Industry, cagr_analysisPeriod) %>%
  left_join(rgdp_industry_share %>% filter(Year == startYear) %>% select(Area, Industry, gpd_share00 = GDP_industry_share),
            by = c("Area" = "Area", "Industry" = "Industry")) %>%
  filter(!is.na(cagr_analysisPeriod), !is.na(gpd_share00))

##################################################
## 1. attempt to regression
fit <- lm(cagr_analysisPeriod~gpd_share00, data = rgdp_cagr_analysisPeriod_share00)

(sum_fit <- summary(fit))
a <- round(coef(sum_fit)[1,1],3)
b <- round(coef(sum_fit)[2,1],4)

fit_eqn <- function(x){a+b*x}

g1 <- ggplot(rgdp_cagr_analysisPeriod_share00, aes(x=gpd_share00, y=cagr_analysisPeriod)) +
  geom_point(aes(colour=Area),size = 3 ) +
  stat_function(fun = fit_eqn, color = tourism.cols(5)) +
  ggtitle("") +
  xlab("GDP industry share") +
  #ylab("GDP growth (CAGR 2000-2012)") +
  ylab(cat("GDP growth (CAGR ", startYear,"-",endYear,")") +
  guides(colour=guide_legend(nrow=4,byrow=TRUE)) +
  theme(legend.position="bottom")
print(g1)
## regression is not ideal, the industry share cannot interprete GDP growth well. 
CairoPDF("testing_outputs/regressionAnalysis/cagr_by_industryShare_regions.pdf", 7, 7)
print(g1)
dev.off()


g2 <- ggplot(rgdp_cagr_analysisPeriod_share00, aes(x=gpd_share00, y=cagr_analysisPeriod)) +
  geom_point(aes(colour=Industry),size = 3 ) +
  stat_function(fun = fit_eqn, color = tourism.cols(5)) +
  ggtitle("") +
  xlab("GDP industry share") +
  #ylab("GDP growth (CAGR 2000-2012)") +
  ylab(cat("GDP growth (CAGR ", startYear,"-",endYear,")") +
  guides(colour=guide_legend(ncol=3,byrow=TRUE)) +
  theme(legend.position="bottom")
print(g2)
## It seems that the points are clustered by industry rather than Area. 
CairoPDF("testing_outputs/regressionAnalysis/cagr_by_industryShare_industries.pdf", 7, 7)
print(g2)
dev.off()


dtf <- rgdp_cagr_analysisPeriod_share00 %>%
  filter(Industry != "Total All Industries") %>%
  filter(Area != "New Zealand") %>%
  group_by(Area) %>%
  mutate(cor_by_area = round(cor(gpd_share00, cagr_analysisPeriod), 2)) %>%
  group_by(Industry) %>%
  mutate(cor_by_industry = round(cor(gpd_share00, cagr_analysisPeriod), 2)) %>%
  ungroup %>%
  mutate(cor_all = round(cor(gpd_share00, cagr_analysisPeriod), 2))


g3 <- 
  dtf %>% 
  mutate(Area = wrap(paste0(Area," (",cor_by_area,")"), 50)) %>%
  ggplot(aes(x=gpd_share00, y=cagr_analysisPeriod)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Area, ncol = 4, scales = "free") +
  ggtitle("") +
  xlab("GDP industry share") +
  #ylab("GDP growth (CAGR 2000-2012)")
  ylab(cat("GDP growth (CAGR ", startYear,"-",endYear,")") 
print(g3)

CairoPDF("testing_outputs/regressionAnalysis/cagr_by_industryShare_regions.corr.pdf", 7, 7)
print(g3)
dev.off()



g4 <- 
  dtf %>% mutate(Industry = wrap(paste0(Industry," (",cor_by_industry,")"), 50)) %>%
  ggplot(aes(x=gpd_share00, y=cagr_analysisPeriod)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ Industry, ncol = 4, scales = "free") +
  ggtitle("") +
  xlab("GDP industry share") +
  #ylab("GDP growth (CAGR 2000-2012)")
  ylab(cat("GDP growth (CAGR ", startYear,"-",endYear,")") 
print(g4)

CairoPDF("testing_outputs/regressionAnalysis/cagr_by_industryShare_industries.corr.pdf", 7, 7)
print(g4)
dev.off()


g5 <- 
  dtf %>%
  ggplot(aes(x=gpd_share00, y=cagr_analysisPeriod)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle(paste0("correlation = ", unique(dtf$cor_all))) +
  xlab("GDP industry share") +
  #ylab("GDP growth (CAGR 2000-2012)")
  ylab(cat("GDP growth (CAGR ", startYear,"-",endYear,")") 
print(g5)

CairoPDF("testing_outputs/regressionAnalysis/cagr_by_industryShare_overall.corr.pdf", 7, 7)
print(g5)
dev.off()

###########################################################################



###########################################################################
## anaysis 2: Try it at rda level with ./data/TAGDP_public.rda
## a model with CAGR2000to2012 as response and two explanatory variables: % agriculture in 2000; and % manufacturing in 2000

## Import data: regional GDP by industry
load("./data/TAGDP_public.rda")

## GDP at RC level raw
GDP_RC_raw_by_Industry <-
  TAGDP_public %>%
  select(Year, Industry = RGDP_industry, Area = Region, GDP) %>%
  group_by(Year, Industry, Area) %>%
  summarise(Value = sum(GDP))

GDP_RC_raw_total <-
  GDP_RC_raw_by_Industry %>%
  group_by(Year, Area) %>%
  summarise(Value = sum(Value))

## GDP at TA level raw
GDP_TA_raw_by_Industry <-
  TAGDP_public %>%
  select(Year, Industry = RGDP_industry, Area = TA, GDP) %>%
  group_by(Year, Industry, Area) %>%
  summarise(Value = sum(GDP))

GDP_TA_raw_total <-
  GDP_TA_raw_by_Industry %>%
  group_by(Year, Area) %>%
  summarise(Value = sum(Value))

## CAGR 2000-2012
GDP_cagr_00_to_12 <-
  GDP_TA_raw_total %>%
#   bind_rows(GDP_RC_raw_total) %>%   #if we contain RC level as well.
  #filter(Year %in% c(2000,2012)) %>%
  filter(Year %in% c(startYear, endYear))
  spread(Year, Value) %>%
  ## cagr 00 to 12
  #mutate(cagr_analysisPeriod = CAGR(.$"2012"/.$"2000", 2012-2000))%>% 
  mutate(cagr_analysisPeriod = CAGR(.$as.character(endYear)/.$as.character(startYear), endYear-startYear))%>% 
  select(Area, cagr_analysisPeriod)

## GDP share by industry 2000
GDP_industry_share00 <- 
  GDP_TA_raw_by_Industry %>%
#   bind_rows(GDP_RC_raw_by_Industry) %>%   #if we contain RC level as well.
  #filter(Year %in% c(2000)) %>%
  filter(Year %in% c(startYear)) %>%
  group_by(Year, Area) %>%
  mutate(GDP_industry_share00 = Value/sum(Value)*100) %>%
  ungroup %>%
  select(Area, Industry, GDP_industry_share00) %>%
  spread(Industry, GDP_industry_share00)

## fit a model with Agriculture and Manufacturing only
fit <- 
  lm(cagr_analysisPeriod~., 
     data = GDP_cagr_00_to_12 %>% 
       left_join(GDP_industry_share00, by = c("Area" = "Area")) %>%
       select(one_of(c("cagr_analysisPeriod", "Agriculture", "Manufacturing")))
  )
(sum_fit <- summary(fit))

## fit a model with all industries
fit <- 
  lm(cagr_analysisPeriod~., 
     data = GDP_cagr_analysisPeriod %>% 
       left_join(GDP_industry_share00, by = c("Area" = "Area")) %>%
       select(-Area)
  )
(sum_fit <- summary(fit))

## stepwise search the best model
fit.best <- MASS::stepAIC(fit, direction="both")

## correct df of stepwise selection 
## http://stats.stackexchange.com/questions/22768/appropriate-residual-degrees-of-freedom-after-dropping-terms-from-a-model
fit.best$df.residual <- fit$df.residual

## summary of fitting results
(sum_fit.best <- summary(fit.best))

op <- par(mfrow=c(2,2))
plot(fit.best)
par(op)


## suspicious outlier
GDP_cagr_analysisPeriod %>% 
  left_join(GDP_industry_share00, by = c("Area" = "Area")) %>%
  add_rownames %>% 
  select(rowname, Area) %>%
  slice(c(3,4,40,41))


op <- par(mfrow=c(2,2))
plot(fit.best, id.n = 66)
par(op)

GDP_cagr_analysisPeriod %>% 
  left_join(GDP_industry_share00, by = c("Area" = "Area")) %>%
  add_rownames %>% 
  select(rowname, Area)

## confident interval for best fitted model
confint(fit.best, level = 0.95)

## confident interval for best fitted model
  GDP_cagr_analysisPeriod %>% 
  left_join(GDP_industry_share00, by = c("Area" = "Area")) %>%
  add_rownames %>% 
  select(rowname, Area, Manufacturing, one_of("Public Administration and Safety")) %>%
  data.frame(fitted.values = fit.best$fitted.values) %>%
  arrange(fitted.values)

