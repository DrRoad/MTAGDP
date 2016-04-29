##
##    Name:       import_NGDP.R
##
##    Objective:  This stage or the process extracts national GDP measures from TRED and aligns
##                inconsistencies between the industries in National GDP and the industries derived
##                from the industries concordance file.
##
##                For the public version of this project, a call for extracted data objects is provided
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2014-08-10
##

##
##  Extract the data from MBIE's database framework
##	
   # Import NGDP & groom
     ngdp <- ImportTS2(TRED, "SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 detailed industry groups (Annual-Mar)",
                            where = "CV1 = 'Gross Domestic Product - production measure' and TimePeriod > '1999-12-31'") %>%
                      select(Year = TimePeriod, NGDP_industry = CV2, NGDP = Value) %>%
                      mutate(Year = year(Year)) %>%
                      filter(!NGDP_industry %in% c('Total All Industries', 'Finance Service Charge'))
                            
   ##
   ##    Carve out only the years within RGDP, and rename measure variable
   ##
         ngdp_pop <- subset(ngdp, Year %in% unique(rgdp_pop_pub$Year))
         names(ngdp_pop)[names(ngdp_pop) == "NGDP"] <- "Freq"

   ##
   ##    We also need to get GST, Import duties, and Other Taxes on Production which are not "industries" so 
   ##       aren't included in the ngdp series imported earlier, but are included in RGDP so need to be added back and treated as an 
   ##       industry for this work
   ##

      GST_Duties_Tax <- ImportTS2(TRED, "SNE - Series, GDP(P), Nominal, Actual, Total (Annual-Mar)",
                                         where = "TimePeriod > '1999-12-31'") %>%
                                  filter(CV1 %in% c("GST", "Import duties", "Other Taxes on Production")) %>%
                                  mutate(Year = year(TimePeriod)) %>%
                                  group_by(Year) %>%
                                  summarise(Freq = sum(Value, na.rm = TRUE)) %>%
                                  ungroup() %>%
                                  mutate(NGDP_industry = "GST on Production, Import Duties and Other Taxes") %>%
                                  filter(Year %in% unique(rgdp_pop_pub$Year))
           
   ##
   ##  Combine the GDP values with GST into a single object
   ##
       ngdp_pop <- bind_rows(ngdp_pop, GST_Duties_Tax)
 

         #GST_Duties_Tax <- GST_Duties_Tax[GST_Duties_Tax$CV1 %in% c("GST", "Import duties", "Other Taxes on Production"),]

         #GST_Duties_Tax <- with(GST_Duties_Tax,
          #                 aggregate(list(Freq = Value),
          #                           list(Year = year(TimePeriod)),
          #                           sum, 
          #                           na.rm = TRUE)
          #                         )
#          GST_Duties_Tax$NGDP_industry <- "GST on Production, Import Duties and Other Taxes"                       
#          GST_Duties_Tax               <- subset(GST_Duties_Tax, Year %in% unique(rgdp_pop_pub$Year))

  ##
  ##  Combine the GDP values with GST into a single object
  ##
      # ngdp_pop <- plyr::rbind.fill(ngdp_pop, GST_Duties_Tax)


