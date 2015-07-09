##
##    Name:  import_NGDP.R
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
	
         ngdp <- ImportTS2(TRED, "SNE - Series, GDP(P), Nominal, Actual, ANZSIC06 detailed industry groups (Annual-Mar)")
         ngdp <- subset(ngdp, CV1 == "Gross Domestic Product - production measure")[ ,c("TimePeriod", "CV2", "Value")]

         names(ngdp) <- c("Year", "NGDP_industry", "NGDP")
         ngdp$Year   <- as.numeric(substring(ngdp$Year, 1, 4))

         ngdp <- subset(ngdp, NGDP_industry != "Total All Industries")
         ngdp <- subset(ngdp, NGDP_industry != "Finance Service Charge") # is always 0, and sometimes NA so best to cut

        
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
   ##

         GST_Duties_Tax <- ImportTS2(TRED, "SNE - Series, GDP(P), Nominal, Actual, Total (Annual-Mar)")
         GST_Duties_Tax <- GST_Duties_Tax[GST_Duties_Tax$CV1 %in% c("GST", "Import duties", "Other Taxes on Production"),]

         GST_Duties_Tax <- with(GST_Duties_Tax,
                           aggregate(list(Freq = Value),
                                     list(Year = year(TimePeriod)),
                                     sum, 
                                     na.rm = TRUE)
                                   )
         GST_Duties_Tax$NGDP_industry <- "GST on Production, Import Duties and Other Taxes"                       
         GST_Duties_Tax <- subset(GST_Duties_Tax, Year %in% unique(rgdp_pop_pub$Year))

  ##
  ##  Combine the GDP values with GST into a single object
  ##
      ngdp_pop <- plyr::rbind.fill(ngdp_pop, GST_Duties_Tax)

