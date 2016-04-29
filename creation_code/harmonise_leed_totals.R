##
##    Programme:  harmonise_leed_totals.r
##
##    Objective:  LEED data are used to estimate how much economic value is generated within each TA
##                and low-level industry.   The various LEED totals for earnings are not identical 
##                for each year when you take them down to industry level for some reason
##                (see:  testing_outputs/LEED Total Filled Jobs - Aggregated vs Individual Industry.xls )
##                This programme aligns the LEED data with LEED37 as the definitive earnings totals
##
##                Data are divided by 10^6 to match the units of GDP tables.
##
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-21
##

      YearTotals37  <- leed37_pop %>%
                       group_by(Year) %>%
                       summarise(earnings_definitive=sum(Freq) / 10 ^ 6)
                              
      YearTotals4   <- leed4_pop %>%
                       group_by(Year) %>%
                       summarise(earnings4=sum(Freq) / 10 ^ 6)
                              
      YearTotals18  <- leed18_pop %>%
                       group_by(Year) %>%
                       summarise(earnings18=sum(Freq) / 10 ^ 6)
                              
      AllYearTotals <- merge(merge(YearTotals37, YearTotals4), YearTotals18)
      
      AllYearTotals$Adjust4   <- with(AllYearTotals, earnings_definitive / earnings4)
      AllYearTotals$Adjust18  <- with(AllYearTotals, earnings_definitive / earnings18)

      if(max(abs(c(AllYearTotals$Adjust4, AllYearTotals$Adjust18) - 1)) > 0.01){
        stop("Some of the earnings adjustments are too large to be plausible")
      }
      
      leed4_pop <- merge(leed4_pop, 
                         AllYearTotals[ , c("Year", "Adjust4")], 
                         by = "Year", 
                         all.x=TRUE)
                         
      leed4_pop$Freq     <- with(leed4_pop, Freq * Adjust4)
      leed4_pop$Adjust4  <- NULL

      leed18_pop <- merge(leed18_pop, 
                          AllYearTotals[ , c("Year", "Adjust18")], 
                          by = "Year", 
                          all.x=TRUE)
                          
      leed18_pop$Freq     <- with(leed18_pop, Freq * Adjust18)
      leed18_pop$Adjust18 <- NULL
