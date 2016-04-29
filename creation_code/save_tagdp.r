##
##    Name:       save_tagdp.R
##
##    Objective:  This saves copies of TAGDP objects out of grunt.R & modify_tagdp.R for
##                public dissemination and forecasting outputs.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2016-04-15
##

##
##    Notes:      Separated out the saving portion to minimise versions of objects
##                to git while testing. 
##
   
##
## 1.  Save final TAGDP objects for exploratory data analyses & dump to .csv
##

   # keep a copy of the TAGDP_defl object for the InYears +2 forecasts
     save(TAGDP_defl, file = "data_intermediate/TAGDP_defl.rda")

   # save a        
     save(TAGDP_public, file = "data/TAGDP_public.rda")                  
   
   # csv dump for web consumption                 
      write.csv(TAGDP_public, file = "data/TAGDP_public.csv", row.names=FALSE)
   
