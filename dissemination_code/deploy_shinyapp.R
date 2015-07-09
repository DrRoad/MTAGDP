##
##    Name:       deploy_shinyapp.R
##
##    Objective:  This script provides the proxy details & permissions for deploying the
##                shiny app for the MTAGDP project.
##
##    Authors:    Peter Ellis, James Hogan, Franz Smith, Sector Performance,   
##                  Ministry of Business, Innovation & Employment
##
##    Date:       2015-05-18
##

##
##    Notes:      This should be modified to have a general deployment for the public version
##                (e.g. to the users desktop browser) that would allow them to explore the data &
##                test functionality in changes to the shiny script.
##

#===========Deploy to shinyapps.io=======================



# Proxy password to get through MBIE firewall
  if(!exists("creds") & Sys.getenv("USERDNSDOMAIN") == "WD.GOVT.NZ"){
     creds <- AskCreds(Title = "User Log In Name and Password", startuid = "", returnValOnCancel = "ID_CANCEL")   
     options(RCurlOptions = list(proxy = 'http://proxybcw.wd.govt.nz:8080',
                              proxyusername = creds$uid, 
                              proxypassword = creds$pwd))
  }

  deployApp("shiny", appName = "mtagdp_test", account = "mbienz", lint = TRUE)
 # deployApp("shiny", appName = "mtagdp", account = "mbienz", lint = FALSE)
