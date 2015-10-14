##
##    Name:       plot_commuting_patterns.R
##
##    Objective:  Creates a network graph illustrating the major commuting connectivity between
##                Territorial Authories based on the 2013 Census.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2015-06-26
##

##
##  1. Read in the commuting data from the 2013 Census
##
       X <- read.csv("data_raw/2013-usual-residence-by-workplace-address-territorial-authority.csv",
                 na.strings       = "..C",
                 sep              = ",",
                 stringsAsFactors = FALSE,
                 check.names      = TRUE,
                 header           = TRUE)
       Travel                  <- melt(X, id = c("Usual.residence"))
       Travel$variable         <- wrap(str_replace_all(Travel$variable, "\\.", " "))
       Travel$variable         <- wrap(str_replace_all(Travel$variable, "\\.", " "))
       Travel$Usual.residence  <- wrap(Travel$Usual.residence)
##
##  2.  Drop the totals and the clutter
##
       Travel <- Travel[!((Travel$Usual.residence %in% c("Total,\nusual\nresidents")) |
                          (Travel$variable        %in% c("Total\nworkplace\naddress","New\nZealand\nNot\nFurther\nDefined" ))),]
       Travel <- Travel[!str_detect(Travel$variable, "Defined"),]
       Travel <- Travel[!str_detect(Travel$variable, "Outside"),]
       Travel <- Travel[!str_detect(Travel$variable, "Address"),]

       Travel <- Travel[!str_detect(Travel$Usual.residence, "Address"),]
       Travel <- Travel[!str_detect(Travel$Usual.residence, "Authority"),]
       Travel <- Travel[!str_detect(Travel$Usual.residence, "Confidentiality"),]
       Travel <- Travel[!str_detect(Travel$Usual.residence, "Source"),]
       Travel <- Travel[!str_detect(Travel$Usual.residence, "..C "),]

##
##  3. Correct Stats NZ non-standard Territorial Authority names
##
       Travel$Usual.residence <- ifelse(Travel$Usual.residence == "Central\nHawke s\nBay\nDistrict", "Central\nHawke's\nBay\nDistrict", Travel$Usual.residence)
       Travel$variable        <- ifelse(Travel$variable == "Central\nHawke s\nBay\nDistrict", "Central\nHawke's\nBay\nDistrict", Travel$variable)

       Travel$variable        <- ifelse(Travel$variable == "Matamata\nPiako\nDistrict", "Matamata-Piako\nDistrict", Travel$variable)
       Travel$variable        <- ifelse(Travel$variable == "Thames\nCoromandel\nDistrict", "Thames-Coromandel\nDistrict", Travel$variable)
       Travel$variable        <- ifelse(Travel$variable == "Queenstown\nLakes\nDistrict", "Queenstown-Lakes\nDistrict", Travel$variable)

       Travel <- Travel[Travel$Usual.residence != "",]
       unique(Travel$Usual.residence)
       unique(Travel$variable)
                    
##
##  4. Create the plot
##                
       png("dissemination_outputs/commuting_patterns.png", 6000, 6000, res=600)
           par(family="Calibri")
           graph(Travel,
                 layout = "circle",
                 node.width = .75,
                 asize = 1,
                 label.cex = 12,
                 label.prop = .65)
           title(main = "Commuting between Regions\n",
                 sub = "Census 2013 Data")
       dev.off()

