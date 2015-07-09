## TEST functions to organise geographic north-to-south arrangement of TAs and industry levels
# 2015-05-29

## two functions for organising geographies:
##  organise_tas  # for Territorial Authorities
##  organise_rcs  # for Regional Councils

## general function to truncate Territorial Authority names & organise the levels N to S
   organise_tas <- function(x, class_name = "TA") {
  	  
  	     x <- data.frame(x)
	              
	              # create a short TA name for graphical output
                    x[ , class_name] <- gsub(" District", "", x[ , class_name])
                    x[ , class_name] <- gsub(" City",     "", x[ , class_name])               

	   # set levels
	     y <- factor(x[ , class_name], 
                        levels = c("Far North", 
                                   "Whangarei",
                                   "Kaipara",
                                   "Auckland",
                                   "Thames-Coromandel",
                                   "Hauraki",
                                   "Waikato",
                                   "Matamata-Piako",
                                   "Hamilton",
                                   "Waipa",
                                   "Otorohanga",
                                   "South Waikato",
                                   "Waitomo",
                                   "Taupo",
                                   "Western Bay of Plenty",
                                   "Tauranga",
                                   "Rotorua",
                                   "Whakatane",
                                   "Kawerau",
                                   "Opotiki",
                                   "Gisborne",
                                   "Wairoa",
                                   "Hastings",
                                   "Napier",
                                   "Central Hawke's Bay",
                                   "New Plymouth",
                                   "Stratford",
                                   "South Taranaki",
                                   "Ruapehu",
                                   "Wanganui",
                                   "Rangitikei",
                                   "Manawatu",
                                   "Palmerston North",
                                   "Tararua",
                                   "Horowhenua",
                                   "Kapiti Coast",
                                   "Porirua",
                                   "Upper Hutt",
                                   "Lower Hutt",
                                   "Wellington",
                                   "Masterton",
                                   "Carterton",
                                   "South Wairarapa",
                                   "Tasman",
                                   "Nelson",
                                   "Marlborough",
                                   "Kaikoura",
                                   "Buller",
                                   "Grey",
                                   "Westland",
                                   "Hurunui",
                                   "Waimakariri",
                                   "Christchurch",
                                   "Selwyn",
                                   "Ashburton",
                                   "Timaru",
                                   "Mackenzie",
                                   "Waimate",
                                   "Waitaki",
                                   "Central Otago",
                                   "Queenstown-Lakes",
                                   "Dunedin",
                                   "Clutha",
                                   "Southland",
                                   "Gore",
                                   "Invercargill"))	

                  if(sum(is.na(y)) > 0){
                     stop("Some TAs did not match in the sorting process or are NA")
                  }                  
                  
                  return(y)

          }

## general function to truncate Regional Council names & organise them N to S
   organise_rcs <- function(x, class_name = "Region") {
  	  
  	     x <- data.frame(x)
	              
	              # create a short RC name for graphical output
                    x[ , class_name] <- gsub(" Region", "", x[ , class_name])            

	   # set levels
	     y <- factor(x[ , class_name], 
                        levels = c("Northland",
                                   "Auckland",
                                   "Waikato",
                                   "Bay of Plenty",
                                   "Gisborne",
                                   "Hawke's Bay",
                                   "Taranaki",
                                   "Manawatu-Wanganui",
                                   "Wellington",
                                   "Marlborough",
                                   "Nelson",
                                   "Tasman",
                                   "West Coast",
                                   "Canterbury",
                                   "Otago",
                                   "Southland"))
                                   
                  if(sum(is.na(y)) > 0){
                     stop("Some RCs did not match in the sorting process or are NA")
                  }                  
                  
                  return(y)
                                   
             }                             
