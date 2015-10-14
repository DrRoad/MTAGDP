##
##    Name:       cluster_analysis.R
##
##    Objective:  Exploratory principal coordinates analysis to visualise TA similarities
##                in industry composition and the relative importance of industries in 
##                distinguishing TAs.
##
##                Cluster analysis plot on average share of total TA GDP also created.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2014-08-10
##

##
##  1. Call to funtionality and core data objects
##

      library(dplyr)
      library(tidyr)
      library(cluster)
      load("data/TAGDP_public.rda")

##
##  2. Create objects for industry shares
##
      shares <- TAGDP_public %>%
                group_by(Year, TA, RGDP_industry) %>%
                summarise(GDP = sum(GDP)) %>%
                ungroup() %>%
                group_by(Year, TA) %>%
                mutate(share = GDP / sum(GDP)) %>%
                select(-GDP) 

      shares_av <- shares %>%
                ungroup() %>%
                group_by(TA, RGDP_industry) %>%
                summarise(share = mean(share)) %>%
                spread(RGDP_industry, share)

       TA_short <- gsub(" District", "", shares_av$TA)
       TA_short <- gsub(" City", "", TA_short)
       row.names(shares_av) <- TA_short

##
## 3. Plots for principal components
##
     # principal compoents biplot
       mod <- princomp(shares_av [ , -1])
       biplot(mod, xlabs = TA_short)

     # cluster analysis of average share
       shares_daisy <- daisy(shares_av[ ,-1])

       shares_diana <- diana(shares_av)
       plot(shares_diana, which.plots = 2, main ="", sub = "")

