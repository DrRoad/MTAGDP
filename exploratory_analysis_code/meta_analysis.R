##
##    Name:       meta_analysis.R
##
##    Objective:  Conducts a "meta analysis" of the repository itself ie how many lines of code, 
##                what size data, et cetera.
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment
##
##    Date:       2015-04-212
##

# call to functions to support the meta-analysis
  source("R/meta_analysis_helper_functions.R")

# analyse this whole repository and give its name
  mtagdp      <- AnalyseRepository(".")
  mtagdp$repo <- "Modelled Territorial Authority Gross Domestic Product"

# overall summary
  summary(mtagdp)

# distribution of length of scripts
  plot(mtagdp)

# what are all those files? print to screen:
  mtagdp

# what words were used in that 5000+ lines of code?
CairoPDF("exploratory_output/wordclouds.pdf", 11, 8)
  Wordcloud2("creation_code", min.freq=5, family = "Calibri")
  grid.text("Words used in the creation code", 0.5, 0.95, gp=gpar(fontfamily= "Calibri", fontsize=20))
  
dev.off()