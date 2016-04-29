# Defines helper functions for analysis of an R repository - counts lines, draws wordclouds, etc.
# Author Peter Ellis, 12 April 2014
# Modified: Pete McMillen, 2 February 2015 - added .PNG & .PDF file count

library(tm)
library(wordcloud)

MakeSpaces <- function(x, From = c("(", ")", "$", "{", "}", ",", "<-", "=", "+", "*", "[", "]", ":", '"', "'", "#")){
  for(i in 1:length(From)){
    x <- gsub(From[i], " ", x, fixed=TRUE)
  }
  return(x)
}

Wordcloud2 <- function(directory, ...){
  txt <- Corpus(DirSource(directory))
  ap.corpus <- tm_map(txt, content_transformer(MakeSpaces))
  ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
  ap.corpus <- tm_map(ap.corpus, content_transformer(tolower))
  ap.tdm <- TermDocumentMatrix(ap.corpus)
  ap.m <- as.matrix(ap.tdm)
  ap.v <- sort(rowSums(ap.m), decreasing = TRUE)
  ap.d <- data.frame(word = names(ap.v), freq = ap.v)
  print(wordcloud(ap.d$word, ap.d$freq, random.order = FALSE, 
                  colors = mbie.cols(), ...))
  
}

CountLines <- function(file){
  length(readLines(as.character(file)))
}

AnalyseRepository <- function(MainDir){
  # Analysis of a repository at MainDir, including its subfolders.
  # Provides a list of every R script, data frame, and source data set 
  # (all CSVs and Excel files are assumed to be source data - this probably isn't true 
  # as sometimes we save a CSV, but will usually be close enough).
  
  Rscripts <- data.frame(Name = dir(MainDir, pattern="^.*\\.R$", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  Rscripts$Lines <- NA
  for(i in 1: nrow(Rscripts)){
    suppressWarnings(
      Rscripts[i, "Lines"] <- CountLines(paste(MainDir, Rscripts[i, "Name"], sep="/"))
    )
  }
  
  Rdata <- data.frame(Name=dir(paste(MainDir, "data", sep="/"), pattern="^.*\\.rda", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(Rdata) > 0) {
    Rdata$SizeKB <- NA
    for (i in 1:nrow(Rdata)){
      Rdata[i, "SizeKB"] <- round(file.info(paste(MainDir, "data", Rdata[i, "Name"], sep="/"))$size  / 1024)
    }
  }
  Excel <- data.frame(Name=dir(MainDir, pattern="^.*\\.xls", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(Excel) > 0) {
    Excel$SizeKB <- NA
    for (i in 1:nrow(Excel)){
      Excel[i, "SizeKB"] <- round(file.info(paste(MainDir, Excel[i, "Name"], sep="/"))$size / 1024)
    }
  }
  
  CSV <- data.frame(Name=dir(MainDir, pattern="^.*\\.csv$", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(CSV) > 0) {
    CSV$SizeKB <- NA
    for (i in 1:nrow(CSV)){
      CSV[i, "SizeKB"] <- round(file.info(paste(MainDir, CSV[i, "Name"], sep="/"))$size / 1024)
    }
  }
  
  GZ <- data.frame(Name=dir(MainDir, pattern="^.*\\.gz$", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(GZ) > 0) {
    GZ$SizeKB <- NA
    for (i in 1:nrow(GZ)){
      GZ[i, "SizeKB"] <- round(file.info(paste(MainDir, GZ[i, "Name"], sep="/"))$size / 1024)
    }
  }
  
  PDF <- data.frame(Name=dir(MainDir, pattern="^.*\\.pdf$", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(PDF) > 0) {
    PDF$SizeKB <- NA
    for (i in 1:nrow(PDF)){
      PDF[i, "SizeKB"] <- round(file.info(paste(MainDir, PDF[i, "Name"], sep="/"))$size / 1024)
    }
  }
  
  PNG <- data.frame(Name=dir(MainDir, pattern="^.*\\.png$", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(PNG) > 0) {
    PNG$SizeKB <- NA
    for (i in 1:nrow(PNG)){
      PNG[i, "SizeKB"] <- round(file.info(paste(MainDir, PNG[i, "Name"], sep="/"))$size / 1024)
    }
  }
  
  SVG <- data.frame(Name=dir(MainDir, pattern="^.*\\.svg$", recursive=TRUE, ignore.case=TRUE, full.names=FALSE))
  if(nrow(SVG) > 0) {
    SVG$SizeKB <- NA
    for (i in 1:nrow(SVG)){
      SVG[i, "SizeKB"] <- round(file.info(paste(MainDir, SVG[i, "Name"], sep="/"))$size / 1024)
    }
  }

  
  results <- list(Rscripts=Rscripts, Rdata=Rdata, Excel=Excel, CSV=CSV, GZ=GZ, PDF=PDF, PNG=PNG, SVG=SVG, repo=MainDir)
  
  # we create a special class for this sort of object so we can have a summary and plot method for it
  class(results) <-"RepoAnalysis"
  return(results)
}

summary.RepoAnalysis <- function(results){
  # summary method for an analysis of a repository that was produced from AnalyseRepository()
  Summary <- paste("\n==================================================\n", 
                   "Summary of the work in the", results$repo, "file system:\n\n", 
                   nrow(results$Rscripts), "R scripts;\n",
                   sum(results$Rscripts$Lines), "lines of code;\n",
                   round(median(results$Rscripts$Lines), 1), "- median number of lines in a script;\n",
                   round(mean(results$Rscripts$Lines, tr=0.2), 1), "- trimmed mean number of lines in a script;\n",
                   nrow(results$CSV), "raw data sets in CSV format;\n",
                   nrow(results$Excel), "raw data sets in Excel format;\n",
                   nrow(results$GZ), "further raw data sets zipped up as .gz files;\n",
                   nrow(results$PDF), "pdf plots (exploratory or final);\n",
                   nrow(results$PNG), "png plots (exploratory or final);\n",
                   nrow(results$SVG), "svg plots (exploratory or final);\n",
                   sum(results$Excel$SizeKB, results$CSV$SizeKB, results$GZ$SizeKB), "KB of raw data;\n",
                   nrow(results$Rdata), "tidy data sets;\n",
                   sum(results$Rdata$SizeKB), "KB of tidy data.\n",
                   "==================================================\n"
                   
  )
  cat(Summary)
  
}

plot.RepoAnalysis <- function(results, nbins="FD", xlab="Number of lines per R script", col="grey80", main=results$repo, ...){
  # plot method for an analysis of a repository that was produced from AnalyseRepostiroy()
  # At this point, it just draws a histogram of the sizes of R scripts.
  hist(results$Rscripts$Lines, xlab=xlab, col=col, main=main, ...)
}
