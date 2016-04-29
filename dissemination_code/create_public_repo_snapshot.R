##
##    Name:       create_public_repo_snapshot.R
##
##    Objective:  Creates a copy of the project for the public release.  Excludes output files
##                (i.e. .png, .pdf, et cetera).
##
##    Authors:    Peter Ellis, Sector Performance, Ministry of Business, Innovation & Employment   
##                  
##    Date:       2015-08-31
##

##
##  1. copies all files over from the live repository to the snapshot for publication
##
     
      all_files <- dir(recursive = TRUE)

      knockout <- c(
        grep("\\.png$", all_files),
        grep("\\.pdf$", all_files),
        grep("\\.rda$", all_files),
        grep("\\.rdata$", all_files),
        grep("\\.Rproj$", all_files),
        grep("\\.png$", all_files),
        grep("\\.dcf$", all_files),
        grep("deploy", all_files),
        grep("exploratory", all_files),  ## simplifying public version with growing ad-hoc, exploratory analyses & outputs
        grep("leed_tables", all_files),  ## these are the custom LEED TA tables from Statistics NZ
        grep("\\.DS_Store", all_files),
        grep(".Rapp.history", all_files),
        grep(".synctex.gz", all_files)
        
      )
  
      all_files <- all_files[-knockout]

      drive <- substring(getwd(), 1, 2)
      file.copy(all_files, paste0(drive, "/MTAGDP/", all_files), overwrite = TRUE)
# file.copy(from=paste0("H:/TAGDP_creation/",all_files), to=paste0("G:/MTAGDP/",all_files), overwrite=TRUE)
# file.copy(from=paste0("H:\\TAGDP_creation\\",all_files), to=paste0("G:\\MTAGDP\\",all_files), overwrite=TRUE)
# then go to other repository on your F: drive (or whatever drive the clone is on), use git tools to see how much changed, 
# commit, and push it to the public shared repository.