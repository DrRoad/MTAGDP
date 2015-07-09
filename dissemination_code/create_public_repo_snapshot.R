# copies all files over from the live repository to the snapshot for publication

all_files <- dir(recursive = TRUE)

knockout <- c(
  grep("\\.png$", all_files),
  grep("\\.pdf$", all_files),
  grep("\\.rda$", all_files),
  grep("\\.rdata$", all_files),
  grep("\\.Rproj$", all_files),
  grep("\\.png$", all_files),
  grep("\\.dcf$", all_files),
  grep("deploy", all_files)
)
  
all_files <- all_files[-knockout]

file.copy(all_files, paste0("F:/MTAGDP/", all_files), overwrite = TRUE)

# then go to other repository on your F: drive, use git tools to see how much changed, 
# commit, and push it to the public shared repository.