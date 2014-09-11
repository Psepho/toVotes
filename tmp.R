

processFiles <- function(pattern) { # Takes a filename pattern and returns a dataframe
  lfiles <- list.files(pattern = fpattern) # Find all files that match the pattern
  results <- do.call("rbind", lapply(lfiles, importWorksheets)) # Collapse all down to a single dataframe
  row.names(results) <- NULL # Clear the rownames
}
fyear <- "2010"
fpattern <- paste(fyear,".*_Mayor.xls?",sep="")  # pattern for filenames
election_results <- processFiles(fpattern)
write.csv(election_results, file=paste(fyear,"_election_results.csv",sep=""))
rm(fpattern,fyear)
election_results <- rbind(read.csv("2003_election_results.csv"), read.csv("2006_election_results.csv"), read.csv("2010_election_results.csv"))
write.csv(election_results[,-1], file="election_results.csv")
