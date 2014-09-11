

processFiles <- function(pattern) { # Takes a filename pattern and returns a dataframe
  lfiles <- list.files(pattern = fpattern) # Find all files that match the pattern
  results <- do.call("rbind", lapply(lfiles, importWorksheets)) # Collapse all down to a single dataframe
  names(results) <-c("year", "candidate", "area", "votes", "type", "ward") # Clean up the names
  row.names(results) <- NULL # Clear the rownames
  results <- subset(results, area!="Total") # Drop the total column
  results <- subset(results, !grepl("Totals", candidate)) # Drop the total rows
}

fyear <- "2010"
fpattern <- paste(fyear,".*_Mayor.xls?",sep="")  # pattern for filenames
election_results <- processFiles(fpattern)
election_results$candidate <- gsub(",","",tolower(election_results$candidate))
election_results$ward <- gsub("Ward","",election_results$ward)
election_results$area <- gsub("x","",election_results$area)
write.csv(election_results, file=paste(fyear,"_election_results.csv",sep=""))
rm(fpattern,fyear)
election_results <- rbind(read.csv("2003_election_results.csv"), read.csv("2006_election_results.csv"), read.csv("2010_election_results.csv"))
write.csv(election_results[,-1], file="election_results.csv")
