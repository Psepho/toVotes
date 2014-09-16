toVotes
=======

This package contains cleaned and collated poll by poll results from the 2003, 2006, and 2010 mayoral elections in Toronto. The original data is from [Toronto Open Data](http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=834689fe9c18b210VgnVCM1000003dd60f89RCRD)

The dataframe contains the:

* year of the election as a factor
* name of the candidate as last name first name
* number of votes received by the candidate
* ward for the polling station
* area for the polling station

## Installation

    library(devtools)
    devtools::install_github("Psepho/toVotes")
    library(toVotes)
    data(toVotes)
    
## Helper function

The package also contains a helper function `importElectionWorksheets` which was used to convert the source Excel files into dataframes. For example, importing the 2010 election results could proceed as:

    processFiles <- function(pattern) { # Takes a filename pattern and returns a dataframe
      lfiles <- list.files(pattern = fpattern) # Find all files that match the pattern
      results <- do.call("rbind", lapply(lfiles, importWorksheets)) # Collapse all down to a single dataframe
      row.names(results) <- NULL # Clear the rownames
    }
    fyear <- "2010"
    fpattern <- paste(fyear,".*_Mayor.xls?",sep="") # Import several files at once
    election_results <- processFiles(fpattern)
