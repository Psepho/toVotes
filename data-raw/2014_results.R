#' Import election data files.
#'
#' A helper function to convert the 2014 Excel files released on Toronto's OpenData site into R dataframes
#'
#' @param none.
#' @return A dataframe containing the results from each worksheet.
#' @examples
#' \dontrun{
#' import2014Results()
#' }
import2014Results <- function() {
  if(file.exists("2014 Results.zip")) {
    # Nothing to do
  }  else {
    download.file("http://www1.toronto.ca/City%20Of%20Toronto/Information%20&%20Technology/Open%20Data/Data%20Sets/Assets/Files/2014%20Results.zip",
                  destfile = "2014 Results.zip")
    unzip("2014 Results.zip", exdir=".")
  }
  fyear <- "2014"
  fpattern <- "MAYOR.xls"
  workbook <- XLConnect::loadWorkbook(fpattern)
  sheet_names <- XLConnect::getSheets(workbook)
  names(sheet_names) <- sheet_names

  sheet_list <- lapply(sheet_names, function(.sheet){ # Take each sheet, melt the data into a long table, and append the year, candidate type, and ward
    ward_votes <- XLConnect::readWorksheet(object=workbook, .sheet, startRow = 2)
    area_names <- unname(unlist(XLConnect::readWorksheet(object=workbook, .sheet, region = "B2:BB2", header = FALSE)))
    area_names <- area_names[-length(area_names)]
    ward_votes <- ward_votes[1:dim(ward_votes)[1]-1, 1:dim(ward_votes)[2]-1]
    names(ward_votes) <- c("candidate", area_names)
    data.frame(fyear,
               tidyr::gather(ward_votes,area, votes, -candidate),
               "Mayor",
               strsplit(.sheet, 'Ward')[[1]][[2]])
  })
  election_results <- do.call("rbind", sheet_list) # Collect the dataframes for each worksheet from the file into a single dataframe
  names(election_results) <- c("year", "candidate", "area", "votes", "type", "ward") # Clean up the names
  election_results$candidate <- gsub(",","",tolower(election_results$candidate))
  row.names(election_results) <- NULL
  election_results <- tbl_df(election_results)
  election_results
}
