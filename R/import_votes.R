#' Import election data files.
#'
#' A helper function to convert the Excel files released on Toronto's OpenData site into R dataframes
#'
#' @param filename The file to import.
#' @return A dataframe containing the results from each worksheet.
#' @examples
#' \dontrun{
#' importWorksheets("2010_Mayor.xls")
#' }
importElectionWorksheets <- function(filename) {
  if (!requireNamespace("XLConnect", quietly = TRUE)) {
    stop("XLConnect needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  workbook <- XLConnect::loadWorkbook(filename)
  sheet_names <- XLConnect::getSheets(workbook)
  names(sheet_names) <- sheet_names
  sheet_list <- lapply(sheet_names, function(.sheet){ # Take each sheet, melt the data into a long table, and append the year, candidate type, and ward
    data.frame(fyear,
               reshape2::melt(XLConnect::readWorksheet(object=workbook, .sheet, startRow=ifelse(fyear=="2003",3,2))), # Different number of header rows in 2003
               ifelse(fyear=="2003",(strsplit(.sheet, ' Ward ')[[1]][[1]]),"Mayor"), # Different worksheet-name structure for 2003
               ifelse(fyear=="2003",strsplit(.sheet, ' Ward ')[[1]][[2]],strsplit(.sheet, ' Ward ')[[1]][[1]]))
  })
  results <- do.call("rbind", sheet_list) # Collect the dataframes for each worksheet from the file into a single dataframe
  names(results) <- c("year", "candidate", "area", "votes", "type", "ward") # Clean up the names
  results <- subset(results, area!="Total") # Drop the total column
  results <- subset(results, !grepl("Totals", candidate)) # Drop the total rows
  results$candidate <- gsub(",","",tolower(election_results$candidate))
  results$ward <- gsub("Ward","",election_results$ward)
  results$area <- gsub("x","",election_results$area)
}
