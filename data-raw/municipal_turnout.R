# Data taken from
import_turnout_file <- function(.file) {
  data <- dplyr::select(readxl::read_excel(.file), Ward, Sub, `% Voted`)
  data <- dplyr::mutate(data, year = stringr::str_extract(.file, "[[:digit:]]{4}"))
  data
}
on_files <- list.files(path = "data-raw",
                       pattern = "elections-voter-statistics-[[:digit:]]{4}.xls",
                       full.names = TRUE)
turnout <- do.call("rbind", lapply(on_files, import_turnout_file))
names(turnout) <- c("ward", "area", "prop_voted", "year")
turnout <- dplyr::filter(turnout, !grepl("Total", ward), !is.na(prop_voted))
turnout <- transform(turnout, year = as.factor(year), ward = as.integer(ward), area = as.integer(area))
turnout <- dplyr::group_by(turnout, year, ward, area)
turnout <- dplyr::transmute(turnout, prop_voted = mean(prop_voted),
            type = "mayoral")
toTurnout <- turnout
devtools::use_data(toTurnout, overwrite = TRUE)
