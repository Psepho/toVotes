# 2008 http://www.elections.ca/scripts/OVR2008/31/data/pollresults_resultatsbureau_canada.zip
# 2011 http://elections.ca/scripts/OVR2011/34/data_donnees/pollresults_resultatsbureau_canada.zip
# 2006 http://www.elections.ca/scripts/OVR2006/25/data_donnees/pollresults_resultatsbureau_canada.zip

sources <- list(year = c(2006, 2008, 2011),
                url = c("http://www.elections.ca/scripts/OVR2006/25/data_donnees/pollresults_resultatsbureau_canada.zip",
                            "http://www.elections.ca/scripts/OVR2008/31/data/pollresults_resultatsbureau_canada.zip",
                            "http://www.elections.ca/scripts/OVR2011/34/data_donnees/pollresults_resultatsbureau_canada.zip")
                )
this_source <- 1 # the results to pull
zip_file <- "data-raw/pollresults_resultatsbureau_canada.zip"
if(file.exists(zip_file)) { # Only download the data once
  # File exists, so nothing to do
}  else {
  download.file(sources$url[[this_source]],
                destfile = zip_file)
  unzip(zip_file, exdir=paste("data-raw", "pollresults", sep = "/")) # Extract the data into data-raw
}

on_files <- list.files(path = "data-raw/pollresults",
                       pattern = "pollresults_resultatsbureau35[[:digit:]]{3}.csv",
                       full.names = TRUE)
federal_results <- do.call("rbind", lapply(on_files, function(.file){readr::read_csv(.file)}))
# Encoding changes across files
names(federal_results) <- iconv(names(federal_results),"WINDOWS-1252","UTF-8")
# Header names change slightly across years, these work, so far
federal_results <- dplyr::select(federal_results, contains("Family"), contains("First"),
              contains("Votes"), matches("Affiliation.*English"),
              contains("District Number"), contains("Polling Station Number"), contains("Incumbent"))
names(federal_results) <- c("last", "first", "votes", "party", "district", "poll", "incumbent")
federal_results <- dplyr::transmute(federal_results,
                            candidate = as.factor(stringr::str_c(federal_results$last,
                                                                 federal_results$first ,
                                                                 sep = " ")),
                            year = as.factor(sources$year[[this_source]]),
                            type = "federal",
                            votes = as.integer(federal_results$votes),
                            party = as.character(federal_results$party),
                            district = as.character(federal_results$district),
                            poll = as.character(federal_results$poll),
                            incumbent = as.logical(ifelse(federal_results$incumbent == "Y", 1, 0))
                            )


to_ed <- readr::read_csv("data-raw/TO_federal_election_districts.csv")[,1]
to_ed <- dplyr::filter(to_ed, !is.na(district))
to_ed$district <- as.character(to_ed$district)
to_votes <- dplyr::inner_join(federal_results, to_ed)
toFederalVotes <- dplyr::bind_rows(toFederalVotes, to_votes)
toFederalVotes$candidate <- as.factor(toFederalVotes$candidate)
toFederalVotes$year <- as.factor(toFederalVotes$year)
toFederalVotes$party <- as.factor(toFederalVotes$party)

save(toFederalVotes, file = "data/toFederalVotes.rda")

# Check Beaches results
library(dplyr)
toFederalVotes %>%
  filter(district == "35005") %>%
  select(candidate, year, votes) %>%
  group_by(candidate, year) %>%
  summarize(votes = sum(votes))

