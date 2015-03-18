# 2008 http://www.elections.ca/scripts/OVR2008/31/data/pollresults_resultatsbureau_canada.zip
# 2011 http://elections.ca/scripts/OVR2011/34/data_donnees/pollresults_resultatsbureau_canada.zip

zip_file <- "data-raw/pollresults_resultatsbureau_canada.zip"
if(file.exists(zip_file)) { # Only download the data once
  # File exists, so nothing to do
}  else {
  download.file("http://www.elections.ca/scripts/OVR2008/31/data/pollresults_resultatsbureau_canada.zip",
                destfile = zip_file)
  unzip(zip_file, exdir=paste("data-raw", "pollresults", sep = "/")) # Extract the data into data-raw
}

on_files <- list.files(path = "data-raw/pollresults",
                       pattern = "pollresults_resultatsbureau35[[:digit:]]{3}.csv",
                       full.names = TRUE)
federal_results <- do.call("rbind", lapply(on_files, function(.file){dplyr::tbl_df(read.csv(.file, na.strings = c("NA")))}))
federal_results <- dplyr::transmute(federal_results,
                            candidate = as.factor(paste(Candidate.s.Family.Name.Nom.de.famille.du.candidat, Candidate.s.First.Name.Prénom.du.candidat, sep = " ")),
                            year = as.factor(2008),
                            type = "federal",
                            votes = as.integer(Candidate.Poll.Votes.Count.Votes.du.candidat.pour.le.bureau),
                            party = Political.Affiliation.Name_English.Appartenance.politique_Anglais,
                            district = as.character(Electoral.District.Number.Numéro.de.circonscription),
                            poll = as.character(Polling.Station.Number.Numéro.du.bureau.de.scrutin),
                            incumbent = as.logical(ifelse(Incumbent.Indicator.Indicateur_Candidat.sortant == "Y", 1, 0))
                            )
to_ed <- read.csv("data-raw/TO_federal_election_districts.csv")[,1]
to_ed <- data.frame(district = as.character(to_ed[!is.na(to_ed)]))
to_votes <- dplyr::inner_join(federal_results, to_ed)
toFederalVotes <- rbind(toFederalVotes, to_votes)

toFederalVotes$votes <- as.integer(toFederalVotes$votes)

save(toFederalVotes, file = "data/toFederalVotes.rda")

# Check Beaches results
library(dplyr)
toFederalVotes %>%
  filter(district == "35005") %>%
  select(candidate, year, votes) %>%
  group_by(candidate, year) %>%
  summarize(votes = sum(votes))

