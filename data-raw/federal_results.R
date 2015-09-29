# 2008 http://www.elections.ca/scripts/OVR2008/31/data/pollresults_resultatsbureau_canada.zip
# 2011 http://elections.ca/scripts/OVR2011/34/data_donnees/pollresults_resultatsbureau_canada.zip
# 2006 http://www.elections.ca/scripts/OVR2006/25/data_donnees/pollresults_resultatsbureau_canada.zip

sources <- list(year = c(2006, 2008, 2011),
                url = c("http://www.elections.ca/scripts/OVR2006/25/data_donnees/pollresults_resultatsbureau_canada.zip",
                        "http://www.elections.ca/scripts/OVR2008/31/data/pollresults_resultatsbureau_canada.zip",
                        "http://www.elections.ca/scripts/OVR2011/34/data_donnees/pollresults_resultatsbureau_canada.zip")
)
this_source <- 3 # the results to pull
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
# toFederalVotes <- to_votes
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


# Geocoding ---------------------------------------------------------------

# Data -------------------------------------------------------------------

library(toVotes)
library(dplyr)
# Extract a list of Toronto Federal Districts
data("toFederalVotes")
fed_votes <- toFederalVotes
# Sumarize just the three major parties, the rest are "Other"
major_parties <- c("Conservative", "Liberal", "NDP")
# Standardize the names
levels(fed_votes$party)[c(16, 17, 19)] <- c("NDP", "NDP", "Conservative")
levels(fed_votes$party)[!(levels(fed_votes$party) %in% major_parties)] <- "Other"
droplevels(fed_votes)
# Add in star candidate status
star_status <- readr::read_csv("data-raw/star_status.csv") %>%
  mutate(year = as.factor(year),
         candidate = as.factor(candidate))
candidates <- dplyr::left_join(fed_votes, star_status) %>%
  group_by(year, district, poll, party) %>%
  summarize(star_candidate = max(star_candidate),
            incumbent = max(incumbent),
            n = n()) %>%
  select(-n)
rm(star_status)
# Summarize votes by year, district, and poll
fed_votes <- fed_votes %>%
  mutate(# Clean up polling labels
    # -[letter]|[number] indicate sub-polls and should be merged
    poll = stringr::str_replace(poll, "(\\d{1,3})(-\\d+\\w?|\\D$)", "\\1")
  ) %>%
  group_by(year, district, poll, party) %>%
  summarize(votes = sum(votes)) %>%
  mutate(prop_votes = votes/sum(votes))

# Any votes in polls > 399 are from specific locations, rather than within boundaries
# votes_poll_locations <- fed_votes %>%
#   filter(poll > 399) %>%
#   group_by(year, district, party) %>%
#   summarize(votes = sum(votes))
# Turns out they are only in one district in 2008
# So we'll leave them alone
#fed_votes <- dplyr::left_join(fed_votes, candidates)
rm(candidates)

# Shapefiles -------------------------------------------------------------------

library(dplyr)
library(rgeos)
library(rgdal)

# Federal election shapefiles are listed here: http://geogratis.gc.ca/api/en/nrcan-rncan/ess-sst/-/(urn:iso:series)federal-electoral-districts-of-canada
# Map image is here: http://www.elections.ca/res/cir/maps2/mapprov.asp?map=Toronto&b=n&prov=35&lang=e
# PD_A is poll boundaries, match PD_NUM from 1 to 399 (normal polls)
# PD_P is poll locations, match PD_NUM from 400 to 499 (single building) and 500 to 599 (mobile)
# Advanced polls (PD_NUM 600 to 699) match both PD_A both PD_P
# FED_CA is electoral districts
# FED_NUM and EMRP_NAME is shape files match Electoral District Number and Polling Station Number in vote data
# S/R polls are Special Voting Rules using special ballots
years <- c(2006, 2008, 2011)
base_url <- "http://ftp2.cits.rncan.gc.ca/pub/geott/electoral/"
# Download and extract the shapefiles
for (year in years) {
  this_shapefile <- paste0("data-raw/pd308.", year, ".zip")
  if(file.exists(this_shapefile)) {
    # Nothing to do
  }  else {
    download.file(paste0(base_url, year, "/pd308.", year, ".zip"), destfile = this_shapefile)
    unzip(this_shapefile, exdir=paste0("data-raw/pd308.", year))
  }
  assign(paste0("poll_boundaries_", year), rgdal::readOGR(dsn = paste0("data-raw/pd308.", year),
                                                        layer = ifelse(year == 2011, "pd_a", "pd308_a")) %>%
           sp::spTransform(sp::CRS('+init=epsg:4326')))
}
rm(base_url, this_shapefile, year, years)
# Census tracts
if(file.exists("data-raw/gct_000b11a_e.zip")) {
  # Nothing to do
}  else {
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip",
                destfile = "data-raw/gct_000b11a_e.zip")
  unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw/gct")
}
census_tracts <- rgdal::readOGR(dsn = "data-raw/gct", layer = "gct_000b11a_e") %>%
  sp::spTransform(sp::CRS('+init=epsg:4326'))
# Toronto Wards
if(file.exists("data-raw/subdivisions_2010.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_subdivision_2010_wgs84.zip",
                destfile = "data-raw/subdivisions_2010.zip")
  unzip("data-raw/subdivisions_2010.zip", exdir="data-raw/to_wards")
}
wards <- rgdal::readOGR(dsn = "data-raw/to_wards", layer = "VOTING_SUBDIVISION_2010_WGS84") %>%
  sp::spTransform(sp::CRS('+init=epsg:4326'))
# Subset the CTs to just those in Toronto
to_census_tracts <- census_tracts[wards,]
# Subset polls to just those in Toronto
to_poll_boundaries_2006 <- poll_boundaries_2006[wards,]
to_poll_boundaries_2008 <- poll_boundaries_2008[wards,]
to_poll_boundaries_2011 <- poll_boundaries_2011[wards,]
rm(wards, poll_boundaries_2006, poll_boundaries_2008, poll_boundaries_2011, census_tracts)

# Spatial aggregation -----------------------------------------------------

# We need a column for each party for the aggregation
library(dplyr)
spread_votes <- fed_votes %>%
  dplyr::select(year, district, poll, party, votes) %>%
  tidyr::spread(party, votes)
spread_votes$district <- as.numeric(spread_votes$district)
spread_votes$poll <- as.numeric(spread_votes$poll)
# Join votes to shapefiles, by district and poll numbers
# Could use some refactoring here. Not sure how to use NSE with left_join
to_poll_boundaries_2006@data <- dplyr::left_join(to_poll_boundaries_2006@data,
                                          dplyr::filter(spread_votes, year == 2006),
                                          by = c("PD_NUM" = "poll", "FED_NUM" = "district"))
to_poll_boundaries_2006@data <- dplyr::select(to_poll_boundaries_2006@data, Other, Conservative, Liberal, NDP)
to_poll_boundaries_2008@data <- dplyr::left_join(to_poll_boundaries_2008@data,
                                          dplyr::filter(spread_votes, year == 2008),
                                          by = c("PD_NUM" = "poll", "FED_NUM" = "district"))
to_poll_boundaries_2008@data <- dplyr::select(to_poll_boundaries_2008@data, Other, Conservative, Liberal, NDP)
to_poll_boundaries_2011@data <- dplyr::left_join(to_poll_boundaries_2011@data,
                                          dplyr::filter(spread_votes, year == 2011),
                                          by = c("PD_NUM" = "poll", "FED_NUM" = "district"))
to_poll_boundaries_2011@data <- dplyr::select(to_poll_boundaries_2011@data, Other, Conservative, Liberal, NDP)
# Consider merging these into a single Spatial object
fed_votes_2006_geo <- aggregate(x = to_poll_boundaries_2006, by = to_census_tracts, FUN = sum, na.rm = TRUE, areaWeighted = TRUE)
fed_votes_2006_geo$id <- to_census_tracts$CTUID
fed_votes_2008_geo <- aggregate(x = to_poll_boundaries_2008, by = to_census_tracts, FUN = sum, na.rm = TRUE, areaWeighted = TRUE)
fed_votes_2008_geo$id <- to_census_tracts$CTUID
fed_votes_2011_geo <- aggregate(x = to_poll_boundaries_2011, by = to_census_tracts, FUN = sum, na.rm = TRUE, areaWeighted = TRUE)
fed_votes_2011_geo$id <- to_census_tracts$CTUID

devtools::use_data(fed_votes_2006_geo, fed_votes_2008_geo, fed_votes_2011_geo,
                   to_poll_boundaries_2006, to_poll_boundaries_2008, to_poll_boundaries_2011, overwrite = TRUE)


# Candidate attributes ----------------------------------------------------

library(dplyr)
spread_star_candidate <- candidates %>%
  dplyr::select(year, district, poll, party, star_candidate) %>%
  tidyr::spread(party, star_candidate)
spread_star_candidate$district <- as.numeric(spread_star_candidate$district)
spread_star_candidate$poll <- as.numeric(spread_star_candidate$poll)
names(spread_star_candidate)[4:7] <- stringr::str_c(names(spread_star_candidate)[4:7], "star-candidate", sep = "_")
spread_incumbent <- candidates %>%
  dplyr::select(year, district, poll, party, incumbent) %>%
  tidyr::spread(party, incumbent)
spread_incumbent$district <- as.numeric(spread_incumbent$district)
spread_incumbent$poll <- as.numeric(spread_incumbent$poll)
names(spread_incumbent)[4:7] <- stringr::str_c(names(spread_incumbent)[4:7], "incumbent", sep = "_")
candidate_attr <- dplyr::left_join(spread_incumbent, spread_star_candidate)
# 2006
candidate_attr_2006 <- to_poll_boundaries_2006
candidate_attr_2006@data <- dplyr::left_join(to_poll_boundaries_2006@data,
                                             dplyr::filter(candidate_attr, year == 2006),
                                             by = c("PD_NUM" = "poll", "FED_NUM" = "district"))
candidate_attr_2006@data <- dplyr::select(candidate_attr_2006@data, 9:16)
candidate_attr_2006 <- aggregate(x = candidate_attr_2006, by = to_census_tracts, FUN = max, na.rm = TRUE)
candidate_attr_2006$id <- to_census_tracts$CTUID
# 2008
candidate_attr_2008 <- to_poll_boundaries_2008
candidate_attr_2008@data <- dplyr::left_join(to_poll_boundaries_2008@data,
                                             dplyr::filter(candidate_attr, year == 2008),
                                             by = c("PD_NUM" = "poll", "FED_NUM" = "district"))
candidate_attr_2008@data <- dplyr::select(candidate_attr_2008@data, 17:24)
candidate_attr_2008 <- aggregate(x = candidate_attr_2008, by = to_census_tracts, FUN = max, na.rm = TRUE)
candidate_attr_2008$id <- to_census_tracts$CTUID
# 2011
candidate_attr_2011 <- to_poll_boundaries_2011
candidate_attr_2011@data <- dplyr::left_join(to_poll_boundaries_2011@data,
                                             dplyr::filter(candidate_attr, year == 2011),
                                             by = c("PD_NUM" = "poll", "FED_NUM" = "district"))
candidate_attr_2011@data <- dplyr::select(candidate_attr_2011@data, 18:25)
candidate_attr_2011 <- aggregate(x = candidate_attr_2011, by = to_census_tracts, FUN = max, na.rm = TRUE)
candidate_attr_2011$id <- to_census_tracts$CTUID
# Combine
candidate_attr_2006@data$year <- 2006
candidate_attr_2008@data$year <- 2008
candidate_attr_2011@data$year <- 2011
candidate_attr <- dplyr::bind_rows(candidate_attr_2006@data,
                                   candidate_attr_2008@data,
                                   candidate_attr_2011@data)
candidate_star <- candidate_attr[,-(1:4)]
names(candidate_star)[1:4] <- c("Other", "Conservative", "Liberal", "NDP")
candidate_incumbent <- candidate_attr[,c(1:4,9:10)]
names(candidate_incumbent)[1:4] <- c("Other", "Conservative", "Liberal", "NDP")
candidate_star <- tidyr::gather(candidate_star, party, star_candidate, 1:4) %>%
  dplyr::mutate(star_candidate = ifelse(star_candidate == 1, 1, 0))
candidate_incumbent <- tidyr::gather(candidate_incumbent, party, incumbent, 1:4) %>%
  dplyr::mutate(incumbent = ifelse(incumbent == 1, 1, 0))
candidate_attr <- dplyr::left_join(candidate_incumbent, candidate_star)
devtools::use_data(candidate_attr, overwrite = TRUE)
rm(candidate_incumbent, candidate_star, candidate_attr_2011, candidate_attr_2008, candidate_attr_2006)
rm(spread_incumbent, spread_star_candidate)
