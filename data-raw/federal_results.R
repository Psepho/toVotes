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
to_fed_districts <- unique(toFederalVotes$district)
# Sumarize just the three major parties, the rest are "Other"
major_parties <- c("Conservative", "Liberal", "NDP")
fed_votes <- toFederalVotes
# Standardize the names
levels(fed_votes$party)[16] <- "NDP"
levels(fed_votes$party)[19] <- "Conservative"
levels(fed_votes$party)[4] <- "NDP"
levels(fed_votes$party)[!(levels(fed_votes$party) %in% major_parties)] <- "Other"
droplevels(fed_votes)
fed_votes <- fed_votes %>%
  mutate(# Clean up polling labels
         # -[letter]|[number] indicate sub-polls and should be merged
         poll = stringr::str_replace(poll, "(\\d{1,3})(-\\d+\\w?|\\D$)", "\\1")
  ) %>%
  group_by(year, district, poll, party) %>%
  summarize(votes = sum(votes)) %>%
  mutate(prop_votes = votes/sum(votes))

# Map -------------------------------------------------------------------

# Shapefiles are listed here: http://geogratis.gc.ca/api/en/nrcan-rncan/ess-sst/-/(urn:iso:series)federal-electoral-districts-of-canada
# Map image is here: http://www.elections.ca/res/cir/maps2/mapprov.asp?map=Toronto&b=n&prov=35&lang=e

library(ggplot2)
library(rgdal)
library(maptools)
library(ggmap)
library(rgeos)
# Download and extract the shapefile
federal_geo_2011_url <- "http://ftp2.cits.rncan.gc.ca/pub/geott/electoral/2011/pd308.2011.zip"
if(file.exists("data-raw/pd308.2011.zip")) {
  # Nothing to do
}  else {
  download.file(federal_geo_2011_url, destfile = "data-raw/pd308.2011.zip")
  unzip("data-raw/pd308.2011.zip", exdir="data-raw/pd308.2011")
}
# PD_A is poll boundaries, match PD_NUM from 1 to 399 (normal polls)
# PD_P is poll locations, match PD_NUM from 400 to 499 (single building) and 500 to 599 (mobile)
# Advanced polls (PD_NUM 600 to 699) match both PD_A both PD_P
# FED_CA is electoral districts
# FED_NUM and EMRP_NAME is shape files match Electoral District Number and Polling Station Number in vote data
# S/R polls are Special Voting Rules using special ballots
# Read in the poll boundary shapefile and filter to just Toronto districts
poll_boundaries <- rgdal::readOGR(dsn = "data-raw/pd308.2011", layer = "pd_a") %>%
  spTransform(CRS('+init=epsg:4326')) %>%
  ggplot2::fortify(region="PD_ID")
# poll_locations <- readOGR(dsn = "data-raw/pd308.2011", layer = "pd_p") %>%
#   spTransform(CRS('+init=epsg:4326')) %>%
#   ggplot2::fortify(region="PD_ID")
# rgdal::ogrListLayers("data-raw/pd308.2011/pd_p.shp")
# rgdal::ogrInfo("data-raw/pd308.2011/pd_p.shp", "pd_p")

ec_id <- dplyr::data_frame(id = poll_boundaries@data$PD_ID,
                           district = poll_boundaries@data$FED_NUM,
                           poll = poll_boundaries@data$PD_NUM) %>%
  filter(district %in% to_fed_districts)

test <- dplyr::ungroup(fed_votes) %>%
  mutate(district = as.integer(district),
         poll = as.integer(poll)) %>%
  dplyr::filter(year == 2011) %>%
  dplyr::left_join(ec_id) %>%
  group_by(district, poll, party)


# Plot the map
# TODO: Update to plot polls, not districts

ggplot(votes, aes(map_id = district)) +
  geom_map(aes(fill = votes), map = fed_geo_2014) +
  scale_colour_brewer("Votes") +
  expand_limits(x = fed_geo_2014$long, y = fed_geo_2014$lat) +
  facet_wrap(~party)

# Getting clipping with this approach

# library(ggmap)
# library(mapproj)
# toronto_map <- qmap("toronto", zoom = 11, maptype = 'terrain')
# toronto_map +
#   geom_polygon(aes(x=long, y=lat, group=group, fill=votes), alpha = 5/6, data=fed_geo) +
#   scale_colour_brewer("Votes") +
#   facet_wrap(~party)

# Residual

# poll_2011 <- "http://ftp2.cits.rncan.gc.ca/pub/geott/electoral/2011/pd308.2011.zip"
# download.file(poll_2011, destfile = "data-raw/pd308.2011.zip")
# unzip("data-raw/pd308.2011.zip", exdir="data-raw/fed")
# library(rgdal)
# poll_bound_shapefile <- poll_bound_shapefile[poll_bound_shapefile$FED_NUM %in% to_fed_districts,]
# poll_bound_shapefile@proj4string
# wgs84proj <- CRS('+init=epsg:4326') #CRS("+proj=longlat +datum=WGS84"))
# poll_bound_shapefile <- spTransform(poll_bound_shapefile, wgs84proj)
# plot(poll_bound_shapefile)
#
# poll_loc_shapefile <- rgdal::readOGR(dsn = "data-raw/fed", layer = "pd_p")
# ogrListLayers("data-raw/fed/pd_p.dbf")
# ogrInfo("data-raw/fed/pd_p.shp", "pd_p")
#
# poll_loc_shapefile <- poll_loc_shapefile[poll_loc_shapefile$FED_NUM %in% to_fed_districts,]
# to_shapefile <- rgdal::spTransform(poll_loc_shapefile, sp::CRS("+proj=longlat +datum=WGS84"))
#
# library(rgdal)
# plot(to_shapefile)
#
# shapefile_2011@proj4string
#
# fed_geo_2014 <- ggplot2::fortify(shapefile_2011, region="FEDNUM")
