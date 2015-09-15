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
levels(fed_votes$party)[17] <- "NDP"
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
poll_boundaries_2011 <- rgdal::readOGR(dsn = "data-raw/pd308.2011", layer = "pd_a") %>%
  spTransform(CRS('+init=epsg:4326'))

# saveRDS(poll_boundaries_2011, file = "data/poll_boundaries_2011.Rds")
# poll_boundaries_2011 <- readRDS("data/poll_boundaries_2011.Rds")

# TODO: Can't get pd_p to import
# poll_locations <- readOGR(dsn = "data-raw/pd308.2011", layer = "pd_p") %>%
#   spTransform(CRS('+init=epsg:4326'))
# rgdal::ogrListLayers("data-raw/pd308.2011/pd_p.shp")
# rgdal::ogrInfo("data-raw/pd308.2011/pd_p.shp", "pd_p")

# Find the ec_ids for Toronto Electoral Districts
ec_id <- dplyr::data_frame(id = poll_boundaries_2011@data$PD_ID,
                           district = poll_boundaries_2011@data$FED_NUM,
                           poll = poll_boundaries_2011@data$PD_NUM) %>%
  filter(district %in% to_fed_districts)
# Just 2011 votes for the 2011 shapefile
fed_votes_2011 <- dplyr::ungroup(fed_votes) %>%
  mutate(district = as.integer(district),
         poll = as.integer(poll)) %>%
  dplyr::filter(year == 2011)
# Add ec_id to the vote data
fed_votes_2011 <- dplyr::left_join(fed_votes_2011, ec_id)
geo_votes_2011 <- ggplot2::fortify(poll_boundaries_2011, region="PD_ID")
# Just the TO ec_ids
geo_votes_2011 <- dplyr::filter(geo_votes_2011, id %in% ec_id$id)
# saveRDS(geo_votes_2011, file = "data/geo_votes_2011.Rds")
# geo_votes_2011 <- readRDS("data/geo_votes_2011.Rds")

# Plot the map
ggplot(fed_votes_2011, aes(map_id = id)) +
  geom_map(aes(fill = cut_interval(prop_votes,length = 0.15)), map = geo_votes_2011) +
  scale_fill_brewer("Proportion of votes", labels=c("Low", rep("",5), "High"), palette = "Oranges") +
  labs(x="", y="", title="2011 Federal General Election") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + # make title bold and add space
  expand_limits(x = geo_votes_2011$long, y = geo_votes_2011$lat) +
  facet_wrap(~party, as.table = FALSE)
ggsave("2011_prop_votes.png", scale = 1, dpi = 200)

# Getting clipping with this approach
fed_votes_2011$id <- as.character(fed_votes_2011$id)
test <- dplyr::left_join(geo_votes_2011, fed_votes_2011)
toronto_map <- qmap("toronto", zoom = 11, maptype = 'terrain')
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill = cut_interval(prop_votes,length = 0.15)), alpha = 5/6, data=test) +
  scale_fill_brewer("Proportion of votes") +
  labs(x="", y="", title="Votes cast by poll in the 2011 Federal election") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + # make title bold and add space
  facet_wrap(~party)


# Spatial aggregation -----------------------------------------------------

library(dplyr)
library(rgeos)
library(rgdal)

# Poll boundaries
poll_boundaries_2011 <- readRDS("data/poll_boundaries_2011.Rds")
# Census tracts
if(file.exists("data-raw/gct_000b11a_e.zip")) {
  # Nothing to do
}  else {
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip",
                destfile = "data-raw/gct_000a11a_e.zip")
  unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw/gct")
}

ct_geo <- rgdal::readOGR(dsn = "data-raw/gct", layer = "gct_000b11a_e") %>%
  spTransform(CRS('+init=epsg:4326'))

# Toronto Wards
if(file.exists("data-raw/subdivisions_2010.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_subdivision_2010_wgs84.zip",
                destfile = "data-raw/subdivisions_2010.zip")
  unzip("data-raw/subdivisions_2010.zip", exdir="data-raw/to_wards")
}
to_geo <- rgdal::readOGR(dsn = "data-raw/to_wards", layer = "VOTING_SUBDIVISION_2010_WGS84") %>%
  spTransform(CRS('+init=epsg:4326'))
proj4string(to_geo) == proj4string(ct_geo)
# Subset the CTs to just those in Toronto
ct_geo_to <- ct_geo[to_geo,]
to_poll_boundaries <- poll_boundaries_2011[ct_geo_to,]
# saveRDS(to_poll_boundaries, file = "data/to_poll_boundaries.Rds")
# to_poll_boundaries <- readRDS("data/to_poll_boundaries.Rds")
plot(to_poll_boundaries)
plot(ct_geo_to)
plot(to_geo)

to_fed_districts <- unique(toFederalVotes$district)
ec_id <- dplyr::data_frame(id = to_poll_boundaries@data$PD_ID,
                           district = to_poll_boundaries@data$FED_NUM,
                           poll = to_poll_boundaries@data$PD_NUM) %>%
  filter(district %in% to_fed_districts)
# Just 2011 votes for the 2011 shapefile
fed_votes_2011 <- dplyr::ungroup(fed_votes) %>%
  mutate(district = as.integer(district),
         poll = as.integer(poll)) %>%
  dplyr::filter(year == 2011) %>%
  select(-year)
# Add ec_id to the vote data
fed_votes_2011 <- dplyr::left_join(fed_votes_2011, ec_id)
to_poll_boundaries@data <- left_join(to_poll_boundaries@data, fed_votes_2011, by = c("PD_ID" = "id"))

votes_agg <- aggregate(x = to_poll_boundaries["votes"], by = ct_geo_to, FUN = sum)
head(votes_agg@data)
sum(votes_agg@data$votes, na.rm = TRUE)
