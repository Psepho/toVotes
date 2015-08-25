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

# Shapefiles are listed here: http://geogratis.gc.ca/api/en/nrcan-rncan/ess-sst/-/(urn:iso:series)federal-electoral-districts-of-canada
# Map image is here: http://www.elections.ca/res/cir/maps2/mapprov.asp?map=Toronto&b=n&prov=35&lang=e

poll_2011 <- "http://ftp2.cits.rncan.gc.ca/pub/geott/electoral/2011/pd308.2011.zip"
download.file(poll_2011, destfile = "data-raw/pd308.2011.zip")
unzip("data-raw/pd308.2011.zip", exdir="data-raw/fed")
library(rgdal)
poll_bound_shapefile <- rgdal::readOGR(dsn = "data-raw/fed", layer = "pd_a")
poll_bound_shapefile <- poll_bound_shapefile[poll_bound_shapefile$FED_NUM %in% to_fed_districts,]
poll_bound_shapefile <- spTransform(poll_bound_shapefile, CRS("+proj=longlat +datum=WGS84"))
plot(poll_bound_shapefile)

poll_loc_shapefile <- rgdal::readOGR(dsn = "data-raw/fed", layer = "pd_p")
ogrListLayers("data-raw/fed/pd_p.dbf")
ogrInfo("data-raw/fed/pd_p.shp", "pd_p")

poll_loc_shapefile <- poll_loc_shapefile[poll_loc_shapefile$FED_NUM %in% to_fed_districts,]
to_shapefile <- rgdal::spTransform(poll_loc_shapefile, sp::CRS("+proj=longlat +datum=WGS84"))

library(rgdal)
plot(to_shapefile)

shapefile_2011@proj4string

fed_geo_2014 <- ggplot2::fortify(shapefile_2011, region="FEDNUM")


federal_geo_2011_url <- "http://ftp2.cits.rncan.gc.ca/pub/geott/electoral/2011/fed308.2011.zip"
if(file.exists("data-raw/fed308.2011.zip")) {
  # Nothing to do
}  else {
  download.file(federal_geo_2011_url, destfile = "data-raw/fed308.2011.zip")
  unzip("data-raw/fed308.2011.zip", exdir="data-raw/fed")
}

shapefile_2011 <- rgdal::readOGR(dsn = "data-raw/fed", layer = "FED_CA_1.0_0_ENG")
shapefile_2011@proj4string
shapefile_2011 <- spTransform(shapefile_2011, CRS("+proj=longlat +datum=WGS84"))
fed_geo_2014 <- ggplot2::fortify(shapefile_2011, region="FEDNUM")
data("toFederalVotes")
to_fed_districts <- unique(toFederalVotes$district)
fed_geo_2014 <- dplyr::filter(fed_geo_2014, id %in% to_fed_districts) #stringr::str_sub(fed_geo_2014$id,1,2) == "35")
names(fed_geo_2014)[length(names(fed_geo_2014))] <- "district"
fed_geo_2014$year <- as.factor("2011")
rm(shapefile_2011)

library(dplyr)
major_parties <- c("Conservative", "Green Party", "Liberal", "NDP-New Democratic Party")
fed_results <- toFederalVotes %>%
  filter(year == "2011", party %in% major_parties) %>% # Getting memory errors for full dataframe
  group_by(year, party, district) %>%
  summarize(votes = sum(votes))
fed_results <- droplevels(fed_results)
levels(fed_results$party)[4] <- "NDP"
fed_geo <- dplyr::left_join(fed_results, fed_geo_2014)
rm(fed_results, fed_geo_2014, federal_geo_2011_url, major_parties)

library(ggplot2)
ggplot(fed_geo) +
  aes(long, lat, group=group, fill=votes) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_colour_brewer("Votes") +
  facet_wrap(~party)


library(ggmap)
library(mapproj)
toronto_map <- qmap("queens park,toronto", zoom = 11, maptype = 'terrain')
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=votes), alpha = 5/6, data=fed_geo) +
  scale_colour_brewer("Votes") +
  facet_wrap(~party)

