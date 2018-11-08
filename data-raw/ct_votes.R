library(magrittr)

# Download shapefiles -----------------------------------------------------

# Census tracts
if(file.exists("data-raw/gct_000b11a_e.zip")) {
  # Nothing to do
}  else {
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip",
                destfile = "data-raw/gct_000b11a_e.zip")
  unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw/gct")
}
census_tracts <- sf::st_read("data-raw/gct", layer = "gct_000b11a_e") %>%
  sf::st_transform(crs = "+init=epsg:4326")

# Toronto wards
if(file.exists("data-raw/voting_location_2014_wgs84.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_location_2014_wgs84.zip",
                destfile = "data-raw/voting_location_2014_wgs84.zip")
  unzip("data-raw/voting_location_2014_wgs84.zip", exdir="data-raw/voting_location_2014_wgs84")
}
toronto_wards <- sf::st_read("data-raw/voting_location_2014_wgs84", layer = "VOTING_LOCATION_2014_WGS84") %>%
  sf::st_transform(crs = "+init=epsg:4326")


# Subset to Toronto CTs ---------------------------------------------------

to_census_tracts <- census_tracts %>%
  sf::st_intersection(toronto_wards) %>%
  dplyr::select(c(1, 4, 8, 13, 22)) %>%
  dplyr::mutate(ward = as.integer(stringr::str_sub(PT_LNG_CD, 1, 2)), # Extract Ward ID
                area = as.integer(stringr::str_sub(PT_LNG_CD, -3, -1))) # Extract Area ID

# For each year and candidate, sum votes by CT
# Spread vote_geo by year and candidate

library(toVotes)
data("toVotes")
vote_geo <- sf::st_sf(dplyr::left_join(toVotes, toPollGeo))
tmp <- as(vote_geo, "Spatial")


spread_votes <- toVotes %>%
  dplyr::select(year, candidate, votes) %>%
  tidyr::spread(year, candidate, votes)

tmp <- sf::st_interpolate_aw(to_prov_geo, to_census_tracts, sum)



# Old aggregation approach ------------------------------------------------

# Shapefiles -------------------------------------------------------------------

library(rgeos)
library(rgdal)

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

# Toronto wards
if(file.exists("data-raw/voting_location_2014_wgs84.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_location_2014_wgs84.zip",
                destfile = "data-raw/voting_location_2014_wgs84.zip")
  unzip("data-raw/voting_location_2014_wgs84.zip", exdir="data-raw/voting_location_2014_wgs84")
}
toronto_wards <- rgdal::readOGR(dsn = "data-raw/voting_location_2014_wgs84", layer = "VOTING_LOCATION_2014_WGS84") %>%
  sp::spTransform(sp::CRS('+init=epsg:4326'))
# Subset the CTs to just those in Toronto
to_census_tracts <- census_tracts[toronto_wards,]
toronto_wards@data %<>%
  dplyr::mutate(ward = as.integer(stringr::str_sub(PT_LNG_CD, 1, 2)), # Extract Ward ID
                area = as.integer(stringr::str_sub(PT_LNG_CD, -3, -1))) # Extract Area ID

# Spatial aggregation -----------------------------------------------------

# We need a column for each candidate for the aggregation

# Subset to just major candidates

major_candidates <- toVotes %>% # Restrict major candidates to anyone that recieved more than 10,000 votes
  dplyr::group_by(candidate) %>%
  dplyr::summarise(votes = sum(votes)) %>%
  dplyr::filter(votes > 10000) %>%
  dplyr::arrange(desc(votes)) %>%
  dplyr::select(candidate)

spread_votes <- toVotes %>%
  dplyr::right_join(major_candidates) %>%
  dplyr::select(-type) %>%
  tidyr::unite("candidate_year", candidate, year) %>%
  dplyr::filter(!is.na(area)) %>%
  tidyr::spread(candidate_year, votes)

# Join votes to shapefiles, by district and poll numbers

to_geo_votes <- toronto_wards
to_geo_votes@data <- dplyr::select(to_geo_votes@data, ward, area)
to_geo_votes@data <- dplyr::left_join(to_geo_votes@data, spread_votes)

# Consider merging these into a single Spatial object
to_ct_votes <- aggregate(x = to_geo_votes, by = to_census_tracts, FUN = sum, na.rm = TRUE, areaWeighted = TRUE)
to_ct_votes$ct_id <- to_census_tracts$CTUID
to_ct_votes_data <- to_ct_votes@data %>%
  tidyr::gather(candidate_year, votes, -ward, -area, -ct_id) %>%
  tidyr::extract(candidate_year, c("candidate", "year"), "([[:alnum:]]+.[[:alnum:]]+)_([[:alnum:]]+)")

readr::write_csv(to_ct_votes_data, "to_votes_ct.csv")
