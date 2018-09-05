library(magrittr)
library(toVotes)
vote_geo <- sf::st_sf(dplyr::left_join(toVotes, toPollGeo))

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

spread_votes <- vote_geo %>%
  dplyr::select(year, candidate, votes, geometry) %>%
  tidyr::spread(year, candidate, votes)

tmp <- sf::st_interpolate_aw(to_prov_geo, to_census_tracts, sum)

