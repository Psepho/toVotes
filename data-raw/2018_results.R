library(magrittr)
# Download results --------------------------------------------------------

if(file.exists("data-raw/2018_Results.zip")) {
  # Nothing to do
}  else {
  download.file("https://www.toronto.ca/ext/open_data/catalog/data_set_files/2018_Results.zip", destfile = "data-raw/2018_Results.zip")
  unzip("data-raw/2018_Results.zip", exdir="data-raw")
}

# Convert to data frame ---------------------------------------------------

mayor_file <- "data-raw/2018_Toronto_Poll_By_Poll_Mayor.xlsx"
results <- mayor_file %>%
  readxl::excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map_df(~ readxl::read_excel(path = mayor_file, sheet = .x, skip = 1), .id = "sheet") %>%
  dplyr::filter(Subdivision != "Mayor") %>%
  dplyr::filter(!grepl('Totals', Subdivision)) %>%
  dplyr::rename(ward = sheet, candidate = Subdivision) %>%
  dplyr::mutate(ward = as.integer(stringr::str_remove(ward, "Ward "))) %>%
  tidyr::gather(area, votes, -ward, -candidate) %>%
  dplyr::filter(!is.na(votes)) %>%
  dplyr::mutate(year = as.factor(2018), type = "Mayor",
                area = as.integer(area),
                candidate = as.factor(candidate)) %>%
  dplyr::filter(!is.na(area)) # Removes the totals for each ward
data("toVotes")
toVotes <- dplyr::bind_rows(toVotes, results)
usethis::use_data(toVotes, overwrite = TRUE)


# Aggregate to census tracts ----------------------------------------------

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
  dplyr::filter(year == 2018) %>%
  dplyr::group_by(candidate) %>%
  dplyr::summarise(votes = sum(votes)) %>%
  dplyr::filter(votes > 100000) %>%
  dplyr::arrange(desc(votes)) %>%
  dplyr::select(candidate)

spread_votes <- toVotes %>%
  dplyr::filter(year == 2018) %>%
  dplyr::right_join(major_candidates) %>%
  dplyr::select(-type, -year) %>%
  dplyr::filter(!is.na(area)) %>%
  tidyr::spread(candidate, votes)

# Join votes to shapefiles, by district and poll numbers

to_geo_votes <- toronto_wards
to_geo_votes@data <- dplyr::select(to_geo_votes@data, ward, area)
to_geo_votes@data <- dplyr::left_join(to_geo_votes@data, spread_votes)

# Consider merging these into a single Spatial object
to_ct_votes <- aggregate(x = to_geo_votes, by = to_census_tracts, FUN = sum, na.rm = TRUE, areaWeighted = TRUE)
to_ct_votes$ct_id <- to_census_tracts$CTUID
to_ct_votes_data <- to_ct_votes@data %>%
  tidyr::gather(candidate, votes, -ward, -area, -ct_id)
