library(magrittr)
library(tidyverse)

# TODO: Aggregate provincial results into Census Tracts

# Geocoding ---------------------------------------------------------------

# Provincial election shapefiles are listed here: https://www.elections.on.ca/en/voting-in-ontario/electoral-district-shapefiles/limited-use-data-product-licence-agreement/download-shapefiles.html
# POLL_DIV_1, POLL_DIV_3 are the poll division number (integer and three-character forms)
# ED_ID, DATA_COMPI are the Electoral District number (integer and three-character forms)
# KPI04 is the Electoral District _name_, as used on the WikiPedia election page

base_shapefile_url <- "https://www.elections.on.ca/content/dam/NGW/sitecontent/2016/preo/shapefiles/"
pd_shapefile <- "Polling%20Division%20Shapefile%20-%202014%20General%20Election.zip"

# Download and extract the provincial shapefile
prov_shapefile <- paste0("data-raw/", pd_shapefile)
if(file.exists(prov_shapefile)) {
  # Nothing to do
}  else {
  download.file(paste0(base_shapefile_url, pd_shapefile), destfile = prov_shapefile)
  unzip(prov_shapefile, exdir="data-raw/prov_shapefile")
}
prov_geo <- sf::st_read("data-raw/prov_shapefile", layer = "PDs_Ontario") %>%
  sf::st_transform(crs = "+init=epsg:4326")
rm(base_shapefile_url, pd_shapefile, prov_shapefile)

# Need electoral district names and numbers to match with party affiliations below
electoral_districts <- prov_geo %>%
  dplyr::transmute(electoral_district = as.character(DATA_COMPI),
                   electoral_district_name = stringr::str_to_title(KPI04)) %>%
  dplyr::group_by(electoral_district, electoral_district_name) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(electoral_district_name = stringr::str_replace_all(utf8::as_utf8(electoral_district_name), "\u0097", " ")) %>%
  dplyr::select(electoral_district, electoral_district_name)

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

# Download raw data -------------------------------------------------------

raw_results_file <- "http://www.elections.on.ca/content/dam/NGW/sitecontent/2017/results/Poll%20by%20Poll%20Results%20-%20Excel.zip"

zip_file <- "data-raw/Poll%20by%20Poll%20Results%20-%20Excel.zip"
if(file.exists(zip_file)) { # Only download the data once
  # File exists, so nothing to do
}  else {
  download.file(raw_results_file,
                destfile = zip_file)
  unzip(zip_file, exdir="data-raw") # Extract the data into data-raw
  file.rename("data-raw/GE Results - 2014 EN", "data-raw/pollresults")
}

# Extract votes -----------------------------------------------------------

file_pattern <- "*_[[:digit:]]{3} FINAL.xls" # Can use this to filter down to specific files
poll_data <- list.files(path = "data-raw/pollresults", pattern = file_pattern, full.names = TRUE) %>% # Find all files that match the pattern
  purrr::set_names() %>%
  purrr::map_df(readxl::read_excel, skip= 1, sheet = 1, col_types = "text", .id = "file") %>%   # Import each file and merge into a dataframe
  # Specifying sheet = 1 just to be clear we're ignoring the rest of the sheets
  # Declare col_types since there are duplicate surnames and map_df can't recast column types in the rbind
  # For example, Bell is in both district 014 and 063
  dplyr::select(-starts_with("X__")) %>% # Drop all of the blank columns
  dplyr::select(1:2,4:8,15:dim(.)[2]) %>% # Reorganize a bit and drop unneeded columns
  dplyr::rename(poll_number = `POLL NO.`) %>%
  tidyr::gather(candidate, votes, -file, -poll_number) %>% # Convert to a long table
  dplyr::filter(!is.na(votes),
                poll_number != "Totals") %>%
  dplyr::mutate(electoral_district = stringr::str_extract(file, "[[:digit:]]{3}"),
                votes = as.numeric(votes)) %>%
  dplyr::select(-file) %>%
  dplyr::left_join(electoral_districts)

# Candidate parties -------------------------------------------------------

candidate_webpage <- "https://en.wikipedia.org/wiki/Ontario_general_election,_2014#Candidates_by_region"
candidate_tables <- "table" # Use an xpath selector to get the drop down list by ID

candidates <- xml2::read_html(candidate_webpage) %>%
  rvest::html_nodes(candidate_tables) %>% # Pull tables from the wikipedia entry
  .[16:28] %>% # Drop unecessary tables
  rvest::html_table(fill = TRUE)

# Setup empty dataframe to store results
candidate_parties <- tibble::as_tibble(
  electoral_district_name = NULL,
  party = NULL,
  candidate = NULL
)

for(i in seq_along(1:length(candidates))) { # Messy, but works
  this <- candidates[[i]]
  # The header spans mess up the header row, so renaming
  names(this) <- c(this[1,-c(3,4)], "NA", "Incumbent")
  # Get rid of the blank spacer columns
  this <- this[-1, ]
  # Drop the NA columns by keeping only odd columns
  this <- this[,seq(from = 1, to = dim(this)[2], by = 2)]
  this %<>%
    tidyr::gather(party, candidate, -`Electoral District`) %>%
    dplyr::rename(electoral_district_name = `Electoral District`) %>%
    dplyr::filter(party != "Incumbent")
  candidate_parties <- dplyr::bind_rows(candidate_parties, this)
}
rm(this, i)

# Join electoral district numbers into the candidate and parties data

candidate_parties %<>%
  dplyr::mutate(electoral_district_name = stringr::str_replace_all(electoral_district_name, "—", " ")) %>%
  dplyr::left_join(electoral_districts) %>%
  dplyr::filter(!candidate == "") %>%
  tidyr::separate(candidate, into = c("first","candidate"), extra = "merge", remove = TRUE)

poll_data_party_match_table <- poll_data %>%
  group_by(candidate, electoral_district_name) %>%
  summarise() %>%
  fuzzyjoin::stringdist_left_join(candidate_parties,
                                  ignore_case = TRUE) %>%
  dplyr::select(candidate = candidate.x,
                # electoral_district_name = electoral_district_name.x,
                party = party,
                electoral_district = electoral_district) %>%
  dplyr::filter(!is.na(party))

poll_data %<>%
  dplyr::left_join(poll_data_party_match_table)

rm(poll_data_party_match_table, candidate_parties, candidates, candidate_tables, candidate_webpage,
   raw_results_file, zip_file, file_pattern)

# Spatial votes -----------------------------------------------------------
# Join poll_data with prov_geo
# Subset to Toronto first

spread_poll_data<- poll_data %>%
  dplyr::group_by(electoral_district_name, electoral_district, poll_number, party) %>%
  dplyr::summarize(votes = sum(votes)) %>%
  tidyr::spread(party, votes)

to_prov_geo <- prov_geo %>%
  sf::st_intersection(toronto_wards) %>%
  dplyr::mutate(electoral_district = as.character(DATA_COMPI),
                electoral_district_name = stringr::str_to_title(KPI04),
                poll_number = POLL_DIV_3) %>%
  dplyr::mutate(electoral_district_name = stringr::str_replace_all(utf8::as_utf8(electoral_district_name), "\u0097", " ")) %>%
  dplyr::select(geometry, electoral_district, electoral_district_name, poll_number) %>%
  dplyr::left_join(spread_poll_data) %>%
  sf::st_as_sf()

to_census_tracts <- census_tracts %>%
  sf::st_intersection(toronto_wards)

tmp <- sf::st_interpolate_aw(to_prov_geo, to_census_tracts, sum)

ggplot() + geom_sf(data = to_prov_geo, aes(fill = Liberal))

devtools::use_data(prov_geo, overwrite = TRUE)
