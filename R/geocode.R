geocodePolls <- function() {
  # Download shapefiles -----------------------------------------------------
  years <- c(2006, 2010, 2014)
  base_shapefile_url <- "http://opendata.toronto.ca/gcc/"
  shapefile_prefix <- "data-raw/subdivision_"
  for (year in years) {
    if(file.exists(paste(shapefile_prefix, year,".zip",sep=""))) {
      # Nothing to do
    }  else {
      download.file(paste0(base_shapefile_url, ifelse(year == 2014, "voting_subdivsion_", "voting_subdivision_"), year, "_wgs84.zip"),
                    destfile = paste0(shapefile_prefix,year,".zip"))
      unzip(paste(shapefile_prefix, year, ".zip", sep=""), exdir=paste0("data-raw/", year))
    }

    assign(paste("geo", year, sep = "_"),
           sf::st_read(paste0("data-raw/", year)) %>%
             sf::st_transform(crs = "+init=epsg:4326") %>% # Not strictly necessary, since already using this projection
             dplyr::mutate(ward = as.integer(stringr::str_sub(AREA_LONG, 1, 2)), # Extract Ward ID
                           area = as.integer(stringr::str_sub(AREA_LONG, -3, -1)), # Extract Area ID
                           year = as.factor(year)) %>%
             dplyr::select(ward, area, year, geometry)
    )
  }
  geo_2003 <- geo_2006 %>% # No shapefiles are available for 2003, so assuming same as 2006
    dplyr::mutate(year = as.factor(2003))
  rm(year, years, base_shapefile_url, shapefile_prefix)

  # Merge shapefiles --------------------------------------------------------------

  toPollGeo <- rbind(geo_2014, geo_2010, geo_2006, geo_2003)
  rm(geo_2014, geo_2003, geo_2006, geo_2010)

  # Clean up ----------------------------------------------------------------

  toPollGeo <- dplyr::inner_join(toPollGeo, toVotes::ward_regions, by=c("ward"))
  devtools::use_data(toPollGeo, overwrite = TRUE)
  toPollGeo
}
