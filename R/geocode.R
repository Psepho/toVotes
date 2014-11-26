geocodePolls <- function() {
  # Download shapefiles -----------------------------------------------------
  years <- c(2006, 2010, 2014)
  for (year in years) {
    if(file.exists(paste("data-raw/subdivisions_",year,".zip",sep=""))) {
      # Nothing to do
    }  else {
      download.file(paste("http://opendata.toronto.ca/gcc/", ifelse(year == 2014,"voting_subdivsion_", "voting_subdivision_"), year, "_wgs84.zip", sep = ""),
                    destfile = paste("data-raw/subdivisions_",year,".zip",sep=""))
      unzip(paste("data-raw/subdivisions_", year, ".zip", sep=""), exdir="data-raw")
    }
    shape <- paste("shapefile_", year, sep="")
    file <- paste("VOTING_SUBDIVISION_",year,"_WGS84",sep="")
    assign(shape,rgdal::readOGR(dsn = "data-raw", layer = file))
    rm(file,shape)
  }
  rm(year,years)
  # Shapefiles --------------------------------------------------------------
  # Extract the data object for each year and then combine into one data frame
  geo_2014 <- ggplot2::fortify(shapefile_2014,region="AREA_NAME")
  geo_2014$year <- as.integer(2014)
  geo_2010 <- ggplot2::fortify(shapefile_2010,region="AREA_NAME")
  geo_2010$year <- as.integer(2010)
  geo_2006 <- ggplot2::fortify(shapefile_2006,region="AREA_NAME")
  geo_2006$year <- as.integer(2006)
  geo_2003 <- ggplot2::fortify(shapefile_2006,region="AREA_NAME") # Assuming 2006 locations for 2003 geo
  geo_2003$year <- as.integer(2003)
  toPollGeo <- rbind(geo_2014, geo_2010,geo_2006,geo_2003)
  toPollGeo$ward <- as.integer(stringr::str_sub(toPollGeo$id,1,2))
  toPollGeo$area <- as.integer(stringr::str_sub(toPollGeo$id,-3,-1))
  toPollGeo$year <- as.factor(toPollGeo$year)
  rm(geo_2014, geo_2003, geo_2006, geo_2010)
  toPollGeo <- dplyr::inner_join(toPollGeo,toVotes::ward_regions, by=c("ward"))
  toPollGeo
}
