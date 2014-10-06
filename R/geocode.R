geocodePolls <- function() {
  # Download shapefiles -----------------------------------------------------

  years <- c(2006,2010)
  for (year in years) {
    if(file.exists(paste("subdivisions_",year,".zip",sep=""))) {
      # Nothing to do
    }  else {
      download.file(paste("http://opendata.toronto.ca/gcc/voting_subdivision_",year,"_wgs84.zip",sep=""),
                    destfile = paste("subdivisions_",year,".zip",sep=""))
      unzip(paste("subdivisions_",year,".zip",sep=""), exdir="")
    }
    shape <- paste("shapefile_",year,sep="")
    file <- paste("VOTING_SUBDIVISION_",year,"_WGS84.shp",sep="")
    assign(shape,maptools::readShapeSpatial(file, proj4string=CRS("+proj=longlat +datum=WGS84")))
    rm(file,shape)
  }
  rm(year,years)

  # Shapefiles --------------------------------------------------------------

  # Extract the data object for each year and then combine into one data frame
  geo_2010 <- ggplot2::fortify(shapefile_2010,region="AREA_NAME")
  geo_2010$year <- as.integer(2010)
  geo_2006 <- ggplot2::fortify(shapefile_2006,region="AREA_NAME")
  geo_2006$year <- as.integer(2006)
  geo_2003 <- ggplot2::fortify(shapefile_2006,region="AREA_NAME") # Assuming 2006 locations for 2003 geo
  geo_2003$year <- as.integer(2003)
  toPollGeo <- rbind(geo_2010,geo_2006,geo_2003)
  toPollGeo$ward <- as.integer(stringr::str_sub(toPollGeo$id,1,2))
  toPollGeo$area <- as.integer(stringr::str_sub(toPollGeo$id,-3,-1))
  # For 2014, assume same geography as 2010
  geo_2014 <- toPollGeo %.%
    dplyr::filter(year==2010) %.%
    dplyr::mutate(year=2014)
  toPollGeo <- rbind(toPollGeo, geo_2014)
  toPollGeo$year <- as.factor(toPollGeo$year)
  rm(geo_2014, geo_2003, geo_2006, geo_2010)
  toPollGeo <- dplyr::inner_join(toPollGeo,toVotes::ward_regions, by=c("ward"))
  rm(ward_regions)
  toPollGeo
}
