require("rgdal")
require(sp)
require("openxlsx")
require("ggplot2")
require(pbmcapply)
require(doSNOW)


# Set to use multiple (n-1) cores
cores=detectCores()
cluster <- makeCluster(cores[1]-1)
registerDoSNOW(cluster)

# Set datasources
# Precinct shapefile folder
map_dir <- "/Users/pbl/Downloads/election-geodata-master/data/12-florida/county/2016/SAR"
# Registered Voter Data
vote_dir <- "/Users/pbl/Documents/20171010_VoterDetail/geocoded"
# Precinct Vote File
precinct_vote_dir <- "/Users/pbl/Downloads/precinctlevelelectionresults2016gen"

# Load shapefile data
ogrListLayers(paste0(map_dir, "/Sarasota_Pcts_2016.TAB"))
test_map <- readOGR(paste0(map_dir, "/Sarasota_Pcts_2016.TAB"))
test_vote_data <- read.table(paste0(vote_dir, "/SAR_20171010.txt"))
precinct_vote_data <- read.delim(paste0(precinct_vote_dir, "/SAR_PctResults20161108.txt"))

# Remove NAs
test_vote_data <- test_vote_data[!is.na(test_vote_data$lon),]

# Generate list of polygon points
#ggmap_data <- test_map
#ggmap_data@data$id <- rownames(ggmap_data@data)
#ggmap_data.points <- fortify(ggmap_data, region="id")
#ggmap_data.df <- join(ggmap_data.points, ggmap_data@data, by="id")
#ggmap_data.points <- merge(ggmap_data.points, ggmap_data@data, by = "id")

coordinates(test_vote_data) <- ~lon + lat
proj4string(test_vote_data) <- proj4string(ggmap_data)
over(test_vote_data, as(ggmap_data, "SpatialPolygons"))

# TODO: check edges
# Get Precinct Function
# if relative is true, will include in recinct if coordinates match to relative interior or vertex
# returns NA if no match is found
getPrecinct <- function(voter, polygon_points, polygons, relative = TRUE) {
  for (i in unique(polygon_points$PRECINCT)) {
    id_pol_points <- subset(polygon_points, PRECINCT == i)
    in_or_out <- point.in.polygon(point.x = voter$lon, point.y = voter$lat, pol.x = id_pol_points$long, pol.y = id_pol_points$lat)
    if (in_or_out == 1) {return (i)}
    else if (((in_or_out == 2) | (in_or_out == 3)) && relative == TRUE ) {return (i)}
    
  }
  return(getPrecinct2(voter, polygons))
}

getPrecinct2 <- function(voter, polygons) {
  coordinates(voter) <- ~lon + lat 
  proj4string(voter) <- proj4string(polygons)
  in_poly <- over(voter, as(polygons, "SpatialPolygons"))
  if (is.na(in_poly)) { return (in_poly)}
  return(polygons[in_poly,]$PRECINCT)
}

getPrecinctList <- function(voter, polygon_points, relative = FALSE) {
  ins_outs <- c()
  for (i in unique(polygon_points$PRECINCT)) {
    id_pol_points <- subset(polygon_points, PRECINCT == i)
    in_or_out <- point.in.polygon(point.x = voter$lon, point.y = voter$lat, pol.x = id_pol_points$long, pol.y = id_pol_points$lat)
    ins_outs <- c(ins_outs, in_or_out)
    }
  return(ins_outs)
}


# Find the precinct each voter lives in from the shapefile info
# Set up a progress bar
pb <- progressBar(min = 0, max = nrow(test_vote_data))
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
precincts <- foreach (i=1:nrow(test_vote_data), .packages=c("sp", "pbmcapply"), .options.snow=opts, .combine=rbind) %dopar% {
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=nrow(test_vote_data))
  precinct_id <- getPrecinct(test_vote_data[i,], ggmap_data.points, ggmap_data)
  #if (is.na(precinct_id)) {precinct_id <- getPrecinct2(test_vote_data[i,], ggmap_data)}
}
#test_vote_data$precinct_id <- precincts

pb <- progressBar(min = 0, max = nrow(kumbwa))
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
precincts <- foreach (i=1:nrow(kumbwa), .packages=c("sp", "pbmcapply"), .options.snow=opts, .combine=rbind) %dopar% {
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=nrow(test_vote_data))
  precinct_id <- getPrecinctList(kumbwa[i,], ggmap_data.points)
}
#test_vote_data2$precinct_id <- precincts


# Close cluster
stopCluster(cluster) 


ggplot(ggmap_data.df) + 
aes(long,lat,group=group,fill="black") + 
geom_polygon() +
geom_path(color="white") +
coord_equal() 
