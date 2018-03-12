require("RPostgreSQL")

# Get files fro VoterDetail directory
data_directory <- "/Users/pbl/Documents/20171010_VoterDetail"

# List files
data_files <- list.files(data_directory)

# Already Geocoded directory

geocode_dir <- paste0(data_directory, "/geocoded")

# already coded files
geocoded_files <- list.files(geocode_dir)

# usernames
# mac
user <- "pbl"

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it

# pc
#pw <- { "admin" }

# mac
pw <- { "Student!"}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "geocoder",
                 host = "localhost", port = 5432,
                 user = "pbl", password = pw)
rm(pw) # removes the password

# Read County level data
for (j in 1:length(data_files)) {
  
  # Skip if already geocoded
  if(data_files[j] %in% geocoded_file) { 
    cat(paste0("Skipping: ", data_files[j], ", already geocoded.\n"))
    next 
    }
  
  # Add a slash
  test <- paste0(data_directory, "/", data_files[j])
  
  # Read county data
  county_data <- read.delim(test, header=FALSE)
  county_data$lon <- NA
  county_data$lat <- NA
  
  
  # Geocode county level data
  for (i in 1:nrow(county_data)) {
    
    # Set postgresql query with address
    #clean street name
    street_address <- gsub('\'','', as.character(county_data[i,8]))
    
    # Paste full address with street name, city, state, and zipcode, within query
    address <- paste(street_address, as.character(county_data[i,10]), "FL", as.character(county_data[i, 12]))
    query <- paste0("SELECT g.rating, ST_AsText(ST_SnapToGrid(g.geomout,0.00001))  
                    As wktlonlat, (addy).address As stno, (addy).streetname As street, 
                    (addy).streettypeabbrev As styp, (addy).location As city, 
                    (addy).stateabbrev As st,(addy).zip FROM geocode('", address ,"',1) As g;")
    
    # Get postgresql coordinates
    df_postgres <- dbGetQuery(con, query)
    
    # If result is empty, skip
    if (nrow(df_postgres) == 0) {next}
    
    # Extract coordinates
    coord <- strsplit(df_postgres$wktlonlat, "\\s|\\(|\\)")
    
    # Name columns
    county_data[i,'lon'] <- coord[[1]][2]
    county_data[i,'lat'] <- coord[[1]][3]
    
    # Print linenumber every 100 rows
    if (i %% 100 == 0) {print(paste("row", i))}
    
  }
  
  write.table(county_data, file=paste0(data_directory, "/geocoded/", data_files[j]))
  
  print(paste("Completed:", data_files[j]))
}







address <- "4949 Eastchester Dr. Sarasota FL 34234"
query <- paste0("SELECT g.rating, ST_AsText(ST_SnapToGrid(g.geomout,0.00001)) As wktlonlat, (addy).address As stno, (addy).streetname As street, (addy).streettypeabbrev As styp, (addy).location As city, (addy).stateabbrev As st,(addy).zip FROM geocode('", address ,"',1) As g;")

df_postgres <- dbGetQuery(con, query)


####
# 