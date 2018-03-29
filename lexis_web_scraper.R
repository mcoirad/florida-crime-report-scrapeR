library(RSelenium)
library(futile.logger)
library(utils)
#library(jmvcore)

# Import try/catch function
source("retry_function.R")

### Settings

sleep_timer <- 2

date_range <- "1 year"

remDr <- RSelenium::rsDriver()

remDr$client$maxWindowSize(winHand = "current") 

data_write_dir <- paste0(getwd(), "/downloaded_lexis_data")

zipcode_dir <- paste0(getwd(), "/lexis_zipcodes")

done_zipcodes <- list.files(data_write_dir)

done_zipcodes <- sapply( strsplit(sapply(strsplit(done_zipcodes,"[_]"), `[`, 2), "[.]"), '[', 1 )


# Main function, takes: 
#   zipcode of area to be searched, 
#   county/city name for purposes of naming file, and 
#   sleep-timer which will make scraping slower, pausing between the javascript actions.
getCrimeData <- function(zipcode, county, sleeptimer = 1) {
  
  # if zipcode is already done, skip
  if (zipcode %in% done_zipcodes) {
    print(paste("skipping", zipcode))
    return (NULL)
  }
  
  print(paste("Scraping crime data at", zipcode))
  
  remDr$client$navigate("https://communitycrimemap.com/")
  
  # Remove dialogs if they exist
  GetAnnounceButton <- function() {announcements_button <<- remDr$client$findElements('css', "button[id*=\"ext-gen\"]")}
  
  GetAnnounceButton()
  
  retry(announcements_button[[1]]$clickElement(), retryFunction = GetAnnounceButton, maxErrors =1, returnError = FALSE)
  retry(announcements_button[[2]]$clickElement(), retryFunction = GetAnnounceButton, maxErrors =1, returnError = FALSE)
  retry(announcements_button[[1]]$clickElement(), retryFunction = GetAnnounceButton, maxErrors =1, returnError = FALSE)
  retry(announcements_button[[2]]$clickElement(), retryFunction = GetAnnounceButton, maxErrors =1, returnError = FALSE)
  
  Sys.sleep(1 * sleep_timer)
  
  
  # Search for zipcode
  GetAddressInput <- function() { address_input <<- remDr$client$findElement(using = "css", "[id = 'search-address-input']") }
  retry(GetAddressInput(), retryFunction = GetAddressInput)
  address_input$clearElement()
  retry(address_input$sendKeysToElement(list(as.character(zipcode))), retryFunction = GetAddressInput)
  
  Sys.sleep(1 * sleep_timer)
  
  # Click Submit
  GetSearchSubmit <- function() { search_submit <<- remDr$client$findElement(using = "css", "[id = 'search-submit-button']") }
  retry(GetSearchSubmit(), retryFunction = GetSearchSubmit)
  search_submit$clickElement()
  
  Sys.sleep(1 * sleep_timer)
  
  # Open Date Range Bar
  
  GetDateRangeButton <- function() { date_range_button <<- remDr$client$findElements('css', "div[class*=\"x-tool x-tool-toggle\"]") }
  GetDateRangeButton()
  
  retry(date_range_button[[3]]$clickElement(), retryFunction = GetDateRangeButton, maxErrors =1, returnError = FALSE)
  
  Sys.sleep(1 * sleep_timer)
  
  # Enter date range (1 year)
  getDateRange<- function() { dateRange <<- remDr$client$findElements(using = "css", "input[class*= 'x-form-text x-form-field']")[[2]]}
  getDateRange()
  Sys.sleep(1 * sleep_timer)
  
  dateRange$clearElement()
  retry(dateRange$sendKeysToElement(list(date_range, "\uE007")), retryFunction = getDateRange)

  Sys.sleep(1 * sleep_timer)
  
  retry(dateRange$sendKeysToElement(list("\uE007")), retryFunction = getDateRange)
  
  
  # Get zoom in
  GetZoomInButton <- function() { zoom_in_button <<- remDr$client$findElement(using = "css", "[title='Zoom in']") }
  retry(GetZoomInButton(), retryFunction = GetZoomInButton)
  
  # Zoom out 3 times
  GetZoomButton <- function() { zoom_out_button <<- remDr$client$findElement(using = "css", "[title='Zoom out']") }
  retry(GetZoomButton(), retryFunction = GetZoomButton)
  
  zoom_out_button$clickElement()
  zoom_out_button$clickElement()
  
  Sys.sleep(1 * sleep_timer)
  
  # Switch to data tab
  GetDataTab <- function() { data_tab <<- remDr$client$findElements(using = "css", "a[class='x-tab-right']") }
  GetDataTab()
  data_tab[[2]]$clickElement()
  
  Sys.sleep(1 * sleep_timer)
  
  
  # Get number of records
  GetRecords <- function() { records_num <<- remDr$client$findElements(using = "css", "div[class='xtb-text']") }
  GetRecords()
  GetRecordsText <- function() { 
    records_text <<- records_num[[3]]$getElementText() 
    records_text <<- substr(records_text, (nchar(records_text) - 2), nchar(records_text))
    }
  GetRecordsText()
  
  # Create method for switching to map tab
  GetMapTab <- function() { map_tab <<- remDr$client$findElements(using = "css", "a[class='x-tab-right']") }
  
  # Zoom in until < 500 records
  while (records_text == '500') {
    # Switch to map tab
    GetMapTab()
    map_tab[[1]]$clickElement()
    
    # Zoom in once
    zoom_in_button$clickElement()
    Sys.sleep(1 * sleep_timer)
    #Switch to data tab
    GetDataTab()
    data_tab[[2]]$clickElement()
    # update Records
    GetRecords()
    GetRecordsText()
  }
  
  # if no records, skip and write empty file
  
  if (records_text == "iew") {
    print(paste("Skipping zipcode,", zipcode, ", no records found."))
    write.csv("", file=paste0(data_write_dir, "/", county, "_", zipcode, ".csv"))
    return (NULL) 
  }
  
  # Extract only numbers
  records_text <- as.numeric(gsub("[^0-9]", "", records_text))
  
  #empty dataset
  crime_data <- data.frame(type="", incident_num="", crime="", date="", location="", address="", accuracy="", agency="",stringsAsFactors=FALSE)
  
  image = ""
  getImageUrl <- function(x) {image_url <<- as.character(x$getElementAttribute("src"))}
  
  
  # Iterate through data table
  for (j in 1:(records_text %/% 20 + 1)) {
    GetImages <- function () {images <<- remDr$client$findElements('xpath', "//img")}
    
    # get tag body
    GetColID <- function() {col_id <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-1']")}
    GetColCrime <- function() {col_crime <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-2']")}
    GetColDate <- function() {col_date <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-3']")}
    GetColLoc <- function() {col_location <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-4']")}
    GetColAdd <- function() {col_address <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-5']")}
    GetColAcc <- function() {col_accuracy <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-6']")}
    GetColAgency <- function() {col_agency <<- remDr$client$findElements('css', "div[class='x-grid3-cell-inner x-grid3-col-7']")}
    
    GetColID()
    GetColCrime()
    GetColDate()
    GetColLoc()
    GetColAdd()
    GetColAcc()
    GetColAgency()
    for (i in 1:length(col_id)) {
      k <- i + (j * 20 - 20)
      retry(GetImages())
      retry(getImageUrl(images[[84 + i - 1]]), sleep = 0.5, retryFunction = GetImages)
      retry(crime_data[k,1] <- image_url)
      retry(crime_data[k,2] <- as.character(col_id[[i]]$getElementText()[[1]]), retryFunction = GetColID)
      retry(crime_data[k,3] <- as.character(col_crime[[i]]$getElementText()[[1]]), retryFunction = GetColCrime)
      retry(crime_data[k,4] <- as.character(col_date[[i]]$getElementText()[[1]]), retryFunction = GetColDate)
      retry(crime_data[k,5] <- as.character(col_location[[i]]$getElementText()[[1]]), retryFunction = GetColLoc)
      retry(crime_data[k,6] <- as.character(col_address[[i]]$getElementText()[[1]]), retryFunction = GetColAdd)
      retry(crime_data[k,7] <- as.character(col_accuracy[[i]]$getElementText()[[1]]), retryFunction = GetColAcc)
      retry(crime_data[k,8] <- as.character(col_agency[[i]]$getElementText()[[1]]), retryFunction = GetColAgency)
      
      
    }
    next_button <<- remDr$client$findElement('css', "button[class*='x-btn-text x-tbar-page-next']")
    next_button$clickElement()
    crime_data <<- crime_data
    print(paste0("Scraping page ", j,  " of report"))
    Sys.sleep(1* sleep_timer)
    
  }
  
  print("Writing Data to File")
  write.csv(crime_data, file=paste0(data_write_dir, "/", county, "_", zipcode, ".csv"))
  
}

# iterate through list of zipcodes
for (f in list.files(zipcode_dir)) {
  zipcodes <- read.table(paste0(zipcode_dir, "/", f))
  for (z in 1:nrow(zipcodes)) {
    zipcode <- zipcodes[z,]
    retry(getCrimeData(as.character(zipcode), county = strsplit(f, "[.]")[[1]][1], sleeptimer = 2), maxErrors= 10, retryFunction = function() {
      tryCatch({remDr$client$close()}, error=function(e){})
      remDr <<- RSelenium::rsDriver()
      remDr$client$maxWindowSize(winHand = "current") 
    })
  }
}
