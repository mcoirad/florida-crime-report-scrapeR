library(RSelenium)
library(futile.logger)
library(utils)
#library(jmvcore)

# Import try/catch function
source("retry_function.R")

### Settings
start_date <- "9/1/2017"
end_date <- "11/4/2017"

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
  if (retry(announcements_button <- remDr$client$findElements('xpath', "//*[@id=\"ext-gen177\"]"), maxErrors=5) != "FAILURE") {
    announcements_button[[1]]$clickElement()
  }
  
  if (retry(announcements_button <- remDr$client$findElements('xpath', "//*[@id=\"ext-gen153\"]"), maxErrors=5) != "FAILURE") {
    announcements_button[[1]]$clickElement()
  }
  
  # Search for zipcode
  GetAddressInput <- function() { address_input <<- remDr$client$findElement(using = "css", "[id = 'search-address-input']") }
  retry(GetAddressInput(), retryFunction = GetAddressInput())
  address_input$clearElement()
  retry(address_input$sendKeysToElement(list(as.character(zipcode))), retryFunction = GetAddressInput())
  
  # Click Submit
  GetSearchSubmit <- function() { search_submit <<- remDr$client$findElement(using = "css", "[id = 'search-submit-button']") }
  retry(GetSearchSubmit(), retryFunction = GetSearchSubmit())
  search_submit$clickElement()
  
  //*[@id="map_canvas"]/div/div/div[8]/div[1]/div/button[2]
  
  what_button <- remDr$client$findElement(using = 'id', value = "filtersWhat")
  
  # Open crime type filter menu
  script <- "Map.LoadFiltersPanel('what'); Map.TogglePinGraphic(true);"
  remDr$client$executeScript(script, args = list(""))
  
  # Filter out non-violent crime violent crimes
  # burglary
  script <- "Map.WhatPanelClick('3', false);"
  remDr$client$executeScript(script, args = list(""))
  # disturbing the peace
  script <- "Map.WhatPanelClick('4', false);"
  remDr$client$executeScript(script, args = list(""))
  # drugs
  script <- "Map.WhatPanelClick('5', false);"
  remDr$client$executeScript(script, args = list(""))
  # DUI
  script <- "Map.WhatPanelClick('6', false);"
  remDr$client$executeScript(script, args = list(""))
  # Fraud
  script <- "Map.WhatPanelClick('7', false);"
  remDr$client$executeScript(script, args = list(""))
  # motor vehicle theft
  script <- "Map.WhatPanelClick('9', false);"
  remDr$client$executeScript(script, args = list(""))
  # larceny
  script <- "Map.WhatPanelClick('12', false);"
  remDr$client$executeScript(script, args = list(""))
  # vandalism
  script <- "Map.WhatPanelClick('13', false);"
  remDr$client$executeScript(script, args = list(""))
  # vehicle break in
  script <- "Map.WhatPanelClick('14', false);"
  remDr$client$executeScript(script, args = list(""))
  
  Sys.sleep(0.5 * sleeptimer)
  
  
  # Open time filter menu
  script <- "Map.LoadFiltersPanel('when'); Map.TogglePinGraphic(true);"
  remDr$client$executeScript(script, args = list(""))
  
  # Set custom date range
  script <- "Map.CheckDateRange('Custom Time Range');"
  remDr$client$executeScript(script, args = list(""))
  
  Sys.sleep(1* sleeptimer)
  
  # When calling elements from the DOM we have to wrap in the try function in case it fails lol
  getFromDate<- function() { fromDate <<- remDr$client$findElement(using = "css", "[id = 'dateFrom']")}
  getFromDate()
  fromDate$clearElement()
  retry(fromDate$sendKeysToElement(list(start_date)), retryFunction = getFromDate())
  
  getToDate<- function() { toDate <<- remDr$client$findElement(using = "css", "[id = 'dateTo']") }
  getToDate()
  toDate$clearElement()
  retry(toDate$sendKeysToElement(list(end_date)), retryFunction = getToDate())
  
  getFromDate()
  fromDate$clearElement()
  retry(fromDate$sendKeysToElement(list(start_date)), retryFunction = getFromDate())
  
  # Apply
  script <- "Map.ApplyExplicitDate();"
  remDr$client$executeScript(script, args = list(""))
  
  Sys.sleep(0.5 * sleeptimer)
  
  # Close time filter menu
  script <- "Map.LoadFiltersPanel('when'); Map.TogglePinGraphic(true);"
  remDr$client$executeScript(script, args = list(""))
  
  # Location Search
  zipcodeInput<- remDr$client$findElement(using = "css", "[id = 'locationSearch']")
  zipcodeInput$clearElement()
  zipcodeInput$sendKeysToElement(list(zipcode, key = "enter"))
  script <- "Map.GeocodeSearchAddress();"
  remDr$client$executeScript(script, args = list(""))
  
  # Zoom out a couple of times
  
  script <- "Map.ZoomOut();"
  Sys.sleep(1 * sleeptimer)
  remDr$client$executeScript(script, args = list(""))
  Sys.sleep(1 * sleeptimer)
  remDr$client$executeScript(script, args = list(""))
  Sys.sleep(1 * sleeptimer)
  remDr$client$executeScript(script, args = list(""))
  Sys.sleep(1 * sleeptimer)
  
  # If more than 1000 records zoom in once
  if (retry(statusbar <<- remDr$client$findElements('xpath', "//*[@id=\"divStatusBarMaxRecords\"]"), maxErrors=5) != "FAILURE") {
    while (!is.element("visibility: hidden", strsplit(as.character(statusbar[[1]]$getElementAttribute("style")), "[;]")[[1]][1])) {
      script <- "Map.ZoomIn();"
      remDr$client$executeScript(script, args = list(""))
      Sys.sleep(1 * sleeptimer)
    }
  }
  
  # If no records skip, but an write empty file.
  if (retry(statusbar <<- remDr$client$findElements('xpath', "//*[@id=\"divStatusBarNoDataFound\"]"), maxErrors=5) != "FAILURE") {
    if (!is.element("visibility: hidden;", statusbar[[1]]$getElementAttribute("style"))) {
      print(paste("Skipping zipcode,", zipcode, ", no records found."))
      write.csv("", file=paste0(data_write_dir, "/", county, "_", zipcode, ".csv"))
      return (NULL) 
    }
  }
  # same for second status bar
  if (retry(statusbar <<- remDr$client$findElements('xpath', "//*[@id=\"divStatusBarNoDataProvided\"]"), maxErrors=5) != "FAILURE") {
    if (!is.element("visibility: hidden;", statusbar[[1]]$getElementAttribute("style"))) {
      print(paste("Skipping zipcode,", zipcode, ", no records found."))
      write.csv("", file=paste0(data_write_dir, "/", county, "_", zipcode, ".csv"))
      return (NULL) 
    }
  }
  
  
  
  # Load report view
  script <- "Map.OpenReport();"
  remDr$client$executeScript(script, args = list(""))
  Sys.sleep(5 * sleeptimer)
  # get number of records
  getRecordDiv <- function () { record_div <<- remDr$client$findElement('xpath', "//*[@id=\"divCrimeReportHeader\"]/span[2]")}
  retry(getRecordDiv(), sleep = (0.5 * sleeptimer))
  num_record <- retry(as.numeric(strsplit(as.character(record_div$getElementText()), " ")[[1]][1]), retryFunction=getRecordDiv)
  
  # data frame
  crime_data <- data.frame(type="", description="", incident_num="", location="", agency="", date="", stringsAsFactors=FALSE)
  
  # iterate through pages
  for (j in 1:(num_record %/% 15 + 1)) {
    # get images
    images <<- remDr$client$findElements('xpath', "//img")
    
    # get tag body
    tbody <<- remDr$client$findElements('xpath', "//td[@role = 'gridcell']")
    
    # set refresh/retry functions
    getTbody <- function() {tbody <<- remDr$client$findElements('xpath', "//td[@role = 'gridcell']")}
    img_xpath <<- ""
    getImage <- function() {image <<- remDr$client$findElement('xpath', img_xpath)}
    getImageUrl <- function() {image_url <<- as.character(image$getElementAttribute("src"))}
    retry(getTbody())
    
    # Extract data
    for (i in 1:(length(tbody) / 7 )) {
      
      
      k <- i + (j * 15 - 15)
      img_xpath <<- paste0("//*[@id=\"CrimeIncidents\"]/div[2]/table/tbody/tr[", i, "]/td[2]/img")
      retry(getImage(), sleep = 0.5, retryFunction = getTbody)
      retry(getImageUrl(), sleep = 0.5, retryFunction = getImage)
      retry(crime_data[k,1] <- strsplit(image_url, "[/]")[[1]][8])
      retry(crime_data[k,2] <- as.character(tbody[i * 7 - 4][[1]]$getElementText()), retryFunction = getTbody)
      retry(crime_data[k,3] <- as.character(tbody[i * 7 - 3][[1]]$getElementText()), retryFunction = getTbody)
      retry(crime_data[k,4] <- as.character(tbody[i * 7 - 2][[1]]$getElementText()), retryFunction = getTbody)
      retry(crime_data[k,5] <- as.character(tbody[i * 7 - 1][[1]]$getElementText()), retryFunction = getTbody)
      retry(crime_data[k,6] <- as.character(tbody[i * 7][[1]]$getElementText()), retryFunction = getTbody)
      
    }
    
    # Access next page
    next_page <- remDr$client$findElement('xpath', "//*[@id=\"CrimeIncidents\"]/div[3]/a[3]")
    next_page$clickElement()
    crime_data <<- crime_data
    print(paste0("Scraping page ", j,  " of report"))
    Sys.sleep(1* sleeptimer)
    
  }
  print("Writing Data to File")
  write.csv(crime_data, file=paste0(data_write_dir, "/", county, "_", zipcode, ".csv"))
}