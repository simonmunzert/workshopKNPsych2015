namesScrape <- function(phonename, update.file = FALSE, timeout = 30) {
## transform phonename
	phonename <- tolower(phonename)
## load libraries
	x <- c("stringr", "RSelenium", "rvest")
	lapply(x, require, character.only=T)
## create folder
	dir.create(str_c("phonebook_", phonename), showWarnings = FALSE)
	filename <- str_c("phonebook_", phonename, "/phonebook_", phonename, ".html")
	if (file.exists(filename) & update.file == FALSE) {
		message("Data already scraped; using data from ", file.info(filename)$mtime)
	} else {
## retrieve and save html

	  # get webdriver running
	  url <- "http://www.telefonbuch.de/"
	  checkForServer()
	  startServer() 
	  remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox") 
	  remDr$open() 
	  remDr$navigate(url) 
	  
	  # enter name
	  css <- '#what'
	  toWhat <- remDr$findElement(using = 'css', value = css) 
	  clickTo <- toWhat$clickElement()
	  writeTo <- toWhat$sendKeysToElement(list(phonename)) 
	  
	  # click on search button
	  css <- '.ico'
	  searchElem <- remDr$findElement(using = 'css', value = css)
	  resultsPage <- searchElem$clickElement() # click on button
	  
	  # sub-select persons
	  css <- '.filters > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)'
	  personElem <- remDr$findElement(using = 'css', value = css)
	  resultsPage <- personElem$clickElement() # click on button
	  
	  # scroll down to footer
	  Sys.sleep(.5)
	  css <- "#footer"
	  footerElem <- remDr$findElement(using = 'css', value = css)
	  timer <- Sys.time() + timeout
	  while(Sys.time() <= timer) {
	    remDr$mouseMoveToLocation(webElement = footerElem)
	    Sys.sleep(.5)
	  }
	  
	  # store phone book page
	  output <- remDr$getPageSource(header = TRUE)
	  write(output[[1]], file = filename)
	  
	  # close connection
	  remDr$closeServer()
	  
	}
}
