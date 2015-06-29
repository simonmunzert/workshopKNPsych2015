### -------------------------
### Workshop: Web Scraping mit R - Eine kleine Einführung
### Simon Munzert
### Universität Konstanz
### -------------------------


### Vorbereitungen ----------

# Workspace aufräumen
rm(list=ls(all=TRUE))

# Pakete installieren und laden
pkgs <- c("rvest", "jsonlite", "stringr", "httr", "plyr", "ggplot2", "ggmap", "tm", "wordcloud", "RColorBrewer", "wikipediatrend", "RSelenium")
# install.packages(pkgs)
lapply(pkgs, library, character.only=T)

# Arbeitspfad speichern
basepath <- getwd()


# Beispiel 1: Namenskarten aus Telefondaten --------------

# Arbeitsverzeichnis setzen
wd <- ("/data/phonebookNamemaps")
setwd(paste0(basepath, wd))

source("cs-namemaps-scraping-function-v2.r")
source("cs-namemaps-extraction-function-v2.r")
source("cs-namemaps-mapping-function-v2.r")

phoneName <- "Reips"
namesScrape(phoneName, update.file = FALSE, timeout = 30)
entries_df <- namesParse(phoneName)

pdf(file = str_c("phonebook_", phoneName, "/map-", phoneName, ".pdf"), height = 7.5, width = 7.5, family = "URWTimes")
namesPlot(entries_df, phoneName)
dev.off()
