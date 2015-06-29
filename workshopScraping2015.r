### -------------------------
### Workshop: Web Scraping mit R - Eine kleine Einführung
### Simon Munzert
### Universität Konstanz
### -------------------------


### Vorbereitungen ----------

# Workspace aufräumen
rm(list=ls(all=TRUE))

# Pakete installieren und laden
pkgs <- c("rvest", "jsonlite", "stringr", "httr", "plyr", "ggplot2", "ggmap", "tm", "wordcloud", "RColorBrewer", "wikipediatrend", "RSelenium", "ROAuth", "streamR")
# install.packages(pkgs)
lapply(pkgs, library, character.only=T)

# Arbeitspfad speichern
basepath <- getwd()



# Beispiel 1: Schlagzeilen-Scraping ------------------

# SelectorGadget
browseURL("http://selectorgadget.com/")

# FAZ
url <- "http://www.faz.net"
html_parsed <- html(url, encoding = "UTF-8")
css <- '#FAZContentLeftInner .Headline' 
headings_faz <- html_nodes(html_parsed, css = css)
headings_faz <- html_text(headings_faz)
head(headings_faz)

# Was sind die Top-Begriffe? Wordcloud
source("data/wordCloudMaker.r")
wordCloudMaker(headings_faz, min.freq = 2, language = "german", max.words = 25, rot.per = .20, return.data = TRUE)




# Beispiel 2: Brauereien in Deutschland ------------------

#Arbeitsverzeichnis setzen
wd <- ("/data/breweriesGermany")
setwd(paste0(basepath, wd))

# Schritt 1: Scraping
url <- "http://www.biermap24.de/brauereiliste.php"
browseURL(url)
content <- html(url, encoding = "utf8")
anchors <- html_nodes(content, xpath = "//tr/td[2]")
cities <- html_text(anchors)
cities
cities <- str_trim(cities)
cities <- cities[str_detect(cities, "^[[:upper:]]+.")]
length(cities)
length(unique(cities))
sort(table(cities))

# Schritt 2: Geo-Coding
# dieser Prozess dauert eine Weile, außerdem nur 2500 API-Anfragen pro Tag möglihc
# schon vorher ausgeführt und in Datei abgespeichert
if ( !file.exists("breweries_geo.RData")){
  pos <- geocode(cities)
  geocodeQueryCheck()
  save(pos, file="breweries_geo.RData")
} else {
  load("breweries_geo.RData")
}
head(pos)

# Schritt 3: Kartendarstellung
mapGermany <- get_map(location = 'Germany', zoom = 6)
breweryMap <-
  ggmap(mapGermany) + 
  geom_point(data = pos,
             aes(x = lon, y = lat),
             colour = "#F54B1A70", size=3, na.rm=T)
breweryMap




# Übung ------------------------------------------------

# Scrapen Sie die aktuellen Schlagzeilen von Spiegel Online!
# Scrapen Sie die Liste bekannter Alumni von folgender Seite:
url <- "https://de.wikipedia.org/wiki/Universität_Konstanz"




# Beispiel 4: Tabellen-Scraping ------------------------

# Seite parsen
url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_in_the_world"
parsed_url <- html(url, encoding = "utf8")

# Tabelle extrahieren
tabs <- html_table(parsed_url, header = TRUE, fill = TRUE)
tab <- tabs[[3]]

# Tabelle untersuchen
names(tab)
names(tab) <- c("rank", "building", "city", "country", "height", "height_ft", "floors", "built")
head(tab)
tab$height <- tab$height %>% str_extract("^[[:digit:].]+") %>% as.numeric()
tab$built <- tab$built %>% str_extract("^[[:digit:].]+") %>% as.numeric()

hist(tab$built)
summary(tab$height)

plot(tab$built, tab$height)
sort(table(tab$country))


# Übung ------------------------------------------------

# Scrapen Sie die Tabelle, die einen Überblick über aktuell gefährdete Welterbestätten der UNESCO gibt, von folgender Website:
url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"
browseURL(url)



# Beispiel 5: JSON-Datenverarbeitung -------------------

# Quelle für Statistiken zu Artikelaufrufen auf Wikipedia
browseURL("http://stats.grok.se")

# Daten importieren
json  <- "http://stats.grok.se/json/en/latest90/Pegida"
data  <- fromJSON(json)
summary(data)


# Daten bereinigen
date  <- as.Date(names(data$daily_views))
views <- unlist(data$daily_views)
wiki_df <- data.frame(date = date, views = views)
head(wiki_df)

ggplot(wiki_df, aes(date, views)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 20), size=1.5) +
  theme_bw()


# Nutzung des wikipediatrend-Pakets
library(wikipediatrend)
pegida <- wp_trend(page = "Pegida", from = Sys.Date() - 365, to = Sys.Date(), lang = "de")
class(pegida)

ggplot(pegida, aes(date, count, group = page, color = page)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 20), size=1.5) +
  theme_bw()




# Beispiel 6: Twitter-Mining ---------------------------

# Registrierung
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- Sys.getenv("twitter_api_key")
consumerSecret <- Sys.getenv("twitter_api_secret")
twitCred <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, requestURL = requestURL, accessURL = accessURL, authURL = authURL)
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# stop here, copy URL into browser, enter PIN into console and press enter. then continue.
save(twitCred, file = "data/twitter_auth.Rdata")

# the twitCred object stores credentials which have to be passed to the API to get access. once you have stored this information, you do not have to execute the code above again in later sessions. just load the twitter_auth.Rdata file and execute registerTwitterOAuth(twitCred) from the twitteR package
load("data/twitter_auth.RData")

# Tweets filtern
filterStream("data/tweets_stream.json", track = c("Grexit"), timeout = 1000, oauth = twitCred)
tweets <- parseTweets("data/tweets_stream.json", simplify = FALSE)
names(tweets)
cat(tweets$text[1])


# Tweets darstellen
tweets_red <- tweets[!is.na(tweets$place_lon),]
dim(tweets_red)
mapEurope <- get_map(location = 'Europe', zoom = 4)
grexitMap <-
  ggmap(mapEurope) + 
  geom_point(data = tweets_red,
             aes(x = place_lon, y = place_lat),
             size = 3, alpha = 1/2, color = "darkblue", na.rm=T) +  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), plot.background = element_blank())
grexitMap

