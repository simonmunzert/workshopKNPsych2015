### -----------------------------
### political scientists on Wikipedia
### simon munzert
### -----------------------------


## goals ------------------------

  #  build a ‘collaboration’ network of psychologists

## tasks ------------------------

  # gather list of psychologists
  # fetch Wikipedia entries
  # identify links
  # construct connectivity matrix
  # visualize network



## packages ---------------------

library(stringr) # string processing
library(rvest) # scraping suite
library(d3Network) # visualizing networks


## directory --------------------

wd <- ("./data/wikipediaPsychologists")
setwd(wd)


## code -------------------------

## step 1: inspect page
url <- "https://en.wikipedia.org/wiki/List_of_psychologists"
browseURL(url)


## step 2: retrieve links
html <- html(url)
anchors <- html_nodes(html, xpath="//ul/li/a[1]")
links <- html_attr(anchors, "href")

links_iffer <-
  seq_along(links) >=
  seq_along(links)[str_detect(links, "Ali_ibn_Abbas_al-Majusi")] &
  seq_along(links) <=
  seq_along(links)[str_detect(links, "Karen_Wynn")] &
  str_detect(links, "/wiki/")
links_index <- seq_along(links)[links_iffer]
links <- links[links_iffer]
length(links)


##  step 3: extract names
names <- html_attr(anchors, "title")[links_index]
names <- str_replace(names, " \\(.*\\)", "")
names <- iconv(names, "utf8", "latin1")


## step 4: fetch personal wiki pages
baseurl <- "http://en.wikipedia.org"
HTML <- list()
Fname <- str_c(basename(links), ".html")
URL <- str_c(baseurl, links)
# loop
for ( i in seq_along(links) ){
  # url
  url <- URL[i]
  # fname
  fname <- Fname[i]
  # download
  if ( !file.exists(fname) ) download.file(url, fname)
  # read in files
  HTML[[i]] <- html(fname)
}


## step 5: identify links between psychologists
# loop preparation
connections <- data.frame(from=NULL, to=NULL)
# loop
for (i in seq_along(HTML)) {
  pslinks <- html_attr(
    html_nodes(HTML[[i]], xpath="//a"),
    "href")
  links_in_pslinks <- seq_along(links)[links %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks!=i]
  connections <- rbind(
    connections,
    data.frame(
      from=rep(i, length(links_in_pslinks)),
      to=links_in_pslinks
    )
  )
}

# results
names(connections) <- c("from", "to")
dim(connections)
head(connections)

# reduce network for displaying purposes
connections <- connections
for (i in unique(connections$from)) {
  num_links <- nrow(connections[connections$from==i,])
  if(num_links < 20) {
    connections <- subset(connections, connections$from != i)
  }
}

connections <- connections[connections$to %in% connections$from,]

# make symmetrical
connections <- rbind(
  connections,
  data.frame(from=connections$to,
             to=connections$from)
)



## step 6: visualize connections
dir.create("figures")
d3ForceNetwork(Links = connections, Nodes = data.frame(name = names), Source = "from", Target = "to", opacity = 1, zoom = T, width = 1200, height = 800, linkDistance = 400, file = "figures/connections_names.html")
browseURL('figures/connections_names.html')




