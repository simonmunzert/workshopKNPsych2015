namesPlot <- function(geodf, phonename, print.names = FALSE) {
## load libraries
	x <- c("stringr", "ggmap")
	lapply(x, require, character.only = TRUE)
## produce map
	i <- 0
	n <- 1
	mapGermany <- get_map(location = 'Germany', zoom = 6)
	
	capitalize <- function(string){ # define function, curtesy of Hmisc package
	capped <- grep("^[^A-Z]*$", string, perl = TRUE)
	substr(string[capped], 1, 1) <- toupper(substr(string[capped], 
	                                               1, 1))
	return(string)
	}
	if (print.names == FALSE) {
		nameMap <-
		    ggmap(mapGermany) + 
		    geom_point(data = geodf,
		               aes(x = lon, y = lat),
		               colour = "#F54B1A70", size=2, na.rm=T) +
		    ggtitle(str_c("People named '", capitalize(phonename), "' in Germany")) + 
	      theme(axis.line=element_blank(),axis.text.x=element_blank(),
	            axis.text.y=element_blank(),axis.ticks=element_blank(),
	            axis.title.x=element_blank(),
	            axis.title.y=element_blank())
		}
	  if (print.names == TRUE) {
	    nameMap <-
	      ggmap(mapGermany) + 
	      geom_point(data = geodf,
	                 aes(x = lon, y = lat),
	                 colour = "#F54B1A70", size=2, na.rm=T) +
	      ggtitle(str_c("People named '", capitalize(phonename), "' in Germany")) + 
	      theme(axis.line=element_blank(),axis.text.x=element_blank(),
	            axis.text.y=element_blank(),axis.ticks=element_blank(),
	            axis.title.x=element_blank(),
	            axis.title.y=element_blank()) +
	         geom_text(geodf, aes(label=name),hjust=0, vjust=0)
	  }
		return(nameMap)
}


