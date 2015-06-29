namesParse <- function(phonename) {
	filename <- str_c("phonebook_", phonename, "/phonebook_", phonename, ".html")
## load libraries
	x <- c("stringr", "rvest")
	lapply(x, require, character.only = TRUE)
## parse html
	tb_parse <- rvest::html(filename, encoding = "UTF-8")
## check number of hits
	css <- '#sticky > form > div > ul:nth-child(1) > li:nth-child(2) > strong'
	num_results <- tb_parse %>% html_nodes(css) %>% html_text() %>% str_extract('[[:digit:]]+') %>% as.numeric()
## retrieve zipcodes and names
	xpath <- '//div[@class="vcard"]'
	entries <- tb_parse %>% html_nodes(xpath = xpath) %>% html_text() 
	entries_clean <- entries %>% str_replace_all("\\n|\\t|\\r", "")
	zipcodes_vec <- entries_clean %>% str_extract("[[:digit:]]{5}") %>% as.numeric()
	names_vec <- entries_clean %>% str_extract("^[[:alpha:] -.,]+([ ]{3})") %>% str_trim()
## build data frame
	entries_df <- data.frame(plz = zipcodes_vec, name = names_vec)
## match coordinates to zipcodes
	plz_df <- read.delim("plz_de.txt", stringsAsFactors = FALSE, encoding = "UTF-8")
	geodf <- merge(entries_df, plz_df, by = "plz", all.x = TRUE)
	geodf <- geodf[!is.na(geodf$lon),]
## return data frame
	geodf <- geodf[,!names(geodf) %in% "X.loc_id"]
	return(geodf)
}