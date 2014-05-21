require(httr)

wdgetitem <- function(id, ...) UseMethod("wdgetitem")


# get wikidata item
wdgetitem.default <- function(id, lang="en", print=TRUE) {
	
	if(missing(id)) { # get random item
		url <- paste0("http://www.wikidata.org/wiki/Special:Statistics?uselang=en")
		raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
		if(raw$status_code!=200) stop("error loading wikidata statistics")
		stat <- httr::content(raw, as="text")
		stat.list <- strsplit(stat, "\n")[[1]]
		stat.line <- stat.list[grep("Content pages", stat.list)]
		stat.line <- sub(".*Content pages</a></td><td class=\"mw-statistics-numbers\">", "", stat.line)
		stat.line <- substr(stat.line, 1, 15)
		stat <- as.numeric(gsub("[^0-9]", "", stat.line))
		id <- sample(1:stat, 1)
		cat("id not set - get random item Q", id, " ...\n\n", sep="")
	}
	
	if(is.numeric(id)) id <- paste0("Q", id)
			
	# prepare request
	id <- paste("ids", id, sep="=")
	lang <- paste("languages", lang, sep="=")
	id <- paste(id, lang, "format=json", sep="&")
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetentities&", id)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
	
	# parse
	item <- httr::content(raw, as="parsed")
	
	if(is.null(item$success)) warning("failed\n", "code: ", item$error[[1]], " - ", item$error[[2]]) 
	else {
		class(item) <- "wditem"
		if(print) print(item)
		invisible(item)
	}
}


# print item info
print.wditem <- function(item) {
	
	cat("\n\tWikidata item\n\n")
	
	# label
	cat("Label:", item$entities[[1]]$labels[[1]]$value, "\n")
	
	# aliases
	num.alias <- length(item$entities[[1]]$aliases[[1]])
	if(num.alias>0) {
		al <- unlist(item$entities[[1]]$aliases[[1]])
		cat("Aliases:\t", paste(al[names(al)=="value"], collapse=", "), "\n")
	}
	
	# description
	cat("Description:", item$entities[[1]]$descriptions[[1]]$value, "\n")
	
	# num claims
	cat("Claims:\t\t", length(item$entities[[1]]$claims), "\n")
}
