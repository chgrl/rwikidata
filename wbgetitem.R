require(httr)

wbgetitem <- function(id, ...) UseMethod("wbgetitem")


# get wikidata item
wbgetitem.default <- function(id, lang="en") {
			
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
		class(item) <- "wbgetitem"
		print(item)
		invisible(item)
	}
}


# print item info
print.wbgetitem <- function(item) {
	
	cat("\n\tWikidata item\n\n")
	
	# label
	cat("label:", item$entities[[1]]$labels[[1]]$value, "\n")
	
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
