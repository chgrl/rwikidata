require(httr)

wdsearch <- function(search, ...) UseMethod("wdsearch")

# search wikidata items
wdsearch.default <- function(search, lang="en", lim=10, print=TRUE) {
		
	# prepare request
	search <- paste("search", search, sep="=")
	lang <- paste("language", lang, sep="=")
	limit <- paste("limit", lim, sep="=")
	query <- paste(search, lang, "format=json&type=item", limit, sep="&")
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbsearchentities&", query)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
	
	# parse
	result <- httr::content(raw, as="parsed")
	
	if(is.null(result$success)) warning("search failed\n", "code: ", result$error[[1]], " - ", result$error[[2]]) 
	else {
		class(result) <- "wdsearch"
		if(print) print(result)
		invisible(result)
	}
}

# print search results
print.wdsearch <- function(result) {
	
	cat("\n\tWikidata search\n\n")
	
	# seach info
	cat("Search term:\t\t", result$searchinfo$search, "\n")
	
	# number of results
	num.results <- length(result$search)
	cat("Number of results:\t", num.results, "\n\n")
		
	# results
	if(num.results>0) {
		cat("Results:\n")
		for(i in 1:num.results) {
			label <- result$search[[i]]$label
			id <- result$search[[i]]$id
			if(is.null(result$search[[i]]$description)) desc <- "\n"
			else desc <- paste("-", result$search[[i]]$description, "\n")
			cat(i, "\t", label, paste0("(", id, ")"), desc)
		}
	}
}
