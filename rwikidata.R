require(httr, RJSONIO)

# search wikidata entities
wbSearchEntities <- function(search, lang="en", lim=10) {
	
	# prepare request
	search <- paste("search", search, sep="=")
	lang <- paste("language", lang, sep="=")
	limit <- paste("limit", lim, sep="=")
	query <- paste(search, lang, "format=json&type=item", limit, sep="&")
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbsearchentities&", query)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
	
	# parse
	res <- httr::content(raw, as="parsed")
	
	if(is.null(res$success)) warning("search failed\n", "code: ", res$error[[1]], " - ", res$error[[2]]) 
	else {
		printResponse(res, "search", lim)
		invisible(res)
	}
}


# print response of wbSearchEntities
printResponse <- function(response, action, lim=NA) {
	
	if(action=="search") {	# SEARCH
		# seach info
		cat("Search term:\t\t", response$searchinfo, "\n")
		
		# number of results
		num.results <- length(response$search)
		cat("Number of results:\t", num.results, paste0("(limit set to ", lim, ")\n\n"))
		
		# results
		if(num.results>0) {
			cat("Results:\n")
			for(i in 1:num.results) {
				label <- response$search[[i]]["label"]
				id <- response$search[[i]]["id"]
				if(is.na(response$search[[i]]["description"])) desc <- "\n"
				else desc <- paste("-", response$search[[i]]["description"], "\n")
				cat(i, "\t", label, paste0("(", id, ")"), desc)
			}
		}
	} else if(action=="entity") {	# ENTITY
		# entity
		cat("Entity:\t\t\t", response$entities[[1]]$labels[[1]]["value"], "\n")
		
		# aliases
		num.alias <- length(response$entities[[1]]$aliases[[1]])
		if(num.alias>0) {
			al <- unlist(res$entities[[1]]$aliases[[1]])
			cat("Aliases:\t\t", paste(al[names(al)=="value"], collapse=", "), "\n")
		}
		
		# description
		cat("Description:\t", response$entities[[1]]$descriptions[[1]]["value"], "\n")
	}
}


# get wikidata entity
wbGetEntities <- function(ent, lang="en") {
		
	# prepare request
	id <- paste("ids", ent, sep="=")
	lang <- paste("languages", lang, sep="=")
	ent <- paste(id, lang, "format=json", sep="&")
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetentities&", ent)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
	
	# parse
	res <- httr::content(raw, as="parsed")
	
	if(is.null(res$success)) warning("failed\n", "code: ", res$error[[1]], " - ", res$error[[2]]) 
	else {
		printResponse(res, "entity")
		invisible(res)
	}
}
