
#' Get a Wikidata item
#'
#' @param id The Wikidata item id, as string (including the 'Q') or integer value (without the 'Q')
#' @param lang Language abbreviation (ISO language codes), as string - default is \code{"en"}
#' @param print Logical - if \code{TRUE} (default) the item information are printed
#' @return A list with basic information about the item
#' @export
#' @examples
#' \dontrun{
#' wdgetitem(144786)
#' zapa.item <- wdgetitem(id="q144786", lang="pl", print=FALSE)
#' wdgetitem() # get random item
#' }
wdgetitem <- function(id, lang="en", print=TRUE) UseMethod("wdgetitem")


#' Get a Wikidata item - default method
#'
#' @import httr
#' @param id The Wikidata item id, as string (including the 'Q') or integer value (without the 'Q')
#' @param lang Language abbreviation (ISO language codes), as string
#' @param print Logical - if \code{TRUE} the item information are printed
#' @return A list with basic information about the item
wdgetitem.default <- function(id, lang, print) {
	
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


#' Print method for wditem
#'
#' @param item wditem object
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
