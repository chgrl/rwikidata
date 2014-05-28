
#' Get a Wikidata property
#'
#' @import httr
#' @param pid The Wikidata property id, as string (including the 'P') or integer value (without the 'P')
#' @param lang Language abbreviation (ISO language codes), as string - default is \code{"en"}
#' @param print Logical - if \code{TRUE} (default) the property information are printed
#' @return A vector of two: property value and property description
#' @export
#' @examples
#' \dontrun{
#' wdgetproperty(31)
#' prop <- wdgetproperty(pid="p31", lang="pl", print=FALSE)
#' }
wdgetproperty <- function(pid, lang="en", print=TRUE) {
	
	# https://www.wikidata.org/wiki/Property%3aP246?uselang=en
	
	if(is.numeric(pid)) pid <- paste0("P", pid)
			
	# prepare request
	url <- paste0("https://www.wikidata.org/wiki/Property%3a", pid, "?uselang=", lang)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
	if(raw$status_code!=200) stop("property not found")
	
	# parse
	prop <- httr::content(raw, as="text")
	
	# find a BETTER way here!
	# <span class="wb-value " dir="auto"> ... </span> 
	prop.list <- strsplit(prop, "\n")[[1]]								# split prop into single lines
	prop.val.desc <- prop.list[grep("wb-value ", prop.list)]			# search for lines with "wb-value " -> value [1] + description [2]
	prop.val.desc <- substr(prop.val.desc, 2, nchar(prop.val.desc)-1)	# remove first/last character of line
	prop.val.desc <- sub(".*>", "", prop.val.desc)						# remove everything before value
	prop.val.desc <- sub("<.*", "", prop.val.desc)						# remove everything after value
	
	if(any(prop.val.desc=="")) warning("parsing error")
	else {
		class(prop.val.desc) <- "wdproperty"
		if(print) print(prop.val.desc)
		invisible(prop.val.desc)
	}
}


#' Print method for wdproperty
#'
#' @param property wdproperty object
print.wdproperty <- function(property) {
	
	cat("\n\tWikidata property\n\n")
	
	# property
	cat("Property:", property[1], "\n")
	
	# description
	cat("Description:", property[2], "\n")
}
