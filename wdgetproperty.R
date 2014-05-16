require(httr)

wdgetproperty <- function(id, ...) UseMethod("wdgetproperty")


# get wikidata property
wdgetproperty.default <- function(id, lang="en") {
	
	# https://www.wikidata.org/wiki/Property%3aP246?uselang=en
			
	# prepare request
	url <- paste0("https://www.wikidata.org/wiki/Property%3a", id, "?uselang=", lang)
	
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
		print(prop.val.desc)
		invisible(prop.val.desc)
	}
}


# print item info
print.wdproperty <- function(property) {
	
	cat("\n\tWikidata property\n\n")
	
	# property
	cat("Property:", property[1], "\n")
	
	# description
	cat("Description:", property[2], "\n")
}
