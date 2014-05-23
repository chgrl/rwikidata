require(httr)

wdclaimcontent <- function(guid, ...) UseMethod("wdguid")
#wdclaimcontent <- function(id, prop, ...) UseMethod("wdidprop")


# get content of specific claim
wdguid.default <- function(guid, lang="en", print=TRUE) {
		
	# prepare request
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetclaims&format=json&claim=", guid)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
		
	# parse
	claim <- httr::content(raw, as="parsed")
	if(length(claim$claims)==0) warning("claim not found") 
	claim <- claim$claims
	
	# get info
	item <- wdgetitem(strsplit(claim[[1]][[1]]$id, "$", fixed=TRUE)[[1]][1], lang=lang, print=FALSE)$entities[[1]]$labels[[1]]$value
	prop <- wdgetproperty(claim[[1]][[1]]$mainsnak$property, lang=lang, print=FALSE)[1]
	type <- claim[[1]][[1]]$mainsnak$datatype
	
	# get content
	content <- NULL
	if(type=="string" || type=="url") {
		content <- claim[[1]][[1]]$mainsnak$datavalue$value
	} else if(type=="wikibase-item") {
		item.id <- paste0("Q", claim[[1]][[1]]$mainsnak$datavalue$value$"numeric-id")
		item.label <- wdgetitem(item.id, lang, print=FALSE)$entities[[1]]$labels[[1]]$value
		content <- c(item.id, item.label)
	} else if(type=="time") {
		time <- claim[[1]][[1]]$mainsnak$datavalue$value$time
		time <- gsub("+", "", time, fixed=TRUE)
		time <- gsub("^0+", "", time)
		if(substr(time, 1,1)=="-") time <- paste0("0", time)
		content <- strptime(strsplit(time, "Z")[[1]][1], "%Y-%m-%dT%H:%M:%S")
	} else if(type=="globe-coordinate") {
		content <- c(claim[[1]][[1]]$mainsnak$datavalue$value$latitude, claim[[1]][[1]]$mainsnak$datavalue$value$longitude)
	} else if(type=="commonsMedia") {
		url <- paste0("http://commons.wikimedia.org/wiki/File:", claim[[1]][[1]]$mainsnak$datavalue$value)
		content <- url
	} else if(type=="quantity") {
		content <- claim[[1]][[1]]$mainsnak$datavalue$value
	}
	
	content <- list(id=item, property=prop, type=type, content=content)
	class(content) <- "wdcontent"
	#if(print) print(content)
	invisible(content)
}


#wdidprop.default <- function(id, prop, print=TRUE) {
#	
#}


# print content
#print.wdcontent <- function(content) {
#	
#}
