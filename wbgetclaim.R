require(httr)

wdgetclaim <- function(id, guid, ...) UseMethod("wdgetclaim")
getclaim <- function(item, claim, ...) UseMethod("getclaim")


# get claims of wikidata item by api request
wdgetclaim.default <- function(id, claim, print=TRUE) {
	
	if(is.numeric(id)) id <- paste0("Q", id)
	
	# prepare request
	if(missing(claim) && !missing(id)) {	# get all claims
		url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetclaims&format=json&entity=", id)
	} else if(!missing(claim) && missing(id)) {	# get specific claim
		url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetclaims&format=json&claim=", claim)
	}
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
		
	# parse
	claim <- httr::content(raw, as="parsed")
	
	if(length(claim$claims)==0) warning("no claims found") 
	else {
		class(claim) <- "wdclaim"
		if(print) print(claim)
		invisible(claim)
	}
}


# get claims of wikidata item
getclaim.default <- function(item, claim, print=TRUE) {

	if(is.null(item$entities[[1]]$claims)) warning("no claims found in item", substitute(item))
	else {
		if(missing(claim)) {	# get all claims
			wdclaim <- item$entities[[1]]$claims
		} else if(length(claim)==1) {	# get specific claim
			wdclaim <- item$entities[[1]]$claims[[claim]]
		} else {	# get set of claims
			wdclaim <- list(item$entities[[1]]$claims[[claim[1]]])
			for(i in 2:length(claim)) wdclaim[[i]] <- item$entities[[1]]$claims[[claim[i]]]
		}
		
		class(wdclaim) <- "wdclaim"
		if(print) print(wdclaim)
		invisible(wdclaim)
	}
}


# print claim info
print.wdclaim <- function(claim) {
	
	cat("\n\tWikidata claim\n\n")
	
	# get ids and names
	claim.num <- length(claim$claims)
	claim.id <- names(claim$claims)
	claim.name <- NULL
	if(claim.num>0) for(i in 1:claim.num) claim.name <- append(claim.name, wdgetproperty(claim.id[i], print=FALSE))
	else stop("no claims found")
	claim.name[nchar(claim.name)>30] <- paste(substr(claim.name[nchar(claim.name)>30], 1, 30), "...")
	
	# prepare output
	claim.tbl <- cbind(claim.id, claim.name)
	
	# print
	print(claim.tbl, quote=FALSE)
}
