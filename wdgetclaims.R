require(httr)

wdgetclaims <- function(id, ...) UseMethod("wdgetclaims")
getclaims <- function(item, ...) UseMethod("getclaims")


# get claims of wikidata item by api request
wdgetclaims.default <- function(id, print=TRUE) {
	
	if(is.numeric(id)) id <- paste0("Q", id)
	
	# prepare request
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetclaims&format=json&entity=", id)
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
		
	# parse
	claim <- httr::content(raw, as="parsed")
	
	if(length(claim$claims)==0) warning("no claims found")
	else {
		claim <- claim$claims
		class(claim) <- "wdclaims"
		if(print) print(claim)
		invisible(claim)
	}
}


# get claims of wikidata item
getclaims.default <- function(item, print=TRUE) {

	if(is.null(item$entities[[1]]$claims)) warning("no claims found in item", substitute(item))
	else {
		# get claim
		wdclaim <- item$entities[[1]]$claims
		
		class(wdclaim) <- "wdclaims"
		if(print) print(wdclaim)
		invisible(wdclaim)
	}
}


# print claim info
print.wdclaims <- function(claim) {
	
	# get ids and names
	claim.num <- length(claim)
	claim.id <- names(claim)
	claim.name <- NULL
	if(claim.num>0) for(i in 1:claim.num) claim.name <- append(claim.name, wdgetproperty(claim.id[i], print=FALSE)[1])
	else stop("no claims found")
	claim.name[nchar(claim.name)>25] <- paste(substr(claim.name[nchar(claim.name)>25], 1, 25), "...")
	
	# get guids
	claim.guid <- NULL
	if(claim.num>0) for(i in 1:claim.num) {
		claim.set <- claim[[i]]
		#guid <- NULL
		#for(j in 1:length(claim.set)) guid <- append(guid, claim.set[[j]]$id)
		#guid <- paste(guid, collapse=", ")
		guid <- claim.set[[1]]$id
		if(length(claim.set)>1) guid <- paste0(guid, ", ...")
		claim.guid <- append(claim.guid, guid)
	}
	
	# prepare output
	claim.tbl <- as.data.frame(cbind(claim.id, claim.name, claim.guid))
	names(claim.tbl) <- c("Property", "Claim", "GUID")
		
	# print
	cat("\n\tWikidata claims\n\n")
	print(claim.tbl, quote=FALSE, right=FALSE, row.names=FALSE)
}
