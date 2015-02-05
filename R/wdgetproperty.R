
#' Get a Wikidata property
#'
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
	prop <- query(url, "text")
	
	# find a BETTER way here!
	# <span class="wikibase-labelview-text">...</span>
	# <span class="wikibase-descriptionview-text">...</span> 
	prop.list <- strsplit(prop, "\n")[[1]]										# split prop into single lines
	prop.val <- prop.list[grep("wikibase-labelview-text", prop.list)]			# search for special line
	prop.val <- substr(prop.val, 2, nchar(prop.val)-1)							# remove first/last character of line
	prop.val <- sub(".*>", "", prop.val)										# remove everything before value
	prop.val <- sub("<.*", "", prop.val)										# remove everything after value
	prop.desc <- prop.list[grep("wikibase-descriptionview-text", prop.list)]	# search for special line
	prop.desc <- substr(prop.desc, 2, nchar(prop.desc)-1)						# remove first/last character of line
	prop.desc <- sub(".*>", "", prop.desc)										# remove everything before value
	prop.desc <- sub("<.*", "", prop.desc)										# remove everything after value
	
	prop <- c(prop.val, prop.desc)
	if(any(prop=="")) warning("parsing error")
	else {
		class(prop) <- "wdgetproperty"
		if(print) print(prop)
		invisible(prop)
	}
}


#' Print method for wdgetproperty
#'
#' @param x wdgetproperty object
#' @param \dots Arguments to be passed to methods
#' @method print wdgetproperty
#' @S3method print wdgetproperty
print.wdgetproperty <- function(x, ...) {
	
	cat("\n\tWikidata property\n\n")
	
	# property
	cat("Property:", x[1], "\n")
	
	# description
	cat("Description:", x[2], "\n")
}
