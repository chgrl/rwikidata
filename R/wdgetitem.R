
#' Get a Wikidata item
#'
#' @param qid The Wikidata item id, as string (including the 'Q') or integer value (without the 'Q')
#' @param lang Language abbreviation (ISO language codes), as string - default is \code{"en"}
#' @param print Logical - if \code{TRUE} (default) the item information are printed
#' @return A list with basic information about the item
#' @export
#' @examples
#' \dontrun{
#' wdgetitem(144786)
#' zapa.item <- wdgetitem(qid="q144786", lang="pl", print=FALSE)
#' wdgetitem() # get random item
#' }
wdgetitem <- function(qid, lang="en", print=TRUE) {
	
	if(missing(qid)) { # get random item
    stat <- query("http://www.wikidata.org/wiki/Special:Statistics?uselang=en","text")
		stat.list <- strsplit(stat, "\n")[[1]]
		stat.line <- stat.list[grep("Content pages", stat.list)]
		stat.line <- sub(".*Content pages</a></td><td class=\"mw-statistics-numbers\">", "", stat.line)
		stat.line <- substr(stat.line, 1, 15)
		stat <- as.numeric(gsub("[^0-9]", "", stat.line))
		qid <- sample(1:stat, 1)
		cat("qid not set - get random item Q", qid, " ...\n\n", sep="")
	}
	
	if(is.numeric(qid)) qid <- paste0("Q", qid)
			
	# prepare request
	qid <- paste("ids", qid, sep="=")
	lang <- paste("languages", lang, sep="=")
	qid <- paste(qid, lang, "format=json", sep="&")
	url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetentities&", qid)
	
	# execute request
  item <- query(url, "parsed")
	if(is.null(item$success)) warning("failed\n", "code: ", item$error[[1]], " - ", item$error[[2]]) 
	else {
		class(item) <- "wdgetitem"
		if(print) print(item)
		invisible(item)
	}
}


#' Print method for wdgetitem
#'
#' @param x wdgetitem object
#' @param \dots Arguments to be passed to methods
#' @method print wdgetitem
#' @S3method print wdgetitem
print.wdgetitem <- function(x, ...) {
	
	cat("\n\tWikidata item\n\n")
	
	# label
	cat("Label:", x$entities[[1]]$labels[[1]]$value, "\n")
	
	# aliases
	num.alias <- length(x$entities[[1]]$aliases[[1]])
	if(num.alias>0) {
		al <- unlist(x$entities[[1]]$aliases[[1]])
		cat("Aliases:\t", paste(al[names(al)=="value"], collapse=", "), "\n")
	}
	
	# description
	cat("Description:", x$entities[[1]]$descriptions[[1]]$value, "\n")
	
	# num claims
	cat("Claims:\t\t", length(x$entities[[1]]$claims), "\n")
}
