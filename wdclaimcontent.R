
#' Get content of a specific claim
#'
#' @param guid The Wikidata claim guid, as string
#' @param lang Language abbreviation (ISO language codes), as string - default is \code{"en"}
#' @param print Logical - if \code{TRUE} (default) the claim content is printed
#' @param ... Arguments passed to methods, e.g. \code{open.ext} - if \code{TRUE} (default) external sources of the claim like images or URLs are opened
#' @return A list containing meta information of the claim and its content
#' @seealso \code{\link{wdgetclaims}} to get a list of all claims
#' @export
#' @examples
#' \dontrun{
#' wdclaimcontent("q144786$25DC2C5E-D59F-4C9B-A307-1DBDF3215576")
#' zapa.coa <- wdclaimcontent(guid="q144786$25DC2C5E-D59F-4C9B-A307-1DBDF3215576", 
#'   lang="pl", print=FALSE, open.ext=FALSE)
#' }
wdclaimcontent <- function(guid, lang="en", print=TRUE, ...) UseMethod("wdclaimcontent")

#wdclaimcontent <- function(qid, pid, lang="en", print=TRUE, ...) UseMethod("wdclaimcontent")


#' Get content of a specific claim - default method
#'
#' @import httr
#' @param guid The Wikidata claim guid, as string
#' @param lang Language abbreviation (ISO language codes), as string
#' @param print Logical - if \code{TRUE} the claim content is printed
#' @param ... Arguments passed to methods
#' @return A list containing meta information of the claim and its content
#' @keywords internal
wdclaimcontent.default <- function(guid, lang, print, ...) {
		
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
	if(type=="string" || type=="url") { # string
		content <- claim[[1]][[1]]$mainsnak$datavalue$value
	} else if(type=="wikibase-item") { # Wikidata item
		item.id <- paste0("Q", claim[[1]][[1]]$mainsnak$datavalue$value$"numeric-id")
		item.label <- wdgetitem(item.id, lang, print=FALSE)$entities[[1]]$labels[[1]]$value
		content <- c(item.id, item.label)
	} else if(type=="time") { # time
		time <- claim[[1]][[1]]$mainsnak$datavalue$value$time
		time <- gsub("+", "", time, fixed=TRUE)
		time <- gsub("^0+", "", time)
		if(substr(time, 1,1)=="-") time <- paste0("0", time)
		content <- strptime(strsplit(time, "Z")[[1]][1], "%Y-%m-%dT%H:%M:%S")
	} else if(type=="globe-coordinate") { # geographic coordinate
		content <- c(claim[[1]][[1]]$mainsnak$datavalue$value$latitude, claim[[1]][[1]]$mainsnak$datavalue$value$longitude)
	} else if(type=="commonsMedia") { # Wiki commons
		file <- claim[[1]][[1]]$mainsnak$datavalue$value
		# get commons website
		url <- paste0("http://commons.wikimedia.org/wiki/File:", file, "?uselang=en")
		url <- gsub(" ", "%20", url)
		raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
		web <- httr::content(raw, as="text")
		
		# get image url
		web.list <- strsplit(web, "\n")[[1]]
		img.url <- web.list[grep("fullMedia", web.list)]
		#<div class=\"fullMedia\"><a href=\"//upload.wikimedia.org/wikipedia/commons/b/ba/Flag_of_Germany.svg\" class=\"internal\" title=\"Flag of Germany.svg\">Original file</a>
		img.url <- sub(".*class=\"fullMedia\"><a href=\"", "", img.url)
		img.url <- sub("\" class=\"internal\".*", "", img.url)
		fname <- tail(strsplit(img.url, "/", fixed=TRUE)[[1]], 1)
		
		# download image and convert to png
		download.file(url=paste0("http:", img.url), destfile=file.path(tempdir(), fname))
		out <- paste0(file.path(getwd(), head(strsplit(fname, ".", fixed=TRUE)[[1]], -1)), ".png")
		# ImageMagick required
		if(tolower(tail(strsplit(fname, ".", fixed=TRUE)[[1]], 1))!="png") system(paste("convert", file.path(tempdir(), fname), out))
		content <- out
	} else if(type=="quantity") { # quantity
		content <- claim[[1]][[1]]$mainsnak$datavalue$value
	}
	
	content <- list(guid=guid, item=item, property=prop, type=type, content=content)
	class(content) <- "wdcontent"
	if(print) print(content, ...)
	invisible(content)
}


#wdclaimcontent.idprop <- function(qid, pid, print=TRUE) {
#	
#}


#' Print method for wdcontent
#'
#' @param content wdcontent object from \code{\link{wdclaimcontent}}
#' @keywords internal
#' @param open.ext Logical - if \code{TRUE} external sources of the claim like images or URLs are opened
print.wdcontent <- function(content, open.ext=TRUE) {
	cat("\n\tWikidata claim content\n\n")
	cat(paste("GUID:", content$guid, "\n"))
	cat(paste("Item:", content$item, "\n"))
	cat(paste("Property:", content$property, "\n"))
	cat(paste("Type:", content$type, "\n\n"))
	
	if(content$type=="string" || content$type=="time" || content$type=="quantity") {
		cat(content$content, "\n")
	} else if(content$type=="url") {
		cat(content$content, "\n")
		if(open.ext) browseURL(content$content)
	} else if(content$type=="wikibase-item") {
		cat(content$content[1], "-", content$content[2], "\n")
	} else if(content$type=="globe-coordinate") {
		cat(content$content[1], ",", content$content[2], "\n")
		if(open.ext && is.numeric(content$content[1]) && is.numeric(content$content[2])) browseURL(paste0("http://www.openstreetmap.org/#map=3", content$content[1], "/", content$content[2]))
	} else if(content$type=="commonsMedia") {
		cat("PNG image:", content$content, "\n")
		if(open.ext) {
			stopifnot(require(png))
			img <- readPNG(content$content)
			dim.img <- dim(img)
			dev.new(width=5*dim.img[2]/dim.img[1], height=5)
			par(mar=c(0,0,0,0))
			plot(c(0, dim.img[2]), c(0, dim.img[1]), type="n", xlab="", ylab="")
			rasterImage(img, 1, 1, dim.img[2], dim.img[1])
		}
	}
}
