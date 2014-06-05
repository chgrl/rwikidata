
#' Get content of a specific claim
#'
#' @import httr
#' @param qid The Wikidata item qid, as string (including the 'Q') or integer value (without the 'Q') - required unless guid is provided
#' @param pid The Wikidata property id, as string (including the 'P') or integer value (without the 'P') - required unless guid is provided
#' @param guid The Wikidata claim guid, as string - required unless qid and pid are provided
#' @param lang Language abbreviation (ISO language codes), as string - default is \code{"en"}
#' @param print Logical - if \code{TRUE} (default) the claim content is printed
#' @param ... Arguments passed to methods, e.g. \code{open.ext} - if \code{TRUE} (default) external sources of the claim like images or URLs are opened
#' @return A list containing meta information of the claim and its content
#' @seealso \code{\link{wdgetclaims}} to get a list of all claims
#' @export
#' @examples
#' \dontrun{
#' wdclaimcontent(qid=144786, pid=17)
#' wdclaimcontent(guid="q144786$258996D0-319A-4A14-971A-6F3326C05ADD")  # same same
#'
#' # with qualifiers
#' wdclaimcontent(183, 36)
#'
#' # different content types
#' wdclaimcontent(guid="q206904$26C11CAB-2FEE-4A03-9774-9B70777616B3")  # wikidata item
#' wdclaimcontent(guid="q52$61ba10af-455f-021c-19d5-d1a0df4d65f5")  # date/time
#' wdclaimcontent(guid="q7186$5675dddd-438d-bd8d-3355-8117747e182d")  # string
#' wdclaimcontent(guid="Q206904$920876CE-1C0E-40ED-BB3C-ACB2AC15870C")  # url
#' wdclaimcontent(guid="q2280$91220430-4b56-8c53-d75d-ba85ce8a2629")  # geocoordinates
#' wdclaimcontent(guid="q144786$25DC2C5E-D59F-4C9B-A307-1DBDF3215576")  # commons image
#' zapa.coa <- wdclaimcontent(guid="q144786$25DC2C5E-D59F-4C9B-A307-1DBDF3215576", 
#'   lang="pl", print=TRUE, open.ext=FALSE)
#' }
wdclaimcontent <- function(qid, pid, guid, lang="en", print=TRUE, ...) {
		
	# prepare request
	if(!missing(qid) && !missing(pid) && missing(guid)) {
		if(is.numeric(qid)) qid <- paste0("Q", qid)
		if(is.numeric(pid)) pid <- paste0("P", pid)
		url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetclaims&format=json&entity=", qid, "&property=", pid)
	} else if(missing(qid) && missing(pid) && !missing(guid)) url <- paste0("http://www.wikidata.org/w/api.php?action=wbgetclaims&format=json&claim=", guid)
	else stop("either qid and pid or guid only required")
	
	# get content
	wdcontent <- wdclaimcontent.int(url, lang)
	
	if(print) print(wdcontent, ...)
	invisible(wdcontent)
}


#' Internal method to execute API request and process the content
#'
#' @param url API request URL
#' @param lang Language abbreviation (ISO language codes), as string
#' @return A list of guid, item, property, type and content
#' @seealso \code{\link{wdclaimcontent}}
#' @keywords internal
wdclaimcontent.int <- function(url, lang) {
	
	# execute request
	raw <- GET(url, config=add_headers("User-agent"="rwikidata"))
		
	# parse
	claim <- httr::content(raw, as="parsed")
	if(length(claim$claims)==0) warning("claim(s) not found") 
	claim <- claim$claims
	n.claims <- length(claim[[1]])
	
	for(cl in 1:n.claims) {
	
		# get info
		guid <- claim[[1]][[cl]]$id	# if qid and pid given - only first claim, there might be more than one
		item <- wdgetitem(strsplit(claim[[1]][[cl]]$id, "$", fixed=TRUE)[[1]][1], lang=lang, print=FALSE)$entities[[1]]$labels[[1]]$value
		prop <- wdgetproperty(claim[[1]][[cl]]$mainsnak$property, lang=lang, print=FALSE)[1]
		type <- claim[[1]][[cl]]$mainsnak$datatype
		
		# get content
		content <- NULL
		if(type=="string" || type=="url") { # string
			content <- claim[[1]][[cl]]$mainsnak$datavalue$value
		} else if(type=="wikibase-item") { # Wikidata item
			item.id <- paste0("Q", claim[[1]][[cl]]$mainsnak$datavalue$value$"numeric-id")
			item.label <- wdgetitem(item.id, lang, print=FALSE)$entities[[1]]$labels[[1]]$value
			content <- c(item.id, item.label)
		} else if(type=="time") { # time
			time <- claim[[1]][[cl]]$mainsnak$datavalue$value$time
			time <- gsub("+", "", time, fixed=TRUE)
			time <- gsub("^0+", "", time)
			if(substr(time, 1,1)=="-") time <- paste0("0", time)
			content <- strptime(strsplit(time, "Z")[[1]][1], "%Y-%m-%dT%H:%M:%S")
		} else if(type=="globe-coordinate") { # geographic coordinate
			content <- c(claim[[1]][[cl]]$mainsnak$datavalue$value$latitude, claim[[1]][[cl]]$mainsnak$datavalue$value$longitude)
		} else if(type=="commonsMedia") { # Wiki commons
			file <- claim[[1]][[cl]]$mainsnak$datavalue$value
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
			amount <- as.numeric(claim[[1]][[cl]]$mainsnak$datavalue$value$amount)
			unit <- claim[[1]][[cl]]$mainsnak$datavalue$value$unit
			content <- amount
			attr(content, "unit") <- unit
		}
		
		# qualifiers
		qualifiers <- list()
		if(!is.null(claim[[1]][[cl]]$qualifiers)) {
			qualifiers <- claim[[1]][[cl]]$qualifiers
			n.qual <- length(qualifiers)
			for(i in 1:n.qual) {
				### todo: qualifier might be a list - qualifiers[[i]][[x]]
				qual.prop <- wdgetproperty(qualifiers[[i]][[1]]$property, lang=lang, print=FALSE)[1]
				qual.type <- qualifiers[[i]][[1]]$datatype
				if(!is.null(qualifiers[[i]][[1]]$datavalue)) qualifiers[[i]] <- getqualifier(qualifiers[[i]][[1]]$datavalue, qual.prop, qual.type, lang)
			}
		}
		
		if(n.claims>1) {
			if(cl==1) wdcontent <- list(item=item, property=prop, claims=list())
			if(length(qualifiers)==0) wdcontent$claims[[cl]] <- list(guid=guid, type=type, content=content)
			else wdcontent$claims[[cl]] <- list(guid=guid, type=type, qualifiers=qualifiers, content=content)
		} else {
			if(length(qualifiers)==0) wdcontent <- list(guid=guid, item=item, property=prop, type=type, content=content)
			else wdcontent <- list(guid=guid, item=item, property=prop, type=type, qualifiers=qualifiers, content=content)
		}
	}
	
	class(wdcontent) <- "wdcontent"
	return(wdcontent)
}


#' Internal method to get and prepare the qualifiers
#'
#' @param qualifier Raw qualifier object
#' @param lang Language abbreviation (ISO language codes), as string
#' @return Qualifier value - object depends on qualifier type
#' @seealso \code{\link{wdclaimcontent.int}}
#' @keywords internal
getqualifier <- function(qualifier, prop, type, lang) {
	if(type=="string") {
		value <- qualifier$value
		attr(value, "type") <- "string"
	} else if(type=="time") {
		time <- qualifier$value$time
		time <- gsub("+", "", time, fixed=TRUE)
		time <- gsub("^0+", "", time)
		if(substr(time, 1,1)=="-") time <- paste0("0", time)
		value <- strptime(strsplit(time, "Z")[[1]][1], "%Y-%m-%dT%H:%M:%S")
		attr(value, "type") <- "time"
	} else if(type=="quantity") {
		value <- as.numeric(qualifier$value$amount)
		attr(value, "unit") <- qualifier$value$unit
		attr(value, "type") <- "quantity"
	} else if(type=="wikibase-item") {
		item.id <- paste0("Q", qualifier$value$"numeric-id")
		item.label <- wdgetitem(item.id, lang, print=FALSE)$entities[[1]]$labels[[1]]$value
		value <- c(item.id, item.label)
		attr(value, "type") <- "wikibase-item"
	} else if(type=="globe-coordinate") {
		value <- c(qualifier$value$latitude, qualifier$value$longitude)
		attr(value, "type") <- "globe-coordinate"
	} else warning("qualifier not recognized", call.=FALSE)
	
	attr(value, "property") <- prop
	return(value)
}


#' Print method for wdcontent
#'
#' @param content wdcontent object from \code{\link{wdclaimcontent}}
#' @param open.ext Logical - if \code{TRUE} external sources of the claim like images or URLs are opened
print.wdcontent <- function(content, open.ext=TRUE) {
	cat("\n\tWikidata claim content\n\n")
	
	if(is.null(content$claims)) { # single claim content
		cat(paste("GUID:", content$guid, "\n"))
		cat(paste("Item:", content$item, "\n"))
		cat(paste("Property:", content$property, "\n"))
		cat(paste("Type:", content$type, "\n\n"))
		
		# content
		if(content$type=="string") {
			cat(content$content, "\n")
		} else if(content$type=="time") {
			cat(as.character(content$content), "\n")
		} else if(content$type=="quantity") {
			cat(content$content, attr(content$content, "unit"))
		} else if(content$type=="url") {
			cat(content$content, "\n")
			if(open.ext) browseURL(content$content)
		} else if(content$type=="wikibase-item") {
			cat(content$content[1], "-", content$content[2], "\n")
		} else if(content$type=="globe-coordinate") {
			cat(content$content[1], ",", content$content[2], "\n", sep="")
			if(open.ext && is.numeric(content$content[1]) && is.numeric(content$content[2])) browseURL(paste("http://www.openstreetmap.org/#map=10", content$content[1], content$content[2], sep="/"))
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
		
		# qualifiers
		# ...
	} else { # multiple claim content
		cat(paste("Item:", content$item, "\n"))
		cat(paste("Property:", content$property, "\n"))
		
		# content
		for(i in 1:length(content$claims)) {
			cat(paste("\n", i, "\nGUID:", content$claims[[i]]$guid, "\n"))
			cat(paste("Type:", content$claims[[i]]$type, "\n"))
			
			if(content$claims[[i]]$type=="string") {
				cat(content$claims[[i]]$content, "\n")
			} else if(content$claims[[i]]$type=="time") {
				cat(as.character(content$claims[[i]]$content), "\n")
			} else if(content$claims[[i]]$type=="quantity") {
				cat(content$claims[[i]]$content, attr(content$claims[[i]]$content, "unit"))
			} else if(content$claims[[i]]$type=="url") {
				cat(content$claims[[i]]$content, "\n")
				if(open.ext) browseURL(content$claims[[i]]$content)
			} else if(content$claims[[i]]$type=="wikibase-item") {
				cat(content$claims[[i]]$content[1], "-", content$claims[[i]]$content[2], "\n")
			} else if(content$claims[[i]]$type=="globe-coordinate") {
				cat(content$claims[[i]]$content[1], ",", content$claims[[i]]$content[2], "\n", sep="")
				if(open.ext && is.numeric(content$claims[[i]]$content[1]) && is.numeric(content$claims[[i]]$content[2])) browseURL(paste("http://www.openstreetmap.org/#map=10", content$claims[[i]]$content[1], content$claims[[i]]$content[2], sep="/"))
			} else if(content$claims[[i]]$type=="commonsMedia") {
				cat("PNG image:", content$claims[[i]]$content, "\n")
				if(open.ext) {
					stopifnot(require(png))
					img <- readPNG(content$claims[[i]]$content)
					dim.img <- dim(img)
					dev.new(width=5*dim.img[2]/dim.img[1], height=5)
					par(mar=c(0,0,0,0))
					plot(c(0, dim.img[2]), c(0, dim.img[1]), type="n", xlab="", ylab="")
					rasterImage(img, 1, 1, dim.img[2], dim.img[1])
				}
			}
		}
		
		# qualifiers
		# ...
	}					
}
