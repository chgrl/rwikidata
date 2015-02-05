#' Internal method to execute API request and return the content
#'
#' @importFrom httr GET content stop_for_status add_headers
#' @param url API request URL
#' @param as_what Desired type of output, one of \code{raw}, \code{text} or \code{parsed.content}
#' @return Content in specified type
#' @keywords internal
query <- function(url, as_what){
	result <- GET(url, config=add_headers("User-agent"="rwikidata"))
	stop_for_status(result)
	result <- content(result, as=as_what)
	return(result)
}
