#'@importFrom httr GET content stop_for_status add_headers
query <- function(url, as_what){
  result <- GET(url, config=add_headers("User-agent"="rwikidata"))
  stop_for_status(result)
  result <- content(result, as = as_what)
  return(result)
}