% Generated by roxygen2 (4.0.1): do not edit by hand
\name{wdsearch}
\alias{wdsearch}
\title{Search for Wikidata items}
\usage{
wdsearch(search, lang = "en", lim = 10, print = TRUE)
}
\arguments{
\item{search}{A string to search for}

\item{lang}{Language abbreviation (ISO language codes), as string - default is \code{"en"}}

\item{lim}{Maximal number of results, as integer - default is \code{10}}

\item{print}{Logical - if \code{TRUE} (default) the search results are printed}
}
\value{
A list of search results and meta information
}
\description{
Search for Wikidata items
}
\examples{
\dontrun{
wdsearch("zakopane")
zapa <- wdsearch(search="zakopane", lang="pl", lim=1, print=FALSE)
}
}

