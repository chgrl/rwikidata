% Generated by roxygen2 (4.0.1): do not edit by hand
\name{wdgetitem}
\alias{wdgetitem}
\title{Get a Wikidata item}
\usage{
wdgetitem(qid, lang = "en", print = TRUE)
}
\arguments{
\item{qid}{The Wikidata item id, as string (including the 'Q') or integer value (without the 'Q')}

\item{lang}{Language abbreviation (ISO language codes), as string - default is \code{"en"}}

\item{print}{Logical - if \code{TRUE} (default) the item information are printed}
}
\value{
A list with basic information about the item
}
\description{
Get a Wikidata item
}
\examples{
\dontrun{
wdgetitem(144786)
zapa.item <- wdgetitem(qid="q144786", lang="pl", print=FALSE)
wdgetitem() # get random item
}
}

