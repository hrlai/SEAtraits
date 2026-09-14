\name{get_versions}
\alias{get_versions}
\title{List available versions for a Zenodo record}
\usage{
get_versions(
  record_id,
  path = file.path("data", record_id),
  update = TRUE
)
}
\arguments{
\item{record_id}{Zenodo record ID used to query the versions endpoint.}

\item{path}{Directory where metadata and database files are cached.}

\item{update}{If \code{TRUE}, refresh the cached versions JSON before reading it.}
}
\value{
A tibble with \code{publication_date}, \code{doi}, \code{version}, and
\code{id}.
}
\description{
Retrieve the cached or live Zenodo versions metadata for a record.
}
\examples{
\dontrun{
get_versions(record_id = "1234567")
}
}
