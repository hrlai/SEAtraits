\name{get_version_latest}
\alias{get_version_latest}
\title{Get the latest version for a Zenodo record}
\usage{
get_version_latest(
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
A character scalar giving the newest available version.
}
\description{
Return the newest version string available for a Zenodo record.
}
\examples{
\dontrun{
get_version_latest(record_id = "1234567")
}
}
