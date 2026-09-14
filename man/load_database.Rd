\name{load_database}
\alias{load_database}
\title{Load a Zenodo-hosted \code{traits.build} database}
\usage{
load_database(
  record_id,
  doi = NULL,
  version = NULL,
  path = file.path("data", record_id),
  update = FALSE
)
}
\arguments{
\item{record_id}{Zenodo record ID used to query the versions endpoint.}

\item{doi}{DOI for a specific database version.}

\item{version}{Version number of the database to load.}

\item{path}{Directory where metadata and database files are cached.}

\item{update}{If \code{TRUE}, refresh the cached versions JSON before loading.}
}
\value{
An object with class \code{"traits.build"}.
}
\description{
\code{load_database()} queries the versions available for a Zenodo
\code{record_id}, resolves a requested DOI or version, downloads the matching
\code{.rds} artifact when needed, and loads it into R with class
\code{"traits.build"}.
}
\examples{
\dontrun{
database <- load_database(record_id = "1234567", version = "1.0.0")
}
}
\seealso{
\code{\link{get_versions}}, \code{\link{get_version_latest}}
}
