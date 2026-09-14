#' Load a Zenodo-hosted `traits.build` database
#'
#' `load_database()` queries the versions available for a Zenodo `record_id`,
#' resolves a requested DOI or version, downloads the matching `.rds` artifact
#' when needed, and loads it into R with class `"traits.build"`.
#'
#' @param record_id Zenodo record ID used to query the versions endpoint.
#' @param doi DOI for a specific database version.
#' @param version Version number of the database to load.
#' @param path Directory where metadata and database files are cached.
#' @param update If `TRUE`, refresh the cached versions JSON before loading.
#'
#' @return An object with class `"traits.build"`.
#' @seealso [get_versions()], [get_version_latest()]
#' @export
#'
#' @examples
#' \dontrun{
#' database <- load_database(record_id = "1234567", version = "1.0.0")
#' }
load_database <- function(record_id,
                          doi = NULL,
                          version = NULL,
                          path = file.path("data", record_id),
                          update = FALSE) {
    record_id <- validate_record_id(record_id)

    if (is.null(doi) && is.null(version)) {
        stop(
            "Please supply either `doi` or `version`. ",
            "Try `get_versions(record_id)` to inspect available versions.",
            call. = FALSE
        )
    }

    if (!is.null(version)) {
        version <- strip_version_prefix(version)
    }

    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    res <- load_json(record_id = record_id, path = path, update = update)
    metadata <- create_metadata(res)

    if (!is.null(doi)) {
        if (!doi %in% metadata$doi) {
            stop("Requested version or DOI is not available.", call. = FALSE)
        }

        version <- metadata$version[metadata$doi == doi][[1]]
    }

    if (!version %in% metadata$version) {
        stop("Requested version or DOI is not available.", call. = FALSE)
    }

    version_index <- match(
        version,
        strip_version_prefix(res$hits$hits$metadata$version)
    )
    selected_files <- get_version_files(res$hits$hits$files, version_index)
    rds_index <- grep("\\.rds$", selected_files$key)

    if (!length(rds_index)) {
        stop("No .rds artifact found for the requested version.", call. = FALSE)
    }

    rds_index <- rds_index[[1]]
    url <- selected_files$links$self[[rds_index]]
    filename <- file.path(path, selected_files$key[[rds_index]])

    if (!file.exists(filename)) {
        download_database(url = url, filename = filename)
    }

    message("Loading data from '", filename, "'")
    data <- readRDS(filename)
    class(data) <- "traits.build"

    data
}

#' List available versions for a Zenodo record
#'
#' @param record_id Zenodo record ID used to query the versions endpoint.
#' @param path Directory where metadata and database files are cached.
#' @param update If `TRUE`, refresh the cached versions JSON before reading it.
#'
#' @return A tibble with `publication_date`, `doi`, `version`, and `id`.
#' @export
#'
#' @examples
#' \dontrun{
#' get_versions(record_id = "1234567")
#' }
get_versions <- function(record_id,
                         path = file.path("data", record_id),
                         update = TRUE) {
    record_id <- validate_record_id(record_id)

    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    res <- load_json(record_id = record_id, path = path, update = update)
    create_metadata(res)
}

#' Get the latest version for a Zenodo record
#'
#' @param record_id Zenodo record ID used to query the versions endpoint.
#' @param path Directory where metadata and database files are cached.
#' @param update If `TRUE`, refresh the cached versions JSON before reading it.
#'
#' @return A character scalar giving the newest available version.
#' @export
#'
#' @examples
#' \dontrun{
#' get_version_latest(record_id = "1234567")
#' }
get_version_latest <- function(record_id,
                               path = file.path("data", record_id),
                               update = TRUE) {
    versions <- get_versions(record_id = record_id, path = path, update = update)
    versions$version[[1]]
}

load_json <- function(record_id, path, update) {
    file_json <- file.path(path, paste0("record-", record_id, ".json"))

    if (!file.exists(file_json) || isTRUE(update)) {
        res <- jsonlite::read_json(
            paste0("https://zenodo.org/api/records/", record_id, "/versions"),
            simplifyVector = TRUE
        )
        jsonlite::write_json(res, file_json, auto_unbox = TRUE)
    }

    jsonlite::fromJSON(file_json, simplifyVector = TRUE)
}

create_metadata <- function(res) {
    metadata <- res$hits$hits$metadata
    publication_date <- as.Date(metadata$publication_date)
    version <- as.character(
        numeric_version(strip_version_prefix(metadata$version))
    )
    id <- sub("^10\\.5281/zenodo\\.", "", metadata$doi)

    version_data <- tibble::tibble(
        publication_date = publication_date,
        doi = metadata$doi,
        version = version,
        id = id
    )

    version_data[order(version_data$publication_date, decreasing = TRUE), ]
}

download_database <- function(url, filename) {
    timeout <- getOption("timeout")
    options(timeout = max(300, timeout))
    on.exit(options(timeout = timeout), add = TRUE)

    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    tmp_file <- paste0(tempfile(), ".download")

    message("Downloading database to '", filename, "'")
    result <- utils::download.file(
        url = url,
        destfile = tmp_file,
        method = "auto",
        quiet = FALSE,
        mode = "wb",
        cacheOK = TRUE
    )

    if (!identical(result, 0L) && !identical(result, 0)) {
        stop("Could not download database.", call. = FALSE)
    }

    if (!file.rename(tmp_file, filename)) {
        if (!file.copy(tmp_file, filename, overwrite = TRUE)) {
            file.remove(tmp_file)
            stop("Could not move downloaded database into place.", call. = FALSE)
        }

        file.remove(tmp_file)
    }
}

strip_version_prefix <- function(version) {
    sub("^v", "", version)
}

validate_record_id <- function(record_id) {
    if (missing(record_id) || is.null(record_id) || !nzchar(as.character(record_id))) {
        stop("`record_id` must be supplied.", call. = FALSE)
    }

    as.character(record_id)
}

get_version_files <- function(files, version_index) {
    if (is.data.frame(files) || !is.null(files$key)) {
        return(files)
    }

    files[[version_index]]
}
