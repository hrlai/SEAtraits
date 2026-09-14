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

    version_input <- version

    if (!is.null(version)) {
        version <- strip_version_prefix(version)
    }

    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    res <- load_json(record_id = record_id, path = path, update = update)
    metadata <- create_metadata(res, include_index = TRUE)

    if (!is.null(doi)) {
        if (!doi %in% metadata$doi) {
            stop("Requested version or DOI is not available.", call. = FALSE)
        }

        version <- metadata$version[metadata$doi == doi][[1]]
    }

    if (!version %in% metadata$version) {
        stop("Requested version or DOI is not available.", call. = FALSE)
    }

    if (!is.null(doi)) {
        selected_record <- metadata[metadata$doi == doi, , drop = FALSE]
    } else {
        selected_record <- metadata[metadata$raw_version == version_input, , drop = FALSE]

        if (!nrow(selected_record)) {
            selected_record <- metadata[metadata$version == version, , drop = FALSE]
        }
    }

    selected_record <- selected_record[1, , drop = FALSE]
    selected_files <- get_version_files(
        res$hits$hits$files,
        selected_record$index[[1]]
    )
    rds_index <- grep("\\.rds$", selected_files$key)

    if (length(rds_index) > 1) {
        non_flattened <- !grepl("flattened", selected_files$key, fixed = TRUE)
        rds_index <- rds_index[non_flattened[rds_index]]
    }

    if (!length(rds_index)) {
        stop("No .rds artifact found for the requested version.", call. = FALSE)
    }

    if (length(rds_index) > 1) {
        stop(
            "Multiple eligible .rds artifacts found for the requested version.",
            call. = FALSE
        )
    }

    rds_index <- rds_index[[1]]
    url <- selected_files$links$self[[rds_index]]
    filename <- file.path(path, selected_files$key[[rds_index]])

    if (!file.exists(filename)) {
        download_database(url = url, filename = filename)
    }

    message("Loading data from '", filename, "'")
    data <- readRDS(filename)
    class(data) <- unique(c("traits.build", class(data)))

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

    if (!nrow(versions)) {
        stop("No versions available for the requested record.", call. = FALSE)
    }

    versions$version[[1]]
}

load_json <- function(record_id, path, update) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    file_json <- file.path(path, paste0("record-", record_id, ".json"))

    if (!file.exists(file_json) || isTRUE(update)) {
        res <- jsonlite::read_json(
            paste0("https://zenodo.org/api/records/", record_id, "/versions"),
            simplifyVector = TRUE
        )
        jsonlite::write_json(res, file_json, auto_unbox = TRUE)
        return(res)
    }

    jsonlite::fromJSON(file_json, simplifyVector = TRUE)
}

create_metadata <- function(res, include_index = FALSE) {
    metadata <- res$hits$hits$metadata
    publication_date <- as.Date(metadata$publication_date)
    version_rank <- numeric_version(strip_version_prefix(metadata$version))
    version <- as.character(version_rank)
    id <- res$hits$hits$id

    if (is.null(id)) {
        id <- sub("^10\\.5281/zenodo\\.", "", metadata$doi)
    }

    version_data <- tibble::tibble(
        publication_date = publication_date,
        doi = metadata$doi,
        version = version,
        id = as.character(id),
        raw_version = metadata$version,
        index = seq_along(metadata$doi)
    )
    order_index <- order(
        -as.numeric(version_data$publication_date),
        -xtfrm(version_rank)
    )
    version_data <- version_data[order_index, ]

    if (!include_index) {
        return(version_data[, c("publication_date", "doi", "version", "id")])
    }

    version_data
}

download_database <- function(url, filename) {
    timeout <- getOption("timeout", 60)
    options(timeout = max(300, timeout))
    on.exit(options(timeout = timeout), add = TRUE)

    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    tmp_file <- paste0(tempfile(), ".download")
    on.exit(unlink(tmp_file), add = TRUE)

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
    if (is.data.frame(files)) {
        if (nrow(files) == 1L) {
            return(files)
        }

        return(files[version_index, , drop = FALSE])
    }

    if (!is.null(files$key)) {
        if (length(files$key) == 1L) {
            return(files)
        }

        return(list(
            key = files$key[[version_index]],
            links = list(self = get_version_self_link(files$links, version_index))
        ))
    }

    files[[version_index]]
}

get_version_self_link <- function(links, version_index) {
    if (is.data.frame(links)) {
        return(links$self[[version_index]])
    }

    links[[version_index]]$self
}
