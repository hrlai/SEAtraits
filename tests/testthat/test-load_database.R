write_versions_cache <- function(path, record_id, records) {
    payload <- list(
        hits = list(
            hits = lapply(seq_along(records), function(index) {
                record <- records[[index]]
                files <- record$files

                if (is.null(files)) {
                    files <- list(
                        list(
                            key = record$key,
                            links = list(self = record$self)
                        )
                    )
                }

                list(
                    id = record$id %||% as.character(index),
                    metadata = list(
                        publication_date = record$publication_date,
                        doi = record$doi,
                        version = record$version
                    ),
                    files = files
                )
            })
        )
    )

    jsonlite::write_json(
        payload,
        file.path(path, paste0("record-", record_id, ".json")),
        auto_unbox = TRUE
    )
}

`%||%` <- function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

test_that("load_database requires doi or version", {
    expect_snapshot(error = TRUE, {
        load_database(record_id = "1234567")
    })
})

test_that("load_database errors for an unavailable version", {
    path <- tempfile("versions-")
    dir.create(path)

    write_versions_cache(
        path = path,
        record_id = "1234567",
        records = list(
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.200",
                version = "V1.0.0",
                key = "traits-build-1.0.0.rds",
                self = "https://example.org/traits-build-1.0.0.rds"
            )
        )
    )

    expect_snapshot(error = TRUE, {
        load_database(record_id = "1234567", version = "9.9.9", path = path)
    })
})

test_that("create_metadata ignores entries without versions", {
    res <- list(
        hits = list(
            hits = list(
                id = c("300", "200", "100"),
                metadata = data.frame(
                    publication_date = c("2024-03-01", "2024-02-01", "2024-01-01"),
                    doi = c(
                        "10.5281/zenodo.300",
                        "10.5281/zenodo.200",
                        "10.5281/zenodo.100"
                    ),
                    version = c(NA, "v1.0.0", ""),
                    stringsAsFactors = FALSE
                )
            )
        )
    )

    metadata <- SEAtraits:::create_metadata(res)

    expect_identical(metadata$version, "1.0.0")
    expect_identical(metadata$id, "200")
})

test_that("create_metadata standardizes metadata fields", {
    res <- list(
        hits = list(
            hits = list(
                id = c("200", "100"),
                metadata = data.frame(
                    publication_date = c("2024-02-01", "2024-01-01"),
                    doi = c("10.5281/zenodo.200", "10.5281/zenodo.100"),
                    version = c("v1.0.0", "0.9.0"),
                    stringsAsFactors = FALSE
                )
            )
        )
    )

    metadata <- SEAtraits:::create_metadata(res)

    expect_s3_class(metadata, "tbl_df")
    expect_identical(
        names(metadata),
        c("publication_date", "doi", "version", "id")
    )
    expect_identical(metadata$version, c("1.0.0", "0.9.0"))
    expect_identical(metadata$id, c("200", "100"))
})

test_that("get_version_latest breaks publication-date ties by version", {
    path <- tempfile("versions-")
    dir.create(path)

    write_versions_cache(
        path = path,
        record_id = "1234567",
        records = list(
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.100",
                version = "1.0.0",
                key = "traits-build-1.0.0.rds",
                self = "https://example.org/traits-build-1.0.0.rds"
            ),
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.200",
                version = "1.1.0",
                key = "traits-build-1.1.0.rds",
                self = "https://example.org/traits-build-1.1.0.rds"
            )
        )
    )

    expect_identical(
        get_version_latest(record_id = "1234567", path = path, update = FALSE),
        "1.1.0"
    )
})

test_that("get_version_latest returns the newest normalized version", {
    path <- tempfile("versions-")
    dir.create(path)

    write_versions_cache(
        path = path,
        record_id = "1234567",
        records = list(
            list(
                publication_date = "2024-01-01",
                doi = "10.5281/zenodo.100",
                version = "0.9.0",
                key = "traits-build-0.9.0.rds",
                self = "https://example.org/traits-build-0.9.0.rds"
            ),
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.200",
                version = "v1.0.0",
                key = "traits-build-1.0.0.rds",
                self = "https://example.org/traits-build-1.0.0.rds"
            )
        )
    )

    expect_identical(
        get_version_latest(record_id = "1234567", path = path, update = FALSE),
        "1.0.0"
    )
})

test_that("get_version_files handles a single-version cached payload", {
    files <- list(
        key = "traits-build-1.0.0.rds",
        links = list(self = "https://example.org/traits-build-1.0.0.rds")
    )

    selected <- SEAtraits:::get_version_files(files, 1)

    expect_identical(selected$key, "traits-build-1.0.0.rds")
    expect_identical(
        selected$links$self,
        "https://example.org/traits-build-1.0.0.rds"
    )
})

test_that("get_version_files selects the requested entry from vectorized lists", {
    files <- list(
        key = c("traits-build-0.9.0.rds", "traits-build-1.0.0.rds"),
        links = data.frame(
            self = c(
                "https://example.org/traits-build-0.9.0.rds",
                "https://example.org/traits-build-1.0.0.rds"
            ),
            stringsAsFactors = FALSE
        )
    )

    selected <- SEAtraits:::get_version_files(files, 2)

    expect_identical(selected$key, "traits-build-1.0.0.rds")
    expect_identical(
        selected$links$self,
        "https://example.org/traits-build-1.0.0.rds"
    )
})

test_that("get_version_files handles vectorized link lists", {
    files <- list(
        key = c("traits-build-0.9.0.rds", "traits-build-1.0.0.rds"),
        links = list(
            list(self = "https://example.org/traits-build-0.9.0.rds"),
            list(self = "https://example.org/traits-build-1.0.0.rds")
        )
    )

    selected <- SEAtraits:::get_version_files(files, 2)

    expect_identical(selected$key, "traits-build-1.0.0.rds")
    expect_identical(
        selected$links$self,
        "https://example.org/traits-build-1.0.0.rds"
    )
})

test_that("load_database uses the selected metadata row for duplicates", {
    path <- tempfile("database-")
    dir.create(path)
    record_id <- "1234567"

    older <- structure(list(source = "older"), class = "existing")
    newer <- structure(list(source = "newer"), class = "existing")

    saveRDS(older, file.path(path, "traits-build-older.rds"))
    saveRDS(newer, file.path(path, "traits-build-newer.rds"))

    write_versions_cache(
        path = path,
        record_id = record_id,
        records = list(
            list(
                publication_date = "2024-01-01",
                doi = "10.5281/zenodo.100",
                version = "v1.0",
                key = "traits-build-older.rds",
                self = "https://example.org/traits-build-older.rds"
            ),
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.200",
                version = "1.0.0",
                key = "traits-build-newer.rds",
                self = "https://example.org/traits-build-newer.rds"
            )
        )
    )

    database <- load_database(
        record_id = record_id,
        version = "1.0.0",
        path = path,
        update = FALSE
    )

    expect_identical(database$source, "newer")
    expect_identical(class(database), c("traits.build", "existing"))
})

test_that("load_database prefers non-flattened rds artifacts", {
    path <- tempfile("database-")
    dir.create(path)
    record_id <- "1234567"

    canonical <- structure(list(source = "canonical"), class = "existing")
    saveRDS(canonical, file.path(path, "traits-build.rds"))

    write_versions_cache(
        path = path,
        record_id = record_id,
        records = list(
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.200",
                version = "1.0.0",
                files = list(
                    list(
                        key = "traits-build-flattened.rds",
                        links = list(self = "https://example.org/traits-build-flattened.rds")
                    ),
                    list(
                        key = "traits-build.rds",
                        links = list(self = "https://example.org/traits-build.rds")
                    )
                )
            )
        )
    )

    database <- load_database(
        record_id = record_id,
        version = "1.0.0",
        path = path,
        update = FALSE
    )

    expect_identical(database$source, "canonical")
})

test_that("load_database errors when multiple eligible rds artifacts remain", {
    path <- tempfile("database-")
    dir.create(path)

    write_versions_cache(
        path = path,
        record_id = "1234567",
        records = list(
            list(
                publication_date = "2024-02-01",
                doi = "10.5281/zenodo.200",
                version = "1.0.0",
                files = list(
                    list(
                        key = "traits-build-a.rds",
                        links = list(self = "https://example.org/traits-build-a.rds")
                    ),
                    list(
                        key = "traits-build-b.rds",
                        links = list(self = "https://example.org/traits-build-b.rds")
                    )
                )
            )
        )
    )

    expect_snapshot(error = TRUE, {
        load_database(record_id = "1234567", version = "1.0.0", path = path)
    })
})

test_that("download_database copies into place when rename fails", {
    path <- tempfile("download-")
    dir.create(path)
    filename <- file.path(path, "traits-build.rds")
    copied_to <- NULL
    base_file_copy <- base::file.copy
    tmp_base <- file.path(path, "staged-download")
    tmp_file <- paste0(tmp_base, ".download")

    testthat::local_mocked_bindings(
        download.file = function(url, destfile, method, quiet, mode, cacheOK) {
            writeBin(charToRaw("db"), destfile)
            0L
        },
        .package = "utils"
    )
    testthat::local_mocked_bindings(
        tempfile = function(...) {
            tmp_base
        },
        file.rename = function(from, to) {
            FALSE
        },
        file.copy = function(from, to, overwrite = FALSE) {
            copied_to <<- to
            base_file_copy(from, to, overwrite = overwrite)
        },
        .package = "base"
    )

    SEAtraits:::download_database("https://example.org/database.rds", filename)

    expect_identical(copied_to, filename)
    expect_true(file.exists(filename))
    expect_false(file.exists(tmp_file))
})

test_that("download_database cleans up temporary files on download errors", {
    path <- tempfile("download-")
    dir.create(path)
    filename <- file.path(path, "traits-build.rds")
    tmp_base <- file.path(path, "downloaded-file")
    tmp_file <- paste0(tmp_base, ".download")

    testthat::local_mocked_bindings(
        tempfile = function(...) {
            tmp_base
        },
        .package = "base"
    )
    testthat::local_mocked_bindings(
        download.file = function(url, destfile, method, quiet, mode, cacheOK) {
            writeBin(charToRaw("db"), destfile)
            stop("boom")
        },
        .package = "utils"
    )

    expect_snapshot(error = TRUE, {
        suppressMessages(
            SEAtraits:::download_database(
                "https://example.org/database.rds",
                filename
            )
        )
    })
    expect_false(file.exists(tmp_file))
})
