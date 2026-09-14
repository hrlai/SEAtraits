write_versions_cache <- function(path, record_id, records) {
    payload <- list(
        hits = list(
            hits = lapply(records, function(record) {
                list(
                    metadata = list(
                        publication_date = record$publication_date,
                        doi = record$doi,
                        version = record$version
                    ),
                    files = list(
                        list(
                            key = record$key,
                            links = list(self = record$self)
                        )
                    )
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

test_that("load_database requires doi or version", {
    expect_snapshot(error = TRUE, {
        load_database(record_id = "1234567")
    })
})

test_that("create_metadata standardizes metadata fields", {
    res <- list(
        hits = list(
            hits = list(
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

test_that("download_database copies into place when rename fails", {
    path <- tempfile("download-")
    dir.create(path)
    filename <- file.path(path, "traits-build.rds")
    copied_to <- NULL
    base_file_copy <- base::file.copy

    testthat::local_mocked_bindings(
        download.file = function(url, destfile, method, quiet, mode, cacheOK) {
            writeBin(charToRaw("db"), destfile)
            0L
        },
        .package = "utils"
    )
    testthat::local_mocked_bindings(
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
