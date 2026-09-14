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
