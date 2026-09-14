# load_database requires doi or version

    Code
      load_database(record_id = "1234567")
    Condition
      Error:
      ! Please supply either `doi` or `version`. Try `get_versions(record_id)` to inspect available versions.

# load_database errors for an unavailable version

    Code
      load_database(record_id = "1234567", version = "9.9.9", path = path)
    Condition
      Error:
      ! Requested version or DOI is not available.

# load_database errors when multiple eligible rds artifacts remain

    Code
      load_database(record_id = "1234567", version = "1.0.0", path = path)
    Condition
      Error:
      ! Multiple eligible .rds artifacts found for the requested version.

# download_database cleans up temporary files on download errors

    Code
      suppressMessages(SEAtraits:::download_database(
        "https://example.org/database.rds", filename))
    Condition
      Error in `utils::download.file()`:
      ! boom

