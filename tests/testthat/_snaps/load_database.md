# load_database requires doi or version

    Code
      load_database(record_id = "1234567")
    Condition
      Error:
      ! Please supply either `doi` or `version`. Try `get_versions(record_id)` to inspect available versions.

# download_database cleans up temporary files on download errors

    Code
      suppressMessages(SEAtraits:::download_database(
        "https://example.org/database.rds", filename))
    Condition
      Error in `utils::download.file()`:
      ! boom

