# SEAtraits

`SEAtraits` is a minimal R package for loading any Zenodo-hosted database built
with `traits.build`.

## Installation

```r
# install.packages("pak")
pak::pak("hrlai/SEAtraits")
```

## Usage

```r
library(SEAtraits)

versions <- get_versions(record_id = "16249456")
latest <- get_version_latest(record_id = "16249456")
database <- load_database(record_id = "16249456", version = latest)
```

The package caches the Zenodo versions JSON under `file.path("data",
record_id)` by default, downloads the selected `.rds` file when needed, and
returns the loaded object with class `"traits.build"`.
