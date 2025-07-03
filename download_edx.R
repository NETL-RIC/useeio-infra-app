# ABOUT ----------------------------------------------------------------------
#
# download_edx.R
#
# Authors: F. Hanna and T.W. Davis
#
# Last updated: 2025-07-03
#
# Purpose: This script assists downloading required input datasets that are
# found on NETL's EDX (http://edx.netl.doe.gov/)

# LIBRARIES ------------------------------------------------------------------
library(httr)
library(jsonlite)

# API PROMPT -----------------------------------------------------------------
# User inputs: set API
# Thanks IRTFM (https://stackoverflow.com/a/27114322)
cat("Please enter your EDX API key: ")
edx_api_key <- readLines("stdin", n = 1)

# DOWNLOAD RESOURCES ---------------------------------------------------------
# Set output folder within this git repo; should be ./src/inputs
dir_1 <- getwd()
target_subfolder <- "src/inputs"
output_dir <- file.path(dir_1, target_subfolder)

# Add resource id for the files to be downloaded from EDX
resource_ids <- list(
  model_rds = "5da4b595-7261-4e1f-a16e-eb4c7c243bd0",
  ipcc_gwp_factors = "36699d2f-a1ca-4788-90eb-37f88ebf3a92",
  examples_grouped = "c9463532-635d-44fa-81f8-744f47acfc2b",
  eeio_template = "79d9f605-c753-4c03-bcd4-46b569dbcad3",
  inventory_cw = "f9c8ff07-7b7d-4b5b-8728-6d0ba809a404",
  example_upload = "2397cfda-e47a-4eb3-beb0-f9ba64be6052",
  dict_sheet = "08b2e3a8-3de1-41bb-9d1c-0f14d1f8d299"
)
edx_url <- "https://edx.netl.doe.gov/resource/"
edx_dl <- "/download"

# Loop through the files and download them
for (name in names(resource_ids)){
  resource_id <- resource_ids[[name]]
  url <- paste(edx_url, resource_id, edx_dl, sep = "")

  headers <- if (!is.null(edx_api_key)) {
    add_headers(Authorization = edx_api_key)
  } else {
    NULL
  }
  message("Downloading ", name, " from ", url)

  res <- GET(url, headers, write_disk(tempfile(), overwrite = TRUE))

  # Extract filename from Content-Disposition header;
  # this is done to maintain filenames as used in the codebase
  # (final intended application).
  cd <- headers(res)$`content-disposition`
  file_name <- sub('.*filename="?([^";]+)"?.*', '\\1', cd)
  final_path <- file.path(output_dir, file_name)

  # Move temp file to final destination
  file.rename(res$content, final_path)
}
