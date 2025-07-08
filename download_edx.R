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
  model_rds = "d7da5bd4-7b3d-443b-8f5a-38f0212577be",
  ipcc_gwp_factors = "416b3a71-94df-4a98-8b3e-a96cf0195574",
  examples_grouped = "90958b5b-4a1d-4ae0-9c85-834dfcc72997",
  eeio_template = "dd3e6f8a-c77f-4443-b94b-c8f6a03af29f",
  inventory_cw = "b810ebcd-4e5e-4298-9158-d5598089e0ab",
  example_upload = "1605ecfc-2b02-46b5-831d-9ae1837f930c",
  dict_sheet = "6ff80ddd-0dbb-4ba0-90ec-c4e1f0a224c0"
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
