# Step 1 (2003-2006): download and unzip raw PAXRAW_C/D accelerometer files from CDC.

library(tidyverse)

# Specify the URL of the ZIP file
# https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination

unzip_dir = here::here("data",  "2003_2006", "accelerometry", "raw")

# Create the directory to unzip (if it doesn't already exist)
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
}

if (!dir.exists(here::here("data", "2003_2006", "accelerometry", "raw"))) {
  dir.create (here::here("data", "2003_2006", "accelerometry", "raw"), recursive = TRUE)
}

pax_d_url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2005/DataFiles/PAXRAW_D.zip"
pax_c_url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2003/DataFiles/PAXRAW_C.zip"
# Specify the file path to save the downloaded ZIP file
zip_file = here::here("data",  "2003_2006", "accelerometry","raw", "paxraw_d.zip")

# Download the ZIP file
download.file(pax_d_url, zip_file, mode = "wb")  # Use mode = "wb" for binary files


# Unzip the file
unzip(zip_file, exdir = unzip_dir)

zip_file = here::here("data", "2003_2006", "accelerometry", "raw", "paxraw_c.zip")

# Download the ZIP file
download.file(pax_c_url, zip_file, mode = "wb")  # Use mode = "wb" for binary files


# Unzip the file
unzip(zip_file, exdir = unzip_dir)

