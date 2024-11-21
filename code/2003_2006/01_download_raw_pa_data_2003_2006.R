library(tidyverse)

# Specify the URL of the ZIP file
# https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination
pax_d_url = "https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAXRAW_D.ZIP"
pax_c_url = "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAXRAW_C.ZIP"
# Specify the file path to save the downloaded ZIP file
zip_file = here::here("data",  "2003_2006", "accelerometry","raw", "paxraw_d.zip")

# Specify the directory to unzip the contents
unzip_dir = here::here("data",  "2003_2006", "accelerometry", "raw")

# Download the ZIP file
download.file(pax_d_url, zip_file, mode = "wb")  # Use mode = "wb" for binary files

# Create the directory to unzip (if it doesn't already exist)
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
}

# Unzip the file
unzip(zip_file, exdir = unzip_dir)

zip_file = here::here("data", "2003_2006", "accelerometry", "raw", "paxraw_c.zip")

# Download the ZIP file
download.file(pax_c_url, zip_file, mode = "wb")  # Use mode = "wb" for binary files

# Create the directory to unzip (if it doesn't already exist)
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
}

# Unzip the file
unzip(zip_file, exdir = unzip_dir)

