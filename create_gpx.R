library(pdftools)
library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(pgirmess)
library(rmarkdown)

download.file("https://huntfish.mdc.mo.gov/sites/default/files/downloads/page/Truman_Lake_FA_Maps.pdf", "truman_maps.pdf", mode = "wb")

pdf <- pdf_text("~/truman_maps.pdf")

df <- pdf[11:20] %>%  # ===== Read pdf pages 11-20
  read_lines(skip=1) %>%    # ===== Read each line, skip first row
  grep(("^Tru"), ., value = TRUE) %>% # ===== Select lines starting with "Tru"
  paste(collapse = '\n') %>% # ===== Recombine
  read_fwf(fwf_empty(.)) %>%  # ===== Create columns as fixed width delimitation
  unite("Lake", X1:X2, sep = " ", na.rm = TRUE, remove = TRUE) %>% # ===== Combine unintended split of first column
  separate(., X3, into = c("ID","Type"), sep = "\\s", extra = "merge") %>% # ===== Separate the ID and Type
  dplyr::mutate(Type = stringr::str_trim(Type, side = "left")) %>% # ===== This removes the leading space observed in Type
  separate(., X5, into = c("Longitude","Latitude"), sep = "\\s", extra = "merge") %>% # ===== This separates, but exposed leading spaces in Latitudes
  dplyr::mutate(Latitude = stringr::str_trim(Latitude, side = "left")) %>% # ===== This removes the leading space observed in Latitudes
  separate(., Latitude, into = "Latitude", sep = "\\s", extra = "drop") %>%
  unique() %>% # ===== Remove any duplicate rows
  rename("Year" = "X4")

df # ===== Veiw a sample of the created dataframe

write_csv(df, "truman_lake_fa.csv") # ===== Why not store a small, local copy...

# ===== Using the unit to manually enter a "Brush pile" I was able to export a sample and read it from the unit GPX file
# <wpt lat="38.1530311704" lon="-93.4115445614">
#   <time>2020-06-23T12:49:17Z</time>
#   <name>BRUSH1</name>
#   <sym>Weed Bed</sym>
#   <extensions>
#     <gpxx:WaypointExtension>
#         <gpxx:Temperature>25.21</gpxx:Temperature>
#         <gpxx:Depth>10.12</gpxx:Depth>
#         <gpxx:DisplayMode>SymbolAndName</gpxx:DisplayMode>
#     </gpxx:WaypointExtension>
#     <uuidx:uuid>5790a449-ee83-42ef-be6f-7dbe1698f594</uuidx:uuid>
#   </extensions>
# </wpt>
# ===== Or... Read sample data on the card from an existing GPX file
files <- list.files(pattern='\\d\\d.GPX$', full.names=TRUE) # ===== Identify gpx files that contain waypoints
allwaypoints <- list()  # ===== Create a list and loop to read waypoints
for (i in 1:length(files)) {
  allwaypoints[[i]] <- readGPX(files[i], tracks=FALSE, routes=FALSE)$waypoints[, c('name', 'lon', 'lat', 'time')]
}
allwaypoints <- do.call('rbind', allwaypoints) # ===== Combine all waypoints into a dataframe

# ===== Set and populate variables to construct the new GPX
lat <- df$Latitude
lon <- df$Longitude
name <- paste(df$ID,df$Type,df$Year, sep = ' ')
df$time <- gsub("\\s", "", paste(ymd(today()),'T',strftime(now(), format="%H:%M:%S"),'Z'))
time <- df$time
sym <- as.character('Weed Bed')
df2 <- data.frame(lat,lon,time,name,sym)
gpx <-data.frame(ID=name,Long= lon,
                 Lat= lat)
writeGPX(gpx, filename = "base",type="w")
file.copy("base.gpx", "/Volumes/GARMIN/Garmin/UserData") # ===== This file can now be "merged" within the Garmin unit  via ActiveCaptain
