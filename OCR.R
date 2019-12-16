# EDS Project
# Zhinan Chen

#get packages and set working directory
library(tesseract)
if (!require("pacman")) install.packages("pacman")
p_load(textreadr, tesseract, stringr)
library(pacman)
p_load(tidyverse, data.table, ggplot2, rgdal, sf, RecordLinkage)
library(tidycensus)

getwd()
# Change that to the directory on your own computer
setwd("/Users/chenzhinan/Desktop/EDS")

#change file names
file.rename(list.files(pattern="Perm*"), paste0("Perm", 1:168, ".pdf"))

#create a list of files
directory <- "/Users/chenzhinan/Desktop/EDS/permits"
file.list <- paste(directory, "/",list.files(directory, pattern = "*.pdf"), sep = "")
summary(file.list)

# Check which pages in the permits have the information we need
for (myFile in file.list){
  #myFile <- file.list[1]
  myDirectory <- dirname(path = myFile)
  tempName <- basename(myFile)
  tempName <- substr(tempName, start = 1, stop = nchar(tempName)-4)
  index <- which(file.list==myFile)
  print(index)
  numPages <- pdftools::pdf_info(myFile)$pages
  outputName <- paste0("/Users/chenzhinan/Desktop/EDS/pages/", tempName, "_", index, ".pdf")
  if (numPages == 1){
    pdftools::pdf_subset(input = myFile, pages = 1, output = outputName)
  } else if (numPages == 2){
    pdftools::pdf_subset(input = myFile, pages = 2, output = outputName)
  } else if (numPages >= 3){
    pdftools::pdf_subset(input = myFile, pages = 3, output = outputName)
  }
}
# Found out that among the 168 files, 1 has 1 page while the rest has more than 3 pages.
# Among the files with multiple pages, the information we want to get is either or page 2 or 3
# Fixed the problem by replacing the 1-page document with a new one

#convert scanned PDFs to text files
j <-1
mycsv <- list()
for (myFile in file.list){
  pngfile <- pdftools::pdf_convert(pdf = myFile, format = "png", pages = c(2:3), dpi = 600)
  text <- tesseract::ocr(pngfile)
  mycsv[[j]] <- text
  j <- j+1
}
View(mycsv)
write.csv(mycsv, file = "mycsv.csv")

# create data frame and write variables to it
permitinfo <- data.frame(matrix(ncol = 3, nrow = 168))
colnames(permitinfo) <- c("Permit_Number", "Service_Area","Area")


# Find strings that have the information and write them to the table
for (mytext in mycsv){
  start1 = regexpr('PERMIT NUMBER:', mycsv) 
  permitinfo$Permit_Number = substr(mycsv, start1+14, start1+19)
  
  start2 = regexpr('APPROVED SERVICE AREA:', mycsv)
  end2 = regexpr('In accordance with', mycsv)
  permitinfo$Service_Area = substr(mycsv, start2+22, end2-3)
  
  start3 = regexpr('approximately', mycsv)
  end3 = regexpr('acres', mycsv)
  permitinfo$Area = substr(mycsv, start3+13, end3-1)
  
}

# Write the data to a csv file
write.table(permitinfo, file = "MyData.csv")

# Get census data from US census bureau
census_api_key("18938f278197578ee47a8dad71141344868d8deb", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
options(tigris_use_cache = TRUE)
v<-load_variables(2017, "acs5", cache = TRUE)

## race, get total population and white alone (not hispanic or latino)
race17 <- get_acs(state = "AL", geography = "block group", variables = c("B03002_001", "B03002_003"), year = 2017, geometry = TRUE, cache_table = TRUE, output = "wide")
race17 <- mutate(race17, nonwhite17 = 1 - B03002_003E/B03002_001E)

## income for 2017 add up all hh income < $25k year
inc17 <- get_acs(state = "AL", geography = "block group", variables = c("B19001_001", "B19001_002", "B19001_003", "B19001_004", "B19001_005"), year = 2017, cache_table = TRUE, output = "wide")
inc17 <- mutate(inc17, underpov17 = B19001_002E + B19001_003E + B19001_004E + B19001_005E)
inc17 <- mutate(inc17, percunderpov17 = underpov17/B19001_001E)

data17 <- merge(race17, inc17, by = "GEOID", type = "full")  
data17 <- mutate(data17, ejind17 = percunderpov17 + nonwhite17)

### plot map of census blocks
ggplot(data = data17)  +
  geom_sf()

### read in google my maps layer
landfills.shp <- readOGR("Alabama Landfills.kml", "MSW Facilities")
## convert google maps sp geometry into sf geometry
landfills.sf <- st_as_sf(landfills.shp, coords = c("lon", "lat"), crs = 4269)
## convert google maps coordinate system (4269) into census - AL (2759)
landfills.sf <- st_transform(landfills.sf, 2759)
data17.sf <- st_transform(data17.sf, 2759) 
## create 1 mile buffer
landfill_buff <- st_buffer(landfills.sf$geometry, 1609.34)

### plot the map with landfills
{ggplot() +
  geom_sf(data = data17.sf, aes(fill= nonwhite17, color=nonwhite17)) +
  geom_sf(data = landfills.sf, color = 'black') +
  geom_sf(data = landfill_buff, color = 'red')}
