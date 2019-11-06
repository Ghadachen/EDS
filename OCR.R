# EDS Project
# Zhinan Chen

#get packages and set working directory
library(tesseract)
if (!require("pacman")) install.packages("pacman")
p_load(textreadr, tesseract, stringr)

getwd()
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
