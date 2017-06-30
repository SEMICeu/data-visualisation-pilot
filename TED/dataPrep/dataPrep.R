install.packages("jsonlite",repos = "http://cran.us.r-project.org")
library(jsonlite)



### download file###
args <- commandArgs(trailingOnly=TRUE)
apiKey <- args[1]

downloadData <- function(pagenum) {
  downloadURL <- paste("http://ted.europa.eu/api/latest/notices/search?apiKey=", 
                       apiKey,
                       "&q=AC%3D[1]&scope=2&pageSize=1000&pageNum=",
                       pagenum,
#                       "&sortField=ND&fields=ND,NUTS,DT,NC,ND,PD,PR,TD,MA,DI",
#                       "&sortField=ND&fields=AA,AC,CY,DS,MA,NC,ND,OC,OJ,OL,OY,PC,PD,PR,RC,RP,TD,TY,NUTS",
#                       "&sortField=ND&fields=AA,AC,CY,DI,DS,DT,MA,NC,ND,OC,OJ,OL,OY,PC,PD,PR,RC,RN,RP,TD,TY,NUTS,content",
"&sortField=ND&fields=ND,RC,MA,DI,TD,PD,DT",
  sep="")
  downloadURL <- URLencode(downloadURL)
  downloadData <- readLines(downloadURL, warn="F") 
  downloadData <- fromJSON(downloadData)
  downloadData <- downloadData$results 
  return(downloadData)
}



pageNum <- 1 
PreviousData<- NULL
CurrentData <- downloadData(pageNum)
TEDData <- NULL
while (is.null(PreviousData) || !identical(CurrentData, PreviousData)){
  if (is.null(TEDData)){
    TEDData <- CurrentData
  } else {
    TEDData  <- rbind.data.frame(TEDData, CurrentData)
  }
  pageNum <- pageNum + 1 
  PreviousData <- CurrentData
  CurrentData <- downloadData(pageNum)
  print(pageNum)
}

remove(CurrentData)
remove(PreviousData)
remove(apiKey)
remove(pageNum)

TEDData <- unique(TEDData)


### select currently open tenders ###
currentDate <- format(Sys.Date(), "%Y-%m-%d")
ListOfValueToKeep <- c("D","M","O","A","3")

TEDData$DataToKeep <-TEDData$TD %in% ListOfValueToKeep
TEDData <-TEDData[TEDData$DataToKeep == TRUE, ]

TEDData[is.na(TEDData$PD),] <- currentDate 
TEDData$DataToKeep <- with(TEDData, currentDate >= as.Date(PD))
TEDData <-TEDData[TEDData$DataToKeep == TRUE, ]

TEDData[is.na(TEDData$DT),]$DT <- as.Date(currentDate )
TEDData$DataToKeep <- with(TEDData, currentDate <= as.Date(TEDData$DT))
TEDData <-TEDData[TEDData$DataToKeep == TRUE, ]

TEDData <- subset(TEDData, select=-c(DataToKeep))
TEDData <- TEDData[!is.na(TEDData$DT),]


remove(currentDate)
remove(ListOfValueToKeep)



save <- TEDData

TEDData <- save

TEDData <- subset(TEDData, select = c("ND","RC","MA","DI"))
TEDData <- unique(TEDData)
### create output ###

OutputData <- list() 

for (i in 1:nrow(TEDData)) { #for each notice 
  noticeID <- TEDData$ND[i]
  noticeYear <- substr(noticeID, 1, 4)
  noticeID <- substr(noticeID, 5, nchar(noticeID))
  noticeID <- paste(noticeID, noticeYear, sep = "-")
  noticeDescription <- list(ID= noticeID, LB= TEDData$DI[[i]], MA=TEDData$MA[[i]])
  
  for (j in 1:length(TEDData$RC[[i]])) { # for each NUTS
    NUTSregionName <- TEDData$RC[[i]][j]
    NUTSLevel <- nchar(gsub('[^0-9]*', "",NUTSregionName))

    for (k in 0:NUTSLevel) {
      if (any(names(OutputData) == NUTSregionName)) {
        OutputData[[NUTSregionName]] <- append(OutputData[[NUTSregionName]], list(notice=noticeDescription))
        OutputData[[NUTSregionName]] <- unique(OutputData[[NUTSregionName]])
      } else {
        tmp <- list (notice = noticeDescription)
        OutputData[[NUTSregionName]] <-  tmp
      }
      
      NUTSregionName <- substr(NUTSregionName, 1, nchar(NUTSregionName) - 1)
      
    }
  }
}

### write file ###
cat('var TEDdata = \n', file = "../Datasets/TEDdata.js")
cat(toJSON(OutputData, pretty=TRUE), file = "../Datasets/TEDdata.js", append = TRUE)
