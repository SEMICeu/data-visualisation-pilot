
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. data preparation ########################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("Data preparation \n")

### filter dataset Project 
Dataset_Projects <- merge(Dataset_Projects, Dataset_Countries, by.x=c("coordinatorCountry"), by.y=c("euCode"), all.x=TRUE)
#select only EU-28 countries
Dataset_Projects <- Dataset_Projects[Dataset_Projects$EU28 == TRUE,]
Dataset_Projects <- Dataset_Projects[!is.na(Dataset_Projects$coordinatorCountry),]

#remove useless column 
Dataset_Projects <- subset(Dataset_Projects, select=-c(reference,acronym,status,programme,topics,frameworkProgramme,title,endDate,projectUrl,objective,fundingScheme,coordinator,participants,subjects,
                                                       FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))

#clean data + set correct type for columns 
Dataset_Projects$totalCost <- sub(',', '.',Dataset_Projects$totalCost)
Dataset_Projects$totalCost <- as.numeric(Dataset_Projects$totalCost)

Dataset_Projects$startDate <- as.numeric(format(Dataset_Projects$startDate, "%Y"))

# remove projects with no start date => ~200 project
Dataset_Projects <- Dataset_Projects[!is.na(Dataset_Projects$startDate),]

# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. compute number Of Project coordinated per country/year #################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute number Of Project coordinated per country/year \n")

output <-aggregate(Dataset_Projects$coordinatorCountry,
                    by=list(Dataset_Projects$coordinatorCountry, Dataset_Projects$startDate),
                    FUN=length)
# output dataset will store all the results 
output <- plyr::rename(output,c("Group.1"="Country",
                                "Group.2" = "Year",
                                "x" = "numberOfProjectCoordinators"))

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. compute total budget managed by coordinator per country/year ############################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute total budget managed by coordinator per country/year \n")

##### compute totalBudgetManagedByCoordinators per country
tmp <- Dataset_Projects[!is.na(Dataset_Projects$totalCost),]
tmp <-aggregate(tmp$totalCost,
                   by=list(tmp$coordinatorCountry, tmp$startDate),
                   FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "totalBudgetManagedAsCoordinators"))

#save the result in output dataset
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)

remove(tmp)
remove(Dataset_Projects)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. compute the number of project participation (i.e. coordinator or participant role) per country/year ######################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute the number of project participation (i.e. coordinator or participant role) per country/year \n")

##### total number of project per country, year, framework contract
func_TotalNbOfProject <- function (datasetOrg, datasetProject) {
  #datasetOrg <- Dataset_FP7Organizations
  #datasetProject <- Dataset_FP7Projects
  tmp <- merge(datasetOrg, datasetProject, by.x=c("projectReference"), by.y=c("reference"), all.x=TRUE)
  tmp <- merge(tmp, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
  tmp <- tmp[tmp$EU28 == TRUE,]
  tmp <- tmp[!is.na(tmp$country),]
  tmp$startDate <- as.Date(as.POSIXlt(as.character(tmp$startDate), format="%Y-%m-%d"))
  tmp$startDate <- as.numeric(format(tmp$startDate, "%Y"))
  tmp <- tmp[!is.na(tmp$startDate),]
  tmp <-aggregate(tmp$country, by=list(tmp$country, tmp$startDate), FUN=length)
  return(tmp)
}

tmp1 <- func_TotalNbOfProject(Dataset_FP6Organizations, Dataset_FP6Projects)
tmp2 <- func_TotalNbOfProject(Dataset_FP7Organizations, Dataset_FP7Projects)
tmp3 <- func_TotalNbOfProject(Dataset_H2020Organizations, Dataset_H2020Projects)

tmp<-rbind(rbind(tmp1, tmp2), tmp3)

#group per country, year
tmp <-aggregate(tmp$x, by=list(tmp$Group.1, tmp$Group.2), FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "TotalOfProject"))

#save the result in output dataset
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)

remove(tmp1)
remove(tmp2)
remove(tmp3)
remove(tmp)
remove(func_TotalNbOfProject)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. compute the number of project participation (as participant) per country/year ###########################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute the number of project participation (as participant) per country/year \n")

output$numberOfProjectParticipants <- output$TotalOfProject - output$numberOfProjectCoordinators

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. add the GDP per country/year ############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the GDP per country/year \n")

output <- merge(output, Dataset_Countries, by.x=c("Country"), by.y=c("euCode"), all.x=TRUE)
output <- subset(output, select=-c(FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))

#save the result in output dataset
output <- merge(output, Dataset_GDP, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
remove(Dataset_GDP)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VII. add the population per country/year ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the population per country/year \n")

#save the result in output dataset
output <- merge(output, Dataset_population, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
remove(Dataset_population)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VIII. add the population by educational attainment level ##############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the population by educational attainment level \n")



Dataset_PopByEducAttainED34 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$isced11 == "ED3_4", ]
Dataset_PopByEducAttainED34 <- subset(Dataset_PopByEducAttainED34, select=-c(isced11))
Dataset_PopByEducAttainED34 <- plyr::rename(Dataset_PopByEducAttainED34,c("values" = "% pop. with a secondary education"))

Dataset_PopByEducAttainED58 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$isced11 == "ED5-8", ]
Dataset_PopByEducAttainED58 <- subset(Dataset_PopByEducAttainED58, select=-c(isced11))
Dataset_PopByEducAttainED58 <- plyr::rename(Dataset_PopByEducAttainED58,c("values" = "% pop. with a tertiary education"))

output <- merge(output, Dataset_PopByEducAttainED34, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
output <- merge(output, Dataset_PopByEducAttainED58, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)

remove(Dataset_PopByEducAttainED34)
remove(Dataset_PopByEducAttainED58)
remove(Dataset_PopByEducAttain)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# IX. add the total intramural R&D expenditure #########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the total intramural R&D expenditure \n")

Dataset_RDExpendAllSect <- Dataset_RDExpend[Dataset_RDExpend$sectperf == "TOTAL", ]
Dataset_RDExpendAllSect <- subset(Dataset_RDExpendAllSect, select=-c(sectperf))
Dataset_RDExpendAllSect$values <- Dataset_RDExpendAllSect$values * 1000000
Dataset_RDExpendAllSect <- plyr::rename(Dataset_RDExpendAllSect,c("values" = "National R&D Budget"))


output <- merge(output, Dataset_RDExpendAllSect, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
remove(Dataset_RDExpendAllSect)
remove(Dataset_RDExpend)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# X. compute GDP per capita per country/year #################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute GDP per capita per country/year \n")

output$GDP <- sub(',', '',output$GDP)
output$GDP <- as.numeric(output$GDP)
output$GDP <- output$GDP * 1000000

output$Population <- sub(' ', '',output$Population)
output$Population <-as.numeric(output$Population)
output$GDPPerCapita <- output$GDP / output$Population

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XI. compute budget managed as coordinators per capita per country/year #####################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute budget managed as coordinators per capita per country/year \n")

output$totalBudgetManagedAsCoordinatorsPerCapita <- output$totalBudgetManagedAsCoordinators / output$Population

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XII. compute cumulative information per country ############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute cumulative information per country \n")

output$numberOfProjectCoordinatorsCumulative <- ave(output$numberOfProjectCoordinators, output$Country, FUN = cumsum)
output$totalBudgetManagedAsCoordinatorsCumulative <- ave(output$totalBudgetManagedAsCoordinators, output$Country, FUN = cumsum)
output$TotalOfProjectCumulative <- ave(output$TotalOfProject, output$Country, FUN = cumsum)
output$numberOfProjectParticipantsCumulative <- ave(output$numberOfProjectParticipants, output$Country, FUN = cumsum)
output$totalBudgetManagedAsCoordinatorsPerCapitaCumulative <- ave(output$totalBudgetManagedAsCoordinatorsPerCapita, output$Country, FUN = cumsum)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XIII. rename attributes and select date range ###############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("exporting the updated data to /Datasets/Motionchart.csv \n")

output <- plyr::rename(output,c("Country" = "CountryShort",
                                "name"="Country", 
                                "numberOfProjectCoordinators" = "Nr. of coordinated projects",
                                "totalBudgetManagedAsCoordinators" = "Budget of coordinated projects",
                                "TotalOfProject" = "Total nr. of projects",
                                "numberOfProjectParticipants" = "Nr. of projects as participant",
                                "GDPPerCapita" = "GDP / capita",
                                "totalBudgetManagedAsCoordinatorsPerCapita" = "Budget of coordinated projects / capita",
                                "numberOfProjectCoordinatorsCumulative" = "Nr. of coordinated projects - Cumulative",
                                "totalBudgetManagedAsCoordinatorsCumulative" = "Budget of coordinated projects - Cumulative",
                                "TotalOfProjectCumulative" = "Total nr. of projects - Cumulative",
                                "numberOfProjectParticipantsCumulative" = "Nr. of projects as participant - Cumulative",
                                "totalBudgetManagedAsCoordinatorsPerCapitaCumulative" = "Budget of coordinated projects / capita - Cumulative"
                                ))

output <- output[output$Year <= arg_UpdateMotionChart_Year, ]
output$GDP <- format(output$GDP, scientific = FALSE)

#change Year to new Date(Year,month)
Year <- paste("new Date(",output$Year,",1)", sep="")
output$Year <- Year

#reorder the output and delete ConutryShort
output <- subset(output, select=-c(CountryShort))
output <- output[c(6, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)]

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XIII. convert to correct js format and save file ############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

# write column names to file
writeColNames <- function (data) {
  #open first square bracket
  cat('[', file="../Datasets/motionChart.js", append = TRUE)
  #go from first until second to last value (last value should not have a comma after it)
  #write the valued between quotes and a comma in between values
  for (i in 1:(length(data)-1)) {
    cat('"', file="../Datasets/motionChart.js", append = TRUE)
    cat(colnames(data)[i], file="../Datasets/motionChart.js", append = TRUE)
    cat('", ', file="../Datasets/motionChart.js", append = TRUE)
  }
  #write the last value (no comma afterwards) and close square bracket 
  cat('"', file="../Datasets/motionChart.js", append = TRUE)
  cat(colnames(data)[length(data)], file="../Datasets/motionChart.js", append = TRUE)
  cat('"], \n', file="../Datasets/motionChart.js", append = TRUE)
}

writeData <- function (data) {
  for (i in 1:nrow(data)) {
    # for every row of the data, open the row
    cat('[', file="../Datasets/motionChart.js", append = TRUE)
    
    for (j in 1:length(data)) {
      # for every value of that row
      
      if ( is.na(data[i,j]) ) {
        # if the value is NA, write null
        if (i != nrow(data) && j == length(data)) {
          # if it is the last value of the row, but not of the file, write null and close row
          cat('null], \n', file="../Datasets/motionChart.js", append = TRUE)
        } else if (i == nrow(data) && j == length(data)) {
          # if it is the last value of the row and the file, write null and close the file
          cat('null] \n ]; ', file="../Datasets/motionChart.js", append = TRUE)
        } else {
          # for all other values, write null and a comma in between
          cat('null, ', file="../Datasets/motionChart.js", append = TRUE)
        }
        
      } else {
        # if the value is not NA
        
        if (j == 1 ) {
          # first value should be written between brackets
          cat('"', file="../Datasets/motionChart.js", append = TRUE)
          cat(data[i,j], file="../Datasets/motionChart.js", append = TRUE)
          cat('", ', file="../Datasets/motionChart.js", append = TRUE)
        } else if (i != nrow(data) && j == length(data)) {
          # last value, not on last row, should close the row and have a comma
          cat(data[i,j], file="../Datasets/motionChart.js", append = TRUE)
          cat('], \n', file="../Datasets/motionChart.js", append = TRUE)
        } else if (i == nrow(data) && j == length(data)) {
          # last value on last row should close the row and the file
          cat(data[i,j], file="../Datasets/motionChart.js", append = TRUE)
          cat('] \n ];', file="../Datasets/motionChart.js", append = TRUE)
        } else {
          # every other value should not be between quotes and have a comma in between them
          cat(data[i,j], file="../Datasets/motionChart.js", append = TRUE)
          cat(', ', file="../Datasets/motionChart.js", append = TRUE)
        }
      }
    }
  }
}

#write info in file
cat('var data = [ \n', file = "../Datasets/motionChart.js")
writeColNames(output)
writeData(output)



today <- Sys.Date()
today <- format(today, format="%d/%m/%Y")
fileConn<-file("../Datasets/UpdateMotionChart.txt")
write(today, fileConn)
close(fileConn)

