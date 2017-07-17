
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. FUNCTIONS LINK ##########################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("loading functions \n")

func_size <- function(vector){
  return(length(unlist(vector)))
}

func_order <- function(vector){
  return (as.character(sort(unlist(vector))))
}

#take a vector (e.g. [A,B,C]) and return all the possible pair of value (e.g. [[A, B],[A, C],[B, C]]) 
func_createPair <- function(vector){
  if (length(vector) > 1) {
    return (t(combn(unlist(vector), 2)))
  }
  else
  {
    return (c())
  }
}

#transform a list [a,b,c,d,e,f] in a frame with 2 columns [[a,b,c],[d,e,f]]
func_createFrame <- function(vector){
  return(data.frame(matrix(unlist(vector), ncol = 2, byrow=FALSE)))
}

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function(frameworkContract, dataset) {
  #select only eu country
  dataset <- merge(dataset, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
  dataset <- dataset[dataset$EU28 == TRUE, ]
  dataset <- dataset[!is.na(dataset$EU28),]

  #remove useless column 
  dataset <- subset(dataset, select=-c(projectAcronym, role, id, name.x, shortName, activityType, endOfParticipation, ecContribution, street, city, postCode,
                                        organizationUrl, contactType, contactTitle, contactFirstNames, contactLastNames, contactFunction, contactTelephoneNumber, contactFaxNumber,
                                        contactEmail, FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen, country))
  dataset <- plyr::rename(dataset,c("name.y"="country"))

  #for each project, list the country that participated 
  dataset <-aggregate(dataset$country, 
                      by=list(dataset$projectReference),
                      FUN=paste)
  dataset <- plyr::rename(dataset,c("Group.1"="Project",
                                    "x" = "ListCountries"))
  dataset$ListCountries <- sapply(dataset$ListCountries, unique)
  
  
  # create all the link between two country per project (projA [AA,AB,BC], projB [AA,AB,BC]...) 
  dataset$ListCountries <- sapply(dataset$ListCountries, func_order)
  dataset$size <- sapply(dataset$ListCountries, func_size)
  dataset<- dataset[dataset$size > 1,]  
  dataset <- subset(dataset, select=-c(size, Project))

  
  # create all the link between two country
  dataset$ListCountries <- sapply(dataset$ListCountries, func_createPair)
  data <- list()
  for (i in 1:nrow(dataset)) {
    data[[i]] =func_createFrame(dataset[i,])
  }

  tmp <- do.call(rbind, data)
  output <-data.frame(tmp)
  
  #aggregate to have: country 1 | country 2 | nb link
  output<-aggregate(output$X1,
                    by=list(output$X1, output$X2),
                    FUN=length)
  output <-data.frame(output)
  output <- plyr::rename(output,c("Group.1" = "country1",
                                  "Group.2" = "country2",
                                  "x" = "nbLinkTmp"))
  
  #add a column for the framework contract 
  output$frameworkContract <- frameworkContract
  

  return(output)
}

# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. Build Dataset link ######################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

cat("producing the data for FP6 \n")
FP6_link <- func_BuildLink("FP6", Dataset_FP6Organizations)
FP6_link$frameworkContract<- "FP6"
cat("producing the data for FP7 \n")
FP7_link <- func_BuildLink("FP7", Dataset_FP7Organizations)
FP7_link$frameworkContract<- "FP7"
cat("producing the data for H2020 \n")
H2020_link <- func_BuildLink("H2020", Dataset_H2020Organizations)
H2020_link$frameworkContract<- "H2020"

cat("merging the produced data for FP6, FP7, H2020 \n")
Dataset_link <-rbind(rbind(FP6_link, FP7_link), H2020_link)

cat("exporting the updated data to /Datasets/Chord.csv \n")
options(scipen = 10)
write.table(Dataset_link, "../Datasets/chord.csv", sep = ",", quote = FALSE, row.names = FALSE)
options(scipen = 0)

