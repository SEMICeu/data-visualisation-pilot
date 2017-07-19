require(data.table)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
subset_H2020Organizations <- subset(Dataset_H2020Organizations, select=c(projectReference, name, activityType, country))

Dataset_Countries = read.csv("../Datasets/inputData/countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Countries <- subset(Dataset_Countries, select=c("euCode", "name"))
Countries <- plyr::rename(Countries, c("name" = "countryName"))

# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. FUNCTIONS LINK #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_BuildLinkForOneOrg <- function(dataset, orgName){
  #list all the projects link to the current organisation 
  listProject <- dataset[dataset$name == orgName, ]$projectReference
  listProject <- unique(listProject)
  
  #list all the org that have participated to these projects 
  listOrg  <- dataset[dataset$projectReference %in% listProject,]$name
  listOrg <- unique(listOrg)
  
  datasetTmp <- dataset[dataset$name %in% listOrg,]
  datasetTmp <- dataset[dataset$projectReference %in% listProject,]
  datasetTmp <- subset(datasetTmp, select=c(projectReference, name))
  datasetTmp <- datasetTmp[datasetTmp$name != orgName, ]
  
  if (nrow(datasetTmp) == 0)
  {
    output <- data.frame(Source = as.character(), Target = as.character(), Type = as.character())
  }
  else 
  {
    #compute the number of project per organisation
    datasetTmp <-aggregate(datasetTmp$name, 
                           by=list(datasetTmp$projectReference),
                           FUN=paste, simplify = FALSE)
    datasetTmp <- plyr::rename(datasetTmp,c("Group.1"="Project",
                                            "x" = "Organisations"))
    #create all the pair org1, org2 
    data <- list()
    for (i in 1:nrow(datasetTmp)) {
      tmp <- data.frame(orgName, datasetTmp[i,]$Organisations)
      colnames(tmp) <- c("Source", "Target")
      data[[i]] = tmp
    }
    
    tmp <- do.call(rbind, data)
    output <-data.frame(tmp)
    output<-aggregate(output$Source,
                      by=list(output$Source, output$Target),
                      FUN=length)
    output <-data.frame(output)
    output$Type <- "Undirected"
    colnames(output) <- c("Source", "Target", "Weight", "Type")
    output$Org <- orgName
  }
  
  return (output) 
}

func_BuildLink <- function(dataset) {
  #dataset <- Dataset_H2020Organizations
  #dataset <- head(dataset, n=5000)
  
  #list all organisation 
  listOrg <- unique(dataset$name)

  # For each of the organisations list with who it is working 
  for (i in 1:length(listOrg)) {
    dataOrg =func_BuildLinkForOneOrg(dataset, listOrg[i])
    
    fileName <- listOrg[i]
    fileName = gsub(' ','_', fileName)
    fileName = gsub('\\*','', fileName)
    fileName = gsub('/','', fileName)
    fileName = gsub('\'','', fileName)
    fileName = gsub('\"','', fileName)
    fileName = gsub(',','', fileName)
    fileName = gsub('&','', fileName)
    fileName = gsub(':','', fileName)
    
    name <- paste("../Datasets/outputOrgNetwork/", fileName, "_edge.csv", sep = "")
    options(scipen = 10)
    tryCatch(
      {
        write.table(dataOrg, name, sep = ";", quote = FALSE, row.names = FALSE)
      },
      error=function(cond){
#        message(paste("Error: Could not write edege file: ", name))
      }
    )
    options(scipen = 0)
    

    ListOrgWithLink <- c(as.character(dataOrg$Source), as.character(dataOrg$Target))
    ListOrgWithLink <- unique(ListOrgWithLink)
    if (length(ListOrgWithLink) == 0){
      ListOrgWithLink <- c(listOrg[i])
    }
      
    
    listNode <- subset(dataset, select=c(name, activityType))
    listNode  <- listNode[listNode$name %in% ListOrgWithLink,]
    data_t = data.table(listNode)
    
    setkeyv(data_t, c('name', 'activityType'))
    listNode = data_t[,Weight := .N, by = c("name", "activityType")]
    listNode <- unique(data.frame(listNode))    
    
    listNode$Id <- as.character(listNode$name)
    listNode$ActivityType <- as.character(listNode$activityType)
    listNode$NbProject <- as.character(listNode$Weight)
    listNode <- subset(listNode, select=c(Id, ActivityType, NbProject))
    listNode <- rbind(listNode, c("TEST", "TEST", 1))    

    name <- paste("../Datasets/outputOrgNetwork/", fileName, "_node.csv", sep = "")
    tryCatch(
      {
        write.table(listNode, name, sep = ";", quote = FALSE, row.names = FALSE)
      },
      error=function(cond){
        #message(paste("Error: Could not write node file: ", name))
      }
    )
    remove(listNode)
    
    if (i%%1000 == 0){
      print(paste(i, "on", length(listOrg), ":", Sys.time()))
    }
  }
  
  return ()
}

func_prepareDataset <- function (dataset) {
  dataset$name <- gsub("\"", "", dataset$name)
  dataset <- dataset[!duplicated(dataset[,c('name')]),]
  
  dataset <- merge(dataset, Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
  dataset <- subset(dataset, select=-c(projectReference, country))
  dataset <- plyr::rename(dataset, c("countryName" = "country"))
  abbreviation <- c("PRC", "HES", "REC", "PUB", "OTH")
  activity <- c("Private for-profit entities", "Education Establishments", "Research Organisations", "Public bodies", "Other")
  matching <- data.frame(abbreviation, activity, stringsAsFactors=FALSE)
  
  dataset <- merge(dataset, matching, by.x=c("activityType"), by.y=c("abbreviation"), all.x=TRUE)
  
  dataset <- subset(dataset, select=-c(activityType))
  
  return (dataset)
}


func_BuildOrganisationsJS <- function (dataset) {
  #dataset <- subset_H2020Organizations

  dataset <- func_prepareDataset(dataset)
  
  cat('var organisations = [ \n', file = "../Datasets/organisations.js")
  for (i in 1:nrow(dataset)) {
    cat('{ \n', file = "../Datasets/organisations.js", append = TRUE)
    
     for (j in 1:length(dataset)) {
         if ( j == length(dataset) && i != nrow(dataset) ) {
           cat('"', file = "../Datasets/organisations.js", append = TRUE)
           cat(colnames(dataset)[j], file = "../Datasets/organisations.js", append = TRUE)
           cat('": "', file = "../Datasets/organisations.js", append = TRUE)
           cat(dataset[i,j], file = "../Datasets/organisations.js", append = TRUE)
           cat('" \n }, \n', file = "../Datasets/organisations.js", append = TRUE)
         } else if ( j == length(dataset) && i == nrow(dataset) ) {
             cat('"', file = "../Datasets/organisations.js", append = TRUE)
             cat(colnames(dataset)[j], file = "../Datasets/organisations.js", append = TRUE)
             cat('": "', file = "../Datasets/organisations.js", append = TRUE)
             cat(dataset[i,j], file = "../Datasets/organisations.js", append = TRUE)
             cat('" \n } \n ];', file = "../Datasets/organisations.js", append = TRUE)
         } else {
           cat('"', file = "../Datasets/organisations.js", append = TRUE)
           cat(colnames(dataset)[j], file = "../Datasets/organisations.js", append = TRUE)
           cat('": "', file = "../Datasets/organisations.js", append = TRUE)
           cat(dataset[i,j], file = "../Datasets/organisations.js", append = TRUE)
           cat('", \n', file = "../Datasets/organisations.js", append = TRUE)
         }
     }
  }
}

# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

dir.create("../Datasets/outputOrgNetwork/", showWarnings = FALSE)

Sys.time()
func_BuildLink(subset_H2020Organizations)
func_BuildOrganisationsJS(subset_H2020Organizations)
Sys.time()


today <- Sys.Date()
today <- format(today, format="%d/%m/%Y")
fileConn<-file("../Datasets/UpdateOrgNetwork.txt")
write(today, fileConn)
close(fileConn)

