# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATA ##########################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
# based on which scripts will be executed, different data is needed/loaded
if (arg_UpdateChord || arg_UpdateOrgNetwork) {
  cat("loading countries data for chord diagram \n")
  # countries
  Dataset_Countries = read.csv("../Datasets/inputData/countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  listCountries <- sort(Dataset_Countries$euCode)
}

if ( arg_UpdateMotionChart ) {
  cat("loading countries data for motionchart \n")
  # countries
  Dataset_Countries = read.csv("../Datasets/inputData/countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  #select only EU-28 countries 
  Dataset_Countries <- Dataset_Countries[Dataset_Countries$EU28 == TRUE,]
  
  
  # FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
  # FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
  # H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
  
  if (arg_UpdateData) {
    cat("loading CORDIS data for motionchart \n")
    download.file("http://cordis.europa.eu/data/cordis-fp6projects.csv", destfile = "../Datasets/inputData/cordis-fp6Projects.csv")
    download.file("http://cordis.europa.eu/data/cordis-fp7projects.csv", destfile = "../Datasets/inputData/cordis-fp7Projects.csv")
    download.file("http://cordis.europa.eu/data/cordis-h2020projects.csv", destfile = "../Datasets/inputData/cordis-h2020Projects.csv")
  }
  
  Dataset_FP6Projects = read.csv("../Datasets/inputData/cordis-fp6Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  Dataset_FP7Projects = read.csv("../Datasets/inputData/cordis-fp7Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  Dataset_H2020Projects = read.csv("../Datasets/inputData/cordis-h2020Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  
  #combine the 3 datasets
  cat("combining CORDIS data for motionchart \n")
  Dataset_FP7Projects$startDate <- gsub('/', '-', Dataset_FP7Projects$startDate)
  Dataset_FP7Projects$endDate <- gsub('/', '-', Dataset_FP7Projects$endDate)
  Dataset_FP6Projects[1] <- NULL 
  Dataset_FP7Projects[1] <- NULL 
  Dataset_H2020Projects[1] <- NULL 
  Dataset_FP6Projects$startDate <- as.Date(as.POSIXlt(as.character(Dataset_FP6Projects$startDate), format="%Y-%m-%d"))
  Dataset_FP7Projects$startDate <- as.Date(as.POSIXlt(as.character(Dataset_FP7Projects$startDate), format="%d-%m-%Y"))
  Dataset_H2020Projects$startDate <- as.Date(as.POSIXlt(as.character(Dataset_H2020Projects$startDate), format="%Y-%m-%d"))
  
  names(Dataset_FP7Projects)[names(Dataset_FP7Projects)=="id"] <- "reference"
  names(Dataset_H2020Projects)[names(Dataset_H2020Projects)=="id"] <- "reference"
  Dataset_Projects <- rbind(rbind(Dataset_FP6Projects, Dataset_FP7Projects), Dataset_H2020Projects)

  cat("loading eurostat data for motionchart \n")
  # Population: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en : demo_pjan_1_Data.csv
  # GDP: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=naida_10_gdp&lang=en : naida_10_gdp_1_Data.csv
  # Population by educational attainment level (%) - main indicators (edat_lfse_03): http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=edat_lfse_03&lang=en - edat_lfse_03_1_Data.csv 
  # Total intramural R&D expenditure (GERD) by sectors of performance: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=rd_e_gerdtot&lang=en : rd_e_gerdtot_1_Data.csv

  cat("installing functionalities needed for the automatic download of eurostat data \n")   
  #install packages needed for downloading and transforming data from eurostat
  install.packages("eurostat",repos = "http://cran.us.r-project.org")
  install.packages("gdata",repos = "http://cran.us.r-project.org")
  library(eurostat)
  library(gdata)
  
  #function for transforming eurostat data
  transformEurostatDataset = function(dataset) {
    dataset = data.frame(dataset)
    dataset = dataset[dataset$geo %in% c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR",
                               "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT",
                               "RO", "SI", "SK", "FI", "SE", "UK", "IS", "LI", "NO", "CH", "ME",
                               "MK", "AL", "RS", "TR", "AD", "BY", "BA", "XK", "MD", "MC", "RU",
                               "SM", "UA", "AM", "AZ", "GE"),]
    dataset <- drop.levels(dataset)
    dataset$time = as.numeric(substring(as.character(dataset$time), 0, 4))
    dataset = dataset[dataset$time >= 2003, ]
  }
  
  cat("downloading the eurostat data \n")
  #download eurostat data
  
  Dataset_population = get_eurostat("demo_pjan")
  Dataset_GDP = get_eurostat("naida_10_gdp")
  Dataset_PopByEducAttain = get_eurostat("edat_lfse_03")
  Dataset_RDExpend = get_eurostat("rd_e_gerdtot")
  
  write.table(Dataset_population, "../Datasets/inputData/Dataset_population.csv", sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(Dataset_GDP, "../Datasets/inputData/Dataset_GDP.csv", sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(Dataset_PopByEducAttain, "../Datasets/inputData/Dataset_PopByEducAttain.csv", sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(Dataset_RDExpend, "../Datasets/inputData/Dataset_RDExpend.csv", sep = "\t", quote = FALSE, row.names = FALSE)
  
  #transform eurostat data
  Dataset_population = transformEurostatDataset(Dataset_population)
  Dataset_GDP = transformEurostatDataset(Dataset_GDP)
  Dataset_PopByEducAttain = transformEurostatDataset(Dataset_PopByEducAttain)
  Dataset_RDExpend = transformEurostatDataset(Dataset_RDExpend)
  
  #Eurostat data prepration 
  Dataset_GDP <- Dataset_GDP[Dataset_GDP$na_item == "B1GQ",]
  Dataset_GDP <- Dataset_GDP[Dataset_GDP$unit == "CP_MEUR",]
  Dataset_GDP <- subset(Dataset_GDP, select=-c(unit, na_item))
  Dataset_GDP <- plyr::rename(Dataset_GDP,c("values"="GDP"))
  
  
  Dataset_population <- Dataset_population[Dataset_population$age == "TOTAL",]
  Dataset_population <- Dataset_population[Dataset_population$sex == "T",]
  Dataset_population <- subset(Dataset_population, select=-c(age, sex, unit))
  Dataset_population <- plyr::rename(Dataset_population,c("values"="Population"))
  
  Dataset_PopByEducAttain <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$age =="Y15-64",]
  Dataset_PopByEducAttain <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$sex =="T",]
  Dataset_PopByEducAttain <- subset(Dataset_PopByEducAttain, select=-c(sex, age, unit))
  
  
  Dataset_RDExpend <- Dataset_RDExpend[Dataset_RDExpend$unit =="MIO_EUR",]
  Dataset_RDExpend <- subset(Dataset_RDExpend, select=-c(unit))
  Dataset_RDExpend$values <- sub(' ', '',Dataset_RDExpend$values)
  Dataset_RDExpend$values <- as.numeric(Dataset_RDExpend$values)
  
  
}

if ( arg_UpdateChord || arg_UpdateMotionChart ) {
  cat("loading CORDIS data used for the chord and motionchart. This will download if either one of them is being updated \n")
  # FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
  # FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
  if (arg_UpdateData) {
    download.file("http://cordis.europa.eu/data/cordis-fp6organizations.csv", destfile = "../Datasets/inputData/cordis-fp6organizations.csv")
    download.file("http://cordis.europa.eu/data/cordis-fp7organizations.csv", destfile = "../Datasets/inputData/cordis-fp7organizations.csv")
  }
  Dataset_FP6Organizations = read.csv("../Datasets/inputData/cordis-fp6organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  Dataset_FP7Organizations = read.csv("../Datasets/inputData/cordis-fp7organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  names(Dataset_FP7Organizations)[names(Dataset_FP7Organizations)=="projectID"] <- "projectReference"

}

if ( arg_UpdateChord || arg_UpdateMotionChart || arg_UpdateOrgNetwork ) {
  cat("downloading CORDIS data used for the chord and organisations network. This will download if either one of them is being updated \n")
  # H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
  if (arg_UpdateData) {
    download.file("http://cordis.europa.eu/data/cordis-h2020organizations.csv", destfile = "../Datasets/inputData/cordis-h2020organizations.csv")
  }
  Dataset_H2020Organizations = read.csv("../Datasets/inputData/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
  names(Dataset_H2020Organizations)[names(Dataset_H2020Organizations)=="projectID"] <- "projectReference"
}