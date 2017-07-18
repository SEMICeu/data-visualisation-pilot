#RWeka does not work in JAVA_HOME environment variable is set, empty it for this R session
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")


install.packages("./Library/getopt_1.20.0.zip",repos = NULL, type="source")
install.packages("./Library/slam_0.1-40.zip",repos = NULL, type="source")
install.packages("./Library/Rcpp_0.12.12.zip",repos = NULL, type="source")
install.packages("./Library/rJava_0.9-8.zip",repos = NULL, type="source")
install.packages("./Library/NLP_0.1-10.zip",repos = NULL, type="source")
install.packages("./Library/RWekajars_3.9.1-3.zip",repos = NULL, type="source")
install.packages("./Library/RWeka_0.4-34.zip",repos = NULL, type="source")
install.packages("./Library/XML_3.98-1.9.zip",repos = NULL, type="source")
install.packages("./Library/tm_0.7-1.zip",repos = NULL, type="source")
install.packages("./Library/RJSONIO_1.3-0.zip",repos = NULL, type="source")
install.packages("./Library/plyr_1.8.4.zip",repos = NULL, type="source")
install.packages("./Library/optparse_1.3.2.zip",repos = NULL, type="source")

library(RWeka)
library(XML)
library(tm)
library(RJSONIO)
library(plyr)
library(optparse)
library(tools)

option_list = list(
  make_option(c("-U", "--Update"), type="logical", default=TRUE,
              help="update data", metavar=NULL)
);
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
arg_UpdateData <- opt$U

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATA ################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
if (arg_UpdateData) {
  download.file("http://ec.europa.eu/transparencyregister/public/consultation/statistics.do?action=getLobbyistsXml&fileType=NEW", destfile = "../Datasets/full_export_new.xml")
}
  
cat("downloading the transparency data \n")
xmlfile <-  xmlTreeParse("../Datasets/full_export_new.xml", useInternalNodes = TRUE)
countries = read.csv("../Datasets/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. TRANSFORM DATA FROM XML TO DATA.FRAME ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("Extract the XML \n")

func_parseXMLNodeInterestRepresentative <- function(xmlNode){
  # xmlNode <- rootNode[["resultList"]][i][[1]]
  country <- xmlValue(xmlNode[["contactDetails"]][["country"]])
  cat <- xmlValue(xmlNode[["category"]][["mainCategory"]])
  subCat <- xmlValue(xmlNode[["category"]][["subCategory"]])
  goal <-   xmlValue(xmlNode[["goals"]])
  activity <- xmlValue(xmlNode[["activities"]][["activityEuLegislative"]])
  listInterest <-xmlSApply(xmlNode[["interests"]], function(x) xmlSApply(x, xmlValue))
  inter <- paste(listInterest, collapse=";")
  output <- data.frame(countries = character(),
                       categories = character(),
                       subCategories = character(), 
                       goals = character(),
                       activities = character(),
                       interest = character())
  output<-rbind(output, data.frame(countries = country,
                                   categories = cat, 
                                   subCategories = subCat, 
                                   goals = goal, 
                                   activities = activity,
                                   interest = inter)) 
  return(output)
}

rootNode = xmlRoot(xmlfile)
data <- list()
for (i in 1:xmlSize(rootNode[["resultList"]])){
  data[[i]] =func_parseXMLNodeInterestRepresentative(rootNode[["resultList"]][i][[1]])
}

tmp <- do.call(rbind, data)
dataset <- data.frame(tmp)

#remove useless variables 
remove(i)
remove(data)
remove(rootNode)
remove(xmlfile)
remove(tmp)
remove(func_parseXMLNodeInterestRepresentative)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR MAP ###############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("Prepare data for the map \n")
func_BuildDatasetForMapBasedOnFilterCategories <- function(data, filterValue){
  data <- data[data$categories == filterValue, ]
  output <-aggregate(data$countries, 
                     by=list(data$countries),
                     FUN=length, simplify = FALSE)
  output <- plyr::rename(output,c("Group.1"="Country",
                                  "x" = as.character(filterValue)))
  return (output)
}

func_BuildDatasetForMapBasedOnFilterSubCategories <- function(data, filterValue){
  data <- data[data$subCategories == filterValue, ]
  output <-aggregate(data$countries, 
                     by=list(data$countries),
                     FUN=length, simplify = FALSE)
  output <- plyr::rename(output,c("Group.1"="Country",
                                  "x" = as.character(filterValue)))
  return (output)
}


listCategory <- unique(dataset$categories)
listSubCategory <- unique(dataset$subCategories)


datasetMap <-aggregate(dataset$countries, 
                       by=list(dataset$countries),
                       FUN=length, simplify = FALSE)
datasetMap <- plyr::rename(datasetMap,c("Group.1"="Country",
                                        "x" = "all"))

data <- list()
i<-1

for (j in 1:length(listCategory)){
  data[[i]] = func_BuildDatasetForMapBasedOnFilterCategories(dataset, listCategory[j])
  i<- i+1
}

for (j in 1:length(listSubCategory)){
  data[[i]] = func_BuildDatasetForMapBasedOnFilterSubCategories(dataset, listSubCategory[j])
  i<- i+1
}


for (i in 1:length(data)){
  datasetMap <- merge(datasetMap, data[i], by.x=c("Country"), by.y=c("Country"), all.x=TRUE)
}
datasetMap <- subset(datasetMap, select=-c(all))
datasetMap[is.na(datasetMap)] <- 0


countries$name <- toupper(countries$name)
countries <- subset (countries, select =c(name, EU28))
datasetMap <- merge(datasetMap, countries, by.x=c("Country"), by.y=c("name"), all.x=TRUE)
datasetMap <- datasetMap[datasetMap$EU28 == TRUE,]
datasetMap <- datasetMap[!is.na(datasetMap$Country),]


datasetMap <- data.frame(lapply(datasetMap, as.character), stringsAsFactors=FALSE)

countryList <- as.data.frame(datasetMap[,1])
countryList <- plyr::rename(countryList,c("datasetMap[, 1]"="Country"))
datasetMap <- subset(datasetMap, select=-c(EU28))
datasetMap$Country <- tolower(datasetMap$Country)
datasetMap$Country <- toTitleCase(datasetMap$Country)

#write info in file
writeData <- function(data) {
  for (i in 1:(nrow(datasetMap)-1)) {
    
    cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(datasetMap$Country[i], file = "../Datasets/datasetMap.js", append = TRUE)
    cat('": { \n', file = "../Datasets/datasetMap.js", append = TRUE)
    
    for (j in 2:(length(datasetMap)-1)) {
      cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
      cat(colnames(data)[j], file = "../Datasets/datasetMap.js", append = TRUE)
      cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
      cat(data[i,j], file = "../Datasets/datasetMap.js", append = TRUE)
      cat(", \n", file = "../Datasets/datasetMap.js", append = TRUE)
    }
    cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(colnames(data)[length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
    cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(data[i, length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
    cat("}, \n", file = "../Datasets/datasetMap.js", append = TRUE)
    
  }
  cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
  cat(datasetMap$Country[nrow(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
  cat('": { \n', file = "../Datasets/datasetMap.js", append = TRUE)
  
  for (j in 2:(length(datasetMap)-1)) {
    cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(colnames(data)[j], file = "../Datasets/datasetMap.js", append = TRUE)
    cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
    cat(data[nrow(datasetMap), j], file = "../Datasets/datasetMap.js", append = TRUE)
    cat(", \n", file = "../Datasets/datasetMap.js", append = TRUE)
  }
  cat('"', file = "../Datasets/datasetMap.js", append = TRUE)
  cat(colnames(data)[length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
  cat('": ', file = "../Datasets/datasetMap.js", append = TRUE)
  cat(data[nrow(datasetMap),length(datasetMap)], file = "../Datasets/datasetMap.js", append = TRUE)
  cat("} \n };", file = "../Datasets/datasetMap.js", append = TRUE)
  
}

cat('var categoriesData = { \n', file = "../Datasets/datasetMap.js")
writeData(datasetMap)

remove(data)
remove(i)
remove(j)
remove(listCategory)
remove(listSubCategory)
remove(countryList)
remove(datasetMap)
remove(func_BuildDatasetForMapBasedOnFilterCategories)
remove(func_BuildDatasetForMapBasedOnFilterSubCategories)
remove(writeData)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. BUILD DATA.FRAME FOR BUBBLE #############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

cat("Prepare data for the bubble chart \n")

listInterest <- paste(dataset$interest, collapse = ';')
listInterest <- gsub(";;",";", listInterest)

listInterest <- strsplit(listInterest, ";")[[1]]
listInterest <- as.data.frame(listInterest)
listInterest <- 
  aggregate(listInterest$listInterest, 
            by=list(listInterest$listInterest), 
            FUN=length)
listInterest <- plyr::rename(listInterest,c("Group.1"="name", 
                                            "x"="count"))
listInterest$name <- as.character(listInterest$name)
listInterest[listInterest$name=="Agriculture and Rural Development",]$name <- "Agriculture"
listInterest[listInterest$name=="Financial Stability, Financial Services and Capital Markets Union ",]$name <- "Finance"
listInterest[listInterest$name=="Fisheries and Aquaculture",]$name <- "Fisheries" 
listInterest[listInterest$name=="Foreign and Security Policy and Defence",]$name <- "Security Policy"
listInterest[listInterest$name=="General and Institutional Affairs",]$name <- "General Affairs"
listInterest[listInterest$name=="Justice and Fundamental Rights",]$name <- "Justice"


write.table(listInterest, "../Datasets/BubbleChartData.txt", sep = ",", quote = FALSE, row.names = FALSE)

remove(listInterest)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. BUILD DATA.FRAME FOR WORDCLOUD ###########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

cat("Prepare data for the wordcloud \n")

func_CleanString <- function(data){
  docs <- VCorpus(VectorSource(data) )
  docs <- tm_map(docs, removePunctuation)   
  docs <- tm_map(docs, removeNumbers)  
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, removeWords, c(stopwords("english"), 
                                      stopwords("danish"),
                                      stopwords("dutch"), 
                                      stopwords("finnish"), 
                                      stopwords("french"), 
                                      stopwords("german"), 
                                      stopwords("hungarian"), 
                                      stopwords("italian"),
                                      stopwords("norwegian"), 
                                      stopwords("portuguese"), 
                                      stopwords("spanish"), 
                                      stopwords("norwegian"), 
                                      stopwords("swedish")))
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)
}

# aggregate all goals and activities 
listActivity <- paste(dataset$activities, collapse = ' ')

# remove punctuation, number, put everything to lower, remove stop words and white space
listActivity <- func_CleanString(listActivity)

# # create document-term matrix (standard with frequency)
# dtm_activity <- DocumentTermMatrix(listActivity, control=list(wordLengths=c(3,Inf)))

# create two-gram document-term matrix (standard with frequency)
TwogramTokenizer <- function(x) NGramTokenizer(x,
                                              Weka_control(min = 2, max = 2))
dtm_activity_twogram <- DocumentTermMatrix(listActivity, control = list(tokenize = TwogramTokenizer))

# Generate list of 100 most frequent words
freq_activity_twogram <- head(sort(colSums(as.matrix(dtm_activity_twogram)), decreasing=TRUE),100)

#Multiply frequencies for low frequency files to enlarge visualisation
freq_activity_twogram_multiplied <- freq_activity_twogram * 8


# Build JSON

# JSON_activity_twogram <- toJSON(freq_activity_twogram)
JSON_activity_twogram_multiplied <- toJSON(freq_activity_twogram_multiplied)


#write info in file
cat('var data = { \n', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js")
cat('wordCountMapping_activity_two: \n', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat(JSON_activity_twogram_multiplied, file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat(',
wordsList_activity_two:
[', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
for (i in 1:(length(names(freq_activity_twogram_multiplied))-1)) {
  cat('"',  file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
  cat(names(freq_activity_twogram_multiplied)[i], file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
  cat('", \n', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
}
cat('"',  file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat(names(freq_activity_twogram_multiplied)[length(names(freq_activity_twogram_multiplied))], file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)
cat('"
    ]
}', file = "../Datasets/wordCountMapping_activity_twogram_multiplied_100.js", append = TRUE)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. BUILD DATA.FRAME FOR LINECHART ##########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

cat("Prepare data for the linechart \n")

# function to calculate the number of organisations of a certain category (filterValue)
func_aggregateByCategory <- function(data, filterValue){
  # filter the data to only contain the selected category (filterValue)
  data <- data[data$short == filterValue, ]
  # sum every occurence
  output <-aggregate(data, 
                     by=list(data),
                     FUN=length, simplify = FALSE)
  # select the sum and return it
  output$name <- output$x$`1`
  return (output$name)
}

# read current data
dataLinechart <- read.csv("../Datasets/Data_linechart.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")

# rename categories in dataframe and convert to chr
short <- c("Trade/business/professional associations", "NGO", "Consultancies, law firms and self-employed", 
           "Think tanks, research and academic institutions", "Organisations representing sub national authorities and other",
           "Organisations representing churches and religious communities")
long <- c("II - In-house lobbyists and trade/business/professional associations", "III - Non-governmental organisations", 
          "I - Professional consultancies/law firms/self-employed consultants", "IV - Think tanks, research and academic institutions",
          "VI - Organisations representing local, regional and municipal authorities, other public or mixed entities, etc.",
          "V - Organisations representing churches and religious communities")
matching <- data.frame(short, long, stringsAsFactors = FALSE)
newData <- merge(dataset, matching, by.x=c("categories"), by.y=c("long"), all.x=TRUE)

# take categories data and list all categories
newData <- subset(newData, select=c(short))
listCategory <- unique(newData$short)

# initialize dataframe and fill in current date
data <- data.frame("Total"=0, "date"=format(Sys.Date(), format="%Y%m%d"))

# calculate the number of organisations for every category
for (j in 1:length(listCategory)){
  data[,listCategory[j]] = func_aggregateByCategory(newData, listCategory[j])
}

# calculate the total
data$Total = (data$`Consultancies, law firms and self-employed` 
            + data$`Trade/business/professional associations` 
            + data$NGO
            + data$`Think tanks, research and academic institutions` 
            + data$`Organisations representing churches and religious communities`
            + data$`Organisations representing sub national authorities and other`)

# rename columns of file that was read in
dataLinechart <- plyr::rename(dataLinechart, c("Trade.business.professional.associations" = "Trade/business/professional associations",
                                               "Consultancies..law.firms.and.self.employed" = "Consultancies, law firms and self-employed",
                                               "Think.tanks..research.and.academic.institutions" = "Think tanks, research and academic institutions",
                                               "Organisations.representing.churches.and.religious.communities" = "Organisations representing churches and religious communities",
                                               "Organisations.representing.sub.national.authorities.and.other" = "Organisations representing sub national authorities and other"))
# append the new data to the end of the file
dataLinechart <- rbind(dataLinechart, data)

# write the new file
write.table(dataLinechart, "../Datasets/Data_linechart.csv", sep = ",", quote = TRUE, row.names = FALSE)

# remove variables
rm(matching, long, short, listCategory, j, newData, data, dataLinechart)

