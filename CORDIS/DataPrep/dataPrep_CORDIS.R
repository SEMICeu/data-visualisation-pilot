
install.packages("./Library/optparse_1.3.2.zip",repos = NULL, type="source")
install.packages("./Library/plyr_1.8.4.zip",repos = NULL, type="source")
install.packages("./Library/data.table_1.10.4.zip",repos = NULL, type="source")

library(optparse)
library(plyr)
library(data.table)


option_list = list(
  make_option(c("-A", "--All"), type="integer", default=NULL, 
              help="update all visualisations, argument used to Update data until a specific year", metavar="YEAR"),
  
  make_option(c("-M", "--MotionChart"), type="character", default=NULL, 
              help="update Motion Chart visualisation, argument used to Update data until a specific year", metavar="YEAR"),
  
	make_option(c("-O", "--OrgNetwork"), default=NULL, action="store_true",
	            help="update Organisation Netword visualisation", metavar=NULL),
  
	make_option(c("-C", "--Chord"), default=NULL, action="store_true",
	            help="update chart visualisation", metavar=NULL),

  make_option(c("-U", "--Update"), type="logical", default=TRUE,
              help="update data", metavar=NULL)

);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (opt$h){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}


arg_UpdateMotionChart <- FALSE
arg_UpdateMotionChart_Year <- 2016
arg_UpdateOrgNetwork <- FALSE
arg_UpdateChord <- FALSE
arg_UpdateData <- opt$U

if (!is.null(opt$A)){
  arg_UpdateMotionChart <- TRUE
  arg_UpdateOrgNetwork <- TRUE
  arg_UpdateChord <- TRUE
  arg_UpdateMotionChart_Year <- opt$A
}

if (!is.null(opt$M)){
  arg_UpdateMotionChart <- TRUE
  arg_UpdateMotionChart_Year <- opt$M
  print("The data for the motion chart will be updated ")
}

if (!is.null(opt$C)){
  arg_UpdateChord <- TRUE
  print("The data for the chord diagram will be updated ")
}

if (!is.null(opt$O)){
  arg_UpdateOrgNetwork <- TRUE
  print("The data for the organisations network will be updated ")
}

source("dataPrep_loadDatasets.R")

if ( arg_UpdateMotionChart) {
  print("The data for the motion chart are about to be updated ")
  source("dataPrep_motionChart.R")
  print("The data for the motion chart have been updated ")
}

if ( arg_UpdateChord) {
  print("The data for the chord diagram are about to be updated ")
  source("dataPrep_chord.R")
  print("The data for the chord diagram have been updated ")
}


if ( arg_UpdateOrgNetwork) {
  print("The data for the organisation network are about to be updated ")
  source("dataPrep_orgNetwork.R")
  print("The data for the organisation network have been updated ")
}

