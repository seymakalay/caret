#rm(list=ls())
rm(list=ls())
library(cluster);library(caret);library(shiny);library(e1071) #;library(shinymanager) ;library(fpc); 
library(stats);library(readr)
library(ggplot2)#;library(labeling);
library(DT)#;library(tools)
library(stringr);library(rpivotTable) #;library(readr);library(pROC)
#library(leaflet); library(leaflet.extras)
library(dplyr); library(readxl)#;library(sp);library(shp2graph);library(AppliedPredictiveModeling)
#library(htmltools); library(htmlwidgets); library(raster); library(httr)  ;library(scales)
#library(shinyBS);library(shinydashboard);library(shinyjs); library(curl)

#set_config(use_proxy(url="10.3.100.207",port=8080))

#setwd("C:/Users/seyma/Docs/shinny/caret/caret")
set.seed(87)
ClustereData <- read.csv("try.this.ClustereData.csv",header = T, sep = ",")
#ClustereData <- read_excel("ClustereData.xlsx")

ClustereData <- as.data.frame(ClustereData )

ClustedMean <- read_excel("ClustedMean.xlsx")
clustedmean1row <- read_excel("df.xlsx")

'ClustereData$INDICE <- as.numeric(ClustereData$INDICE)
ClustereData$index.oninvestment <- as.numeric(ClustereData$index.oninvestment)
ClustereData$index.on.prodection <- as.numeric(ClustereData$index.on.prodection)
'






#ClustereData <- na.omit(ClustereData); names(ClustereData)
#attach(ClustereData)

'c_I1I2Indice <- ClustereData[,c("INDICE", "index.oninvestment","index.on.prodection")]
names(c_I1I2Indice)'



#pal <- colorFactor(palette = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2"),
#                   levels = c( "1st.C", "2nd.C", "3rd.C","4rd.C","5th.C","6th.C", "7th.C","8th.C", "9th.C", "10th.C"))

'segmentationOriginal <- read.csv("segmentationOriginal.csv",header = T, sep = ",")

setupData <- function(trainpercent, randomseed)
{
  # Make sure computations can be reproducible.
  set.seed(randomseed)
  
  #segmentationOriginal <- read.csv("segmentationOriginal.csv",header = T, sep = ",")
  # Cell segmenation data
  #data(segmentationOriginal)

  #segmentationOriginal <- read.csv("segmentationOriginal.csv",header = T, sep = ",")
  
  # Use Case to divide into raw training and test and final Test sets, as
  # defined in original data.
  # Drop "Cell" ID and Case from original data.frame.
  rawTrain  <- segmentationOriginal[segmentationOriginal$Case == "Train",][,-1:-2]
  finalTest <- segmentationOriginal[segmentationOriginal$Case == "Test",][,-1:-2]
  
  # Remove near-zero variance variables from rawTrain.
  # Use freqCut=2 to get rid of mostly skewed variables with few unqiue values.
  nzv <- nearZeroVar(rawTrain, freqCut=2, saveMetrics=TRUE)
  count.nzv <- sum(nzv$nzv)
  count.nzv
  if (count.nzv > 0)
  {
    rawTrain  <- rawTrain[, !nzv$nzv]
    finalTest <- finalTest[, !nzv$nzv]
  }
  
  # Remove variables with high correlation
  HIGH.CORRELATION.CUTOFF <- 0.90
  cor.matrix <- cor(rawTrain[,-1])
  cor.high   <- findCorrelation(cor.matrix, HIGH.CORRELATION.CUTOFF)
  
  high.corr.remove <- row.names(cor.matrix)[cor.high]
  high.corr.remove
  
  rawTrain <- rawTrain[,  -cor.high]
  finalTest <- finalTest[, -cor.high]
  
  # Partition raw training data into a training and validation set.
  inTrainSet <- createDataPartition(y=rawTrain$Class, p=trainpercent, list=FALSE)
  training <- rawTrain[inTrainSet,]
  
  validation <- rawTrain[-inTrainSet,]
  invisible( list(training=training, validation=validation, finalTest=finalTest) )
}

# Train learning model.  Apply to out-of-sample test data to compute
# confusion matrix and related data.
generic.fit <- function(method, trainingData, validationData, centerscale)
{
  preprocess.methods <- NULL
  if (centerscale) preprocess.methods = c("center", "scale")
  
  fit <- train(Class ~ ., data = trainingData,
               preProcess = preprocess.methods, method = method)
  
  OutOfSample  <- predict(fit, newdata = validationData)
  confusion <- confusionMatrix(validationData$Class, OutOfSample)
  my.resample <-  c( accurarcy = (mean(fit$resample[[1]])),  #fit$resample
                     kappa = (mean(fit$resample[[2]])))                                    
  
  invisible( list(fit = fit, confusion = confusion,  my.resample = my.resample ) )
}

# Setup data and use specified caret learning method.
responseRoutine <- function(method, centerscale, trainpercent, randomseed)
{
  d <- setupData(trainpercent, randomseed)
  fit <- generic.fit(method, d$training, d$validation, centerscale)
  invisible(fit)
}
'
########################################################
########################################################

'
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
  user = c("1", "fanny", "victor", "benoit"),
  password = c("1", "azerty", "12345", "azerty"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)
'


HELP.HTML <- paste(
  
  'Machine Learning</i></a> class.<br>',
  '<hr> <b>Concept</b>:&nbsp; Use interactive interface to select',
  'machine learning parameters for the <a',
  'href="http://caret.r-forge.r-project.org/">caret package</a> to',
  'solve a problem involving image segmentation.&nbsp; This interactive',
  'interface may provide insights on best machine learning approach for',
  'a given problem.&nbsp; Future:&nbsp; perhaps with small set of',
  'metadata, the approach could be used with a variety of problems and',
  'datasets.<br><br>',
  'To see data:  library(AppliedPredictiveModeling) and then',
  'data(segmentationOriginal).<br><br>',
  'Some variables with near-zero variance and/or high correlation to',
  'other variables have been removed.',
  '<hr> <b>Inputs</b>:&nbsp; At the left, select various preprocessing',
  'options and caret method.<br>',
  '<br>',
  '<b>Fit tab</b>:&nbsp; Output from caret train function applied to',
  'subset of original training data.<br>',
  '<br>',
  '<b>ConfusionMatrix tab</b>:&nbsp; Confusion matrix, sensitivity,',
  'specificity and related data from caret confusionMatrix function',
  'applied to an out-of-sample validation dataset. Use to compare',
  'models.&nbsp; [A final test set is reserved and is not used in the',
  'current implementation.]<br>',
  '<br>',
  '<b>DotPlot tab</b>:&nbsp; dotPlot using output from caret varImp',
  'function that gives importance rankings of variables included in the',
  'fit.<br>',
  '<hr> <b>Reference</b>:&nbsp; Andrew A Hill, Peter LaPan, Yizheng Li',
  'and Steve Haney.&nbsp; <a',
  'href="http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2080643/">Impact',
  'of image segmentation on high-content screening data quality for',
  'SK-BR-3 cells</a>, <i>BMC Bioinformatics</i>, Sept. 2007.')


