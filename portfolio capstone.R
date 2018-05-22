library(quantmod)
library(xts)

start <- as.Date("2016-01-01")
end <- Sys.Date()
# Please, note that the end date is used dynamically, the results obtained in the end can differ, if 
# executed in another day.


pool <- getSymbols(Symbols = c( "GE", "AAPL", "PFE", "MSFT", "NFLX", "C", "DIS", "GOOG", "BAC", "FB"), src = "yahoo", 
                   from = start, to = end)

portfolio <- 0.35 * AAPL + 0.3 * GOOG + 0.15 * BAC + 0.1 * MSFT + 0.1 * FB 
colnames(portfolio) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
head(portfolio)
# have the portfolio
# have the pool
# figure out the assets and weights


portClose <- portfolio[,4]
colnames(portClose) <- "Close"
head(AAPL)
plot(AAPL[,"AAPL.Close"])

assetList <- list(GE[, "GE.Close"], AAPL[,"AAPL.Close"], PFE[, "PFE.Close"], MSFT[,"MSFT.Close"], NFLX[,"NFLX.Close"], C[,"C.Close"],
                  DIS[,"DIS.Close"], GOOG[,"GOOG.Close"], BAC[,"BAC.Close"], FB[,"FB.Close"])
length(assetList)
assets <- do.call("cbind", assetList)
head(assets)
headAssets <- head(assets, h = 5)

# the same results
library(gtools)
comb <- combinations(n=10,r=5,v=colnames(headAssets), set = TRUE, repeats = FALSE)
combData <- as.data.frame(comb)
#View(combData)


library(dplyr)
library(tidyr)

colnames(combData)
combDataSep <- combData %>% separate(V1, into = c("Asset1", "Close"), sep = "\\.")
combDataSep <- combDataSep %>% separate(V2, into = c("Asset2", "Close"), sep = "\\.")
combDataSep <- combDataSep %>% separate(V3, into = c("Asset3", "Close"), sep = "\\.")
combDataSep <- combDataSep %>% separate(V4, into = c("Asset4", "Close"), sep = "\\.")
combDataSep <- combDataSep %>% separate(V5, into = c("Asset5", "Close"), sep = "\\.")
combDataSep[,"Close"] <- NULL
combDataSep <- combDataSep[, c("Asset1", "Asset2", "Asset3", "Asset4", "Asset5")]
head(combDataSep)

# Construct Regression having the names in the data
colnames(assets) <- c("GE", "AAPL", "PFE", "MSFT", "NFLX", "C", "DIS", "GOOG", "BAC", "FB")
head(assets)
assets[,as.character(combDataSep[1,])] # select the wanted assets from the cobination

#dataAssetsCombList[[1]] <- data.frame(assets[,as.character(combDataSep[1,])])
#dataAssetsCombList[[2]] <- data.frame(assets[,as.character(combDataSep[2,])])

dataAssetsCombList <- list()
head(portfolio)
portfolioClose <- portfolio[,"Close"]

for(i in 1:nrow(combDataSep)){
  dataAssetsCombList[[i]] <- data.frame(assets[,as.character(combDataSep[i,])])
  dataAssetsCombList[[i]]$Close <- portfolioClose
}

str(dataAssetsCombList)
head(dataAssetsCombList) # combination list of the assets

library(plyr)

# regression on a component of the list 
dataAssetsCombList[[1]]$Close <- portfolioClose
lm(data = dataAssetsCombList[[1]], formula = Close ~ AAPL+BAC+C+DIS+FB)

# try on the overall combination
results <- list()
for(i in 1:length(dataAssetsCombList)){
  results[i] <- dlply(dataAssetsCombList[[i]], .variables = NULL, .fun = lm, formula = dataAssetsCombList[[i]][,6] ~ dataAssetsCombList[[i]][,1] + 
                     dataAssetsCombList[[i]][,2] + dataAssetsCombList[[i]][,3] + dataAssetsCombList[[i]][,4] + 
                     dataAssetsCombList[[i]][,5])
}

# get the coefficients 
coeffData <- list()
for(i in 1:length(results)){
  coeffData[[i]] <- as.data.frame(coef(results[[i]]))
  rownames(coeffData[[i]])[2:6] <- colnames(dataAssetsCombList[[i]])[1:5]
}

# obtained coefficients data and example to picture on a component case
coeffData[[1]]
head(dataAssetsCombList[[1]])

port1 <- coeffData[[1]][1,] + coeffData[[1]][2,] * dataAssetsCombList[[1]][1,1] + coeffData[[1]][2,] * dataAssetsCombList[[1]][1,2] + 
  coeffData[[1]][3,] * dataAssetsCombList[[1]][1,3] + coeffData[[1]][4,] * dataAssetsCombList[[1]][1,4] + 
  coeffData[[1]][5,] * dataAssetsCombList[[1]][1,5]
head(portfolioClose)

# Compute errors
head(dataAssetsCombList)
errorList <- list()

for(i in 1:length(coeffData)){
  errorList[[i]] <- sum((dataAssetsCombList[[i]][,6] - (coeffData[[i]][1,] + coeffData[[i]][2,] * dataAssetsCombList[[i]][,1] + 
                                                      coeffData[[i]][2,] * dataAssetsCombList[[i]][,2] + 
                                                      coeffData[[i]][3,] * dataAssetsCombList[[i]][,3] + 
                                                      coeffData[[i]][4,] * dataAssetsCombList[[i]][,4] + 
                                                      coeffData[[i]][5,] * dataAssetsCombList[[i]][,5]))^2) 
}


# If wanted account for specific cases #
# errorList[[i]] <- (dataAssetsCombList[[i]][1,6] - (coeffData[[i]][1,] + coeffData[[i]][2,] * dataAssetsCombList[[i]][1,1] + 
#                                                      coeffData[[i]][2,] * dataAssetsCombList[[i]][1,2] + 
#                                                      coeffData[[i]][3,] * dataAssetsCombList[[i]][1,3] + 
#                                                      coeffData[[i]][4,] * dataAssetsCombList[[i]][1,4] + 
#                                                      coeffData[[i]][5,] * dataAssetsCombList[[i]][1,5]))^2 

errorList
# make a data of errors
dataError <- data.frame(errorList[[1]])
for(i in 2:length(errorList)){
  dataError <- rbind(dataError, errorList[[i]])
}


colnames(dataError) <- "Error"
dataError$ID <- 1:nrow(dataError)
#View(dataError)

dataErrorSpecific <- dataError[dataError$Error < 198617.31 ,] # 1, 21
dataErrorSpecific

head(dataAssetsCombList[[172]]) # BAC DIS GE GOOG MSFT 
head(dataAssetsCombList[[193]]) # BAC GE GOOG MSFT PFE

plot(portfolio$Close)

for(i in 1:nrow(dataAssetsCombList[[146]])){
  dataAssetsCombList[[172]][i,7] <- coeffData[[172]][1,] + 
    coeffData[[172]][2,] * dataAssetsCombList[[172]][i,1] + 
    coeffData[[172]][2,] * dataAssetsCombList[[172]][i,2] + 
    coeffData[[172]][3,] * dataAssetsCombList[[172]][i,3] + 
    coeffData[[172]][4,] * dataAssetsCombList[[172]][i,4] + 
    coeffData[[172]][5,] * dataAssetsCombList[[172]][i,5]
}

dataAssetsCombList[[172]] <- as.xts(dataAssetsCombList[[172]])

colnames(dataAssetsCombList[[172]])[7] <- "Model"
plot(dataAssetsCombList[[172]]$Close, main = "Actual vs Predicted", col = "Blue")
lines(dataAssetsCombList[[172]]$Model, col = "Red")

for(i in 1:nrow(dataAssetsCombList[[193]])){
  dataAssetsCombList[[193]][i,7] <- coeffData[[193]][1,] + 
    coeffData[[193]][2,] * dataAssetsCombList[[193]][i,1] + 
    coeffData[[193]][2,] * dataAssetsCombList[[193]][i,2] + 
    coeffData[[193]][3,] * dataAssetsCombList[[193]][i,3] + 
    coeffData[[193]][4,] * dataAssetsCombList[[193]][i,4] + 
    coeffData[[193]][5,] * dataAssetsCombList[[193]][i,5]
}

dataAssetsCombList[[193]] <- as.xts(dataAssetsCombList[[193]])

colnames(dataAssetsCombList[[193]])[7] <- "Model"
plot(dataAssetsCombList[[193]]$Close, main = "Actual vs Predicted", col = "Blue")
lines(dataAssetsCombList[[193]]$Model, col = "Red")



