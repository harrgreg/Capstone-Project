library(quantmod)
library(JADE)
library(BSSasymp)
library(xts)

library(ggplot2)
library(grid)
library(dplyr)
library(plotly)

start <- as.Date("2016-01-01")
end <- Sys.Date()
# Please, note that the end date is used dynamically, the results obtained in the end can differ, if 
# executed in another day.

pool <- getSymbols(Symbols = c( "AAPL", "JPM", "COST", "DIS", "MSFT", "AXP", "CAT", "GS", "FDX", "GOOG"), 
                   src = "yahoo", from = start, to = end)

portfolio1 <- 0.25 * AAPL + 0.25 * GOOG + 0.15 * GS + 0.2 * FDX +  0.15 * CAT 
portfolio2 <- 0.15 * AAPL + 0.3 * GOOG + 0.2 * GS + 0.25 * FDX +  0.1 * CAT 
portfolio3 <- 0.18 * AAPL + 0.2 * GOOG + 0.3 * GS + 0.12 * FDX +  0.2 * CAT 
portfolio4 <- 0.11 * AAPL + 0.35 * GOOG + 0.15 * GS + 0.29 * FDX +  0.1 * CAT 
portfolio5 <- 0.15 * AAPL + 0.2 * GOOG + 0.25 * GS + 0.2 * FDX +  0.2 * CAT 

colnames(portfolio1) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(portfolio2) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(portfolio3) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(portfolio4) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(portfolio5) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
# making Close data 
portClose1 <- portfolio1$Close
portClose2 <- portfolio2$Close
portClose3 <- portfolio3$Close
portClose4 <- portfolio4$Close
portClose5 <- portfolio5$Close

Close <- cbind(portClose1, portClose2, portClose3, portClose4, portClose5)

plot(Close)
normClose <- diff(Close)
plot(normClose)

#### Running JADE algorithm ####
jadeClose <- JADE(normClose, n.comp = 5, eps = 1e-06, maxiter = 100, na.action = na.omit)
plot.ts(bss.components(jadeClose), nc = 1, main = "JADE solution")

ICclose <- as.data.frame(rownames(jadeClose$S))
rownames(jadeClose$S) <- NULL
ICclose <- cbind(ICclose, as.data.frame(jadeClose$S))
class(ICclose)
colnames(ICclose)[1] <- "date"
class(as.numeric(normClose$Close))
#View(ICclose)


num1 <- as.data.frame(as.numeric(diff(AAPL$AAPL.Close)))
num1 <- num1[-1,]
num2 <- as.data.frame(as.numeric(diff(JPM$JPM.Close)))
num2 <- num2[-1,]
num3 <- as.data.frame(as.numeric(diff(COST$COST.Close)))
num3 <- num3[-1,]
num4 <- as.data.frame(as.numeric(diff(DIS$DIS.Close)))
num4 <- num4[-1,]
num5 <- as.data.frame(as.numeric(diff(MSFT$MSFT.Close)))
num5 <- num5[-1,]
num6 <- as.data.frame(as.numeric(diff(AXP$AXP.Close)))
num6 <- num6[-1,]
num7 <- as.data.frame(as.numeric(diff(CAT$CAT.Close)))
num7 <- num7[-1,]
num8 <- as.data.frame(as.numeric(diff(GS$GS.Close)))
num8 <- num8[-1,]
num9 <- as.data.frame(as.numeric(diff(FDX$FDX.Close)))
num9 <- num9[-1,]
num10 <- as.data.frame(as.numeric(diff(GOOG$GOOG.Close)))
num10 <- num10[-1,]

num_list <- list(num1, num2, num3, num4, num5, num6, num7, num8, num9, num10)
num_list


######################
#### Based on DTW ####
######################

## 1st component with DTW ##
list_distance_matrix <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix[[i]] <- dtw(ICclose$IC.1, num_list[[i]])
}

min <- list_distance_matrix[[1]]$distance
index <- 1
for (i in 2:length(num_list)) {
  if(min > list_distance_matrix[[i]]$distance){
    min <- list_distance_matrix[[i]]$distance
    index <- i
  }
}
min
index # 6

# plot the results
err1 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.1-num_list[[index]])^2)
  err1 <- c(err1,x)
}
plot(err1)
which.min(err1)


plot(which.min(err1) * ICclose$IC.1, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 2nd component with DTW ##
list_distance_matrix <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix[[i]] <- dtw(ICclose$IC.2, num_list[[i]])
}

min <- list_distance_matrix[[1]]$distance
index <- 1
for (i in 2:length(num_list)) {
  if(min > list_distance_matrix[[i]]$distance){
    min <- list_distance_matrix[[i]]$distance
    index <- i
  }
}
min
index # 5

# plot the results
err2 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.2-num_list[[index]])^2)
  err2 <- c(err2,x)
}
plot(err2)
which.min(err2)


plot(which.min(err2) * ICclose$IC.2, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 3rd component with DTW ##
list_distance_matrix <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix[[i]] <- dtw(ICclose$IC.3, num_list[[i]])
}

min <- list_distance_matrix[[1]]$distance
index <- 1
for (i in 2:length(num_list)) {
  if(min > list_distance_matrix[[i]]$distance){
    min <- list_distance_matrix[[i]]$distance
    index <- i
  }
}
min
index # 5

# plot the results
err3 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.3-num_list[[index]])^2)
  err3 <- c(err3,x)
}
plot(err3)
which.min(err3)


plot(which.min(err3) * ICclose$IC.3, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 4th component with DTW ##
list_distance_matrix <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix[[i]] <- dtw(ICclose$IC.4, num_list[[i]])
}

min <- list_distance_matrix[[1]]$distance
index <- 1
for (i in 2:length(num_list)) {
  if(min > list_distance_matrix[[i]]$distance){
    min <- list_distance_matrix[[i]]$distance
    index <- i
  }
}
min
index # 5

# plot the results
err4 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.4-num_list[[index]])^2)
  err4 <- c(err4,x)
}
plot(err4)
which.min(err4)


plot(which.min(err4) * ICclose$IC.4, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 5th component with DTW ##
list_distance_matrix <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix[[i]] <- dtw(ICclose$IC.5, num_list[[i]])
}

min <- list_distance_matrix[[1]]$distance
index <- 1
for (i in 2:length(num_list)) {
  if(min > list_distance_matrix[[i]]$distance){
    min <- list_distance_matrix[[i]]$distance
    index <- i
  }
}
min
index # 5

# plot the results
err5 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.5-num_list[[index]])^2)
  err5 <- c(err5,x)
}
plot(err5)
which.min(err5)


plot(which.min(err5) * ICclose$IC.5, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index]]), type = "l", ylim = c(-10,10))
par(new = FALSE)



#############################################
#### Resuts Based on Visual Similarities ####
#############################################

## 1st component ##
err1 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.1-num_list[[9]])^2)
  err1 <- c(err1,x)
}
plot(err1)
which.min(err1)


plot(which.min(err1) * ICclose$IC.1, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[9]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 2nd component ##
err2 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.2-num_list[[10]])^2)
  err2 <- c(err2,x)
}
plot(err2)
which.min(err2)


plot(which.min(err2) * ICclose$IC.2, type = "l", col = "red", ylim = c(-50,50))
par(new = TRUE)
plot(as.numeric(num_list[[10]]), type = "l", ylim = c(-50,50))
par(new = FALSE)


## 3rd component ##
err3 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.3-num_list[[1]])^2)
  err3 <- c(err3,x)
}
plot(err3)
which.min(err3)


plot(which.min(err3) * ICclose$IC.3, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[1]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 4th component ##
err4 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.4-num_list[[7]])^2)
  err4 <- c(err4,x)
}
plot(err4)
which.min(err4)


plot(which.min(err4) * ICclose$IC.4, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[7]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


## 5th component ##
# should be 8th, nevertheless it does not capture the pattern
err5 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.5-num_list[[8]])^2)
  err5 <- c(err5,x)
}
plot(err5)
which.min(err5)


plot(which.min(err5) * ICclose$IC.5, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[8]]), type = "l", ylim = c(-10,10))
par(new = FALSE)

# the 5th component is more or less like 7, it captures some patters but we have a nicer result already.
err5 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.5-num_list[[7]])^2)
  err5 <- c(err5,x)
}
plot(err5)
which.min(err5)


plot(which.min(err5) * ICclose$IC.5, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[7]]), type = "l", ylim = c(-10,10))
par(new = FALSE)

########################################
#### Some Graphics for Presentation ####
########################################
candleChart(portfolio1, up.col = "black", dn.col = "red", theme = "white")
candleChart(portfolio2, up.col = "black", dn.col = "red", theme = "white")
candleChart(portfolio3, up.col = "black", dn.col = "red", theme = "white")
candleChart(portfolio4, up.col = "black", dn.col = "red", theme = "white")
candleChart(portfolio5, up.col = "black", dn.col = "red", theme = "white")

plot(AAPL[, "AAPL.Close"], main = "AAPL")
# comparison side by side
par(mfrow = c(2,1))
plot(abs(which.min(err2) * ICclose$IC.5 - num_list[[8]]), type = "line")
plot(abs(which.min(err2) * ICclose$IC.5 - num_list[[7]]), type = "line")
par(mfrow = c(1,1))

