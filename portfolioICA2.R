library(quantmod)
library(JADE)
library(BSSasymp)
library(dtw)
library(xts)

start <- as.Date("2016-01-01")
end <- Sys.Date()
# Please, note that the end date is used dynamically, the results obtained in the end can differ, if 
# executed in another day.

pool <- getSymbols(Symbols = c( "AAPL", "JPM", "COST", "DIS", "MSFT", "AXP", "CAT", "GS", "FDX", "GOOG"), 
                   src = "yahoo", from = start, to = end)


portfolio1 <- 0.5 * AAPL + 0.5 * GOOG 
portfolio2 <-  0.15 * AAPL + 0.85 * GOOG


colnames(portfolio1) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(portfolio2) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# making Close data 
portClose1 <- portfolio1$Close
portClose2 <- portfolio2$Close

Close <- cbind(portClose1, portClose2)
#View(Close)

#### Running JADE algorithm ####
plot(Close)
normClose <- diff(Close)
plot(normClose)
jadeClose <- JADE(normClose, n.comp = 2, eps = 1e-06, maxiter = 200, na.action = na.omit)
plot.ts(bss.components(jadeClose), nc = 1, main = "JADE solution")

ICclose <- as.data.frame(rownames(jadeClose$S))
rownames(jadeClose$S) <- NULL
ICclose <- cbind(ICclose, as.data.frame(jadeClose$S))
class(ICclose)
#View(ICclose)
class(as.numeric(normClose$Close))


#### Aligning the Results ####
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

# 1st component with DTW
list_distance_matrix1 <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix1[[i]] <- dtw(ICclose$IC.1, num_list[[i]])
}

min <- list_distance_matrix1[[1]]$distance
index <- 1
for (i in 2:length(num_list)) {
  if(min > list_distance_matrix1[[i]]$distance){
    min <- list_distance_matrix1[[i]]$distance
    index <- i
  }
}
min
index
# I decided to divide the overall data into epsiodes, then compare them separately.
# The episodewise implementation can be found at the end of the file, however it have not provided any 
# significant results. The final version was left with the general case. 

err11 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.1-num_list[[index]])^2)
  err11 <- c(err11,x)
}
plot(err11)
which.min(err11)


plot(which.min(err11) * ICclose$IC.1, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index]]), type = "l", ylim = c(-10,10))
par(new = FALSE)


# 2nd component with DTW
list_distance_matrix2 <- list()
for (i in 1:length(num_list)) {
  list_distance_matrix2[[i]] <- dtw(ICclose$IC.2, num_list[[i]])
}

min2 <- list_distance_matrix2[[1]]$distance
index2 <- 1
for (i in 2:length(num_list)) {
  if(min2 > list_distance_matrix1[[i]]$distance){
    min2 <- list_distance_matrix1[[i]]$distance
    index2 <- i
  }
}
min2
index2
# Again the 5th component 

err12 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.2-num_list[[index2]])^2)
  err12 <- c(err12,x)
}
plot(err12)
which.min(err12)


plot(which.min(err12) * ICclose$IC.2, type = "l", col = "red", ylim = c(-10,10))
par(new = TRUE)
plot(as.numeric(num_list[[index2]]), type = "l", ylim = c(-10,10))
par(new = FALSE)

# Indeed, it has much similarities and visually they are very similar too. However, there are some aspects
# seen in comparison with the other graphs that the model has not taken into account.


#############################################
#### Resuts Based on Visual Similarities ####
#############################################

## 1st component ##
num1 <- as.data.frame(as.numeric(diff(AAPL$AAPL.Close)))
num1 <- num1[-1,]
# err1 <- sum((ICclose$IC.1-num1)^2)/length(num1) # in neighborhood of this value

err1 <- c()
for ( k in 1:25){
  x <- sum((k*ICclose$IC.1-num1)^2)
  err1 <- c(err1,x)
}
plot(err1)
which.min(err1)


plot(which.min(err1) * ICclose$IC.1, type = "l", col = "red", ylim = c(-7,7))
par(new = TRUE)
plot(as.numeric(diff(AAPL$AAPL.Close)), type = "l", ylim = c(-7,7))
par(new = FALSE)


# 2nd component
num2 <- as.data.frame(as.numeric(diff(GOOG$GOOG.Close)))
num2 <- num2[-1,]


err2 <- c()
for ( k in 1:20){
  x <- sum((k*ICclose$IC.2-num2)^2)
  err2 <- c(err2,x)
}
plot(err2)
which.min(err2)


plot( which.min(err2) * ICclose$IC.2, type = "l", col = "red", ylim = c(-50,50))
par(new = TRUE)
plot(as.numeric(diff(GOOG$GOOG.Close)), type = "l", ylim = c(-50,50))
par(new = FALSE)

# Even multiplied the IC2 by the factor assessed above, the momdel aligned with the 5th component that is MSFT




#############################
#### EPISODE application ####
#############################

num5_episodes <- list(num_list[[5]][1:100], num_list[[5]][101:200], num_list[[5]][201:300], 
                      num_list[[5]][301:400],num_list[[5]][401:500],num_list[[5]][501:length(num_list[[5]])])

num1_episodes <- list(num_list[[1]][1:100], num_list[[1]][101:200], num_list[[1]][201:300], 
                      num_list[[1]][301:400],num_list[[1]][401:500],num_list[[1]][501:length(num_list[[1]])])
IC1_episodes <- list(ICclose$IC.1[1:100], ICclose$IC.1[101:200], ICclose$IC.1[201:300], 
                     ICclose$IC.1[301:400],ICclose$IC.1[401:500],ICclose$IC.1[501:length(num_list[[1]])])



# num1 episodes THE RIGHT ONE
list_distance_matrix1 <- list()
for (i in 1:length(IC1_episodes)) {
  list_distance_matrix1[[i]] <- dtw(IC1_episodes[[i]], num1_episodes[[i]])
}

dist_data <- as.data.frame(list_distance_matrix1[[1]]$distance)
for (i in 2:length(list_distance_matrix1)) {
  dist_data <- rbind(dist_data, list_distance_matrix1[[i]]$distance)
}
colnames(dist_data) <- "num1"

# num5 episodes 
list_distance_matrix1 <- list()
for (i in 1:length(IC1_episodes)) {
  list_distance_matrix1[[i]] <- dtw(IC1_episodes[[i]], num5_episodes[[i]])
}

dist_data[1,2] <- as.data.frame(list_distance_matrix1[[1]]$distance)
for (i in 2:length(list_distance_matrix1)) {
  dist_data[i,2] <- list_distance_matrix1[[i]]$distance
}
colnames(dist_data)[2] <- "num5"
# it can be seen that the data is more likely to be num 5 which is not the the one 
# there is not any significant difference in episode application.
# As it gavw no significat results then we will not execute the same procedure for the 2nd component.
# View(dist_data)

