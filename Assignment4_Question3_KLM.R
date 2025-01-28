#21:219:220 Fundamentals of Data Visualization, Spring 2021
#Assignment 4 - Question 3
#Kobe Lee Martinez, RUID: 202006380

library(GISTools)
library(Cairo)
library(RColorBrewer)
#Importing color palette from set 2 with all colors in pal.
colorsT <- brewer.pal(8, "Set2")
colors <- add.alpha(colorsT, .5)

#3.1
#sets the working directory
setwd("/Users/kobea/OneDrive/Documents/Rutgers/FDMTLS of Data Vis/coding")
#lists all the files in the directory
list.files()
#importing csv file from wd
myData <- read.csv("Life Expectancy Data-1.csv")
myData
#Obtaining summary of csv file
summary(myData)
#removing nan/null values in the data set
myData <- na.omit(myData)
#indexing columns to use on scatter plot models
scatter6 <- myData[,c(4,7,11,9,19,13,22)]

#taking a random sample of the data
IDX <- sample(1:dim(scatter6)[1])
#taking 50% of the row size
trainSize <- round(0.5*dim(scatter6)[1])
#taking 25% of the row size
validationSize <- round(0.25*dim(scatter6)[1])
#taking 25% of the row size
testSize <- round(0.25*dim(scatter6)[1])
#assigning 50% of the random sample to data.train
data.train <- scatter6[IDX[1:trainSize],]
#assigning 25% of the random sample to data.valid
data.valid <- scatter6[IDX[(trainSize+1):(trainSize+validationSize)],]
#assigning 25% of the random sample to data.test
data.test <- scatter6[IDX[(trainSize+1):(trainSize+testSize)],]
#producing the coefficients of the predictors by life expectancy using the train data
fit1 <- lm(Life.expectancy ~ Alcohol+BMI+Hepatitis.B+thinness..1.19.years+Polio+Schooling, 
           data = data.train)
#producing the predictions of the first model using the validation data
predict1 <- predict(fit1, newdata = data.valid)
#producing the coefficients of the predictors by life expectancy using the train data
fit2 <- lm(Life.expectancy ~ Alcohol+Hepatitis.B+thinness..1.19.years+Polio+Schooling, 
           data = data.train)
#producing the predictions of the second model using the validation data
predict2 <- predict(fit2, newdata = data.valid)
#producing the coefficients of the predictors by life expectancy using the train data
fit3 <- lm(Life.expectancy ~ Alcohol+Hepatitis.B+Polio+Schooling, 
           data = data.train)
#producing the predictions of the third model using the validation data
predict3 <- predict(fit3, newdata = data.valid)
#producing the coefficients of the predictors by life expectancy using the train data
fit4 <- lm(Life.expectancy ~ Alcohol+Polio+Schooling, 
           data = data.train)
#producing the predictions of the fourth model using the validation data
predict4 <- predict(fit4, newdata = data.valid)

#function provided by professor Richard to calculate the AIC scores
calculateAIC <- function(nParams, nObservs, YPred, Y){
    ssE <-sum((YPred-Y)**2)  
    sigma = ssE/nObservs  
    AIC = nObservs *log(sigma) +2*nParams
    return(AIC)
  }

#made an empty verctor to store the AIC scores in
AIC <- NULL
#AIC scores
AIC[1] <- calculateAIC(length(fit1$coefficients), dim(data.valid)[1], predict1, data.valid$Life.expectancy)
AIC[2] <- calculateAIC(length(fit2$coefficients), dim(data.valid)[1], predict2, data.valid$Life.expectancy)
AIC[3] <- calculateAIC(length(fit3$coefficients), dim(data.valid)[1], predict3, data.valid$Life.expectancy)
AIC[4] <- calculateAIC(length(fit4$coefficients), dim(data.valid)[1], predict4, data.valid$Life.expectancy)


#I created a pdf file named "Assignment4_models_KLM.pdf"
pdf_file<-"Assignment4_models_KLM.pdf"
pdf_file
#using the cairo_pdf & par functions, I edited the dimensions of the 
#pdf file
cairo_pdf(pdf_file,width=20,height=9.35)
par(omi = c(.25,.25,.25,.25))
#creates a matrix of 4 numbers, puts them in 4 cols
mat <- matrix(c(1,2,3,4), nrow = 1, ncol = 4, byrow = TRUE)
#sets the heights of the cols/rows in the matrix above
layout(mat, heights = 0.25)
#sets the line width and the margin size specified in inches
par(lwd = 2, mai = c(0.8,0.5,0.8,0.5))
#sets the y range to (40-90)
ylim <- range(pretty(data.valid$Life.expectancy))
#sets the x range to the range of ylim
xlim <- ylim
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of predict1 by Life Expectancy from the validation data in the shapes of filled circles
points(predict1, data.valid$Life.expectancy, pch = 21, cex = 4, col = colorsT[2], bg = colors[2])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 2, at = pretty(data.valid$Life.expectancy), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 1, at = pretty(data.valid$Life.expectancy), line = 0.5, las = 1)
#x-axis label
mtext("Life Expectancy  Predicted",1,line = 2,font = 2)
#y-axis label
mtext("Life Expectancy  Real",2, line = 3,font = 2)
#title of plot
mtext("Model 1",3, line = 1.5,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(data.valid$Life.expectancy ~ predict1), col=colorsT[7],lwd = 4)
#placement for where the AIC score will be placed
text(xlim[1],ylim[2], paste0("AIC = ",round(AIC[1],2)), adj = 0, cex = 1.5)
######
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of predict2 by Life Expectancy from the validation data in the shapes of filled circles
points(predict2, data.valid$Life.expectancy, pch = 21, cex = 4, col = colorsT[1], bg = colors[1])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 2, at = pretty(data.valid$Life.expectancy), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 1, at = pretty(data.valid$Life.expectancy), line = 0.5, las = 1)
#x-axis label
mtext("Life Expectancy  Predicted",1,line = 2,font = 2)
#title of plot
mtext("Model 2",3, line = 1.5,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(data.valid$Life.expectancy ~ predict2), col=colorsT[7],lwd = 4)
#placement for where the AIC score will be placed
text(xlim[1],ylim[2], paste0("AIC = ",round(AIC[2],2)), adj = 0, cex = 1.5)
######
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of predict3 by Life Expectancy from the validation data in the shapes of filled circles
points(predict3, data.valid$Life.expectancy, pch = 21, cex = 4, col = colorsT[3], bg = colors[3])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 2, at = pretty(data.valid$Life.expectancy), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 1, at = pretty(data.valid$Life.expectancy), line = 0.5, las = 1)
#x-axis lable
mtext("Life Expectancy  Predicted",1,line = 2,font = 2)
#title of plot
mtext("Model 3",3, line = 1.5,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(data.valid$Life.expectancy ~ predict3), col=colorsT[7],lwd = 4)
#placement for where the AIC score will be placed
text(xlim[1],ylim[2], paste0("AIC = ",round(AIC[3],2)), adj = 0, cex = 1.5)
######
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of predict4 by Life Expectancy from the validation data in the shapes of filled circles
points(predict4, data.valid$Life.expectancy, pch = 21, cex = 4, col = colorsT[4], bg = colors[4])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(data.valid$Life.expectancy),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 2, at = pretty(data.valid$Life.expectancy), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. Life Expectancy
mtext(pretty(data.valid$Life.expectancy), 1, at = pretty(data.valid$Life.expectancy), line = 0.5, las = 1)
#x-axis label
mtext("Life Expectancy  Predicted",1,line = 2,font = 2)
#title of plot
mtext("Model 4",3, line = 1.5,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(data.valid$Life.expectancy ~ predict4), col=colorsT[7],lwd = 4)
#placement for where the AIC score will be placed
text(xlim[1],ylim[2], paste0("AIC = ",round(AIC[4],2)), adj = 0, cex = 1.5)
#closes file and provides URL for plot(concates. to single image)
dev.off()
######################################

#3.2
fit.test <- lm(Life.expectancy ~ Alcohol+BMI+Hepatitis.B+thinness..1.19.years+Polio+Schooling, 
           data = data.train)
predict.test <- predict(fit.test, newdata = data.test)

#I created a pdf file named "Assignment4_test_KLM.pdf"
pdf_file<-"Assignment4_test_KLM.pdf"
pdf_file
#using the cairo_pdf & par functions, I edited the dimensions of the 
#pdf file
cairo_pdf(pdf_file,width=20,height=9.35)
par(omi = c(.5,.5,.5,.5))
#sets the line width and the margin size specified in inches
par(lwd = 2, mai = c(0.8,0.5,0.8,0.5))
#sets the y range to (40-90)
ylim <- range(pretty(data.test$Life.expectancy))
#sets the x range to the range of ylim
xlim <- ylim
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of predict.test by Life Expectancy from the test data in the shapes of filled circles
points(predict.test, data.test$Life.expectancy, pch = 21, cex = 4, col = colorsT[2], bg = colors[2])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(data.test$Life.expectancy),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(data.test$Life.expectancy),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(data.test$Life.expectancy),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(data.test$Life.expectancy),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(data.test$Life.expectancy), 2, at = pretty(data.test$Life.expectancy), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. Life Expectancy
mtext(pretty(data.test$Life.expectancy), 1, at = pretty(data.test$Life.expectancy), line = 0.5, las = 1)
#x-axis label
mtext("Life Expectancy  Predicted",1,line = 2,font = 2)
#y-axis label
mtext("Life Expectancy  Real",2, line = 3,font = 2)
#title of plot
mtext("Model 1 ~ on test data",3, line = 1.5,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(data.test$Life.expectancy ~ predict.test), col=colorsT[7],lwd = 4)
#closes file and provides URL for plot(concates. to single image)
dev.off()

#Question answers: 
#The test model only allows me to answer three of the questions provided
#In the current model, life expectancy is positively impacted by lifestyle choices such as alcohol. 
#Although consumption of alcohol may not bring about more life, it is tied to those who are wealthy and have time for it.
#Schooling also has a positive impact on the lifespan of humans, since it allows the brains to desire more knowledge and encourages others to keep learning
#Lastly, immunization has a positive impact on life expectancy, since it allows people to live longer and healthier .