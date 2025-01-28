#Importing libraries
library(GISTools)
library(corrplot)
library(Cairo)
#importing colors from the hcl.colors palette Reds with 18 colors/shades
colors <- hcl.colors(n=18,palette = "Reds")

#2.1
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
#indexing columns 4-22 in the data set
newData <- myData[,4:22]

#Obtaining summary of preferred data set
summary(newData)
#renames the columns of the data set
colnames(newData) <- c("Life Expectancy","Adult Mortality","Infant Death","Alcohol",
                       "% Expend","Hep B","Measles","BMI","<5yrs Deaths",
                       "Polio","Tot Expend","Diphtheria","HIV/AIDS",
                       "GDP","Population","Thin (10-19yrs)","Thin (5-9yrs)","Income","Schooling")

#returns a coefficient of the data
cM <- cor(newData)

#I created a pdf file named "Assignment4_correlationmatrix_KLM.pdf"
pdf_file<-"Assignment4_correlationmatrix_KLM.pdf"
pdf_file
#using the cairo_pdf function I edited the dimensions of the 
#pdf file
cairo_pdf(pdf_file,width=20,height=9.35)
par(omi = c(.25,.25,.25,.25))

#creates the correlation matrix as designed in the week 12 slides
#type is the formation of the boxes; method determines the shape; tl.col is the color of the text
#tl.srt is the positon in degrees; diag determines if the data is shown diagnally or not
corrplot(cM, type = "upper", method = "ellipse",
         col = colors[18:1], tl.col = "black", tl.srt = 90, diag=FALSE)
#closes file and provides URL for plot(concates. to single image)
dev.off()
#####################################################################
#2.2
#importing library
library(RColorBrewer)
#Importing color palette from set 2 with all colors in pal.
colorsT <- brewer.pal(8, "Set2")
#indexing 6 columns for scatter plots
scatter6 <- myData[,c(7,11,9,19,13,22)]

#setting the ranges for the scatter plots
ylim <- range(pretty(newData$`Life Expectancy`))
xlim <- range(pretty(scatter6$Alcohol))
xlim1 <- range(pretty(scatter6$Schooling))
xlim2 <- range(pretty(scatter6$Hepatitis.B))
xlim3 <- range(pretty(scatter6$BMI))
xlim4 <- range(pretty(scatter6$Polio))
xlim5 <- range(pretty(scatter6$thinness..1.19.years))


#I created a pdf file named "Assignment4_scatterplots_KLM.pdf"
pdf_file<-"Assignment4_scatterplots_KLM.pdf"
pdf_file
#using the cairo_pdf & par functions, I edited the dimensions of the 
#pdf file
cairo_pdf(pdf_file,width=20,height=9.35)
par(omi = c(.25,.25,.25,.25))
#creates a matrix of 6 numbers, puts them in 3 cols, and sets them by row 
mat <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE)
#sets the heights of the cols/rows in the matrix above
layout(mat, heights = c(0.25,0.25))
#sets the line width and the margin size specified in inches
par(lwd = 2, mai = c(0.5,0.5,0.5,0.5))

#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of Alcohol by Life Expectancy in the shapes of empty circles
points(scatter6$Alcohol, newData$`Life Expectancy`, pch = 21, cex = 4, col = colorsT[3])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(scatter6$Alcohol),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(scatter6$Alcohol),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(newData$`Life Expectancy`), 2, at = pretty(newData$`Life Expectancy`), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. Alcohol
mtext(pretty(scatter6$Alcohol), 1, at = pretty(scatter6$Alcohol), line = 0.5, las = 1)
#x-axis label
mtext("Alcohol",1,line = 2,font = 2)
#y-axis label
mtext("Life Expectancy (yrs)",2, line = 3,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(newData$`Life Expectancy` ~ scatter6$Alcohol), col=colors[15],lwd = 4)
#returns the pearson r number which appears on the graph as r = " "
alc <- cor(scatter6$Alcohol, newData$`Life Expectancy`, method ="pearson")
#placement for where the pearson r will be placed
text(xlim[1],ylim[1], paste0("r = ",round(alc,3)), adj = 0, cex = 1.5)

######
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim1, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of Schooling by Life Expectancy in the shapes of empty circles
points(scatter6$Schooling, newData$`Life Expectancy`, pch = 21, cex = 4, col = colorsT[1])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(scatter6$Schooling),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(scatter6$Schooling),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
#Produces the numbers for the x-axis by the range of col. Schooling
mtext(pretty(scatter6$Schooling), 1, at = pretty(scatter6$Schooling), line = 0.5, las = 1)
#x-axis label
mtext("Schooling",1,line = 2,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(newData$`Life Expectancy` ~ scatter6$Schooling), col=colors[15],lwd = 4)
#returns the pearson r number which appears on the graph as r = " "
sch <- cor(scatter6$Schooling, newData$`Life Expectancy`, method ="pearson")
#placement for where the pearson r will be placed
text(xlim1[1],ylim[1], paste0("r = ",round(sch,3)), adj = 0, cex = 1.5)

######
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim2, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of Hepatitis.B by Life Expectancy in the shapes of empty circles
points(scatter6$Hepatitis.B, newData$`Life Expectancy`, pch = 21, cex = 4, col = colorsT[2])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(scatter6$Hepatitis.B),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(scatter6$Hepatitis.B),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
#Produces the numbers for the x-axis by the range of col. Hepatitis.B
mtext(pretty(scatter6$Hepatitis.B), 1, at = pretty(scatter6$Hepatitis.B), line = 0.5, las = 1)
#x-axis label
mtext("Hep B",1,line = 2,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(newData$`Life Expectancy` ~ scatter6$Hepatitis.B), col=colors[15],lwd = 4)
#returns the pearson r number which appears on the graph as r = " "
hep <- cor(scatter6$Hepatitis.B, newData$`Life Expectancy`, method ="pearson")
#placement for where the pearson r will be placed
text(xlim2[1],ylim[1], paste0("r = ",round(hep,3)), adj = 0, cex = 1.5)

#################
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim3, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of BMI by Life Expectancy in the shapes of empty circles
points(scatter6$BMI, newData$`Life Expectancy`, pch = 21, cex = 4, col = colorsT[4])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(scatter6$BMI),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(scatter6$BMI),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
#Produces the numbers for the y-axis by the range of col. Life Expectancy
mtext(pretty(newData$`Life Expectancy`), 2, at = pretty(newData$`Life Expectancy`), line = 0.2, las = 1)
#Produces the numbers for the x-axis by the range of col. BMI
mtext(pretty(scatter6$BMI), 1, at = pretty(scatter6$BMI), line = 0.5, las = 1)
#x-axis label
mtext("BMI",1,line = 2,font = 2)
#y-axis label
mtext("Life Expectancy (yrs)",2, line = 3,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(newData$`Life Expectancy` ~ scatter6$BMI), col=colors[15],lwd = 4)
#returns the pearson r number which appears on the graph as r = " "
BMI <- cor(scatter6$BMI, newData$`Life Expectancy`, method ="pearson")
#placement for where the pearson r will be placed
text(xlim3[1],ylim[1], paste0("r = ",round(BMI,3)), adj = 0, cex = 1.5)

#################
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim4, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of Polio by Life Expectancy in the shapes of empty circles
points(scatter6$Polio, newData$`Life Expectancy`, pch = 21, cex = 4, col = colorsT[5])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(scatter6$Polio),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(scatter6$Polio),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
#Produces the numbers for the x-axis by the range of col. Polio
mtext(pretty(scatter6$Polio), 1, at = pretty(scatter6$Polio), line = 0.5, las = 1)
#x-axis label
mtext("Polio",1,line = 2,font = 2)
#closes the gaps between the axises and makes the graph appear as a box
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(newData$`Life Expectancy` ~ scatter6$Polio), col=colors[15],lwd = 4)
#returns the pearson r number which appears on the graph as r = " "
Polio <- cor(scatter6$Polio, newData$`Life Expectancy`, method ="pearson")
#placement for where the pearson r will be placed
text(xlim4[1],ylim[1], paste0("r = ",round(Polio,3)), adj = 0, cex = 1.5)

######
#empty plot with specified ranges
plot(x = NULL, y=NULL, xlim=xlim5, ylim=ylim, axes=FALSE, ann=FALSE)
#plots the points of thinness..1.19.years by Life Expectancy in the shapes of empty circles
points(scatter6$thinness..1.19.years, newData$`Life Expectancy`, pch = 21, cex = 4, col = colorsT[6])
#creates the axis for the plot
axis(1, tck = 0.01, lwd = 2, at = pretty(scatter6$thinness..1.19.years),lab = F)
axis(2, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
axis(3, tck = 0.01, lwd = 2, at = pretty(scatter6$thinness..1.19.years),lab = F)
axis(4, tck = 0.01, lwd = 2, at = pretty(newData$`Life Expectancy`),lab = F)
#Produces the numbers for the x-axis by the range of col. thinness..1.19.years
mtext(pretty(scatter6$thinness..1.19.years), 1, at = pretty(scatter6$thinness..1.19.years), line = 0.5, las = 1)
#x-axis label
mtext("Thinness (10-19)",1,line = 2,font = 2)
box(lwd=2)
#adds one or more straight lines through the current plot
abline(lm(newData$`Life Expectancy` ~ scatter6$thinness..1.19.years), col=colors[15],lwd = 4)
#returns the pearson r number which appears on the graph as r = " "
thin <- cor(scatter6$thinness..1.19.years, newData$`Life Expectancy`, method ="pearson")
#placement for where the pearson r will be placed
text(xlim5[1],ylim[1], paste0("r = ",round(thin,3)), adj = 0, cex = 1.5)
#closes file and provides URL for plot(concates. to single image)
dev.off()
