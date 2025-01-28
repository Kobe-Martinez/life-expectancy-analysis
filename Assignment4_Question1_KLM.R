#21:219:220 Fundamentals of Data Visualization, Spring 2021
#Assignment 4 - Question 1
#Kobe Lee Martinez, RUID: 202006380
#Importing libraries
library(GISTools)
library(Cairo)

#1.1
#sets the working directory
setwd("/Users/kobea/OneDrive/Documents/Rutgers/FDMTLS of Data Vis/coding")
#lists all the files in the directory
#importing csv file from wd
list.files()
myData <- read.csv("Life Expectancy Data-1.csv")
myData
#Obtaining summary of csv file
summary(myData)
#removing nan/null values in the data set
myData <- na.omit(myData)

#indexing columns 5-22 in the data set
newData <- myData[,5:22]

#renaming the indexed columns and storing them in to different variables to manipulate when plotting the data
predictors <- c("Adult Mortality","Alcohol","Hep B","BMI",
                "Polio","Diphtheria","GDP","Thin (10-19yrs)","Income") 
predictors.2 <- c("Infant Death","% Expend","Measles","<5yrs Deaths",
                  "Tot Expend","HIV/AIDS","Population","Thin (5-9yrs)","Schooling")

#importing colors from the hcl.colors palette set 3 with 18 colors/shades
colors <- hcl.colors(n=18, palette = "Set 3")
colorsT <- add.alpha(colors, .5)

#I created a pdf file named "Assignment4_boxplot_KLM.pdf"
pdf_file<-"Assignment4_boxplot_KLM.pdf"
pdf_file
#using the cairo_pdf function I edited the dimensions of the 
#pdf file
cairo_pdf(pdf_file,width=20,height=9.35)
par(omi = c(.25,.25,.25,.25))

#created a boxplot of all the columns stored in the dataframe newData
#used log10 around the dataframe to size the values to the graph
boxplot(log10(newData[1:18]+1), col = colors,
        boxwex = 0.7,  yaxt = "n", xaxt = "n", 
        ann = FALSE, ylim = c(0,10), outline = TRUE, frame = F)

#creates the y-axis for the figure with tick marks in increments of 2 and size of -0.01,  
#and the axis line 2 times the line width.
axis(2, at = seq(0,10,2), tck = -0.01, lab = F, lwd = 2)
#plots the actual numbers for the y-axis from (0-10) in increments of 2
mtext(seq(0,10,2), 2, at = seq(0,10,2), line = 0.5, las = 1)
#label for the -axis
mtext("log10 Scores", 2,  line = 2.5, cex = 1.4)
#index columns that were renamed; first row of labels are higher
#than the second row
mtext(predictors, 1,  line = -0.5, cex = 1.05, at = seq(1,18,2))
mtext(predictors.2, 1,  line = 0.5, cex = 1.05, at = seq(2,18,2))

#closes file and provides URL for plot(concates. to single image)
dev.off()

#####################################################
#1.2
#sets the x range to (30-90)
xrange <- c(30,90)
#Takes both values of the x range (30-90) and creates bins for the range in increments of 5
bins <- seq(xrange[1], xrange[2], 5)
#Provides information for Life.expectancy and its breaks using the hist function, but does not plot it
h1 <- hist(myData$Life.expectancy, breaks = bins, plot = FALSE)
#sets the y range from (0-600)
yrange <- c(0,600)


#I created a pdf file named "Assignment4_histogram_KLM.pdf"
pdf_file<-"Assignment4_histogram_KLM.pdf"
pdf_file
#using the cairo_pdf function I edited the dimensions of the 
#pdf file
cairo_pdf(pdf_file,width=10,height=8)
#creates a matrix of 4 numbers, puts them in 2 cols, and sets them by row 
mat <- matrix(c(1,1,2,2), nrow = 2, ncol = 2, byrow = TRUE)
#sets the heights of the cols/rows in the matrix above
layout(mat, heights = c(1.35,0.65))
#sets the line width and the margin size specified in inches
par(lwd = 2, mai = c(0,1,0.8,1), omi = c(.25,.25,.25,.25))

#plots the Life.expectancy in a histogram by the assigned bins
#sets the y axis limits based on yrange and the x axis limits based on xrange
#plots the figure without any axises
hist(myData$Life.expectancy, breaks = bins, col = colorsT[2], ylim = yrange, xlim = xrange,
      yaxt = "n", xaxt = "n", border = colors[2], cex.lab = 1.8, ann = FALSE)
#creates the y-axis for the histogram with tick marks from (0-600) in increments of 100 and size of -0.017,  
#and the axis line 2 times the line width.
axis(2, at = seq(0,600,100), tck = -0.017, lab = F, lwd = 2)
#Produces the numbers for the y-axis (0,600) in increments of 100
mtext(seq(0,600,100), 2, at = seq(0,600,100), las = 2, line = 0.8)
#y-axis label
mtext("Frequency", 2, line = 3.5, cex = 1.4)
#Graph  title
mtext("Distribution of Life Expectancy", 3, line = 2, cex = 1.6)

#sets the line width and the margin size specified in inches 
par(lwd = 2, mai = c(1,1,0,1))
#creates a boxplot based on Life.expectancy col. from myData
#makes sure the boxplot is horizontal
#plots it with no axises, with outliers, no frame, scale factor of 0.5, 
#and ylim by the xrange of the first histogram
boxplot(myData$Life.expectancy, horizontal = TRUE, boxwex = 0.5, col = NA, border = colors[2], yaxt = "n", xaxt = "n", ann = FALSE,
        ylim = xrange, outline = TRUE, frame = F)
#creates the x-axis with tick marks from (30-90) in increments of 10 and size of -0.065,  
#and the axis line 2 times the line width.
axis(1, at = seq(30,90,10), tck = -0.065, lab = F, lwd = 2, las = 1)
#Produces the numbers for the x-axis (30,90) in increments of 10
mtext(seq(30,90,10), 1, at = seq(30,90,10), line = 1.4, cex = 1.2)
#x-axis label
mtext("Life Expectancy (Years)", 1,  line = 4, cex = 1.55)

#closes file and provides URL for plot(concates. to single image)
dev.off()


