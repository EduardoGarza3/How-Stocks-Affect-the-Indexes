###################################
#R_Project#1_Stock Price Changes###
#By Eduardo Garza##################
###################################


Stock_Price_Changes<-Stats_R_DATA_Project_FINAL #chosen data assigned to a new name/variable "Stock_Price_Changes"
dim(Stock_Price_Changes) # shows the dimensions of the data set
summary(Stock_Price_Changes) #generates a "six number summary" statistics of the data set
View(Stock_Price_Changes) # gives the (long form) spreadsheet format of data in new tab
pairs(Stock_Price_Changes[,1:3]) #generates scatter plots comparing Month, Year, and AAPL ADJ CLOSE %Δ

library(ggplot2) # loads the ggplot2 package into R
attach(Stock_Price_Changes) # allows me to tell R what data set I want to pull data from
names(Stock_Price_Changes) # gives me the names of the variables in my data (I used this to CHECK that R recognized my data correctly)

#Checking the distributions of my data I will use the "hist()" function
hist(`AMZN ADJ CLOSE`)+geom_density()#created a histogram for Amazon
#I will re-do the "hist" function for the rest of my stock data
hist(`BABA ADJ CLOSE`)
hist(`AAPL ADJ CLOSE`)
hist(`MSFT ADJ CLOSE`)
hist(`GOOGL ADJ CLOSE`)
hist(`TSLA ADJ CLOSE`)
hist(`FaceBook ADJ CLOSE`)

#after showing a visualization of the distributions I wanted to verify the distributions numerically using the Shapiro-Wilk test
#using 'shapiro.test() for each set of data I can verify normality with thier respective histograms
shapiro.test(`AMZN ADJ CLOSE`)#shapiro test for Amazon
#Amazon is normally distributed
shapiro.test(`BABA ADJ CLOSE`)#shapiro test for AliBABA
#AliBABA is normally distributed
shapiro.test(`AAPL ADJ CLOSE`)#shapiro test for APPLE
#Apple is normally distributed
shapiro.test(`MSFT ADJ CLOSE`)#shapiro test for Microsoft
#Microsoft is normally distributed
shapiro.test(`GOOGL ADJ CLOSE`)#shapiro test for Google
#Google is normally distributed
shapiro.test(`TSLA ADJ CLOSE`)#shapiro test for Tesla
#Tesla is NOT normally distributed and more right skewed
shapiro.test(`FaceBook ADJ CLOSE`)#shapiro test for FaceBook


#Checking Correlations# cor()will allow me to see the strength/weakness in the correlations between stocks and the DOWJones
cor(DOWJones,`AMZN ADJ CLOSE`)# allows me to see the strength of the correlation between the DOWJones and AMZN stock changes
cor(DOWJones,`BABA ADJ CLOSE`)
cor(DOWJones,`AAPL ADJ CLOSE`)
cor(DOWJones,`MSFT ADJ CLOSE`)
cor(DOWJones,`GOOGL ADJ CLOSE`)
cor(DOWJones,`TSLA ADJ CLOSE`)
cor(DOWJones,`FaceBook ADJ CLOSE`)
#for whatever reason R was not accepting the S and P 500 column, no matter how many times I adjusted it in Excel, so I have to specify everytime I want to use the S and P 500 by using the $
cor(DOWJones,Stock_Price_Changes$`S and P 500`)# gives me the correlation between the DOWJones and S and P 500

#since the DOWJones and the S and P 500 have a very strong positive correlation, and as I calculated at all the stocks have a strong positive correlation witht the DOWJ, I can safely assume they will have a strong correlation with the SP also

#the below code allows me to visually demonstrate the correlation between DOWJones and S and P 500
ggplot(Stock_Price_Changes,aes(DOWJones,Stock_Price_Changes$`S and P 500`))+geom_line()
#bellow I assigned the graph from above to the name main_cor
main_cor<-ggplot(Stock_Price_Changes,aes(DOWJones,Stock_Price_Changes$`S and P 500`))+geom_line()
#to center the title on the graph I assigned the name main_cor_title_ to main_cor and then ran ggtitle("DOWJones to S&P)
main_cor_title<-main_cor+ggtitle("DOWJones to S&P")
#I like the left align aesthetic of the title so all I did was run main_cor_title
main_cor_title
#now that I have a main title I want to fix the y-axis title to look more professional
#I renamed my "main_cor_title" and used the xlab and ylab functions to label the x and y axis respectively
allcomp1<-main_cor_title+xlab('DOWJones')+ylab('S&P_500')#this gives me the correlation of S&P and DOWJ over the entire 6 years


############
#I want to now see the different strengths in the correlations between 2020 and 2016 individually to show if their has been a change
############
#I used global environment to import 2 new data sets from Excel, one being 2020 data for 9 months and the equivalent, but for 2016
STOCKS2020<-STOCKS_2020_9M#I renamed the 2020 data set I imported to 'STOCKS2020'
STOCKS2016<-STOCKS_2016_9M#I renamed the 2016 data set I imported to 'STOCKS2016'

#to create a model where I could VISUALIZE the correlations between-
#-between ALL the stocks in my data set, I ran the following packages
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

#cbind allows me to combine vector, matrix or data frame by columns
#I ran the first code below to make sure R was graphing the right data
data2<-cbind(`AMZN ADJ CLOSE`,`BABA ADJ CLOSE`)
#verifying that it worked I assigned dataall to the cbind of all my stocks
dataall<-cbind(`AMZN ADJ CLOSE`,`BABA ADJ CLOSE`,`AAPL ADJ CLOSE`,`MSFT ADJ CLOSE`,`GOOGL ADJ CLOSE`,`TSLA ADJ CLOSE`,`FaceBook ADJ CLOSE`)
chart.Correlation(dataall)#allows me to create a visual of the correlations between all the stocks


#For both data sets included, it is over January to October, since October is the latest date I have of 2020, so October 2016 was the last data pulled for 2016 for fair/even comparison
#I will use the same functions as above, but to know create the graphs per year
###GRAPH FOR 2016###
detach(Stock_Price_Changes)#will tell R to no longer pull data from 'Stock_Price_Changes'
attach(STOCKS2016)#will tell R from what data downloaded to grab its data/info
cor2016<-cbind(`AMZN ADJ CLOSE  `,`BABA ADJ CLOSE `,`AAPL ADJ CLOSE `,`MSFT ADJ CLOSE `,`GOOGL ADJ CLOSE `,`TSLA ADJ CLOSE `,`FaceBook ADJ CLOSE `,STOCKS2016$`S and P 500`,DOWJones)
chart.Correlation(cor2016)

###GRAPH FOR 2020###
detach(STOCKS2016)#Will tell R to no longer grab data from 'STOCKS2016'
attach(STOCKS2020)#Will tell R to grab data from the 'STOCKS2020' data
cor2020<-cbind(`AMZN ADJ CLOSE  `,`BABA ADJ CLOSE `,`AAPL ADJ CLOSE `,`MSFT ADJ CLOSE `,`GOOGL ADJ CLOSE `,`TSLA ADJ CLOSE `,`FaceBook ADJ CLOSE `,STOCKS2020$`S and P 500`,DOWJones)
chart.Correlation(cor2020)
detach(STOCKS2020)#will tell R to no longer grab data from 'STOCKS2020'
attach(Stock_Price_Changes)#will tell R to grab data from 'Stock_Price_Changes'


ggplot(Stock_Price_Changes, aes(TIME,`AMZN ADJ CLOSE`)) + geom_line()
amtime<-ggplot(Stock_Price_Changes, aes(TIME,`AMZN ADJ CLOSE`)) + geom_line()
#used a smooth form of the plot, but the line version was a much better visual
ggplot(Stock_Price_Changes, aes(TIME,`AMZN ADJ CLOSE`)) + geom_smooth()
#used a point form of the plot, but the line version was a much better visual
ggplot(Stock_Price_Changes, aes(TIME,`AMZN ADJ CLOSE`)) + geom_point()

ggplot(Stock_Price_Changes,aes(`Month `,Stock_Price_Changes$`S and P 500`))




