#BEGIN Data Prep

#We will be cleaning data according to the following criteria:

#Date is in the format MM-DD-YYYY
#Data is sorted first by store number (ascending) and second by date (ascending)
#Weekly Sales is rounded to the nearest 2 decimal places
#Temperature is rounded to the nearest whole number
#Fuel Price is rounded to the nearest 2 decimal places
#CPI is rounded to the nearest 3 decimal places
#Unemployment is rounded to the nearest 3 decimal places
#Ensure that there is no missing data




#First, reformatting the date
Walmart_sales <- read.csv("C:/Users/auyea/Downloads/Walmart_sales.csv")
View(Walmart_sales)
#Check date format - is it mm-dd-yyyy
 #no, it is dd-mm-yyyy.
 #We need this reformatted.
print(class(Walmart_sales$Date)) 
#Dates stored as strings. We need them to be dates to reformat.
(Walmart_sales$Date <- as.Date(Walmart_sales$Date, format = "%d-%m-%Y"))
format(as.Date(Walmart_sales$Date, '%d/%m/%Y'), '%m-%d-%Y')
#That was easy! Our dates are reformatted as mm-dd-yyyy

#Next, sorting the data by store number and date.
#Our data happens to already be sorted this way which is great.
#However, if you need to resort the data, you can do so using
#order() or by sorting with the drop down menu in the data explorer, it's super simple.

#Next, we need to do a bunch of simple rounding.

Walmart_sales$Weekly_Sales <- round(Walmart_sales$Weekly_Sales, digits = 2)
Walmart_sales$Temperature <- round(Walmart_sales$Temperature)
Walmart_sales$Fuel_Price <- round(Walmart_sales$Fuel_Price, digits = 2)
Walmart_sales$CPI <- round(Walmart_sales$CPI, digits = 3)
Walmart_sales$Unemployment <- round(Walmart_sales$Unemployment, digits = 3)
View(Walmart_sales)
#easy peasy.

#Finally, to wrap up our data prep we need to take care of any missing observations.

which(is.na(Walmart_sales)) #which() counts and shows the location for missing data.

#END data prep

#------------------------------------------------------------------------------------

#BEGIN business case code

#Which locations have the highest weekly sales? 

#lets take a look at stores by unemployment rate.
View(Walmart_sales)
#To see the top couple of stores, all you need to do is use the arrow on the Weekly Sales column.
#By visual observation, The top few stores are 

#lets take a more definable approach.
  #We will look at stores associated highest average weekly sales
library(dplyr)
sortedList <- Walmart_sales %>% 
  group_by(Store) %>% 
  summarise(Weekly_Sales=mean(Weekly_Sales)) %>%
  arrange(Weekly_Sales) %>%
  print(n = 45)
View(sortedList)

#making a bar chart for the top 20% of average sales
topobs <- subset(sortedList, Weekly_Sales > quantile(sortedList$Weekly_Sales, 0.80))
barplot(height=topobs$Weekly_Sales, names=topobs$Store, 
        col=rgb(0.1,0.9,0.1,0.6),
        xlab="Store Number", 
        ylab="Average Weekly Sales", 
        main="Top 20% of Stores")
#and the bottom 20% of average sales
botobs <- subset(sortedList, Weekly_Sales <= quantile(sortedList$Weekly_Sales, 0.20))
barplot(height=botobs$Weekly_Sales, names=botobs$Store,
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="Store Number", 
        ylab="Average Weekly Sales", 
        main="Bottom 20% of Stores")

#why the discrepancy?

model1 <- lm(Weekly_Sales~. -Store -Date, data = Walmart_sales)
summary(model1) #our model shows temperature and fuel price to not be statistically significant.
model2 <- lm(Weekly_Sales~. -Store -Date -Temperature -Fuel_Price, data = Walmart_sales)
summary(model2)
#Weekly Sales = 1664939.1 + 84509.6(If it's a holiday week) - 1652.8(CPI) - 42542.2(Unemployment Rate (%) * 100)

#Which stores in the dataset have the lowest and highest unemployment rate?  
#What factors do you think are impacting the unemployment rate?

  #lets take a look at stores by unemployment rate.
View(Walmart_sales)
  #To see the top couple of stores, all you need to do is use the arrow on the unemployment column.
  #By visual observation, The top few stores are 12, 28 and 38.

  #lets take a more definable approach.
  #We will look at stores associated with the top 5% of unemployment values.
quantile(Walmart_sales$Unemployment, 0.95) #12.187

subsetUnemp <- subset(Walmart_sales, Unemployment > 12.187)

print(unique(subsetUnemp$Store))
  #looks like it's still just 12, 28, and 38 in the top 5%.

#Now we want to determine why this is the case. Why is the top 5% of unemployment values associated with these three locations?

  #Using our data, we will find variables with a strong correlation with unemployment.
  #First, we will try a correlation matrix.

install.packages("ggstatsplot")
library(ggstatsplot)

  #Only numeric variables

View(Walmart_sales)
myData <- Walmart_sales[3:8]



#creating correlation matrix
print(ggstatsplot::ggcorrmat(
  data = myData,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
))

  #According to our matrix, we have a pretty clear highest correlation, CPI, with a coef = -0.3.
  #This intuitively makes sense; generally inflation and unemployment have an inverse relationship

  #Lets try some regressions.

simpleLinear <- lm(Unemployment~CPI, data = myData)
summary(simpleLinear)
#For every unit of increase in the CPI, we predict the unemployment rate to fall by 0.0144 points.

linear <- lm(Unemployment~CPI + Fuel_Price + Temperature + Holiday_Flag + Weekly_Sales, data = Walmart_sales)
summary(linear)
  #Our model predicts the most influential predictor per unit increase to be Fuel Price, however the units aren't standardized.
  #This equation doesn't really show the weights of each predictor.

View(Walmart_sales)
myData2<-scale(Walmart_sales[,3:7])
myData2 <- data.frame(myData2, Walmart_sales$Unemployment)
colnames(myData2)[6]<-'Unemployment'
View(myData2)
stdlinear <- lm(Unemployment~CPI + Fuel_Price + Temperature + Holiday_Flag + Weekly_Sales, data = myData2)
summary(stdlinear)
  #Again, CPI is found to be the most influential variable, followed by temperature, followed by fuel price and weekly sales.
  #For every unit of increase in the CPI, we predict the unemployment rate to fall by 0.0144 points.





#Is there any correlation between CPI and Weekly Sales?  

  #reference our correlation matrix; there is definitely a negative correlation between CPI and weekly sales.
  
  

#How does the correlation differ when the Holiday Flag is 0 versus when the Holiday Flag is 1?

cor(Walmart_sales$Unemployment,Walmart_sales$CPI) #base R correlation

install.packages("dplyr")
library(dplyr)
Walmart_sales %>% 
  filter(Holiday_Flag == 1) %>% 
  select(Unemployment, CPI) %>% 
  cor
  #correlation: -0.29962

Walmart_sales %>% 
  filter(Holiday_Flag == 0) %>% 
  select(Unemployment, CPI) %>% 
  cor
  #correlation: -0.3021949

  #The negative correlation between Unemployment and CPI is ever so slightly weaker when the holiday flag is up.

#Why do you think Fuel Price is included in this dataset?  
  #Fuel price, while volatile in price, has relatively inelastic demand in the short term.
  #The creator of the dataset likely believed it to be a useful *loose* benchmark of short term, localized CPI.
  #The creator may also have used this dataset to examine relationships between localized fuel price and other variables in the dataset.

