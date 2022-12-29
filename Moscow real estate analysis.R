# ------------------------------RESEARCH BY------------------------------------
#------------------------ABHISHIKT EMMANUEL PRAKASH----------------------------
# Guided by
# Christ (Deemed to be University) Bengaluru
# Department of Statistics:
# Ms. Kavitha R, Mr. Dibu A
# Bangalore - 29
# -----------------------------------------------------------------------------
# The secondary data set for this statistical study has been obtained from Kaggel.
# This data set includes information about one room apartment real estate in Moscow
# The aim of this statistical analysis is to make use of various R functionalities 
# such as plot, bar plot, histogram, skewness, kurtosis, standard deviation and 
# the summary function to study the real estate market in Moscow.
# ------------------------------------------------------------------------------
# This data set includes the following variables:
# metro (nominal) : The nearest metro station to to the apartment
# price (numerical) : The rent price for the apartment
# way (nominal) : Mode of transportation to reach metro station (on foot or by public transport)
# views (numerical) : The number of views for each apartment
# provider (nominal) : A person or agency who is renting apartment
# fee_percent (numerical) : Fee percent of an agency or realtor
# storey (numerical) : The storey, where the apartment located
# minutes (numerical) : Time to reach nearest metro station
# storeys (numerical) : The total number of storeys in a building
# living_area (numerical) : Square foot of the living area
# kitchen_area (numerical) : Square foot of the kitchen area
# total_area (numerical) : Total square of each apartment
# total_area_description (ordinal) : The size of apartment is Large, Medium or small
# distance_description (ordinal) : The distance from the metro station is far or near
library(readxl)# import read excel library 
Data_temp <- read_excel("/Users/abhishikt_mac/Downloads/real_estate_moscow.xlsx")# import excel data set
data <- data.frame(Data_temp)# create data frame
library(ggplot2)# import ggplot2 library
library(moments) # import the moments library
attach(data)
# Basic operations on all the columns in the data set---------------------------

#summary of the data set i.e for each variable
summary(data)

# Basic structure of the data set
str(data) 
# Get the top six rows of the data set
head(data)

#shape of the data set
dim(data)

# Metro stations-----------------------------------------------------------------
# Summary of the variable metro i.e all the metro stations with the number of real estate options near them
head(summary(factor(metro)))

# Prices for each listed real estate---------------------------------------------
# summary
summary(price)

#box plot comparing total area and price
boxplot<-ggplot(data,aes(y=total_area, x = price, fill = total_area_description))+geom_boxplot() + ggtitle("Box plot comparing total area and price")
boxplot

# Inference: Most of the real estate area for large properties is between 35sqft to 40sqft mostly below 40sqft which costs anywhere between 200K to 350K, while for medium properties size range don't vary much and are mostly around 33sqft which costs around 150K to 175K, also the size for small properties is less than 10sqft which costs around 175K to 200K.

# Now we know the minimum, maximum price for an apartment and also the mean price, median price, 1st and 3rd quartile prices
sd(price)
# Such a large standard deviation indicates that the prices of real estate vary highly form the mean
var(price)
# Similarly a high varience is indicating that the prices of real estate in Moscow vary highly from the mean and also there is a wast difference within the prices.
#kurtosis
kurtosis(price)
# A positive kurtosis value represents that the data is highly peaked
#skewness
skewness(price)
# Since the value of skewness is 6.04 the data is highly skewed
# range of real estate prices in Moscow
range(price)
# Histogram to visually represent the price distribution
ggplot(data, aes(x=price)) + geom_histogram( binwidth=20000, fill="#69b3a2", color="#e9ecef", alpha=0.9) + ggtitle("Histogram for real estate prices in Moscow") 

# Mode of transportation--------------------------------------------------------
summary(way)

ggplot(data, aes(x = "", y =way,fill = as.factor(way))) +
  geom_col() +
  coord_polar(theta = "y")+ggtitle("Mode of transport")+
  labs(x="",y="",fill="Mode of transport")

ggplot(data=data,aes(x=way,fill=as.factor(way)))+
  geom_bar(position='dodge')+ggtitle("Bar graph for mode of transport")+labs(x="Mode of transport",y="Frequency",fill="Mode of transport")+
  theme(legend.position='left')
# Inference : Most of the apartments are at walking distance from metro stations

# Number of views for the site--------------------------------------------------
summary( views)
# Inference : Median value fro views is 103

ggplot(data=data, aes(x=factor(total_area_description), y=views,
                      fill=factor(total_area_description))) + 
  geom_boxplot(notch=T) + 
  labs(title="Boxplot showing the relationship between area of flat and number of views", 
       fill = "Flat size", x="Flat size") 

# Inference : This shows medium size flats are in the highest demand and experience a median view count of 500.

# Provider of the apartment-----------------------------------------------------
summary(factor( provider))

ggplot(data=data, aes(x = factor(provider), fill = factor(provider),)) +geom_bar(position="dodge")+ labs(title = "Barplot showing provider distribution", x = "Provider", fill = "Provider") 
# Inference most of the apartments in Moscow are rented out by Reltor or owners
summary(views)

ggplot(data=data, aes(x=storey, y = total_area ,col=provider)) + 
  geom_point() + 
  labs(title="Relationship between number of storey and total area of flat ") + 
  facet_grid(~total_area_description) 

#Inference : Maximum data for large is concentrated towards total area 40sqft, while that for medium is around 30sqft and that of small properties is less than 10sqft
# Fee percentage charged--------------------------------------------------------
summary( fee_percent)
# Inference : 50 percent is the average fee percentage charged by the broker
var( fee_percent)
# Inference: Fee percentage has a high variance i.e value of fee percentage can vary a lot

# Number of stories-------------------------------------------------------------
summary( storey)
# Inference: Number of stories mostly lie between 6 to 9 but some apartments might go up to 613 stories

# Living area-------------------------------------------------------------------
summary( living_area)
# Inference : Average living in Moscow apartments is 20 square foot
range( living_area)
# Inference : Living area ranges from 6 square foot to 37 square foot

# Kitchen area------------------------------------------------------------------
summary( kitchen_area)
#Inference : The average kitchen area in Moscow apartments is 10 square foot
range( kitchen_area)
#Inference : The kitchen area in Moscow apartments varies from 3 square foot to 37 square foot

# Living area-------------------------------------------------------------------
summary( total_area)
range( total_area)
# Inference : The average area of apartments in Moscow is 37 square foot whereas the area might vary anywhere from 1 square foot to 57 square foot 

# Area description--------------------------------------------------------------
summary(factor( total_area_description))

ggplot(data=data,aes(x=total_area_description,fill=as.factor(total_area_description)))+
  geom_bar(position='dodge')+ggtitle("Bar graph for real estate size ")+labs(x="Real estate size",y="Frequency",fill="Legend")+
  theme(legend.position='left')
# Inference : This bar graph shows that most of the real estate in Moscow has an area greater than 35 square foot and very few real estate have an area less than 20 square foot

# Probability of finding a large real estate in Moscow
paste("There is a" ,round(((table( total_area_description)["LARGE"])/nrow(data))*100, 2), "% probability of finding a LARGE appartment in Moscow")

paste("There is a" ,round(((table( total_area_description)["MEDIUM"])/nrow(data))*100, 2), "% probability of finding a MEDIUM appartment in Moscow")

paste("There is a" ,round(((table( total_area_description)["SMALL"])/nrow(data))*100, 2), "% probability of finding a SMALL appartment in Moscow")

# Distance from metro description-----------------------------------------------
summary(factor( distance_description))

ggplot(data=data,aes(x=distance_description,fill=as.factor(distance_description)))+
  geom_bar(position='dodge')+ggtitle("Bar graph distance discription ")+labs(x="Distance",y="Frequency",fill="Legend")+
  theme(legend.position='left')

# Inference : Half of the real estate in Moscow lies such that travel time to metro stations is less than 7 minutes whereas half of the real estate in Moscow lies far from any metro station

# Correlation between price and total area--------------------------------------
cor( price,  total_area)
# Inference: This shows that the prices of real estate and the area of apartment are moderately correlated which means more the area more the prices

# Correlation between price and total area--------------------------------------
cor( price,  views)
# Inference: This shows that the prices of real estate and the number of views are negatively correlated which means higher the number of views lower are the prices

#-------------------------------------------------------------------------------
# HO: Data follows normal distribution 
# HA: Data does not follow normal distribution

shapiro.test(views)

#Inference: 
# Since the p-value is lesser than 0.05, we must accept HA and conclude that the 
# variable is not a part of the normal distribution

# Finding the Minimum and maximum of each variable using the lapply() and 
# sapply() functions: 

lapply(data, max)
sapply(data, min)
# Inference: 
# The lapply and sapply functions are used in order to apply a given function 
# and result in it's value being given out. 

#-------------------------------------------------------------------------------
# Exploring the mean of various variables using the tapply() function:  

tapply(price, total_area_description, mean)
# Inference: 

tapply(total_area, distance_description , mean)
# Inference: 

#------------------------------------
# Analyzing the relationship between Provider and total area results 
# using the Chi-square test

# HO: There is no association between BMI result and Diabetes results
# HA: There is an association between BMI result and Diabetes results
chisq.test(provider, total_area_description)

# Inference: 
# Chi-square test is used to find the association between two categorical 
# variables. Since the p-value is lesser than 0.05, we can conclude that there
# is no association between Provider and total area.

#------------------------------------

# Finding linear regression between views and price of real estate
regression = lm(views~price)
regression
#Inference:
summary(regression)

# Finding linear regression between living area and price
regression = lm(living_area~price)
regression
#Inference:
summary(regression)

# Conclusion--------------------------------------------------------------------
# With the help of this statistical study we are able to identify that in Moscow most of the real estate prices range from 14000 to 38000, and also most apartments are at a walking distance from the nearest metro station, most of the sites experience a view rate upto 500, most of the sites are rented out by either retailer or the owner and also most of the properties are large. We also discovered that higher the number of views lower are the prices and more the area of the apartment more is the price. This assignment has been really helpful in understanding the R functionality and also the use of various R libraries, such as ggplot 2, moments, readxl. 

