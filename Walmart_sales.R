#Library
library(dplyr)
library(lubridate)
library(ggplot2)

#Import data
data<-read.csv("~/Documents/R/Walmart_Sales.csv")


str(data)
head(data)


#Data is sorted first by store number (ascending) and second by date (ascending)

#first put the date on the right format DATE, 
data$Date<-as.Date(data$Date, format="%d-%m-%Y")

#Date is in the format MM-DD-YYYY
#in R once we put the format date we cant choose how the format will be, per defaulr R keep the format YYYY-MM-DD 

#Ensure that there is no missing data
sum(is.na(data)) #there is no missing data

#Data is sorted first by store number (ascending) and second by date (ascending)
data<-arrange(data, Store, Date) 
head(data)

#Weekly Sales is rounded to the nearest 2 decimal places
#Temperature is rounded to the nearest whole number
#Fuel Price is rounded to the nearest 2 decimal places
#CPI is rounded to the nearest 3 decimal places
#Unemployment is rounded to the nearest 3 decimal places
data<-data %>%
  mutate(
    Weekly_Sales=round(Weekly_Sales,2),
    Temperature=round(Temperature,0),
    Fuel_Price=round(Fuel_Price, 2),
    CPI=round(CPI,3),
    Unemployment=round(Unemployment,3)
  )
head(data)

#Which holidays affect weekly sales the most?
#filter by holiday_flag=1 and sort by weekly_sales to find out which holidays affect weelky sales the most

holiday <- data %>%
  filter(Holiday_Flag==1) %>% 
  arrange(desc(Weekly_Sales)) %>%
  mutate(Month=month(Date, label=T)) %>%
  group_by(Month) 
head(holiday)


top_sales <- data %>%
  filter(Holiday_Flag==1) %>% 
  mutate(Month=month(Date, label=T)) %>%
  group_by(Month) %>%
  summarise(avg_weekly_sales=mean(Weekly_Sales),
          max_weekly_sales=max(Weekly_Sales),
          sum_weekly_sales=sum(Weekly_Sales)) %>%
  arrange(desc(max_weekly_sales)) 
head(top_sales)

plot_sales<- ggplot(top_sales, aes(x=Month, y=avg_weekly_sales)) + 
  geom_col(fill="blue")
plot_sales
#November and February holidays affect the most weekly sales, seems to be thanksgiving, st valentine day

#Which stores in the dataset have the lowest and highest unemployment rate?  What factors do you think are impacting the unemployment rate?

Unemployment_rate <- data %>%
  group_by(Store) %>%
  summarise(avg_unemployment=mean(Unemployment)) %>%
  arrange(desc(avg_unemployment))
head(Unemployment_rate,1)
tail(Unemployment_rate,1)
#store 12 has the hightest unemployment rate and store 40 and 23 had the lowest unemployment rate

#Is there any correlation between CPI and Weekly Sales? 

cor(data$CPI, data$Weekly_Sales) #-0.07263407 no correlation 

#How does the correlation differ when the Holiday Flag is 0 versus when the Holiday Flag is 1?
holiday<-filter(data, Holiday_Flag==1)
holiday<-arrange(holiday,Date, Weekly_Sales) 
head(holiday)
cor(holiday$CPI, holiday$Weekly_Sales) #-0.08097043

no_holiday<-filter(data, Holiday_Flag==0)
holiday<-arrange(no_holiday,Date, Weekly_Sales) 
head(no_holiday)
cor(no_holiday$CPI, no_holiday$Weekly_Sales) #-0.0719394

ggplot(data, aes(x=CPI, y=Weekly_Sales, color=factor(Holiday_Flag)))+
  geom_point(alpha=0.3)+
  geom_smooth(method = "lm", se=F)+
  labs(title="Weekly sales vs CPI by holiday", color="holiday flag")

cor(data$Fuel_Price, data$Weekly_Sales) 
cor(data$Weekly_Sales, data$Holiday_Flag)
