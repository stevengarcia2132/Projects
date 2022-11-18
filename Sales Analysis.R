library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)

#part 1 Data sorting and cleaning
sales <- read.csv("~/2022 SCHOOL/FALL 2022/Sales2022.csv")

sales <- select(sales, -c(Balance,Last.Name,Phone.No., Credit.Balance,
                          PL,Sales.External.Id,Marketing.Type,Ext..Account.Number))
sales <- as_tibble(sales)

sales$Account.Start.Date <- mdy(sales$Account.Start.Date)
sales$Contract.Creation.Date <- mdy(sales$Contract.Creation.Date)
sales$Initial.Date <- mdy(sales$Initial.Date)

cols <- c("Customer.Status","Sales.Status", "Autopay","Contract.Name")
sales[cols] <- lapply(sales[cols],factor)


#Part 2 Data Visualization
#total sales by month
sales %>%
  filter(Customer.Status == "Active") %>%
  ggplot(aes(x = month(Account.Start.Date))) + geom_bar(fill = "brown")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  theme(aspect.ratio = 1)+
  labs(x = "Months" , y= "Accounts created", title = "Accounts Created by \n month for Steven")+
  scale_x_continuous(breaks = c(6,7,8),labels=c("June","July","August"))+
  ylim(0,70)
  
  
#total sales by day of week
sales %>%
  filter(Customer.Status == "Active") %>%
  ggplot(aes(x=weekdays(Account.Start.Date))) + geom_bar(fill= "brown")+
  xlim("Monday", "Tuesday","Wednesday", "Thursday", "Friday","Saturday")+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,40)+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  labs(x = "Day of week", y = "Accounts created", title = "Accounts created by \n day of week")
  


month(sales$Account.Start.Date)
days <- weekdays(sales$Account.Start.Date)

for(i in 1:length(days)){
  if(str_detect(days[i], "Satuday") == TRUE){
  print("yessir")
  }
}
#City sales
sales %>%
  str_detect()
head(sales)


