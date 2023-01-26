library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(knitr)
library(scales)

```{r Data importing and cleaning, include=FALSE}

compSales <- read_xlsx("~/2022 SCHOOL/FALL 2022/CompanySales.xlsx")

compSales <- as_tibble(compSales)



cols <- c("State" ,"Customer Status","Sales Status", "Autopay","Contract Name",
          "Primary Service Type", "Marketing Type", "Service Frequency", "Sales Rep")
compSales[cols] <- lapply(compSales[cols],factor)
```

# Pest Control Sales Compared to Lawn Care Sales
```{r Lawn care and pest sales, echo=FALSE}
LawnPest <- compSales %>%
  select(`Primary Service Type`) %>%
  group_by(`Primary Service Type`) %>%
  mutate(Sales = n()) %>%
  distinct(`Primary Service Type`, .keep_all = TRUE)

revenueLawn <- compSales %>%
  filter(`Primary Service Type` == "Lawn/Weed") %>%
  summarise(Total_Revenue = sum(`Contract Value`))

revenuePest <- compSales %>%
    filter(`Primary Service Type` == "Comprehensive Residential Treatment") %>%
    summarise(Total_Revenue = sum(`Contract Value`))


totalRevenue <- data.frame(ServiceType = c("Lawn/Weed", "Comprehensive Pest Control"),
                           Revenue = c(115071,327372))
options(scipen = 999)
ggplot(data = totalRevenue)+
  geom_col(aes(x = ServiceType, y = Revenue, fill = ServiceType),show.legend = F)+
  scale_y_continuous(label=comma, limits=c(min(0),max(400000)))+ 
  geom_text(aes(x = ServiceType, y = Revenue, label= comma(Revenue)),vjust=-1)+
  theme(aspect.ratio = 1)+
  labs(title = "Revenue of Pest Control and Lawn Care")
``` 

# Highest overall contract value by salesman
```{r Analysis, echo=FALSE, message=FALSE, warning=FALSE}
HighestContract <- compSales %>%
  group_by(`Sales Rep`) %>%
  select(`Contract Value`) %>%
  arrange(desc(`Contract Value`)) %>%
  distinct(`Sales Rep`, .keep_all = TRUE) %>%
  ungroup()

colnames(HighestContract) <- c("Salesman","Contract Value") 

kable(HighestContract)

```

## Total Accounts Created
```{r Accounts created by salesman, echo=FALSE}
ggplot(data = compSales) +
  geom_bar(aes(x = `Sales Rep`, color = `Sales Rep`, fill = `Sales Rep`),
           show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Salesman", y = "Accounts Created")+
  geom_text(stat='count',aes(x = `Sales Rep`, label=..count..),vjust=-1)+
  ylim(0,600)

```

## Revenue Created by Salesman
```{r Revenue created by Saleman, echo=FALSE}
revenueSalesman <- compSales %>%
  group_by(`Sales Rep`) %>%
  summarise(Revenue = sum(`Contract Value`)) %>%
  ungroup()
  

ggplot(data = revenueSalesman) +
  geom_col(aes(x= `Sales Rep`, y = Revenue, fill = `Sales Rep`),show.legend = F)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(x = `Sales Rep`, y = Revenue, label= comma(Revenue)),vjust=-1)+
  scale_y_continuous(label=comma, limits=c(min(0),max(200000)))

```

## Comparison of Contract Values
```{r avg contract value, echo=FALSE}
avgCV <- compSales %>%
  group_by(`Sales Rep`) %>%
  summarise(Revenue = mean(`Contract Value`)) %>%
  ungroup


avgPest <- compSales %>%
  group_by(`Sales Rep`) %>%
  filter(`Primary Service Type` == "Comprehensive Residential Treatment") %>%
  summarise(Revenue = mean(`Contract Value`)) %>%
  ungroup

avgLawn <- compSales %>%
  group_by(`Sales Rep`) %>%
  filter(`Primary Service Type` == "Lawn/Weed") %>%
  summarise(Revenue = mean(`Contract Value`)) %>%
  ungroup

ggplot(data = avgCV) +
  geom_col(aes(x= `Sales Rep`, y = Revenue, fill = `Sales Rep`),show.legend = F)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(x = `Sales Rep`, y = Revenue, label= comma(Revenue)),vjust=-1)+
  ylim(0,600)+
  labs(title = "Average Contract Value")

mean(avgCV)


ggplot(data = avgPest) +
  geom_col(aes(x= `Sales Rep`, y = Revenue, fill = `Sales Rep`),show.legend = F)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(x = `Sales Rep`, y = Revenue, label= comma(Revenue)),vjust=-1)+
  ylim(0,600)+
  labs(title = "Average Pest Control Contract Value")

ggplot(data = avgLawn) +
  geom_col(aes(x= `Sales Rep`, y = Revenue, fill = `Sales Rep`),show.legend = F)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(x = `Sales Rep`, y = Revenue, label= comma(Revenue)),vjust=-1)+
  ylim(0,600)+
  labs(title = "Average Lawn Care Contract Value")

```
##Sales by day of Week
```{r dates}

daysofWeek <- compSales %>%
  summarize(day = weekdays(`Contract Creation Date`))

monday <- daysofWeek %>%
  filter(day == "Monday")
nrow(monday)


ggplot(data = compSales)+
  geom_bar(mapping = aes(x= weekdays(`Contract Creation Date`),fill = weekdays(`Contract Creation Date`)),show.legend = F) + 
geom_text(stat='count',aes(x = weekdays(`Contract Creation Date`), label=..count..),vjust=-1)+
  ylim(0,350)+
  labs(title ="Sales by Day of Week")+
  xlab("Days of Week")



