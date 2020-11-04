#import packages
rm(list=ls()); library(tidyverse); library(dplyr);

#import data source
mydata <- read.csv("data/Final_All in one.csv", header=TRUE)
mydata %>% head; sapply(mydata, class)


#cleaning table (omitted 650 rows out of 10k rows)
##changing  character to numeric
mydata$Rating=as.numeric(mydata$Rating)
mydata$SizeMin=as.numeric(mydata$SizeMin); mydata$SizeMax=as.numeric(mydata$SizeMax)
mydata$RevenueMin=as.numeric(mydata$RevenueMin);mydata$RevenueMax=as.numeric(mydata$RevenueMax)
mydata$SalaryEstimateMin=as.numeric(mydata$SalaryEstimateMin);mydata$SalaryEstimateMax=as.numeric(mydata$SalaryEstimateMax)

sapply(mydata, class)

##converting -1 to 'FALSE' in EasyApply column
mydata$EasyApply[mydata$EasyApply %in% "-1"]="FALSE"
mydata %>%
  filter(EasyApply=="TRUE")

##Introducing new columns (SizeCategory, SizeAvg, RevenueAvg, SalaryAvg) by introducting mydata2
mydata2= mydata %>% 
  mutate(SizeCategory = ifelse(SizeMax<=50, "Startup", ifelse(SizeMax<=1000, "Medium", "Large"))) %>% 
  mutate(SalaryAvg=(SalaryEstimateMin+SalaryEstimateMax)/2) %>% 
  mutate(RevenueAvg=(RevenueMin+RevenueMax)/2) %>% 
  mutate(SizeAvg=round((SizeMin+SizeMax)/2, digits=0)) %>% 
  mutate(RiskFactorRevenue= ifelse(RevenueAvg>100000000,3, ifelse(RevenueAvg>10000000,2,1))) %>%
  mutate(RiskFactorSize= ifelse(SizeCategory=="Startup",3, ifelse(SizeCategory=="Medium",2,1)))%>%
  mutate(RiskFactorType= ifelse(Typeofownership=="Government" | Typeofownership=="Nonprofit Organization",1, ifelse(Typeofownership=="Hospital" | Typeofownership=="College / University" | Typeofownership=="School / School District",2,3))) %>%
  mutate(RiskFactor=RiskFactorSize*RiskFactorType/RiskFactorRevenue) %>% 
  mutate(JobCategoryNumeric = ifelse(JobCategory=="Business Analyst", 1, ifelse(JobCategory=="Data Analyst", 2,3))) %>% 
  mutate(SizeCategoryNumeric=ifelse(SizeMax<=50, 1, ifelse(SizeMax<=1000, 2, 3))) %>% 
  mutate(RevenueMillion=format((RevenueMin+RevenueMax)/(2*1000000), digits=0))


sapply(mydata2, class)
mydata2

##getting rid of NA values (-1)
finaldata=mydata2%>% na.omit () %>%
  filter(!SizeMin==-1 & !SizeMax==-1 & !Founded==-1 & !RevenueMax==-1 & !RevenueMin==-1)

#save images for backup
# save(list="finaldata", file="finaldata.Rdata");load(file="finaldata.RData")
save.image("everything.Rdata")

#save final data
saveRDS(finaldata, "finaldata.rds"); saveRDS(finaldata, "Shiny_app_Hojun/finaldata.rds"); saveRDS(finaldata, "Shiny_app_Utkarsh/finaldata.rds")
saveRDS(finaldata, "C:/Users/wjdgh/Documents/R/win-library/4.0/finaldata.rds")

#read data source from R file
finaldata=readRDS("finaldata.rds")


#user-defined function
mode(finaldata$LocationCity)
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

calculate_min <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.min(tabulate(match(x, uniqx)))]
}