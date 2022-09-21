library(httr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(stringr)
library(plotly)
library(rvest)
library(car)
library(ggfortify)
library(corrplot)

url <-'https://data.calgary.ca/resource/vdjc-pybd.json?$limit=50000&$offset=0'

business.license.data <- httr::GET(url = url)

business.license.data <- content(business.license.data, as = "text")

busn.license <- do.call("rbind.data.frame", lapply(business.license.data, fromJSON))

busn.license$comdistnm <- as.factor(busn.license$comdistnm)

# scraping the business license data

#table(busn.license$comdistnm)

#as.data.frame(table(busn.license$comdistnm))


busn.license.count <- as.data.frame(table(busn.license$comdistnm)) # taking just a count of businesses per community
names(busn.license.count) <- c("community.name", "count.businesses") # renaming columns

#scrape median household incomes in calgary
scrape_income_html <- read_html(
  "https://great-news.ca/demographics/?fbclid=IwAR0IXYkRJbOm2REaocdcfmZ_0ORh2htlbmRm-iFIu9k90NrCBbG3E0Axn08")

Table.Income <- html_nodes(scrape_income_html,
                           css = "table")

scraped_income_data <- as.data.frame(html_table(Table.Income))


scraped_income_data$Community <- toupper(scraped_income_data$Community)

#filter the income data by the communities that show up in our data frame
filtered_scraped_income_data <- scraped_income_data %>%
  filter((scraped_income_data$Community %in% busn.license.count$community.name))

filtered_scraped_income_data$Median.Household.Income <- gsub("[\\$,]", "", filtered_scraped_income_data$Median.Household.Income)

income.data <- as.data.frame(filtered_scraped_income_data$Community)

income.data$median.income <- filtered_scraped_income_data$Median.Household.Income

# Rename the community name column
names(income.data)[names(income.data) == "filtered_scraped_income_data$Community"] <- "community.name"

#this table has the median household income data
community.information <- busn.license.count %>%
  filter((busn.license.count$community.name %in% scraped_income_data$Community))

community.information <- merge(community.information, income.data, by.x = "community.name")

community.information$median.income <- as.numeric(community.information$median.income)

# Crime Rate, count of crime, and count of disorder
# Population Data is extracted from City of Calgary open data using an API ##
pop.url <- 'https://data.calgary.ca/resource/rkfr-buzb.json'
calgary.population.data.raw <- httr::GET(url = pop.url)

raw_content_population <- httr::content(calgary.population.data.raw, as = "text")

population.data <- do.call("rbind.data.frame", lapply(raw_content_population, fromJSON))

population.data$res_cnt <- as.numeric(as.character(population.data$res_cnt))

population.data <- cbind(population.data[, 1:128], population.data$multipolygon)


## This Data frame contains info for only residential communities in Calgary ##
residential <- population.data %>%
  filter(class == "Residential")

residential.communities <- unique(residential$name)

## Extracted Crime Data from Calgary Open Data ## 
url <- 'https://data.calgary.ca/resource/848s-4m4z.json?$limit=130000&$offset=0'
calgary.incident.raw.data <- httr::GET(url = url)

raw_content_incident <- httr::content(calgary.incident.raw.data, as = "text")

incident.data <- do.call("rbind.data.frame", lapply(raw_content_incident, fromJSON))

incident.data <- cbind(incident.data[ , 1:10], incident.data$geocoded_column)

## We isolated crime data to 2018 incidents, for the sake of simplicity ##

crime.data <- incident.data %>%
  filter(group_category == "Crime" & year == "2018")


## converted certain colunms from character to numeric ##
crime.data$latitude <- as.numeric(as.character(crime.data$latitude))
crime.data$longitude <- as.numeric(as.character(crime.data$longitude))
crime.data$count <- as.numeric(as.character(crime.data$count))


## Grouped crime data by community name ##
crime.by.community <- crime.data %>%
  group_by(community_name) %>%
  summarise(crime.count = sum(count))

## Filtered solely for the residential communities we defined above ## 
crime.by.community.residential <- crime.by.community %>%
  filter(community_name %in% residential.communities)

crime.by.community.residential <- crime.by.community.residential[order(crime.by.community.residential$community_name), ]

residential.communities <- unique(crime.by.community.residential$community_name)

residential <- residential %>%
  filter(name %in% residential.communities)

residential <- residential[order(residential$name), ]

## Combined the population data with the crime data ## 

residential.stats <- cbind(residential, crime.by.community.residential)

residential.stats.2018 <- cbind(residential.stats[ , 1:10], residential.stats$crime.count)

residential.stats.2018$crime.rate <- (residential.stats.2018$`residential.stats$crime.count`/residential.stats.2018$res_cnt)*100000


## We do the same thing we did above for crime but this time for disorders ## 
## Isolate the data and then merge with data with the dataframe we made above ## 
incident.data$count <- as.numeric(as.character(incident.data$count))

disorder.data <- incident.data %>%
  filter(group_category == "Disorder" & year == "2018")%>%
  group_by(community_name)%>%
  summarise(disorder.count = sum(count)) %>%
  arrange(community_name)
  
disorder.data <- disorder.data[-c(1:47),]
names(disorder.data) <- c("name", "count.disorders")

residential.stats.2018.v2 <- merge(residential.stats.2018, disorder.data, by.x = "name")

## removed some redundant data and then named the colunms ##
residential.stats.2018.v3 <- residential.stats.2018.v2[,-c(2:10)]
row.names(residential.stats.2018.v3) <- NULL
names(residential.stats.2018.v3) <- c("community.name", "crime.count", "crime.rate", "disorder.count")

community.information1 <- merge(community.information, residential.stats.2018.v3, by.x = "community.name")

cor(community.information1[,2:6])

# adding in Community Assessed Home Values
PA.Summary = read.csv('PA_Summary.csv')
names(PA.Summary) <- c("community.name", "Assessed.Value", "lat", "long") # changing names

community.information2 <- merge(community.information1, PA.Summary, by.x = "community.name") # making the merge

corr <- cor(community.information2[,2:7]) # cool looking at correlation

corrplot(corr, method = "circle", type = "upper", order = "hclust")

names(community.information2) <- c("community.name", "count.businesses", "median.income",
                                   "count.crimes", "crime.rate", "count.disorders", 
                                   "Assessed.Value", "lat", "long")

# i renamed because the # count thought it was a comment in rcode 


# regression starts here
fit <- lm(crime.rate ~ count.crimes + count.businesses + count.disorders + median.income + Assessed.Value, data = community.information2)
summary(fit)
# count disorders, count crimes, and count businesses all have high p-values. Probably due to correlation




null.model = lm(crime.rate ~ 1, data = community.information2)
full.model = lm(crime.rate ~ count.crimes + count.businesses + count.disorders + median.income + Assessed.Value, data = community.information2)

step.forward <- step(null.model, 
                     scope = list(lower = null.model, upper = full.model),
                     direction = "forward")
summary(step.forward)

step.backward <- step(full.model, direction = "backward")
summary(step.backward)
summary(step.forward)

# I will go forward with our step.backward model
# it was expected that we would cut disorders and count.businesses, due to seeing how highly correlated it was to count.crimes.

summary(step.backward)


vif(fit)

vif(step.backward)
# vif goes down with no disorder and count.business


autoplot(fit, which = 2, ncol = 1) # testing normality 
autoplot(step.backward,  which = 2, ncol = 1) #testing normality w/ step.backwards. cuts businesscount and disorder

autoplot(fit, which = 4, ncol = 1) #cooks distance outliers
autoplot(step.backward, which = 4, ncol = 1) # cooks distance w/ step.backwards. cuts businesscount and disorder



cooks.D <- as.vector(cooks.distance(step.backward))
p <- 3 # Number of predictors in our model. 
n <- nrow(community.information2) # n is the number of observations


influential.outliers <- which(cooks.D > qf(0.05, p + 1, n - p -1))

community.information2[influential.outliers, ]
# 4 major outliers. Beltline makes sense



# removing the MAJOR outliers 
community.information3 <- community.information2[-influential.outliers, ]
fit.no.outliers <- lm(crime.rate ~ count.crimes + median.income + Assessed.Value, data = community.information3)
summary(fit.no.outliers)


autoplot(fit.no.outliers, which = 2, ncol = 1) # testing normality

autoplot(fit.no.outliers, which = 4, ncol = 1) #cooks distance outliers

cooks.D <- as.vector(cooks.distance(fit.no.outliers))
p <- 3 # Number of predictors in our model. 
n <- nrow(community.information3) # n is the number of observations


influential.outliers <- which(cooks.D > qf(0.05, p + 1, n - p -1))

community.information3[influential.outliers, ]
# as we can see, with the cook's distance plot as well, this outlier is not nearly as influential as the previous 4 outliers.
# this cook's distance plot only goes past 0.15 for its influential outlier, as opposed to going past 4 cook's distance in the previous analysis
# if we compare the two normality qqplot's we can see that removing the outliers just steepens the normality plot. It does not
# fix the normality issues the plot is still only mostly normal within the first standard deviation. (-/+)

# removing the outliers dropped the R-squared by 0.2, which does drop the accuracy of our fit, but shows us how much the 
# outliers were affecting the regression


# this is the Proportion tests


hoods <- unique(community.information3$community.name)

hoods1 <- residential.stats.2018.v2 %>%
  filter(name %in% hoods) %>%
  arrange(name)

community.information3$population <- hoods1$res_cnt


total.crime <- sum(community.information3$count.crimes)
total.population <- sum(community.information3$population)

for(i in 1:nrow(community.information3)){
  results <- prop.test(x = c(community.information3[i,4], total.crime), n = c(community.information3[i,10], total.population), alternative = "greater")
  community.information3[i,11] <- results$p.value
}


## This dataframe shows the top five worst neighbourhoods in Calgary for Crime ## 
greatercrime <- community.information3[c(which(community.information3$V11 <= 0.05)), ]
greatercrime <- greatercrime[order(greatercrime$crime.rate, decreasing = TRUE), ]
greatercrime <- greatercrime[1:5, ]

for(i in 1:nrow(community.information3)){
  results <- prop.test(x = c(community.information3[i,4], total.crime), n = c(community.information3[i,10], total.population), alternative = "less")
  community.information3[i,12] <- results$p.value
}


## This dataframe shows the top five best neighbourhoods in Calgary for Crime ## 
lowercrime <- community.information3[c(which(community.information3$V12 <= 0.05)), ]
lowercrime <- lowercrime[order(lowercrime$crime.rate), ]
lowercrime <- lowercrime[1:5, ]


## This is the code for the 3d Scatter Plots ##
Sys.setenv("plotly_username"="btma431group")
Sys.setenv("plotly_api_key"="YIIqXN1J7J5jhCLUW3DQ")


crime.rate.plot <- plot_ly(community.information3, x = community.information3$median.income, y = community.information3$Assessed.Value, z = community.information3$crime.rate,
                           marker = list(color = "red")) %>%
  layout(title = "Crime, Income and Property Value", scene = list(xaxis = list(title = "Median Income"), yaxis = list(title = "Property Value"), zaxis = list(title = "Crime Rate")))

chart_link <- api_create(crime.rate.plot, filename = "3d Scatter Crime")
chart_link

community.information3$disorder.rate <- (community.information3$count.disorders/community.information3$population)*100000

disorder.rate.plot <- plot_ly(community.information3, x = community.information3$median.income, y = community.information3$Assessed.Value, z = community.information3$disorder.rate,
                              marker = list(color = "blue")) %>%
  layout(title = "Disorders, Income and Property Value", scene = list(xaxis = list(title = "Median Income"), yaxis = list(title = "Property Value"), zaxis = list(title = "Disorder Rate")))

chart_link2 <- api_create(disorder.rate.plot, filename = "3d Scatter Disorder")
chart_link2