# talk about data frames, let's import some 
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")
library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
head(surveys)
show(surveys)
str(surveys)
names(surveys)
colnames(surveys)
row.names(surveys)
summary(surveys)

# indexing and subsetting
surveys (1,6)
surveys[1,6]
surveys[1,]
surveys [,1]
surveys [c(1, 2, 3 ), c(5,6)]
surveys [1:3, 5:6]
surveys [,-1]
surveys [, "sex"]
surveys ["sex"]
surveys$plot_id

surveys_200 <- surveys [200,]
nrow(surveys_200)
nrow(surveys)
surveys_last <- surveys [34786,]
surveys_last2 <- surveys [nrow(surveys),]
