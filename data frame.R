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

#set a factor
surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)
surveyssex <- factor(surveys$sex)
levels(surveyssex)
surveystx <- factor(surveys$taxa)
surveysgen <- factor(surveys$genus)
levels(surveystx)
levels(surveysgen)

# change in the original frame!
surveys$taxa <- factor(surveys$taxa)
surveys$genus <- factor(surveys$genus)
surveys$sex <- factor(surveys$sex)

# count the factors
nlevels(surveystx)

# count the rabbit
sum(surveystx=="Rabbit")

# ask for summary of the 
summary(surveystx)
summary(surveys)

# convert factors

year_fct <- factor(c(1990, 1983,1977,1997))
as.numeric(year_fct)
as.numeric(as.character(year_fct))

# extract levels, convert in to numbers and then the square bracket contains the 
as.numeric(levels(year_fct))[year_fct]
plot(surveys$sex)
summary(surveys$sex)           
sex <- surveys$sex
levels(sex)

# there are some NA
sex <- addNA(sex)
levels(sex)

# how can we rename NA, we use indexing
levels (sex) [3] <- "undetermined"
levels(sex)
sex
plot(sex)

# change M and F in male female
levels(sex)
levels (sex) [1] <- "female"
levels (sex) [2] <- "male"
levels(sex)
levels (sex) [1:2] <- c("female","male")
plot(sex)

#change order -something missing here-
sex <- factor(sex, c(levels = "undetermined", "male", "female"))
plot(sex)

tidy