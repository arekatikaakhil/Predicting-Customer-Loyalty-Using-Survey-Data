#install.packages("ggplot2", "corrplot"))
library(ggplot2)
library(corrplot)


file_path <- ("/Users/nithishbm/Course Work/STAT515/Final Project/Starbucks satisfactory survey.csv")
starbucks_data <- read.csv(file_path)

##CLEANING DATA
#Display the  column names
colnames(starbucks_data)
str(starbucks_data)

#Load the dplyr package
library(dplyr)

#Select specific columns required for research questions
selected_data <- starbucks_data %>%
  select(The.nearest.Starbucks.s.outlet.to.you.is....,
         How.do.you.usually.enjoy.Starbucks.,
         Are.you.currently.....,
         How.likely.you.will.choose.Starbucks.for.doing.business.meetings.or.hangout.with.friends.,
         You.rate.the.WiFi.quality.at.Starbucks.as..)

#Removing all the duplicates
my_data <- distinct(selected_data)

#Re-Checking the column names
colnames(starbucks_data)


#Display the cleaned data
print(my_data)

#Creating new dataset by removing unwanted columns and duplicates 
my_data1 <- read_csv("/Users/nithishbm/Course Work/STAT515/Final Project/clean_survey_analysis.csv")

spec(my_data)

#Summary and statistics of given data
summary(my_data1)

##Research Question 1

##What are the preferences of students compared to employed individuals 
#on choosing starbucks?

#summary table
summary_table <- my_data1 %>%
  group_by(Are.you.currently.....) %>%
  summarise(count = n())

library(ggplot2)

#Plotting a  Bar plot
bar_plot <- ggplot(summary_table, aes(x = reorder(Are.you.currently....., -count), y = count, fill = Are.you.currently.....)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Distribution of Preferences Between Students and Employed Individuals",
       x = "Occupation",
       y = "Count",
       fill = "Occupation") +
  theme_minimal() 

# Print the bar plot
print(bar_plot)


##Research Question 2
#What impact does the proximity to the closest starbucks location 
#have on dining preferences?

#load library ggplot2
library(ggplot2)

# Defining custom order for distances
distance_order <- c("within 1km", "1km - 3km", "more than 3km")

# using custom order to convert variable into factor
my_data1$The.nearest.Starbucks.s.outlet.to.you.is.... <- factor(
  my_data1$The.nearest.Starbucks.s.outlet.to.you.is....,
  levels = distance_order,
  ordered = TRUE
)

# Plotting using ordered bars
library(ggplot2)

ggplot(my_data1, aes(x = The.nearest.Starbucks.s.outlet.to.you.is...., fill = How.do.you.usually.enjoy.Starbucks.)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Dining Choice Based on Distance to Nearest Starbucks Outlet",
       x = "Distance to Nearest Starbucks Outlet",
       y = "Count",
       fill = "Dining Choice") +
  theme_minimal()


##Research Question 3
#What factors including price wifi quality, ambiance rating and service
#rating help predict wether or not customers will return to starbucks?

library(ggplot2)
library(corrplot)


file_path <- "Starbucks satisfactory survey.csv"
starbucks_data <- read.csv(file_path)

file_path <- ("/Users/nithishbm/Course Work/STAT515/Final Project/clean_survey_analysisnew.csv")
starbucks_data <- read.csv(file_path)

str(starbucks_data)


head(starbucks_data)


# Loading the MASS package
library(MASS)

# Fitting the ordinal model
ordinal_model <- polr(as.ordered(rateservice) ~ situational, data = starbucks_data)

summary(ordinal_model)

ggplot(starbucks_data, aes(x = rateservice, 
                           y = situational)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Service at Starbucks", y = "Probablity of choosing Starbucks 
       for Meetings") +
  ggtitle("Relationship between Service and Likelihood of 
          Choosing Starbucks for Meetings/Hangouts")

##Research Question 4
#How does the quality of service at starbucks impact the probability of
#customers choosing it for meetings or hangouts?

#Loading libraries
library(ggplot2)
library(corrplot)
library(tidyr)
library(dplyr)
library()


file_path <- ("/Users/nithishbm/Course Work/STAT515/Final Project/Starbucks satisfactory survey copy.csv")
starbucks_data4 <- read.csv(file_path)

str(starbucks_data4)

Starbucks <- starbucks_data4 %>% select(-Timestamp)

# Rename the columns
colnames(starbucks_data4) <- c('Gender', 'Age', 'Status', 'Income', 'Frequency', 'Method', 
                               'timepervisit', 'nearest', 'membership', 'fequencyofpurchase', 
                               'spending', 'comparerate', 'pricerate', 'promotion', 
                               'rateambiance', 'Wifi', 'rateservice', 'situational', 
                               'source', 'loyalty')

colnames(starbucks_data4)

write.csv(Starbucks, "clean_survey_analysisnew.csv", row.names = FALSE)

starbucks_data4 <- read.csv( "clean_survey_analysisnew.csv")

starbucks_data4 <- na.omit(starbucks_data4)



summary(starbucks_data4)


starbucks_data4$loyalty <- ifelse(trimws(starbucks_data4$loyalty) == 'Yes', 1, 0)

unique(starbucks_data4$loyalty)

library(caret)


# Select predictor variables
x <- starbucks_data4[, c('comparerate', 'pricerate', 'promotion', 'rateambiance', 'Wifi', 'rateservice')]
x

x_scaled <- scale(x)

# Selecting variable for outcome
y <- starbucks_data4$loyalty

# Fitting logistic regression model
model <- glm(y ~ ., family = binomial, data = as.data.frame(x_scaled))

# Print coefficients
print(coef(model))

# Creating diagnostic plots
par(mfrow = c(2, 2))  # Set up a 2x2 plot grid
plot(model)

