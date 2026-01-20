

# install.packages(
#   c("tidyverse", "psych", "ggplot2", "ISLR2"),
#   repos = "https://cloud.r-project.org"
# )



library(tidyverse)
library(psych)
library(ggplot2)
library(ISLR2)
setwd("X:\CODING\Statistical Learning Lab\Assignemnt1\food_coded.csv");

#Fixing GPA and weight as their data is of incorrect type
food = read.csv("food_coded.csv")
str(food)
#describe(food)
food$GPA <- as.numeric(food$GPA)
food$weight = as.numeric(gsub("[^0-9.]", "", food$weight))
sum(food$weight[is.na(food$weight)])

str(food)
colSums(is.na(food))


#imputing Numeric and integer Columns
num_columns = sapply(food, is.numeric)

library(mice)
med = make.method(food)
med[num_columns] = "pmm"
med[!num_columns] = ""

impute_object = mice(food, method = med, m = 1)
food2 = complete(impute_object, action = "long")
colSums(is.na(food2))


#Check Duplicates
anyDuplicated(food2)
#food3 = food2[!duplicated(food), ]


#Return Data types and Central Tendency of Food Data
str(food2)
describe(food2)


#statistical Summary of Numerical Cols
summary(food2[,num_columns])
#dispersion of Numerical Columns
sds = sapply(food2[num_columns],sd)
sds^2


#Data type of All Columns
sapply(food, class)


#######################################################################

# Question No 2
food = food2
colnames(food)

#plotting histogram
library(ggplot2)
library(tidyr)

food$Gender = factor(food$Gender,
                      levels = c(1, 2),
                      labels = c("Male", "Female"))

food_long = food %>%
  pivot_longer(
    cols = c(waffle_calories, tortilla_calories, turkey_calories),
    names_to = "Food_Type",
    values_to = "Calories"
  )

#Histogram
ggplot(food_long, aes(x = Calories, fill = Gender)) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_grid(Food_Type ~ Gender) +
  labs(
    title = "Histogram of Calorie Perception by Food Type and Gender",
    x = "Calories",
    y = "Frequency"
  ) +
  theme_minimal()

#BoxPlot
ggplot(food_long, aes(x = Gender, y = Calories, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Food_Type, scales = "free_y") +
  labs(
    title = "Box Plot of Calorie Perception by Gender",
    x = "Gender",
    y = "Calories"
  ) +
  theme_minimal()



########################################################################

# Question No 3
library(dplyr)

#Waffle vs Tortilla
ggplot(food, aes(x = waffle_calories, y = tortilla_calories, color = Gender)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Waffle vs Tortilla Calorie Perception",
    x = "Waffle Calories",
    y = "Tortilla Calories"
  ) +
  theme_minimal()


#Waffle vs Turkey
ggplot(food, aes(x = waffle_calories, y = turkey_calories, color = Gender)) + geom_point(alpha = 0.7) + labs(
  title = "Waffle vs Turkey Calorie Perception", 
  x = "Waffle Calories",
  y = "Tortilla Calories"
) + theme_minimal()

#Tortilla vs Turkey
ggplot(food, aes(x = tortilla_calories, y = turkey_calories, color = Gender)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Tortilla vs Turkey Calorie Perception",
    x = "Tortilla Calories",
    y = "Turkey Calories"
  ) +
  theme_minimal()


######################################################################################

#Question No 4 
library(reshape2)

calorie_vars = food[, c("waffle_calories",
                          "tortilla_calories",
                          "turkey_calories")]

cor_matrix = cor(calorie_vars, use = "complete.obs")

cor_long = melt(cor_matrix)

#Create the heatmap using ggplot2
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Heatmap of Correlation Between Calorie Perception Variables",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


###########################################################################

# Question No 5

#Convert breakfast to a factor and reshape data to long format.
food = food2

food$breakfast = factor(food$breakfast,
                         levels = c(1, 2),
                         labels = c("Yes", "No"))

food_long <- food %>%
  pivot_longer(
    cols = c(waffle_calories, tortilla_calories, turkey_calories),
    names_to = "Food_Type",
    values_to = "Calories"
  )


#Create boxplots (Breakfast vs Calorie Perception)
ggplot(food_long, aes(x = breakfast, y = Calories, fill = breakfast)) +
  geom_boxplot() +
  facet_wrap(~ Food_Type, scales = "free_y") +
  labs(
    title = "Calorie Perception by Breakfast Choice",
    x = "Breakfast Choice",
    y = "Perceived Calories"
  ) +
  theme_minimal()

#Testing Dependence

#wilcox.test(waffle_calories ~ breakfast, data = food)



#############################################################################

# Question No 6

#Selecting Variable
calorie_data = food2[, c("waffle_calories",
                          "tortilla_calories",
                          "turkey_calories")]

#Correlation Matrix
cor_matrix = cor(calorie_data, use = "complete.obs", method = "pearson")
cor_matrix

#Individual Correlation coeff
cor(food2$waffle_calories, food2$tortilla_calories, use = "complete.obs")
cor(food2$waffle_calories, food2$turkey_calories, use = "complete.obs")
cor(food2$tortilla_calories, food2$turkey_calories, use = "complete.obs")


#Preparing Gender Variable
food2$Gender = factor(food2$Gender,
                       levels = c(1, 2),
                       labels = c("Male", "Female"))

#Q–Q plots for each food item, separately by gender

#1. Waffle Calories
ggplot(food2, aes(sample = waffle_calories)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Gender) +
  labs(
    title = "Normal Probability Plot of Waffle Calories by Gender",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

#2. Tortilla Calories
ggplot(food2, aes(sample = tortilla_calories)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Gender) +
  labs(
    title = "Normal Probability Plot of Tortilla Calories by Gender",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

#3. Turkey Calories
ggplot(food2, aes(sample = turkey_calories)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Gender) +
  labs(
    title = "Normal Probability Plot of Turkey Calories by Gender",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()



