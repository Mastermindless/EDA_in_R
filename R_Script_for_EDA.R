# Improved R Script for Data Analysis

# Loading Libraries
library(tidyverse)    # For data manipulation and visualization
library(readr)        # For reading CSV files
library(ggplot2)      # For advanced data visualization
library(corrplot)     # For correlation plots
library(ggpubr)       # For publication ready plots
library(gridExtra)    # For arranging multiple plots
library(skimr)        # For summary statistics
library(caret)        # For data preprocessing

# Loading the Dataset
df <- read_csv("titanic.csv")

# Enhanced Data Inspection
glimpse(df)
summary(df)
skim(df)

# Checking for Missing Values
missing_data <- colSums(is.na(df))
missing_data

# Data Structure and Types
str(df)

# Distribution of Numerical and Categorical Variables
ggplot(df, aes(x = Age)) + geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() + labs(title = "Distribution of Age")

ggplot(df, aes(x = Sex)) + geom_bar(fill = "orange", color = "black") +
  theme_minimal() + labs(title = "Distribution by Sex")

# Box Plot for Outlier Detection
ggplot(df, aes(x = Sex, y = Age)) + geom_boxplot() +
  theme_minimal() + labs(title = "Age Distribution by Sex")

# Correlation Analysis
correlation_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")
corrplot(correlation_matrix, method = "color")

# Advanced Visualization: Facets, Violin Plots, and Pair Plots
p1 <- ggplot(df, aes(x = Sex, y = Age)) + geom_violin(trim = FALSE) + theme_minimal()
p2 <- ggplot(df, aes(x = Pclass, y = Survived)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
p3 <- ggplot(df, aes(x = Age, y = Fare, color = Sex)) + geom_point() + facet_wrap(~Sex) + theme_minimal()

grid.arrange(p1, p2, p3, ncol = 3)

# Dealing with Missing Values
df <- df %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)) %>%
  drop_na()

# Encoding Categorical Variables
df <- df %>%
  mutate(across(c(Sex, Pclass, Embarked), as.factor)) %>%
  mutate(across(c(Sex, Pclass, Embarked), ~as.numeric(as.factor(.))))

# Scaling Numerical Variables
df_scaled <- df %>%
  select_if(is.numeric) %>%
  scale(center = TRUE, scale = TRUE)

# Additional Statistical Tests (if applicable)
# Example: Comparing Age across different Pclasses using ANOVA
anova_result <- aov(Age ~ Pclass, data = df)
summary(anova_result)

# Exporting the Modified DataFrame
write.csv(df, "titanic_modified.csv")

# Saving the Improved Script
writeLines(capture.output(cat("# Improved R Script for Data Analysis", "\n", sep = "\n")), "improved_data_analysis_script.R")
