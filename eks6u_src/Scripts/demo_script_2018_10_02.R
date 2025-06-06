# Author: RabbitSnore
# Date: 2018-10-02

# Setup

library(tidyverse)

# Create a data frame with 200 cases of 4 variables (one 3-category condition code and 3 random variables)

## Create the condition codes

### Create a vector of names

categories <- c("puppy", "kitten", "turtle")

### Sample from those names 200 times, with replacement

animal <- sample(categories, 200, replace = TRUE)

## Create random variables

## Use rnorm() with varying means and SDs

length <- rnorm(200, 30, 8)
weight <- rnorm(200, 4, 2)
happiness <- rnorm(200, 3.5, 1)

## Bind variables into data frame

animal_data <- data.frame(animal, length, weight, happiness)

# Examine descriptive statistics

## Use the pipe, group_by(), and summarise() to get means, SDs, and medians for each category

animal_data %>% 
  group_by(animal) %>% 
  summarise(
    weight_mean = mean(weight),
    weight_sd = sd(weight),
    weight_median = median(weight)
  )

# Subset data to include only the first two categories

## Use filter() to select only rows that pass the given logical test

subset_1 <- animal_data %>% 
  filter(animal == "puppy" | animal == "kitten")

# Use subset for t-test

## Welch's t-test

t.test(weight ~ animal, data = subset_1)

## Student's t-test

t.test(weight ~ animal, data = subset_1, var.equal = TRUE)

## Because the variables are all random, these tests are not likely to indicate a significant difference.
## In fact, the probability of them indicating a significant difference is exactly 5%.
## Why? Because the null hypothesis is true in these data.

## Cohen's d calculation

### Load effsize package, which contains the cohen.d() function

library(effsize)

### Use cohen.d to calculate the standardized mean difference

cohen.d(weight ~ animal, data = subset_1)

# Use ggplot to create a scatterplot with a regression line

ggplot(animal_data,
       aes( # Assign aesthetic mappings that will be used for all layers
         x = length, # Map a variable onto the x-axis
         y = weight # Map another variable onto the y-axis
       )) + # Each + adds another layer
  geom_point( # Create a point for each case
    aes( # Another aesthetic mapping, specifically for the points
      color = animal # Make each category a different color point. This mapping will not be inherited by other layers
    )) +
  geom_smooth( # Represent the relationship between the x and y variables with a line
    method = "lm" # Get a linear regression line
  ) +
  theme_bw() # use the built-in black and white theme