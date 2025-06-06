# Author: RabbitSnore
# Date: 2018-10-05

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

# Create a frequency plot, with a histogram of raw counts and density curve, with facets for each condition
# All in funky '80s colors

ggplot(animal_data,
       aes(
         x = length # Specify the x variable
       )) +
  facet_wrap(~ animal # Specify the variable containing the conditions for faceting (i.e., a separate panel for each condition)
  ) +
  geom_density( # Ask for a density curve
    aes(y = 4 * ..count..), # Adjust the y-axis scaling so that the curve follows the same scale as the histogram layer below
    fill = "#42e959" # Funky '80s color!
  ) +
  geom_histogram( # Ask for a histogram
    binwidth = 4, # Adjust the width of the bars
    color = "#53c1b8", # Funky '80s color!
    fill = "#bd4bd5", # Funky '80s color!
    alpha = .75 # Adjust the transparency of the bars so we can see the density plot behind it
  )  +
  labs(
    title = "Animals!", # Add a title to the plot
    y = "Count", # Change the y label
    x = "Length" # Change the x label
  ) +
  theme_classic() # Use the built-in classic theme

###################################################
###################################################

# Creating a means plot for an interaction

# Create vectors for conditions in a 2 x 2 design 

A <- c(rep(0, 100), rep(1, 100))
B <- c(rep(0, 50), rep(1, 50), rep(0, 50), rep(1, 50))

# Specify coefficients and intercept

intercept <- 10 # y intercept
bA <- 2 # coefficient for A
bB <- 10 # coefficient for B
bAB <- 40 # coefficient for interaction of A and B

# Simulate data with specified effects

y <- intercept + bA*A + bB*B + bAB*A*B + rnorm(200, 0, 3)

# Bind factors and simulated data into a data frame

df <- data.frame(A, B, y)

# Create a table with the descriptive statistics for plotting

table <- df %>% 
  group_by(A, B) %>% # Group the data by the two factors
  summarise(
    mean = mean(y), # Mean for the dependent variable
    sd = sd(y), # SD for the dependent variable
    se = sd/sqrt(n()), # Standard error of the mean
    ci_upper = mean + (se*1.96), # 95% CI upper bound
    ci_lower = mean - (se*1.96) # 95% CI lower bound
  )

# Use the table of descriptive statistics to create a means plot

ggplot(table, # Specify the descriptive table as the data for plotting
       aes(
         x = as.factor(A), # as.factor() instructs R to treat the numerical codes as categorical
         y = mean, # The mean for each group will be represented by position on the y axis
         group = as.factor(B), # Grouping factor for lines added later
         color = as.factor(B) # Differentiate the B factor with colors
       )) +
  geom_errorbar( # Ask for an error bar
    aes(
      ymin = ci_lower, # Specify the lower limit of the bar
      ymax = ci_upper # Specify the upper limit of the bar
    ),
    width = .4 # Specify the width of the bar
  ) +
  geom_line() + # Link each error bar with a line to visualize the relationship more clearly
  theme_classic()