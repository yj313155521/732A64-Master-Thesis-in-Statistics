library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(scales)
library(patchwork)
library(DescTools)
library(WRS2)

# Comparsion between Different Symmetry Conditions with 3 Factors (Simulation and EFA)
data_1 <- read.csv("Experiment_Result/1.1/RMSE_result_1.csv")
data_2 <- read.csv("Experiment_Result/2.1/RMSE_result_1.csv")
data_3 <- read.csv("Experiment_Result/3.1/RMSE_result_1.csv")

# --------------------------------------------------------------------------------
# Change "data_1" into longer format, preparing for integration


colnames(data_1) <- c("5","6","7","8","9","10")

long_data_1 <- pivot_longer(
  data_1,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_1$`Symmetry Condition` <- "Symmetry"

# --------------------------------------------------------------------------------
# Change "data_2" into longer format, preparing for integration
colnames(data_2) <- c("5","6","7","8","9","10")

long_data_2 <- pivot_longer(
  data_2,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_2$`Symmetry Condition` <- "Positive and Negative Asymmetry"


# --------------------------------------------------------------------------------
# Change "data_3" into longer format, preparing for integration
colnames(data_3) <-  c("5","6","7","8","9","10")

long_data_3 <- pivot_longer(
  data_3,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_3$`Symmetry Condition` <- "Positive Asymmetry"

# --------------------------------------------------------------------------------

# The following is used to combine the above three datasets together.
data <- rbind(long_data_1,long_data_2, long_data_3)
data <- select(data,c("Number of Categories","Symmetry Condition","RMSE"))


# Get the factors columns
data$`Number of Categories` <- factor(data$`Number of Categories`, levels = c(5, 6, 7, 8, 9, 10))

data$`Symmetry Condition` <- factor(data$`Symmetry Condition`, levels = c("Symmetry","Positive and Negative Asymmetry","Positive Asymmetry"))

# --------------------------------------------------------------------------------
# Visualization for outliers and show the exact proportion of outliers in each group.

colors <- c("Symmetry" = "#1E90FF", 
            "Positive and Negative Asymmetry" = "#FFA500",  
            "Positive Asymmetry" = "#4CAF50")  

Outliers_Plot <-ggplot(data) +
  aes(x = `Number of Categories`, y = RMSE, fill = `Symmetry Condition`) +
  geom_boxplot() +
  scale_fill_manual(values = colors)


Outliers_Plot

# Find the outliers proportion for each group

## Define a function to calculate outlier bounds
calculate_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  (x < lower_bound) | (x > upper_bound)
}

## Add a column identifying outliers and calculate proportions
outlier_proportions <- data %>%
  group_by(`Number of Categories`, `Symmetry Condition`) %>%
  mutate(is_outlier = calculate_outliers(RMSE)) %>%  # Identify outliers
  summarise(
    total_count = n(),                            # Total number of observations in the group
    outlier_count = sum(is_outlier, na.rm = TRUE), # Number of outliers in the group
    outlier_proportion = outlier_count / total_count,# Proportion of outliers
    .groups = "drop"
  )


outlier_proportions
print(paste0("The maximum proportion of outliers in all combination of levels is ", percent(max(outlier_proportions$outlier_proportion))))

max_pro <- 0.06
# -------------------------------------------------------------------------------------------------------------------------------------------------
# Homogeneity of variances

leveneTest(RMSE ~ `Number of Categories` * `Symmetry Condition`,
           data = data)


# --------------------------------------------------------------------------------
# The following is used to draw the plot used for showing the relationships between the two factors
df <- data

df.summary <- df %>%
  group_by(`Number of Categories`, `Symmetry Condition`) %>%
  summarise(
    Winsorized_var = var(Winsorize(RMSE, val = quantile(RMSE, probs = c(max_pro, 1-max_pro), na.rm = FALSE))), 
    trimmed_rmse_mean = mean(RMSE, trim = max_pro),
    .groups = "drop"
  )

# This is used to show the mean value and the variance
df.summary

# Define a function that can change the variance in each group into the variance use in plot.
SE_trim <- function(var,n,trim){
  sqrt(var/(n*(1-2*trim)^2))
}


position <- position_dodge(0.2)  # Adjust dodge width as needed
Main_Plot <- ggplot() +
  geom_line(aes(x = `Number of Categories`, y = trimmed_rmse_mean, color = `Symmetry Condition`, group = `Symmetry Condition`), 
            data = df.summary, position = position) +
  geom_errorbar(aes(x = `Number of Categories`, y = trimmed_rmse_mean, ymin = trimmed_rmse_mean - 1.96*SE_trim(Winsorized_var,100,max_pro), ymax = trimmed_rmse_mean + 1.96*SE_trim(Winsorized_var,100,max_pro), color = `Symmetry Condition`), 
                data = df.summary, width = 0.2, position = position) +
  scale_color_manual(values = colors) +
  theme_gray()+ labs(y = "Trimmed RMSE Mean") 

Main_Plot





# --------------------------------------------------------------------------------

# Two-way ANOVA methods (t2way) to get the results 


robust_result <- t2way(RMSE ~ `Number of Categories` * `Symmetry Condition`, data = data, tr = max_pro, alpha=0.05)

robust_result

