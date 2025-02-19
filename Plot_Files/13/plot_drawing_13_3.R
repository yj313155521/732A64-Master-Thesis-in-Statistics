library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(scales)
library(patchwork)
library(DescTools)
library(WRS2)


# Comparsion between Different number of factors used in EFA with 3 Factors (Simulation)
data_1 <- read.csv("Experiment_Result/1.1/RMSE_result_1.csv") # 3 factors used in EFA
data_2 <- read.csv("Experiment_Result/5.1/RMSE_result_1.csv") # 4 factors used in EFA
data_3 <- read.csv("Experiment_Result/5.2/RMSE_result_1.csv") # 5 factors used in EFA
data_4 <- read.csv("Experiment_Result/5.3/RMSE_result_1.csv") # 6 factors used in EFA
data_5 <- read.csv("Experiment_Result/5.4/RMSE_result_1.csv") # 7 factors used in EFA

# --------------------------------------------------------------------------------
# Change "data_1" into longer format, preparing for integration


colnames(data_1) <- c("5","6","7","8","9","10")

long_data_1 <- pivot_longer(
  data_1,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_1$`Number of Factors Used in EFA` <- "3"

# --------------------------------------------------------------------------------
# Change "data_2" into longer format, preparing for integration
colnames(data_2) <- c("5","6","7","8","9","10")

long_data_2 <- pivot_longer(
  data_2,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_2$`Number of Factors Used in EFA` <- "4"


# --------------------------------------------------------------------------------
# Change "data_3" into longer format, preparing for integration
colnames(data_3) <-  c("5","6","7","8","9","10")

long_data_3 <- pivot_longer(
  data_3,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_3$`Number of Factors Used in EFA` <- "5"

# --------------------------------------------------------------------------------
# Change "data_4" into longer format, preparing for integration
colnames(data_4) <-  c("5","6","7","8","9","10")

long_data_4 <- pivot_longer(
  data_4,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_4$`Number of Factors Used in EFA` <- "6"


# --------------------------------------------------------------------------------
# Change "data_5" into longer format, preparing for integration
colnames(data_5) <-  c("5","6","7","8","9","10")

long_data_5 <- pivot_longer(
  data_5,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_5$`Number of Factors Used in EFA` <- "7"

# --------------------------------------------------------------------------------

# The following is used to combine the above three datasets together.
data <- rbind(long_data_1,long_data_2, long_data_3,long_data_4,long_data_5)
data <- select(data,c("Number of Categories","Number of Factors Used in EFA","RMSE"))


# Get the factors columns
data$`Number of Categories` <- factor(data$`Number of Categories`, levels = c(5, 6, 7, 8, 9, 10))

data$`Number of Factors Used in EFA` <- factor(data$`Number of Factors Used in EFA`, levels = c("3","4","5","6","7"))


# --------------------------------------------------------------------------------
# Visualization for outliers and show the exact proportion of outliers in each group.

colors <- c("3" = "#9370DB", 
            "4" = "#1E90FF", 
            "5" = "#FFA500",  
            "6" = "#4CAF50",
            "7" = "#DC143C")  

Outliers_Plot <-ggplot(data) +
  aes(x = `Number of Categories`, y = RMSE, fill = `Number of Factors Used in EFA`) +
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
  group_by(`Number of Categories`, `Number of Factors Used in EFA`) %>%
  mutate(is_outlier = calculate_outliers(RMSE)) %>%  # Identify outliers
  summarise(
    total_count = n(),                            # Total number of observations in the group
    outlier_count = sum(is_outlier, na.rm = TRUE), # Number of outliers in the group
    outlier_proportion = outlier_count / total_count,# Proportion of outliers
    .groups = "drop"
  )


outlier_proportions
print(paste0("The maximum proportion of outliers in all combination of levels is ", percent(max(outlier_proportions$outlier_proportion))))

max_pro <- 0.12


# -------------------------------------------------------------------------------------------------------------------------------------------------
# Homogeneity of variances

leveneTest(RMSE ~ `Number of Categories` * `Number of Factors Used in EFA`,
           data = data)



# --------------------------------------------------------------------------------
# The following is used to draw the plot used for showing the relationships between the two factors
df <- data

df.summary <- df %>%
  group_by(`Number of Categories`, `Number of Factors Used in EFA`) %>%
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
  geom_line(aes(x = `Number of Categories`, y = trimmed_rmse_mean, color = `Number of Factors Used in EFA`, group = `Number of Factors Used in EFA`), 
            data = df.summary, position = position) +
  geom_errorbar(aes(x = `Number of Categories`, y = trimmed_rmse_mean, ymin = trimmed_rmse_mean - 1.96*SE_trim(Winsorized_var,100,max_pro), ymax = trimmed_rmse_mean + 1.96*SE_trim(Winsorized_var,100,max_pro), color = `Number of Factors Used in EFA`), 
                data = df.summary, width = 0.2, position = position) +
  scale_color_manual(values = colors) +
  theme_gray()+ labs(y = "Trimmed RMSE Mean") 

Main_Plot


# --------------------------------------------------------------------------------

# Two-way ANOVA methods (t2way) to get the results 


robust_result <- t2way(RMSE ~ `Number of Categories` * `Number of Factors Used in EFA`, data = data, tr = max_pro, alpha=0.05)

robust_result



