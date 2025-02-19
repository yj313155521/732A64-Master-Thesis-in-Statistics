library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(scales)
library(patchwork)


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

# --------------------------------------------------------------------------------
# Get trimmed data 20%

data_trimmed <- data %>%
  group_by(`Number of Categories`, `Number of Factors Used in EFA`) %>%
  filter(
    RMSE >= quantile(RMSE, 0.2, na.rm = TRUE) & # Remove the bottom 20%
      RMSE <= quantile(RMSE, 0.8, na.rm = TRUE)   # Remove the top 20%
  )



Outliers_Plot_new <-ggplot(data_trimmed) +
  aes(x = `Number of Categories`, y = RMSE, fill = `Number of Factors Used in EFA`) +
  geom_boxplot() +
  scale_fill_manual(values = colors)


Outliers_Plot_new


# --------------------------------------------------------------------------------
# Model establishment

mod <- aov(RMSE ~ `Number of Categories` * `Number of Factors Used in EFA`,
           data = data_trimmed
)

## Homogeneity of variances (It is not necessary since each group is with the same size)
##leveneTest(mod)

# --------------------------------------------------------------------------------
# Get the result of the two-way ANOVA
summary(mod)



# --------------------------------------------------------------------------------
# The following is used to draw the plot used for showing the relationships between the two factors
df <- data

trimmed_sd <- function(x, trim = 0.2) {
  trimmed <- sort(x)[(floor(trim * length(x)) + 1):(ceiling((1 - trim) * length(x)))]
  sd(trimmed)
}

df.summary <- df %>%
  group_by(`Number of Categories`, `Number of Factors Used in EFA`) %>%
  summarise(
    sd = trimmed_sd(RMSE, trim = 0.2),
    RMSE = mean(RMSE, trim = 0.2),
    .groups = "drop"
  )


position <- position_dodge(0.2)  # Adjust dodge width as needed
Main_Plot <- ggplot() +
  geom_line(aes(x = `Number of Categories`, y = RMSE, color = `Number of Factors Used in EFA`, group = `Number of Factors Used in EFA`), 
            data = df.summary, position = position) +
  geom_errorbar(aes(x = `Number of Categories`, y = RMSE, ymin = RMSE - 1.96*sd/sqrt(60), ymax = RMSE + 1.96*sd/sqrt(60), color = `Number of Factors Used in EFA`), 
                data = df.summary, width = 0.2, position = position) +
  scale_color_manual(values = colors) +
  theme_gray()

Main_Plot


# This is used to show the mean value and the variance
df.summary


