library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(patchwork)
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

# Find the basic information about the data set
data <- data
# str(data)
# summary(data)


# --------------------------------------------------------------------------------
# Homogeneity of variances

mod <- aov(RMSE ~ `Number of Categories` * `Number of Factors Used in EFA`,
           data = data
)

leveneTest(mod)

# --------------------------------------------------------------------------------
# Visualization for outliers

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



# Aim to find the outliers threshold

Q1<- quantile(data$RMSE, 0.25)
Q3 <- quantile(data$RMSE, 0.75)
IQR_value <- IQR(data$RMSE)

upper_bound <- Q3 + 1.5 * IQR_value
lower_bound <- Q1 - 1.5 * IQR_value

count <- sum(data$RMSE >  upper_bound | data$RMSE < lower_bound)
proportions <- count / 3000
print(paste0("The proportion of outliers is ", proportions))

# --------------------------------------------------------------------------------
  
# Two-way ANOVA methods (t2way) to get the results 


robust_result <- t2way(RMSE ~ `Number of Categories` * `Number of Factors Used in EFA`,
                       data = data)
robust_result
                                
# --------------------------------------------------------------------------------
# The following is used to draw the plot used for showing the relationships between the two factors
df <- data

df.summary <- df %>%
  group_by(`Number of Categories`, `Number of Factors Used in EFA`) %>%
  summarise(
    sd = sd(RMSE),
    RMSE = mean(RMSE)
  )



# Adjust dodge width for separation
position <- position_dodge(width = 0.3)  # Adjust the width as needed

Main_Plot <- ggplot() +
  geom_line(aes(x = `Number of Categories`, y = RMSE, color = `Number of Factors Used in EFA`, group = `Number of Factors Used in EFA`), 
            data = df.summary, position = position) +
  geom_errorbar(aes(x = `Number of Categories`, y = RMSE, ymin = RMSE - sd, ymax = RMSE + sd, color = `Number of Factors Used in EFA`), 
                data = df.summary, width = 0.2, position = position) +
  scale_color_manual(values = colors) +
  theme_gray()

Main_Plot



# This is used to show the mean value and the variance
df.summary
