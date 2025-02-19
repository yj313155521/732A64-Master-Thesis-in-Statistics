library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(patchwork)
library(WRS2)

# Comparsion between Different Symmetry Conditions with 4 Factors (Simulation and EFA)
data_1 <- read.csv("Experiment_Result/1.2/RMSE_result_1.csv")
data_2 <- read.csv("Experiment_Result/2.2/RMSE_result_1.csv")
data_3 <- read.csv("Experiment_Result/3.2/RMSE_result_1.csv")

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

# Find the basic information about the data set
dat <- data
# str(dat)
# summary(dat)


# --------------------------------------------------------------------------------
# Homogeneity of variances

mod <- aov(RMSE ~ `Number of Categories` * `Symmetry Condition`,
           data = dat
)

leveneTest(mod)

# --------------------------------------------------------------------------------
# Visualization for outliers

colors <- c("Symmetry" = "#1E90FF", 
            "Positive and Negative Asymmetry" = "#FFA500",  
            "Positive Asymmetry" = "#4CAF50")  

Outliers_Plot <- ggplot(dat) +
  aes(x = `Number of Categories`, y = RMSE, fill = `Symmetry Condition`) +
  geom_boxplot() +
  scale_fill_manual(values = colors)
  


Outliers_Plot


# Aim to find the outliers threshold

Q1<- quantile(dat$RMSE, 0.25)
Q3 <- quantile(dat$RMSE, 0.75)
IQR_value <- IQR(dat$RMSE)

upper_bound <- Q3 + 1.5 * IQR_value
lower_bound <- Q1 - 1.5 * IQR_value

count <- sum(dat$RMSE >  upper_bound | dat$RMSE < lower_bound)
proportions <- count / 1800
print(paste0("The proportion of outliers is ", proportions))

# --------------------------------------------------------------------------------

# Two-way ANOVA methods (t2way) to get the results 


robust_result <- t2way(RMSE ~ `Number of Categories` * `Symmetry Condition`,
                       data = dat)
robust_result

# --------------------------------------------------------------------------------
# The following is used to draw the plot used for showing the relationships between the two factors
df <- dat

df.summary <- df %>%
  group_by(`Number of Categories`, `Symmetry Condition`) %>%
  summarise(
    sd = sd(RMSE),
    RMSE = mean(RMSE)
  )


position <- position_dodge(0.2)  # Adjust dodge width as needed
Main_Plot <- ggplot() +
  geom_line(aes(x = `Number of Categories`, y = RMSE, color = `Symmetry Condition`, group = `Symmetry Condition`), 
            data = df.summary, position = position) +
  geom_errorbar(aes(x = `Number of Categories`, y = RMSE, ymin = RMSE - sd/10, ymax = RMSE + sd/10, color = `Symmetry Condition`), 
                data = df.summary, width = 0.2, position = position) +
  scale_color_manual(values = colors) +
  theme_gray()

Main_Plot

# This is used to show the mean value and the variance
df.summary
