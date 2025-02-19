library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(patchwork)
library(WRS2)

# Comparsion between different Matrix and Data settings.(Polychoric Matrix with Ordinal Data & Pearson Matrix with Ordinal Data)
data_1 <- read.csv("Experiment_Result/1.1/RMSE_result_1.csv")
data_2 <- read.csv("Experiment_Result/1.1/RMSE_result_3.csv")


# --------------------------------------------------------------------------------
# Change "data_1" into longer format, preparing for integration


colnames(data_1) <- c("5","6","7","8","9","10")

long_data_1 <- pivot_longer(
  data_1,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_1$`Matrix and Data` <- "Polychoric Matrix with Ordinal Data"

# --------------------------------------------------------------------------------
# Change "data_2" into longer format, preparing for integration
colnames(data_2) <- c("5","6","7","8","9","10")

long_data_2 <- pivot_longer(
  data_2,
  cols = c("5","6","7","8","9","10"), # Select the columns to pivot
  names_to = "Number of Categories",            # New column for the names
  values_to = "RMSE"           # New column for the values
)

long_data_2$`Matrix and Data` <- "Pearson Matrix with Ordinal Data"


# --------------------------------------------------------------------------------


# The following is used to combine the above three datasets together.
data <- rbind(long_data_1,long_data_2)
data <- select(data,c("Number of Categories","Matrix and Data","RMSE"))


# Get the factors columns
data$`Number of Categories` <- factor(data$`Number of Categories`, levels = c(5, 6, 7, 8, 9, 10))

data$`Matrix and Data` <- factor(data$`Matrix and Data`, levels = c("Polychoric Matrix with Ordinal Data", "Pearson Matrix with Ordinal Data"))

# --------------------------------------------------------------------------------

# Find the basic information about the data set
dat <- data
# str(dat)
# summary(dat)


# --------------------------------------------------------------------------------
# Homogeneity of variances

mod <- aov(RMSE ~ `Number of Categories` * `Matrix and Data`,
           data = dat
)

leveneTest(mod)



# --------------------------------------------------------------------------------
# Visualization for outliers

colors <- c("Polychoric Matrix with Ordinal Data" = "#1E90FF", 
            "Pearson Matrix with Ordinal Data" = "#4CAF50")  

Outliers_Plot <- ggplot(dat) +
  aes(x = `Number of Categories`, y = RMSE, fill = `Matrix and Data`) +
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
proportions <- count / 1200
print(paste0("The proportion of outliers is ", proportions))

# --------------------------------------------------------------------------------

# Two-way ANOVA methods (t2way) to get the results 


robust_result <- t2way(RMSE ~ `Number of Categories` * `Matrix and Data`,
                       data = dat)
robust_result

# --------------------------------------------------------------------------------
# The following is used to draw the plot used for showing the relationships between the two factors
df <- dat

df.summary <- df %>%
  group_by(`Number of Categories`, `Matrix and Data`) %>%
  summarise(
    sd = sd(RMSE),
    RMSE = mean(RMSE)
  )


# Adjust position to dodge overlapping elements
position <- position_dodge(0.2)  # Adjust dodge width as needed

Main_Plot <- ggplot() +
  geom_line(aes(x = `Number of Categories`, y = RMSE, color = `Matrix and Data`, group = `Matrix and Data`), 
            data = df.summary, position = position) +
  geom_errorbar(aes(x = `Number of Categories`, y = RMSE, 
                    ymin = RMSE - sd/10, ymax = RMSE + sd/10, 
                    color = `Matrix and Data`), 
                data = df.summary, width = 0.2, position = position) +
  scale_color_manual(values = colors) +
  theme_gray()

Main_Plot




# --------------------------------------------------------------------------------
# This is used to show the mean value and the variance
df.summary
