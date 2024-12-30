# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load the data (replace 'data.csv' with your file path if loading from a CSV)
data <- read.csv(file.choose())
View(data)

# Understanding the Dataset
cat("Column Names and Data Types:\n")
str(data)

cat("\nSummary of Dataset:\n")
summarize_data <- summary(data)
print(summarize_data)

cat("\nChecking Missing Values:\n")
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Univariate Analysis
# Bar Plot of Continent
cat("\nGenerating Bar Plot for Continent Distribution...\n")
ggplot(data, aes(x = Continent)) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Continent Distribution", x = "Continent", y = "Count") +
  theme_minimal()

# Bivariate Analysis
# Box Plot: Average CDR by Development Level
cat("\nGenerating Box Plot for Average CDR by Development Level...\n")
ggplot(data, aes(x = Development_level, y = Average_CDR, fill = Development_level)) +
  geom_boxplot() +
  labs(title = "Average CDR by Development Level", x = "Development Level", y = "Crude Death Rate") +
  theme_minimal()

# Correlation Analysis
cat("\nGenerating Correlation Matrix and Heatmap...\n")
numeric_data <- data %>% select(where(is.numeric))
correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

# Pie Chart: Continent Distribution
cat("\nGenerating Pie Chart for Continent Distribution...\n")
continent_distribution <- data %>% group_by(Continent) %>% summarise(Count = n())
ggplot(continent_distribution, aes(x = "", y = Count, fill = Continent)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Continent Distribution", x = NULL, y = NULL) +
  theme_void()

# Finish EDA
cat("\nEDA Completed.\n")
