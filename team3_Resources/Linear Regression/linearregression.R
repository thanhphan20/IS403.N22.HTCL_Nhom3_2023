library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(stats)

# Đọc dữ liệu
path_economic <- '/data_month.csv'
economic_data <- read.csv(path_economic)
economic_data <- na.omit(economic_data)

# Chuyển đổi kiểu dữ liệu
economic_data$year <- as.integer(economic_data$year)
economic_data$month <- as.integer(economic_data$month)
economic_data$GDP_grow_rate <- as.numeric(economic_data$GDP_grow_rate)
economic_data$GDP_real <- as.numeric(economic_data$GDP_real)
economic_data$GDP_per_capita <- as.numeric(economic_data$GDP_per_capita)
economic_data$agriculture <- as.numeric(economic_data$agriculture)
economic_data$industry <- as.numeric(economic_data$industry)
economic_data$service <- as.numeric(economic_data$service)
economic_data$CPI <- as.numeric(economic_data$CPI)
economic_data$FDI_in <- as.numeric(economic_data$FDI_in)
economic_data$FDI_out <- as.numeric(economic_data$FDI_out)
economic_data$inflation_rate <- as.numeric(economic_data$inflation_rate)
economic_data$exchange_rate <- as.numeric(economic_data$exchange_rate)
economic_data$exchange <- as.numeric(economic_data$exchange)
economic_data$export <- as.numeric(economic_data$export)
economic_data$import <- as.numeric(economic_data$import)
economic_data$workers <- as.numeric(economic_data$workers)
economic_data$unemployment_rate <- as.numeric(economic_data$unemployment_rate)
economic_data$income <- as.numeric(economic_data$income)
economic_data$income_VND <- as.numeric(economic_data$income_VND)
economic_data$date <- as.Date(paste(economic_data$year, economic_data$month, sep='-'), format='%Y-%m')
economic_data <- economic_data[, !(names(economic_data) %in% c('year', 'month'))]

# Tính toán jobs
economic_data$jobs <- economic_data$workers * (1 - economic_data$unemployment_rate/100)

# Vẽ Heatmap
heatmap <- ggplot(data=economic_data, aes(x=1:ncol(economic_data), y=1:ncol(economic_data))) +
  geom_tile(aes(fill = cor(economic_data)), color="white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed()

print(heatmap)

# Vẽ Scatter Plot
scatter1 <- ggplot(data=economic_data, aes(x=exchange, y=income_VND)) +
  geom_point() +
  geom_smooth(method='lm', formula=y ~ poly(x, 2), se=FALSE) +
  ggtitle("Scatter Plot: Exchange Rate vs. Income")

scatter2 <- ggplot(data=economic_data, aes(x=GDP_real, y=income_VND)) +
  geom_point() +
  geom_smooth(method='lm', formula=y ~ poly(x, 2), se=FALSE) +
  ggtitle("Scatter Plot: GDP Real vs. Income")

scatter3 <- ggplot(data=economic_data, aes(x=GDP_per_capita, y=income_VND)) +
  geom_point() +
  geom_smooth(method='lm', formula=y ~ poly(x, 2), se=FALSE) +
  ggtitle("Scatter Plot: GDP Per Capita vs. Income")

scatter4 <- ggplot(data=economic_data, aes(x=import, y=income_VND)) +
  geom_point() +
  geom_smooth(method='lm', formula=y ~ poly(x, 2), se=FALSE) +
  ggtitle("Scatter Plot: Import vs. Income")

scatter5 <- ggplot(data=economic_data, aes(x=export, y=income_VND)) +
  geom_point() +
  geom_smooth(method='lm', formula=y ~ poly(x, 2), se=FALSE) +
  ggtitle("Scatter Plot: Export vs. Income")

print(scatter1)
print(scatter2)
print(scatter3)
print(scatter4)
print(scatter5)

# Phân chia dữ liệu
train_size <- as.integer(0.7 * nrow(economic_data))
test_size <- as.integer(0.2 * nrow(economic_data))
val_size <- nrow(economic_data) - train_size - test_size

train_data <- economic_data[1:train_size,]
test_data <- economic_data[(train_size + 1):(train_size + test_size),]
val_data <- economic_data[(train_size + test_size + 1):nrow(economic_data),]

# Linear Regression - Simple Linear Regression - Independence
x_train <- 1:nrow(train_data)

GDP_grow_train <- train_data$GDP_grow_rate
GDP_capita_train <- train_data$GDP_per_capita
inflation_train <- train_data$inflation_rate
import_train <- train_data$import
export_train <- train_data$export
workers_train <- train_data$workers

model_grow_value <- lm(GDP_grow_train ~ x_train)
model_per_value <- lm(GDP_capita_train ~ x_train)
model_inflation_value <- lm(inflation_train ~ x_train)
model_import_value <- lm(import_train ~ x_train)
model_export_value <- lm(export_train ~ x_train)
model_workers_value <- lm(workers_train ~ x_train)

last_index <- tail(economic_data$index, 1)
last_data <- seq(last_index, last_index + 60, 1)
x_next_5_years <- seq(last_index + 1, last_index + 61, 1)

GDP_per_capita_next_5_years <- predict(model_per_value, newdata=data.frame(x=x_next_5_years))
GDP_grow_rate_next_5_years <- predict(model_grow_value, newdata=data.frame(x=x_next_5_years))
inflation_next_5_years <- predict(model_inflation_value, newdata=data.frame(x=x_next_5_years))
import_next_5_years <- predict(model_import_value, newdata=data.frame(x=x_next_5_years))
export_next_5_years <- predict(model_export_value, newdata=data.frame(x=x_next_5_years))
workers_next_5_years <- predict(model_workers_value, newdata=data.frame(x=x_next_5_years))

independent_variables_next_5_years <- data.frame(
  GDP_grow_rate = GDP_grow_rate_next_5_years,
  GDP_per_capita = GDP_per_capita_next_5_years,
  inflation_rate = inflation_next_5_years,
  import = import_next_5_years,
  export = export_next_5_years,
  workers = workers_next_5_years
)

# Linear Regression - Simple Linear Regression - Income
model <- lm(income_VND ~ GDP_per_capita, data=train_data)

x_next_5_years <- data.frame(GDP_per_capita = GDP_per_capita_next_5_years)
y_next_5_years <- predict(model, newdata=x_next_5_years)

# In kết quả
print("Mô hình:")
print(paste("Testing MSE:", mean((y_test - y_pred_test)^2)))
print(paste("Validation MSE:", mean((y_val - y_pred_val)^2)))
print(paste("Testing MAE:", mean(abs(y_test - y_pred_test))))
print(paste("Validation MAE:", mean(abs(y_val - y_pred_val))))

# Linear Regression - Multiple Linear Regression - Income
independent_variables <- train_data[, c('GDP_grow_rate', 'GDP_per_capita', 'inflation_rate', 'import', 'export', 'workers')]
dependent_variable <- train_data$income_VND

model <- lm(dependent_variable ~ ., data=independent_variables)

x_train <- subset(train_data, select=c('GDP_grow_rate', 'GDP_per_capita', 'inflation_rate', 'import', 'export', 'workers'))
y_train <- train_data$income_VND
y_pred_train <- predict(model, newdata=x_train)

x_test <- subset(test_data, select=c('GDP_grow_rate', 'GDP_per_capita', 'inflation_rate', 'import', 'export', 'workers'))
y_test <- test_data$income_VND
y_pred_test <- predict(model, newdata=x_test)

x_val <- subset(val_data, select=c('GDP_grow_rate', 'GDP_per_capita', 'inflation_rate', 'import', 'export', 'workers'))
y_val <- val_data$income_VND
y_pred_val <- predict(model, newdata=x_val)

dependent_variable_next_5_years <- predict(model, newdata=independent_variables_next_5_years)

# Vẽ đồ thị
plot(train_data$index, train_data$income_VND, type='l', col='blue', xlab='Index', ylab='Income_VND', main='Income Prediction')
lines(test_data$index, test_data$income_VND, col='red')
lines(val_data$index, val_data$income_VND, col='green')
lines(last_data, dependent_variable_next_5_years, col='purple')
legend('topright', legend=c('Train', 'Test', 'Validate', 'Next 5 years'), col=c('blue', 'red', 'green', 'purple'), lty=1:1)

# Hiển thị tóm tắt mô hình
summary(model)