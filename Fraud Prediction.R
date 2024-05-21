install.packages("plotly")
install.packages("dplyr")
library(plotly)
library(dplyr)


# 讀取CSV檔
train_df <- read.csv('archive/Fraudulent_E-Commerce_Transaction_Data.csv')
test_df <- read.csv('archive/Fraudulent_E-Commerce_Transaction_Data_2.csv')


## Data Cleaning
clean_data <- function(df) {
  # 將Transaction Date轉為datetime
  df$Transaction.Date <- as.Date(df$Transaction.Date)
  
  # 每個月幾號、星期幾、月份
  df$Transaction.Day <- as.numeric(format(df$Transaction.Date, "%d"))
  df$Transaction.DOW <- as.numeric(format(df$Transaction.Date, "%u"))
  df$Transaction.Month <- as.numeric(format(df$Transaction.Date, "%m"))
  
  # 修正Customer Age中的異常值
  mean_value <- round(mean(df$Customer.Age, na.rm = TRUE), 0)
  df$Customer.Age <- ifelse(df$Customer.Age <= -9,
                            abs(df$Customer.Age),
                            df$Customer.Age)
  df$Customer.Age <- ifelse(df$Customer.Age < 9,
                            mean_value,
                            df$Customer.Age)
  
  # Shipping Address與Billing Address的異同（異=0，同=1）
  df$Is.Address.Match <- as.integer(df$Shipping.Address == df$Billing.Address)
  
  # 除去不相關的features
  df <- df[, !names(df) %in% c("Transaction.ID", "Customer.ID", "Customer.Location",
                               "IP.Address", "Transaction.Date", "Shipping.Address", "Billing.Address")]
  
  # downcast datatype
  int_col <- sapply(df, is.integer)
  num_col <- sapply(df, is.numeric) & !int_col
  df[int_col] <- lapply(df[int_col], as.integer)
  df[num_col] <- lapply(df[num_col], as.numeric)
  
  return(df)
}

train_df <- clean_data(train_df)
head(train_df)
str(train_df)

--------------------------------------------------------------------
## EDA
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("patchwork")
# install.packages("scales")
library(ggpubr)
library(patchwork)
library(ggplot2)
library(scales)

eda_plot <- function(data) {
  ## Transaction.Amount
  transaction_chart <- ggplot(data, aes(x = Transaction.Amount)) +
    geom_histogram(bins = 200, fill = "blue", color = "black") +
    theme_minimal()+
    scale_fill_brewer(palette = "Pastel1") +  #顏色樣式統一
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +  # 添加邊匡線
    ggtitle("Transaction Amount") +
    scale_x_continuous(n.breaks = 6) +
    scale_y_continuous(n.breaks = 8, labels = comma)
  
  ## Payment.Method
  payment_count <- data %>%
    count(`Payment.Method`) %>%
    arrange(desc(n))
  
  # Plot pie chart
  payment_pie_chart <- ggplot(payment_count, aes(x = "", y = n, fill = `Payment.Method`)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          legend.position = "right") +
    geom_text(aes(label = sprintf("%.1f%%", n/sum(n) * 100)), position = position_stack(vjust = 0.5)) +
    geom_text(aes(x = 1.9, label = `Payment.Method`), position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = "Payment Method", fill = "Payment.Method")
  
  # Plot bar chart
  payment_bar_chart <- ggplot(data, aes(x = factor(`Payment.Method`), fill = factor(`Payment.Method`))) +
    geom_bar() +
    geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title.x = element_blank()) + 
    scale_fill_brewer(palette = "Pastel1") +
    ggtitle("Payment Method") +
    scale_y_continuous(labels = comma) +
    guides(fill = FALSE)
  
  category_count <- data %>%
    count(`Product.Category`) %>%
    arrange(desc(n))
  
  ## Product.Category
  category_count <- data %>%
    count(`Product.Category`) %>%
    arrange(desc(n))
  
  # Plot pie chart
  category_pie_chart <- ggplot(category_count, aes(x = "", y = n, fill = `Product.Category`)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          legend.position = "right") +
    geom_text(aes(label = sprintf("%.1f%%", n/sum(n) * 100)), position = position_stack(vjust = 0.5)) +
    geom_text(aes(x = 2.0, label = `Product.Category`), position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = "Product Category", fill = "Product.Category")
  
  # Plot bar chart
  category_bar_chart <- ggplot(data, aes(x = factor(`Product.Category`), fill = factor(`Product.Category`))) +
    geom_bar() +
    geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title.x = element_blank(),
          # axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    scale_fill_brewer(palette = "Pastel1") +
    scale_y_continuous(n.breaks = 8, labels = comma) +
    ggtitle("Product Category") +
    guides(fill = FALSE)
  
  return(list(transaction_chart = transaction_chart, 
              payment_pie_chart = payment_pie_chart, 
              payment_bar_chart = payment_bar_chart,
              category_pie_chart = category_pie_chart, 
              category_bar_chart = category_bar_chart))
}

plots <- eda_plot(train_df)

# print(plots$transaction_chart)

ggsave("transaction_chart.png", plot = plots$transaction_chart, width = 8, height = 6)
ggsave("payment_pie_chart.png", plot = plots$payment_pie_chart, width = 8, height = 6)
ggsave("payment_bar_chart.png", plot = plots$payment_bar_chart, width = 8, height = 6)
ggsave("category_pie_chart.png", plot = plots$category_pie_chart, width = 8, height = 6)
ggsave("category_bar_chart.png", plot = plots$category_bar_chart, width = 8, height = 6)

# data <- train_df
