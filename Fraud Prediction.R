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

# 創建圓餅圖Function
create_pie_chart <- function(data, column_name) {
  # 計算每個類別數量
  category_count <- data %>%
    count(!!sym(column_name)) %>%
    arrange(desc(n))
  
  pie_chart <- ggplot(category_count, aes(x = "", y = n, fill = factor(!!sym(column_name)))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          legend.position = "right") +
    geom_text(aes(label = sprintf("%.1f%%", n/sum(n) * 100)), position = position_stack(vjust = 0.5)) +
    geom_text(aes(x = 2.0, label = !!sym(column_name)), position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = column_name, fill = column_name)+
    guides(fill = FALSE)
  
  return(pie_chart)
}

# 創建長條圖Function
create_bar_chart <- function(data, column_name) {
  bar_chart <- ggplot(data, aes(x = factor(!!sym(column_name)), fill = factor(!!sym(column_name)))) +
    geom_bar() +
    geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title.x = element_blank()
          # 如果需要旋转 x 轴文本，可以取消下一行的注释
          # axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    scale_fill_brewer(palette = "Pastel1") +
    scale_y_continuous(n.breaks = 8, labels = scales::comma) +
    ggtitle(column_name) +
    guides(fill = FALSE)
  
  return(bar_chart)
}

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
  
  print(transaction_chart)
  ## Payment.Method
  payment_pie_chart <- create_pie_chart(data, "Payment.Method")
  payment_bar_chart <- create_bar_chart(data, "Payment.Method")
  
  ## Product.Category
  category_pie_chart <- create_pie_chart(data, "Product.Category")
  category_bar_chart <- create_bar_chart(data, "Product.Category")
  
  ## Quantity
  quantity_pie_chart <- create_pie_chart(data, "Quantity")
  quantity_bar_chart <- create_bar_chart(data, "Quantity")
  
  # 收集所有圖像並返回
  charts <- mget(ls(pattern = "_chart$"))
  return(charts)
}

charts <- eda_plot(data)

# print(charts$quantity_bar_chart)

# 保存圖像Function
save_plots <- function(charts, width = 8, height = 6, path = getwd()) {
  plot_names <- names(plots)
  for (plot_name in plot_names) {
    ggsave(filename = file.path(path, paste0(plot_name, ".png")), 
           plot = plots[[plot_name]], 
           width = width, 
           height = height)
  }
}

# 保存圖像到本地端
save_plots(charts)

# ggsave("transaction_chart.png", plot = plots$transaction_chart, width = 8, height = 6)
# ggsave("payment_pie_chart.png", plot = plots$payment_pie_chart, width = 8, height = 6)
# ggsave("payment_bar_chart.png", plot = plots$payment_bar_chart, width = 8, height = 6)
# ggsave("category_pie_chart.png", plot = plots$category_pie_chart, width = 8, height = 6)
# ggsave("category_bar_chart.png", plot = plots$category_bar_chart, width = 8, height = 6)
# ggsave("quantity_pie_chart.png", plot = plots$quantity_pie_chart, width = 8, height = 6)
# ggsave("quantity_bar_chart.png", plot = plots$quantity_bar_chart, width = 8, height = 6)

# data <- train_df


