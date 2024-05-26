## EDA
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("patchwork")
# install.packages("scales")
# install.packages("RColorBrewer")
library(RColorBrewer)
library(ggpubr)
library(patchwork)
library(ggplot2)
library(scales)

train_df <- read.csv('archive/Fraudulent_E-Commerce_Transaction_Data.csv')
data <- clean_data(train_df)

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
    geom_text(aes(label = paste0(sprintf("%.1f%%", n/sum(n) * 100), "\n", !!sym(column_name))), 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = column_name, fill = column_name) +
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
    scale_fill_brewer(palette = "Pastel1") + #顏色統一
    scale_y_continuous(n.breaks = 8, labels = scales::comma) +
    ggtitle(column_name) +
    guides(fill = FALSE)
  
  return(bar_chart)
}

# 建立Hist圖表Function
create_hist_chart <- function(data, column_name, peak = 1) {
  # Create the plot
  plot <- ggplot(data, aes(x = !!sym(column_name), y = ..count..)) +
    geom_histogram(stat="bin", bins = 150, fill = "orange", alpha = 0.5, aes(y=..count..)) +
    geom_density(aes(y = ..density.. * mean(..count..) **peak), color = "orange", alpha = 1, linewidth = 1) +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +  # 添加邊匡線
    ggtitle("Customer.Age")+
    scale_x_continuous(n.breaks = 8) 
  
  # Return the plot
  return(plot)
}

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
  payment_pie_chart <- create_pie_chart(data, "Payment.Method")
  payment_bar_chart <- create_bar_chart(data, "Payment.Method")
  
  ## Product.Category
  category_pie_chart <- create_pie_chart(data, "Product.Category")
  category_bar_chart <- create_bar_chart(data, "Product.Category")
  
  ## Quantity
  quantity_pie_chart <- create_pie_chart(data, "Quantity")
  quantity_bar_chart <- create_bar_chart(data, "Quantity")
  
  ## Device Used
  device_pie_chart <- create_pie_chart(data, "Device.Used")
  device_bar_chart <- create_bar_chart(data, "Device.Used")
  
  ## Account.Age.Days
  account_hist_chart <- create_hist_chart(data, "Account.Age.Days", peak = 1.82)
  
  ## Customer.Age
  customer_hist_chart <- create_hist_chart(data, "Customer.Age", peak = 1.4)
  
  ## Transaction.Hour
  hour_count <- data %>%
    count(Transaction.Hour) %>%
    arrange(desc(n)) %>%
    head(15)
  
  colors <- colorRampPalette(brewer.pal(8, "Pastel1"))(15)
  
  transaction_hour_chart <- ggplot(hour_count, aes(x = factor(Transaction.Hour, levels = Transaction.Hour), y = n, fill = factor(Transaction.Hour))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 80, hjust = 1),
          plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +  # 添加邊匡線
    labs(x = "Transaction Hour", y = "Count", title = "Top 15 Transaction Hours") +
    guides(fill=FALSE)  # 移除圖例
  
  ## Is.Fraudulent
  fraudulent_violin_chart <- ggplot(data, aes(x = factor(Is.Fraudulent), y = Transaction.Amount)) +
    geom_violin(aes(fill = factor(Is.Fraudulent))) +  # 使用 fill 參數設置填充顏色
    theme_minimal() +
    scale_fill_manual(values=colors) +
    theme(plot.title = element_text(hjust = 0.5),  # 置中
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +  # 添加邊匡線
    labs(x = "Is Fraudulent", y = "Transaction Amount", title = "Transaction Amount by Fraudulent Status") +
    guides(fill=FALSE)  # 移除圖例
  
  # 收集所有圖像並返回
  charts <- mget(ls(pattern = "_chart$"))
  return(charts)
}

plots <- eda_plot(data)
# print(plots$fraudulent_violin_chart)

# 保存圖像到本地端
save_plots(plots, path = "./image")
save_plots(plots$transaction_hour_chart, width = 10, height = 6, path = "./image")

# ggsave("transaction_chart.png", plot = plots$transaction_chart, width = 8, height = 6)

