install.packages("ggplot2")
install.packages("gridExtra")
install.packages("scales")
library(ggplot2)
library(gridExtra)
library(scales)
train_df <- read.csv('archive/Fraudulent_E-Commerce_Transaction_Data.csv')

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

is_fraudulent_plots <- function(data, path = "./image/") {
  data$`Is.Fraudulent` <- as.factor(data$`Is.Fraudulent`)
  
  columns <- c('Payment.Method', 'Product.Category', 
               'Quantity', 'Device.Used', 'Transaction.DOW', 
               'Transaction.Month', 'Is.Address.Match')
  
  for (col in columns) {
    p <- ggplot(data, aes_string(x = col, fill = "Is.Fraudulent")) +
      geom_bar(position = "dodge") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(hjust = 0.5),  # 置中
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            legend.position = c(0.92, 0.9)) +
      scale_y_continuous(labels = comma) +  
      labs(title = col, fill = "Is.Fraudulent")
    # print(p)
    
    ggsave(filename = paste0(path, "plot_", col, ".png"), plot = p, width = 10, height = 8)
  }
}

is_fraudulent_plots(train_df)
