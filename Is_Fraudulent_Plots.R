install.packages("ggplot2")
install.packages("gridExtra")
install.packages("scales")
library(ggplot2)
library(gridExtra)
library(scales)

train_df$`Is.Fraudulent` <- as.factor(train_df$`Is.Fraudulent`)


columns <- c('Payment.Method', 'Product.Category', 
             'Quantity', 'Device.Used', 'Transaction.DOW', 
             'Transaction.Month', 'Is.Address.Match')

for (col in columns) {
  p <- ggplot(train_df, aes_string(x = col, fill = "Is.Fraudulent")) +
    geom_bar(position = "dodge") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +  
    labs(title = col, fill = "Is.Fraudulent")
  
  print(p)
  
  ggsave(filename = paste0("plot_", col, ".png"), plot = p, width = 10, height = 8)
}

