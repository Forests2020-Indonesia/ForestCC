library(openxlsx)
library(dplyr)
library(ggplot2)

svr_table <- read.xlsx("PROCESSED_DATA/OLDS/RASTER_EVAL/POSTER/actual_prediction_svr.xlsx")
svr_table$`Actual Class` <- findInterval(svr_table$Actual, 
                                           seq(0, 1, by=0.2), rightmost.closed = TRUE)
svr_table$`Predicted Class` <- findInterval(svr_table$Prediction, 
                                              seq(0, 1, by=0.2), rightmost.closed = TRUE)

svr_table %<>% 
  mutate(`Predicted Class` = if_else(Prediction < 0, -Inf, as.double(`Predicted Class`)),
         `Predicted Class` = if_else(Prediction > 10, Inf, as.double(`Predicted Class`)))
conf_matrix <- table(svr_table$`Actual Class`, svr_table$`Predicted Class`)
idx <- c(1:5)
conf_matrix <- cbind(conf_matrix, N = apply(conf_matrix, 1, sum), 
                     Confirmed = sapply(idx, function(x) conf_matrix[x, x + 1]))
conf_matrix <- cbind(conf_matrix, Ratio = round(conf_matrix[,8]/conf_matrix[,7], 2))
conf_matrix <- data.frame(conf_matrix)   
names(conf_matrix)[1:6] <- c("<0", paste0("#", 1:6))

hist(conf_matrix$Ratio)
barplot(conf_matrix$Ratio, 
        names.arg = c("[0-0.2)", "[0.2-0.4)", "[0.4-0.6)", "[0.6-0.8)", "[0.8-1.0]"),
        main = "FRCI Classes Accuracy Prediction",
        xlab = paste("FRCI Range", "(0 \u2264 x \u2264 1)"), ylab = "Accuracy ratio")

svr_table$catFRCI2 <- findInterval(svr_table$Actual, 
                                     seq(0, 1, by=0.2), rightmost.closed = TRUE)
tbl_rmse <- svr_table %>% group_by(catFRCI2) %>% summarise(RMSE = (Actual - Prediction)^2 %>% 
                                                               sum %>% divide_by(n()) %>% 
                                                               sqrt)
tbl_rmse

gRMSE <- tbl_rmse %>% ggplot(aes(x = catFRCI2, y = RMSE, fill = as.factor(catFRCI2)))
gRMSE <- gRMSE + geom_bar(stat = "identity")
gRMSE + 
  scale_fill_manual(values=c("#FF2323", "#FDAE61", 
                             "#BFFFC0", "#56DD42", "#106128")) + 
  #name = "FRCI", 
  guides(fill = "none") +
  #breaks = as.factor(c(1, 2, 3, 4, 5)),
  # labels = c("[0-0.2)", "[0.2-0.4)", "[0.4-0.6)", "[0.6-0.8)", "[0.8-1.0]")) +
  scale_x_continuous(breaks = c(1:5),
                     labels = c("[0-0.2)", "[0.2-0.4)", "[0.4-0.6)", "[0.6-0.8)", "[0.8-1.0]")) +
  labs(x = paste("\nFRCI Range", "(0 \u2264 x \u2264 1)"),
       y = paste("RMSE\n"))

ggsave("PROCESSED_DATA/OLDS/RASTER_EVAL/POSTER/barplot-rmse-svr.tiff", 
       width = 15, height = 8.33333, units = "cm")
