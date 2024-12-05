#' Predict perikymata counts in hominin worn teeth in our evolutionary history
#'
#' @param data a data.frame with the perikymata counts in the deciles that are preserved in the tooth, as long as the tooth ID.
#' @param deciles a character representing the number of deciles to predict perikymata. Only three options are available: "1" predicts perikymata in DC1, "2" predicts perikymata in DC1 and DC2, "3" predicts perikymata in DC1, DC2 and DC3.
#' @param decimals a numeric to indicate the number of decimals for the perikymata number. By default it is set to 1.
#' @return a data.frame with the perikymata prediction per decil, a data.frame with the total perikymata and a plot with the distrubition of perikymata across deciles.
#' @examples
#' # Creation of a dummy dataframe of a premolar with perikymata from DC2 to DC10
# perikymata_example <- data.frame(
#   Tooth = c("Tooth_1", "Tooth_2", "Tooth_3"),
#   DC2  = c(10, 11, 4),
#   DC3  = c(10, 10, 5),
#   DC4  = c(11, 13, 5),
#   DC5  = c(11, 13, 8),
#   DC6  = c(14, 15, 10),
#   DC7  = c(17, 15, 12),
#   DC8  = c(19, 20, 13),
#   DC9  = c(21, 23, 15),
#   DC10 = c(21, 25, 15))
#'
#' # Running the function to predict perikymata number in the first decile (DC1) of this dummy premolar.
#' perikymata_fossil_pred(perikymata_example,
#'                       deciles = "3",
#'                       decimals = 1)
#' @details Reconstruct crown heights and estimate perikymata counts to calculate crown formation times. These variables are key for evolutionary and paleobiological studies.
#' @export

perikymata_fossil_pred <- function(data,
                                  deciles = c("1", "2", "3"),
                                  decimals = 1) {
  # Requirements
  require(keras)
  require(dplyr)
  require(ggplot2)
  require(tidyr)

  if(missing(data)) stop("There is no data.")
  if(missing(deciles)) stop("You must select the number of deciles to predict. 1: one decile (DC1); 2: two deciles (DC1-DC2); 3: three deciles (DC1-DC2-DC3).")
  deciles = match.arg(deciles)

  if (deciles == "1") {
    data_filter <- data %>%
      select("DC2":"DC10")
    new_model_DC1 <- load_model_tf('saved_model/DC1_training')
    predict_DC1 <- as.data.frame(predict(new_model_DC1, as.matrix(data_filter)))
    colnames(predict_DC1) <- c("DC1_pre")
    prediction_table <- cbind(predict_DC1, data_filter)
  }

  if (deciles == "2") {
    data_filter <- data %>%
      select("DC3":"DC10")
    new_model_DC12 <- load_model_tf('saved_model/DC12_training')
    predict_DC12 <- as.data.frame(predict(new_model_DC12, as.matrix(data_filter)))
    colnames(predict_DC12) <- c("DC1_pre", "DC2_pre")
    prediction_table <- cbind(predict_DC12, data_filter)
  }

  if (deciles == "3") {
    data_filter <- data %>%
      select("DC4":"DC10")
    new_model_DC123 <- load_model_tf('saved_model/DC123_training')
    predict_DC123 <- as.data.frame(predict(new_model_DC123, as.matrix(data_filter)))
    colnames(predict_DC123) <- c("DC1_pre", "DC2_pre", "DC3_pre")
    prediction_table <- cbind(predict_DC123, data_filter)
  }

  # Table with predictions
  prediction <- round(prediction_table, decimals) # 1 decimal by default
  prediction_df <- as.data.frame(cbind(Tooth = data$Tooth, prediction))

  # Plot using ggplot of perikymata
  library(dplyr)
  tab <- prediction_df
  row.names(tab) <- tab$Tooth

  tab_df <- tab %>%
    select(-Tooth) %>%
    t() %>%
    as.data.frame() %>%
    mutate(Deciles = c("DC1", "DC2", "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"),
           Type    = c(rep("Predicted", as.numeric(deciles)), rep("Real", 10-as.numeric(deciles)))) %>%
    gather("Tooth", "Perikymata", 1:(ncol(.)-2))

  tab_df$Deciles <- factor(tab_df$Deciles , levels=c("DC1", "DC2", "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"))

  ggplot_perikymata <- ggplot(tab_df, aes(x = Deciles, y = Perikymata, color = Tooth, group = Tooth)) +
    geom_point(aes(x = Deciles, y = Perikymata, shape = Type), size = 3) +
    scale_shape_manual(values=c(15,19)) +
    geom_line(lwd = 1, alpha = 0.5, lty = 1) +
    ylim(0, max(tab_df$Perikymata)) +
    annotate("rect", xmin = 0.7, xmax = as.numeric(deciles)+0.3, ymin = 0, ymax = max(tab_df$Perikymata),
             alpha = .1, fill = "red")

  # Total perikymata count
  total_perikymata <- prediction_df %>%
    mutate(Total_Perikymata = rowSums(across(where(is.numeric)))) %>%
    select(Tooth, Total_Perikymata)

  # Return a list
  return(list(Perikymata_decile = prediction_df,
              Perikymata_total = total_perikymata,
              Perikymata_Decile_Plot = ggplot_perikymata))
}
