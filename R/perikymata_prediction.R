#' Predict perikymata counts in worn human teeth
#'
#' @param data a data.frame with the perikymata counts in the decils that are preserved in the tooth.
#' @param tooth a character representing the tooth type to predict perikymata. Only four tooth types are available: "I" Incisors, "C" Canines, "P" Premolars, "M" Molars
#' @param n_decils a character representing the number of decils to predict perikymata. Only three options are available: "1" predicts perikymata in DC1, "2" predicts perikymata in DC1 and DC2, "3" predicts perikymata in DC1, DC2 and DC3.
#' @param decimals a numeric to indicate the number of decimals for the perikymata number. By default it is set to 1.
#' @return a data.frame with the perikymata prediction per decil.
#' @details Reconstruct crown heights and estimate perikymata counts to calculate crown formation times. These variables are key for evolutionary and paleobiological studies.
#' @export

perikymata_prediction <- function(data,
                    tooth = c("I", "C", "P", "M"),
                    n_decils = c("1", "2", "3"),
                    decimals = 1) {
  # Requirements
  # source("R/pre_mdl.R")
  # source("preproc.R")
  require(dplyr)
  require(caret)
  # pre_mdl()
  if(missing(data)) stop("There is no data.")
  if(missing(tooth)) stop("A tooth type is required. I: incisors; C: canines; P: premolars; M: molars.")
  if(missing(n_decils)) stop("You must select the number of decils to predict. 1: one decil (DC1); 2: two decils (DC1-DC2); 3: three decils (DC1-DC2-DC3).")
  tooth <- match.arg(tooth)
  n_decils = match.arg(n_decils)

  if (tooth == "I" && n_decils == "1") {
    data$DC1 <- 1
    scaled_data <- predict(pre_mdl_I, data)
    pre <- predict(ann_incisor_1_DC1, scaled_data)
    pre_df <- as.data.frame(pre)
    colnames(pre_df) <- "DC1"
    dummy_df <- pre_df %>% #
      mutate(DC2 = DC1) %>%
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_I, dummy_df)
    DC1 <- unscal["DC1"]
  }

  if (tooth == "I" && n_decils == "2") {
    data$DC1 <- 1
    data$DC2 <- 1
    scaled_data <- predict(pre_mdl_I, data)
    pre1 <- predict(ann_incisor_12_DC1, scaled_data)
    pre2 <- predict(ann_incisor_12_DC2, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2))
    colnames(pre_df) <- c("DC1", "DC2")
    dummy_df <- pre_df %>% #
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_I, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
  }

  if (tooth == "I" && n_decils == "3") {
    data$DC1 <- 1
    data$DC2 <- 1
    data$DC3 <- 1
    scaled_data <- predict(pre_mdl_I, data)
    pre1 <- predict(ann_incisor_123_DC1, scaled_data)
    pre2 <- predict(ann_incisor_123_DC2, scaled_data)
    pre3 <- predict(ann_incisor_123_DC3, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2, pre3))
    colnames(pre_df) <- c("DC1", "DC2", "DC3")
    dummy_df <- pre_df %>% #
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_I, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
    DC3 <- unscal["DC3"]
  }

  if (tooth == "C" && n_decils == "1") {
    data$DC1 <- 1
    scaled_data <- predict(pre_mdl_C, data)
    pre <- predict(ann_canine_1_DC1, scaled_data)
    pre_df <- as.data.frame(pre)
    colnames(pre_df) <- "DC1"
    dummy_df <- pre_df %>% #
      mutate(DC2 = DC1) %>%
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_C, dummy_df)
    DC1 <- unscal["DC1"]
  }

  if (tooth == "C" && n_decils == "2") {
    data$DC1 <- 1
    data$DC2 <- 1
    scaled_data <- predict(pre_mdl_C, data)
    pre1 <- predict(ann_canine_12_DC1, scaled_data)
    pre2 <- predict(ann_canine_12_DC2, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2))
    colnames(pre_df) <- c("DC1", "DC2")
    dummy_df <- pre_df %>% #
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_C, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
  }

  if (tooth == "C" && n_decils == "3") {
    data$DC1 <- 1
    data$DC2 <- 1
    data$DC3 <- 1
    scaled_data <- predict(pre_mdl_C, data)
    pre1 <- predict(ann_canine_123_DC1, scaled_data)
    pre2 <- predict(ann_canine_123_DC2, scaled_data)
    pre3 <- predict(ann_canine_123_DC3, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2, pre3))
    colnames(pre_df) <- c("DC1", "DC2", "DC3")
    dummy_df <- pre_df %>% #
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_C, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
    DC3 <- unscal["DC3"]
  }

  if (tooth == "P" && n_decils == "1") {
    data$DC1 <- 1
    scaled_data <- predict(pre_mdl_P, data)
    pre <- predict(ann_premolar_1_DC1, scaled_data)
    pre_df <- as.data.frame(pre)
    colnames(pre_df) <- "DC1"
    dummy_df <- pre_df %>% #
      mutate(DC2 = DC1) %>%
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_P, dummy_df)
    DC1 <- unscal["DC1"]  }

  if (tooth == "P" && n_decils == "2") {
    data$DC1 <- 1
    data$DC2 <- 1
    scaled_data <- predict(pre_mdl_P, data)
    pre1 <- predict(ann_premolar_12_DC1, scaled_data)
    pre2 <- predict(ann_premolar_12_DC2, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2))
    colnames(pre_df) <- c("DC1", "DC2")
    dummy_df <- pre_df %>% #
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_P, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
  }

  if (tooth == "P" && n_decils == "3") {
    data$DC1 <- 1
    data$DC2 <- 1
    data$DC3 <- 1
    scaled_data <- predict(pre_mdl_P, data)
    pre1 <- predict(ann_premolar_123_DC1, scaled_data)
    pre2 <- predict(ann_premolar_123_DC2, scaled_data)
    pre3 <- predict(ann_premolar_123_DC3, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2, pre3))
    colnames(pre_df) <- c("DC1", "DC2", "DC3")
    dummy_df <- pre_df %>% #
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_P, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
    DC3 <- unscal["DC3"]
  }

  if (tooth == "M" && n_decils == "1") {
    data$DC1 <- 1
    scaled_data <- predict(pre_mdl_M, data)
    pre <- predict(ann_molar_1_DC1, scaled_data)
    pre_df <- as.data.frame(pre)
    colnames(pre_df) <- "DC1"
    dummy_df <- pre_df %>% #
      mutate(DC2 = DC1) %>%
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_M, dummy_df)
    DC1 <- unscal["DC1"]  }

  if (tooth == "M" && n_decils == "2") {
    data$DC1 <- 1
    data$DC2 <- 1
    scaled_data <- predict(pre_mdl_M, data)
    pre1 <- predict(ann_molar_12_DC1, scaled_data)
    pre2 <- predict(ann_molar_12_DC2, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2))
    colnames(pre_df) <- c("DC1", "DC2")
    dummy_df <- pre_df %>% #
      mutate(DC3 = DC1) %>%
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_M, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
  }

  if (tooth == "M" && n_decils == "3") {
    data$DC1 <- 1
    data$DC2 <- 1
    data$DC3 <- 1
    scaled_data <- predict(pre_mdl_M, data)
    pre1 <- predict(ann_molar_123_DC1, scaled_data)
    pre2 <- predict(ann_molar_123_DC2, scaled_data)
    pre3 <- predict(ann_molar_123_DC3, scaled_data)
    pre_df <- as.data.frame(cbind(pre1, pre2, pre3))
    colnames(pre_df) <- c("DC1", "DC2", "DC3")
    dummy_df <- pre_df %>% #
      mutate(DC4 = DC1) %>%
      mutate(DC5 = DC1) %>%
      mutate(DC6 = DC1) %>%
      mutate(DC7 = DC1) %>%
      mutate(DC8 = DC1) %>%
      mutate(DC9 = DC1) %>%
      mutate(DC10 = DC1)
    unscal <- unPreProc(pre_mdl_M, dummy_df)
    DC1 <- unscal["DC1"]
    DC2 <- unscal["DC2"]
    DC3 <- unscal["DC3"]
  }

  # Build dataframe with predictions

  if (n_decils == "1") {prediction <- cbind(DC1)}
  if (n_decils == "2") {prediction <- cbind(DC1, DC2)}
  if (n_decils == "3") {prediction <- cbind(DC1, DC2, DC3)}

  prediction <- round(prediction, decimals) # 1 decimal by default
  return(prediction)
}









