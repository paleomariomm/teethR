data1 <- data.frame(
  DC2  = c( 8, 8, 3),
  DC3  = c(12, 9, 5),
  DC4  = c(15,11, 7),
  DC5  = c(16,11,12),
  DC6  = c(14,15,15),
  DC7  = c(17,18,18),
  DC8  = c(18,31,21),
  DC9  = c(19,36,24),
  DC10 = c(29,29,25))

data2 <- data.frame(
  DC3  = c(12,9,5),
  DC4  = c(15,11,7),
  DC5  = c(16,11,12),
  DC6  = c(14,15,15),
  DC7  = c(17,18,18),
  DC8  = c(18,31,21),
  DC9  = c(19,36,24),
  DC10 = c(29,29,25))

data3 <- data.frame(
  DC4  = c(7,10,7),
  DC5  = c(11,10,7),
  DC6  = c(12,12,7),
  DC7  = c(14,9,10),
  DC8  = c(23,12,11),
  DC9  = c(17,17,12),
  DC10 = c(22,19,17))

source("R/pre_mdl.R")
source("R/preproc.R")
library(dplyr)
library(caret)

pred_pk <- function(data,
                    tooth = c("I", "C", "P", "M"),
                    n_decils = c("1", "2", "3"),
                    decimals = 1) {
  # Requirements
  source("R/pre_mdl.R")
  source("R/preproc.R")
  require(dplyr)
  require(caret)

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

pred_pk(data = data3, tooth = "M", n_decils = "3", decimals = 6)









