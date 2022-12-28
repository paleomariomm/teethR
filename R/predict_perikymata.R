predict_perikymata <- function(data,
                               tooth = c("I", "C", "P", "M"),
                               n_decils = c("1", "2", "3"),
                               decimals = 1) {
  if(missing(data)) stop("There is no data.")
  if(missing(tooth)) stop("A tooth type is required. I: incisors; C: canines; P: premolars; M: molars.")
  if(missing(n_decils)) stop("You must select the number of decils to predict. 1: one decil (DC1); 2: two decils (DC1-DC2); 3: three decils (DC1-DC2-DC3).")
  tooth <- match.arg(tooth)
  n_decils = match.arg(n_decils)

  # Function to convert response
  convert_response = function(value, mdl, method, column){
    bounds = mdl[[method]][,column]
    value*diff(bounds) + min(bounds)
  }

  # preProcess to denormalize the data for incisors, canines, premolars and molars
  pre_mdl_I <- structure(list(dim = c(26L, 10L), bc = NULL, yj = NULL, et = NULL,
                              invHyperbolicSine = NULL, mean = NULL, std = NULL, ranges = structure(c(4,
                                                                                                      17, 5, 16, 6, 14, 7, 14, 9, 19, 10, 23, 11, 29, 14, 33, 16,
                                                                                                      33, 18, 50), dim = c(2L, 10L), dimnames = list(NULL, c("DC1",
                                                                                                                                                             "DC2", "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"
                                                                                                      ))), rotation = NULL, method = list(range = c("DC1", "DC2",
                                                                                                                                                    "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"),
                                                                                                                                          ignore = character(0)), thresh = 0.95, pcaComp = NULL,
                              numComp = NULL, ica = NULL, wildcards = list(PCA = character(0),
                                                                           ICA = character(0)), k = 5, knnSummary = function (x,
                                                                                                                              ...)
                                                                             UseMethod("mean"), bagImp = NULL, median = NULL, data = NULL,
                              rangeBounds = c(0, 1)), class = "preProcess")
  pre_mdl_C <- structure(list(dim = c(36L, 10L), bc = NULL, yj = NULL, et = NULL,
                              invHyperbolicSine = NULL, mean = NULL, std = NULL, ranges = structure(c(7,
                                                                                                      18, 7, 19, 8, 21, 7, 23, 8, 29, 11, 36, 13, 38, 16, 47, 17,
                                                                                                      39, 19, 41), dim = c(2L, 10L), dimnames = list(NULL, c("DC1",
                                                                                                                                                             "DC2", "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"
                                                                                                      ))), rotation = NULL, method = list(range = c("DC1", "DC2",
                                                                                                                                                    "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"),
                                                                                                                                          ignore = character(0)), thresh = 0.95, pcaComp = NULL,
                              numComp = NULL, ica = NULL, wildcards = list(PCA = character(0),
                                                                           ICA = character(0)), k = 5, knnSummary = function (x,
                                                                                                                              ...)
                                                                             UseMethod("mean"), bagImp = NULL, median = NULL, data = NULL,
                              rangeBounds = c(0, 1)), class = "preProcess")
  pre_mdl_P <- structure(list(dim = c(68L, 10L), bc = NULL, yj = NULL, et = NULL,
                              invHyperbolicSine = NULL, mean = NULL, std = NULL, ranges = structure(c(4,
                                                                                                      13, 4, 12, 4, 12, 3, 17, 5, 16, 5, 16, 7, 20, 11, 26, 11,
                                                                                                      30, 13, 38), dim = c(2L, 10L), dimnames = list(NULL, c("DC1",
                                                                                                                                                             "DC2", "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"
                                                                                                      ))), rotation = NULL, method = list(range = c("DC1", "DC2",
                                                                                                                                                    "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"),
                                                                                                                                          ignore = character(0)), thresh = 0.95, pcaComp = NULL,
                              numComp = NULL, ica = NULL, wildcards = list(PCA = character(0),
                                                                           ICA = character(0)), k = 5, knnSummary = function (x,
                                                                                                                              ...)
                                                                             UseMethod("mean"), bagImp = NULL, median = NULL, data = NULL,
                              rangeBounds = c(0, 1)), class = "preProcess")
  pre_mdl_M <- structure(list(dim = c(34L, 10L), bc = NULL, yj = NULL, et = NULL,
                              invHyperbolicSine = NULL, mean = NULL, std = NULL, ranges = structure(c(3,
                                                                                                      12, 4, 10, 4, 15, 4, 10, 5, 13, 6, 13, 5, 15, 5, 23, 9, 34,
                                                                                                      14, 34), dim = c(2L, 10L), dimnames = list(NULL, c("DC1",
                                                                                                                                                         "DC2", "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"
                                                                                                      ))), rotation = NULL, method = list(range = c("DC1", "DC2",
                                                                                                                                                    "DC3", "DC4", "DC5", "DC6", "DC7", "DC8", "DC9", "DC10"),
                                                                                                                                          ignore = character(0)), thresh = 0.95, pcaComp = NULL,
                              numComp = NULL, ica = NULL, wildcards = list(PCA = character(0),
                                                                           ICA = character(0)), k = 5, knnSummary = function (x,
                                                                                                                              ...)
                                                                             UseMethod("mean"), bagImp = NULL, median = NULL, data = NULL,
                              rangeBounds = c(0, 1)), class = "preProcess")

  # Combination

  if (tooth == "I" && n_decils == "1") {
    DC1 = convert_response(predict(ann_incisor_1_DC1, data), pre_mdl_I, "ranges", "DC1")
  }

  if (tooth == "I" && n_decils == "2") {
    DC1 = convert_response(predict(ann_incisor_12_DC1, data), pre_mdl_I, "ranges", "DC1")
    DC2 = convert_response(predict(ann_incisor_12_DC2, data), pre_mdl_I, "ranges", "DC2")
  }

  if (tooth == "I" && n_decils == "3") {
    DC1 = convert_response(predict(ann_incisor_123_DC1, data), pre_mdl_I, "ranges", "DC1")
    DC2 = convert_response(predict(ann_incisor_123_DC2, data), pre_mdl_I, "ranges", "DC2")
    DC3 = convert_response(predict(ann_incisor_123_DC3, data), pre_mdl_I, "ranges", "DC3")
  }

  if (tooth == "C" && n_decils == "1") {
    DC1 = convert_response(predict(ann_canine_1_DC1, data), pre_mdl_C, "ranges", "DC1")
  }

  if (tooth == "C" && n_decils == "2") {
    DC1 = convert_response(predict(ann_canine_12_DC1, data), pre_mdl_C, "ranges", "DC1")
    DC2 = convert_response(predict(ann_canine_12_DC2, data), pre_mdl_C, "ranges", "DC2")
  }

  if (tooth == "C" && n_decils == "3") {
    DC1 = convert_response(predict(ann_canine_123_DC1, data), pre_mdl_C, "ranges", "DC1")
    DC2 = convert_response(predict(ann_canine_123_DC2, data), pre_mdl_C, "ranges", "DC2")
    DC3 = convert_response(predict(ann_canine_123_DC3, data), pre_mdl_C, "ranges", "DC3")
  }

  if (tooth == "P" && n_decils == "1") {
    DC1 = convert_response(predict(ann_premolar_1_DC1, data), pre_mdl_P, "ranges", "DC1")
  }

  if (tooth == "P" && n_decils == "2") {
    DC1 = convert_response(predict(ann_premolar_12_DC1, data), pre_mdl_P, "ranges", "DC1")
    DC2 = convert_response(predict(ann_premolar_12_DC2, data), pre_mdl_P, "ranges", "DC2")
  }

  if (tooth == "P" && n_decils == "3") {
    DC1 = convert_response(predict(ann_premolar_123_DC1, data), pre_mdl_P, "ranges", "DC1")
    DC2 = convert_response(predict(ann_premolar_123_DC2, data), pre_mdl_P, "ranges", "DC2")
    DC3 = convert_response(predict(ann_premolar_123_DC3, data), pre_mdl_P, "ranges", "DC3")
  }

  if (tooth == "M" && n_decils == "1") {
    DC1 = convert_response(predict(ann_molar_1_DC1, data), pre_mdl_M, "ranges", "DC1")
  }

  if (tooth == "M" && n_decils == "2") {
    DC1 = convert_response(predict(ann_molar_12_DC1, data), pre_mdl_M, "ranges", "DC1")
    DC2 = convert_response(predict(ann_molar_12_DC2, data), pre_mdl_M, "ranges", "DC2")
  }

  if (tooth == "M" && n_decils == "3") {
    DC1 = convert_response(predict(ann_molar_123_DC1, data), pre_mdl_M, "ranges", "DC1")
    DC2 = convert_response(predict(ann_molar_123_DC2, data), pre_mdl_M, "ranges", "DC2")
    DC3 = convert_response(predict(ann_molar_123_DC3, data), pre_mdl_M, "ranges", "DC3")
  }

  # Build dataframe with predictions

  if (n_decils == "1") {
    prediction <- cbind(DC1 = as.data.frame(DC1))
  }

  if (n_decils == "2") {
    prediction <- cbind(DC1 = as.data.frame(DC1),
                        DC2 = as.data.frame(DC2))
  }

  if (n_decils == "3") {
    prediction <- cbind(DC1 = as.data.frame(DC1),
                        DC2 = as.data.frame(DC2),
                        DC3 = as.data.frame(DC3))
  }

  prediction <- round(prediction, decimals) # 1 decimal by default
  return(prediction)
}
