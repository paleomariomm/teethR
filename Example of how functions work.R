devtools::install_github('paleomariomm/teethR')
library(teethR)

### Reconstruction of crown height

crown_height_recons_2d("images/premolar.png", 
                       tooth_type = "LP", 
                       save_svg = "yes",
                       color_points = "red",
                       color_regression_line = "green",
                       file_name_svg = "tooth_recons2.svg")

### Perikymata prediction when 1 decil is not present 

perikymata_example <- data.frame(
  DC2  = c(10),
  DC3  = c(10),
  DC4  = c(11),
  DC5  = c(11),
  DC6  = c(14),
  DC7  = c(17),
  DC8  = c(19),
  DC9  = c(21),
  DC10 = c(21))

# rownames(perikymata_example) <- c("OMO 25-5", "L 51-54", "MISSING")

perikymata_prediction(perikymata_example,
                      tooth = "P",
                      n_decils = "2",
                      decimals = 1)

