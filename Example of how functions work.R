devtools::install_github('paleomariomm/teethR')
library(teethR)

### Reconstruction of crown height

crown_height_recons_2d("images/premolar.png",
                       tooth_type = "LP",
                       save_svg = "yes",
                       color_points = "red",
                       color_regression_line = "blue",
                       file_name_svg = "tooth_recons.svg")

### Perikymata prediction when 1 decil is not present
library(teethR)


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

perikymata_prediction(perikymata_example,
                      tooth = "P",
                      n_decils = "1",
                      decimals = 1)

