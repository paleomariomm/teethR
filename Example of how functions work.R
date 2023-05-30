perikymata_example <- data.frame(
  DC4  = c(9),
  DC5  = c(11),
  DC6  = c(14),
  DC7  = c(17),
  DC8  = c(19),
  DC9  = c(21),
  DC10 = c(21))

# rownames(perikymata_example) <- c("OMO 25-5", "L 51-54", "MISSING")

perikymata_prediction(perikymata_example,
                   tooth = "C",
                   n_decils = "3",
                   decimals = 2)

library(devtools)
devtools::install_github('paleomariomm/teethR')
library(teethR)

perikymata_prediction(perikymata_example, tooth = "P", n_decils = "3", decimals = 3)

### EXAMPLE RECONSTRUCTION

crown_height_recons_2d("images/molar2.jpg", tooth_type = "LM", interval = "confidence", save_svg = "yes", color_points = "darkblue")



