#' Reconstructing slightly worn human teeth
#'
#' @param image_url Set the URL of the image of the tooth you would like to reconstruct
#' @param tooth_type set the tooth type
#' @param interval set the interval of the regression: prediction / confidence
#' @param color_points select the color of the points of the regression equation ggplot. Default = "red"
#' @param save_svg select if the ggplot should be saved or not as SVG format: yes / no
#' @param file_name_svg if save_svg = "yes", this argument indicates the name of the resulting svg file. Default = "tooth_recons.svg"
#' @return a ggplot object with the regression equation placed appropriately in the region of interest
#' @examples
#' crown_height_recons_2d("images/premolar.png",
#' tooth_type = "LP",
#' save_svg = "yes",
#' color_points = "red",
#' color_regression_line = "blue",
#' file_name_svg = "tooth_recons.svg")
#' @details Reconstructing slightly worn human teeth
#' @export

crown_height_recons_2d <- function(image_url,
                   tooth_type = c("UI", "UC", "UP", "UM", "LI", "LC", "LP", "LM"),
                   color_points = "red",
                   color_regression_line = "blue",
                   save_svg = c("yes", "no"),
                   file_name_svg = "tooth_recons.svg"
                   ) {

  # Load required packages
  require(imager)
  require(grid)
  require(ggplot2)
  require(svglite)
  require(png)
  library(cowplot)
  library(magick)

  # Load data of the polynomial regressions
  load("data/xy_coord_stack.rda")

  # Check if all the arguments are present
  if(missing(image_url)) stop("A valid image address is required")
  if(missing(tooth_type)) stop("A tooth type is required")

  # Check if tooth_type, interval and save_svg have one of the valid values
  tooth_type <- match.arg(tooth_type)
  save_svg   <- match.arg(save_svg)

  # Removing quotation marks from tooth_type
  tooth_type <- noquote(tooth_type)

  # Select points in the image
  img <- load.image(image_url)
  X11()
  plot(img)
  mtext("Step 1. Select dentine horn (1 click)", side = 3, line = 1.5)
  cusp_tip <- locator(n = 1, type = "o", col = "blue")
  points(cusp_tip, pch = 4, col = "blue", cex = 2)
  abline(h = cusp_tip[[2]], col = "red")
  mtext("Step 2. Select outer enamel surface over the red line (1 click)", line = 0.5)
  out_enamel <- locator(n = 1, type = "o", col = "blue")
  points(out_enamel, pch = 4, col = "blue", cex = 2)
  dev.off()

  # Data frame of XY coordinates of dentine horn (dh) and outer enamel (oe)
  dh <-unlist(cusp_tip)   # dentine horn
  oe <-unlist(out_enamel) # outer enamel
  landmark_XY <- data.frame(rbind(dh, oe))

  # Get image dimensions and isolate some coordinates from landmark_XY
  img_wd_px <- dim(img)[1]      # image width in pixels
  img_hg_px <- dim(img)[2]      # image height in pixels
  xpr100 <- landmark_XY[2,1]    # X coordinate of the Xprotoconid 100
  xpr0   <- landmark_XY[1,1]    # X coordinate of the Xprotoconid 0 (dentine horn)
  ypr0   <- landmark_XY[1,2]    # Y coordinate of the Yprotoconid 0 (dentine horn)

  image_dimension <- data.frame(width = img_wd_px, height = img_hg_px)
  rownames(image_dimension) <- image_url

  # XY proportion per tooth type
  index_tooth <- list(UI = 0.5928,
                      LI = 0.6583,
                      UC = 0.5980,
                      LC = 0.5840,
                      UP = 0.6406,
                      LP = 0.6290,
                      UM = 0.7456,
                      LM = 0.6698)

  # Polynomial regression
  poly_reg <- lm(Y ~ X + I(X^2) + I(X^3), data = data.frame(xy_coord_stack[[tooth_type]]))

  # ggplot images depending on the interval selected:
  gg <- ggplot(data.frame(xy_coord_stack[[tooth_type]]), aes(x = X, y = Y)) +
          coord_fixed(ratio = index_tooth[[tooth_type]]) + # aquí poner la proporción por diente
          geom_point(alpha = .20, size = 0.5, shape = 16, color = color_points) +
          stat_smooth(method = 'lm', formula = y ~ poly(x, 3), color = color_regression_line) +
          theme_void() +
          geom_hline(aes(yintercept = 0), color = "red", lty = 2) +
          geom_vline(aes(xintercept = 0), color = "red", lty = 2) +
          scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

  # Calculate proportions of plot width and height compared to image width and height
  difpr_x <- xpr100-xpr0 # distancia en px entre xpr100 y xpr0.
  prop_x  <- difpr_x / img_wd_px # proporción de difpr vs. la anchura de la imagen en px
  difpr_y <- difpr_x * index_tooth[[tooth_type]] # altura en pixeles de Y (entre y0 e y100, teniendo en cuenta proporción con X)
  prop_y  <- difpr_y / img_hg_px # proporción de difpr vs. la anchura de la imagen en px
  prop <- c(prop_x,prop_y) # construimos vector con las dos proporciones
  prop_mx <- max(prop) # seleccionamos la mayor proporción para luego escalar ggplot en la imagen en base a altura/anchura

  # Con este código abrimos ventana con justo el tamaño de la imagen de CT con la que trabajemos
  # https://stackoverflow.com/questions/2129952/creating-a-plot-window-of-a-particular-size
  dev.new(width = img_wd_px, height = img_hg_px, unit = "px", noRStudioGD = TRUE)
  ggoverlap <- ggdraw() +
    draw_image(image_url) +
    draw_plot(gg,
              scale = prop_mx,
              x = (xpr0 / img_wd_px) - 0.5 + prop_x/2,
              y = 0.5 - (ypr0 / img_hg_px) + prop_y/2)

  # Wether the plot should be saved as SVG file or not
  if(save_svg == "yes") {
    ggsave(file = file_name_svg, plot = ggoverlap, limitsize = FALSE)
    dev.off()
  } else {
    dev.off()
  }

  # Return a list of objects
  dev.new(width = img_wd_px, height = img_hg_px, unit = "px", noRStudioGD = TRUE)
  return(list("XY_Coordinates" = landmark_XY,
              "image_dimension_px" = image_dimension,
              "prop_regression_xy" = data.frame(Tooth_type = tooth_type, Proportion = index_tooth[[tooth_type]]),
              "overlap_regression_tooth" = ggoverlap))
}
