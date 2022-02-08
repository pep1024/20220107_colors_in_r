#' Plot all named color in R
#'
#' @return Png file with a plot of all 657 named color in R
#' @export
#'
#' @examples
all_colors_plot <- function(){
  
  # Based on the code in 
  # [R-gallery](https://www.r-graph-gallery.com/42-colors-names.html)  
  
  # Settings
  line <- 66
  col <- 10
  
  # properties for the plot
  old_par <- par(
    mar= c(1, 3, 4, 1) + 0.1, 
    oma = c(2, 0, 0, 0), 
    xaxs = 'i', yaxs = 'i', no.readonly = TRUE)
  
  # Empty chart
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
    axes = FALSE,
    xlab = "", ylab = "", )
  
  # axis guidelines
  axis_color <- "gray25"
  x_axis_pos <- - 1 / col / 2 + c(0.1, 0.5, 1)
  axis(3, at = x_axis_pos, labels = c(1, 5, 10), col = axis_color,
    col.axis = axis_color, cex.axis = 0.7, tcl = -0.2, mgp = c(3,0.5,0))
  y_axis_pos <- 1 - (1/line/2 + c(1, seq(5, 65, by = 5)) / line)
  axis(2, at = y_axis_pos, 
    labels = 10 * c(1, seq(5, 65, by = 5)), las = 2, col = axis_color,
    col.axis = axis_color, cex.axis = 0.7, tcl = -0.2, mgp = c(3,0.5,0))
  
  # sequence on 657 colors + 3 white
  seq_col <- c(1:657, rep(1, 3))
  
  # rect dimensions
  xl <- rep((0:(col - 1)/col),line)
  yb <- sort(rep((0:(line - 1)/line), col), decreasing=T)
  xr <- rep((1:col/col),line) 
  yt <- sort(rep((1:line/line), col), decreasing=T)
  
  # Add color background
  rect(  
    xl, yb, xr, yt,
    border = "white" , 
    col=colors()[seq_col])
  
  # text position
  x_text <- (xl + xr) / 2
  y_text <- (yb + yt) / 2
  
  # Change text color depending on color
  # Based on show_col() function in {scales} package
  # hcl <- farver::decode_colour(colors(), "rgb", "hcl")
  # label_col <- ifelse(hcl[, "l"] > 50, "black", 
  #   "white")
  # Based on R graphics by P. Murell
  huv <- convertColor(
    t(col2rgb(colors())/255), "sRGB", "Luv")
  label_col <- ifelse(huv[, "L"] > 50, "black", 
    "white")
  
  # Color names
  text(x_text, y_text, adj = c(0.5, 0.5),
    c(colors(), rep("", 3)), col = c(label_col, rep("white", 3)),
    cex=0.7)
  
  # title and other decorations
  
  title(main = "All 657 built-in named colors in R", adj = 0, line = 2)
  mtext("n=x+y coordinates gives the color index in colors() vector", 
    line = 0, side = 1, adj = 0, cex = 0.8, col = axis_color)
  mtext("For instance, for x=140 and y=4, n=144 and colors()[144]=='gold2'",
    line = 1, side = 1, adj = 0, cex = 0.8, col = axis_color)
  mtext("https://github.com/pep1024/20220107_colors_in_r", line = 0, side = 1, adj = 1,
    cex = 0.8, col = axis_color)
  mtext("2022-01-11 josepmporra@gmail.com", line = 1, side = 1, adj = 1,
    cex = 0.8, col = axis_color)
  par(old_par)
}

file_name <- "all_R_named_colors_plot.png"
if(!file.exists(file_name)) {
  png(file_name, 
    width = 1200, height = 1200, units = "px", res = 96)
  all_colors_plot()
  dev.off()
} else {
  message(paste0("File: ", file_name, " Already exists"))
}
