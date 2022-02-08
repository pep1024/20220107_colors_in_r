image(1:nrow(ddf), 1, as.matrix(1:nrow(ddf)), 
  col=rgb(ddf$red, ddf$green, ddf$blue, maxColorValue = 255),
  pty = "m", 
  axes = FALSE, ann = FALSE,
  bty ="o", mar = c(0,0,0,0), useRaster = TRUE)


par(mai = c(0, 0, 0, 0))
par(xaxs = 'i', yaxs = 'i')

set.seed(1023)
plot(runif(10), runif(10), type = 'n', 
  yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0, 1))
rect(0,0,1,1, col = "salmon")
points(runif(10), runif(10))

abline(h = c(0, 1), v = c(0, 1), lty = 2)
text(0.05, 0.15, "par(xaxs = 'i', yaxs = 'i') eliminates the white area", adj = 0)
box("plot", lty = "11")

par()$mar


### reproducing image with all named colors in R


all_colors <- r_colors_list()
len_name <- sapply(all_colors$name, nchar)
max(len_name)
all_colors[len_name == max(len_name),]



png("all_colors_plot.png", width = 1200, height = 1200, units = "px", res = 96)
color_view(1)
dev.off()

png("test_margin.png")
oldpar <- par(mai = c(0, 0, 0, 0), xaxs = 'i', yaxs = 'i')

plot.new()

# set the fig margins
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
  axes = FALSE, xlab = "", ylab = "")

abline(h = c(0.5, 0.8), v = c(0.5, 0.8), lty = 4)
box("figure", lty = 1, col = "red")
box("plot", lty = 4, col = "green")
box("inner", lty = 2, col = "blue")
box("outer", lty = 3, col = "black")

dev.off()
par(oldpar)

png("all_colors_plot_with_axes.png", width = 1200, height = 1200, units = "px", res = 96)
all_colors_plot()
dev.off()
