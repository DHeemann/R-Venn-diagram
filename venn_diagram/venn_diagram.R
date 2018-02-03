# This script will help to visualize a venn diagram in the context of multiple regression models. 
# The variables will be reperenseted by circles. The overlap of each variable with the others will be 
# be determined by their squared correlations (explained variance)
# The aim of this is to have a more intuitive understanding of concepts such as 
# collinearity among predictors, semi-partial-
# correlations and partial correlations. 

circle <- function(radius,d,h, col) {
  # draws a circle
  # input:
  #       radius: the radius of the circle
  #       d: position of circle center on the x-axis
  #       h: position of circle center on the y-axis
  #       col : colour of circle 
  x = seq(-radius, radius, by = 0.01)
  y <- sqrt(radius - x^2) 
  x = x + d
  y = y + h
  lines(x,y,type = "l")
  lines(x,-y + 2*h)
  polygon(c(x,rev(x)),c(y,-y + 2*h), col = rgb(t(col2rgb(col)/255), alpha = 0.5))
}



venn_diagram <- function(Ryx1, Ryx2, Rx1x2) {
    # input: 
    #       Ryx1: The squared correlation between y (DV) and x1 (IV)
    #       Ryx2: The squared correlation between y(DV) and x2 (IV)
    #       Rx1x2:The squared correlation between x1 and x2
    
    # finding coordinates of the circle for x1
    d <- seq(0,2, by = 0.001)
    o <- (2*acos(d/2) -d*sqrt(1 - d^2/4))/pi
    dis <- d[which.min(abs(o - Ryx1))]

    
    # finding coordinates of x2
    # distances of x2 to y
    x = seq(-2,2, by = 0.02)
    y = seq(0,2, by = 0.02)
    xy <- (expand.grid(x,y))
    # The formula for 'o' will produce some NA's for values where the distance is bigger than 2.
    # This produces some warnings, which can be ignored
    suppressWarnings({
    c <- apply(xy, 1, function(x){ sqrt(sum(x^2))})
    o <- (2*acos(c/2) - c*sqrt(1 - c^2/4))/pi
    Ryx2_dis <- (abs(o - Ryx2))})
    
    # distances of x2 to x1
    xy2 <- cbind(xy[,1] - dis, xy[,2])
    c2 <- apply(xy2, 1, function(x){ sqrt(sum(x^2))})
    suppressWarnings({
    o2 <- (2*acos(c2/2) - c2*sqrt(1 - c2^2/4))/pi
    Rx1x2_dis<- abs(o2 - Rx1x2)})
    
    # finding the coordinates for which the distances are closest to the one 
    # needed for the specified overlap 
    disx2 <- xy[which.min(Ryx2_dis + Rx1x2_dis), ]

    # empty plot
    plot(0,0, xlim = c(-3,3),ylim = c(-3,3), type = "n", 
         xaxt = "n",yaxt = "n", ann = FALSE, bty='n')
    legend("topleft", c("y", "x1", "x2"), 
           text.col = c("blue", "green", "red"),
           col = c("blue", "green", "red"),
           pch = 16, cex = 2)
    # plotting y
    circle(1, d = 0, h = 0, col = "blue")
    # plotting x1
    circle(1, d = dis, h = 0, col = "green")
    # plotting x2 
    circle(1, d = disx2[,1], h = disx2[,2], col = "red")
    
    # showing how much the numeric approximation deviates from the overlap one would expect from the input: 
    cat(min(Ryx2_dis + Rx1x2_dis, na.rm = TRUE))
}

# example
# venn_diagram(0.7,0.7,0.7)
