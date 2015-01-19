# TODO: Fix plot labels

plot_ss <- function(x, y, showSquares = FALSE, leastSquares = FALSE){
  plot(y~x, asp = 1)# xlab = paste(substitute(x)), ylab = paste(substitute(y)))
  
  if(leastSquares){
    m1 <- lm(y~x)
    y.hat <- m1$fit
  } else{
    cat("Click two points to make a line.")
    pt1 <- locator(1)
    points(pt1$x, pt1$y, pch = 4)
    pt2 <- locator(1)
    points(pt2$x, pt2$y, pch = 4)
    pts <- data.frame("x" = c(pt1$x, pt2$x),"y" = c(pt1$y, pt2$y))
    m1 <- lm(y ~ x, data = pts)
    y.hat <- predict(m1, newdata = data.frame(x))
  }
  r <- y - y.hat
  abline(m1)

  oSide <- x - r
  LLim <- par()$usr[1]
  RLim <- par()$usr[2]
  oSide[oSide < LLim | oSide > RLim] <- c(x + r)[oSide < LLim | oSide > RLim] # move boxes to avoid margins

  n <- length(y.hat)
  for(i in 1:n){
    lines(rep(x[i], 2), c(y[i], y.hat[i]), lty = 2, col = "blue")
    if(showSquares){
    lines(rep(oSide[i], 2), c(y[i], y.hat[i]), lty = 3, col = "orange")
    lines(c(oSide[i], x[i]), rep(y.hat[i],2), lty = 3, col = "orange")
    lines(c(oSide[i], x[i]), rep(y[i],2), lty = 3, col = "orange")
    }
  }

  SS <- round(sum(r^2), 3)
  cat("\r                                ")
  print(m1)
  cat("Sum of Squares: ", SS)
}