# Set up the plot
plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="Współczynnik fałszywych pozytywów (FPR)", ylab="Czułość (TPR)", xaxs="i", yaxs="i")

# Add the grid
abline(v=seq(0, 1, by=0.1), lty=3, col="lightgray")
abline(h=seq(0, 1, by=0.1), lty=3, col="lightgray")

# Add the title
# title(main="Krzywa ROC")

# Add the blue point and label
points(0, 1, col="navy", pch=20, cex=2, lwd=2)
text(0.17, 0.91, "Klasyfikator idealny", pos=3, col="navy")

# Add the green line and label with a thicker line
x_values <- seq(0, 1, length.out=100)
y_values <- x_values^(1/7)
lines(x_values, y_values, col="#007788", lwd=3, lty="dashed")

x_values <- seq(0, 1, length.out=100)
y_values <- x_values^(1/7)

# Add the orange line and label with a thicker line
x_values <- seq(0, 1, length.out=100)
y_values <- x_values^(1/3)
lines(x_values, y_values, col="#FFA700", lwd=3, lty="dashed")

# Add the red line and label with a thicker line
abline(a=0, b=1, col="red", lwd=3, lty="dashed")
text(0.6, 0.36, "Klasyfikator losowy", pos=3, col="red")

x_values <- seq(0, 1, length.out=100)
y_values <- seq(0, 1, length.out=100)

# Add the vector perpendicular to the red line
arrows(0.7, 0.7, 0.665, 0.76, length = 0.1, lwd = 2, col = "black")
text(0.32, 0.73, "Klasyfikator lepszy", pos=4, col="black")

arrows(0.7, 0.7, 0.735, 0.64, length = 0.1, lwd=2, col = "black")
text(0.37, 0.63, "Klasyfikator gorszy", pos=4, col="black")
