# Set up the plot
plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="Współczynnik fałszywych pozytywów (FPR)", ylab="Czułość (TPR)", xaxs="i", yaxs="i")

# Add the grid
abline(v=seq(0, 1, by=0.1), lty=3, col="lightgray")
abline(h=seq(0, 1, by=0.1), lty=3, col="lightgray")

# Add the green line and label with a thicker line
x_values <- seq(0, 1, length.out=100)
y_values <- x_values^(1/7)
lines(x_values, y_values, col="#007788", lwd=3, lty="dashed")
text(0.350, 0.57, "Klasyfikator lepszy niż losowy \n AUC (0,5; 1]", pos=3, col="#007788")

x_values <- seq(0, 1, length.out=100)
y_values <- x_values^(1/7)
polygon(c(x_values, 1, 0), c(y_values, 0, 0), col="#00778822", border=NA)


# Add the red line and label with a thicker line
abline(a=0, b=1, col="red", lwd=3, lty="dashed")
text(0.68, 0.35, "Klasyfikator losowy \n AUC = 0,5", pos=3, col="red")

x_values <- seq(0, 1, length.out=100)
y_values <- seq(0, 1, length.out=100)
polygon(c(x_values, 1, 0), c(y_values, 0, 0), col=rgb(1, 0, 0, alpha=0.2), alpha = 0.5, border=NA)

