

  deciles <-  c(0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)
  lift <- c(3.5 ,3.3, 3.0, 2.5, 2, 1.6, 1.3, 1.1, 1, 1, 1)


# Set up the plot
plot(0, 0, type="n", xlim=c(0, 1), ylim=c(0, max(lift)), xlab="Odsetek próby", ylab="Lift", xaxs="i", yaxs="i")

abline(v=seq(0, max(lift), by=0.1), lty=3, col="lightgray")
abline(h=seq(0, max(lift), by=0.5), lty=3, col="lightgray")

# Add the lift curve
lines(deciles, lift, col="#007788", lwd=2)

# Add the reference line
abline(h=1, col="red", lty=2, lwd = 2)

# Add the title and legend
legend("topright", legend=c("Lift (model lepszy od losowego)", "Lift (klasyfikacja losowa)"), col=c("#007788", "red"), lty=c(1, 2), lwd=c(2, 2))
