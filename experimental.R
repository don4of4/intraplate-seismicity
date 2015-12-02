# Load New Zealand catalogue
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ssNZ")

# Define a function to plot vertical error bars
error.bar <- function(x, y, upper, lower=upper, length=0.05){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, lwd=0.5)
}
# Define a catalogue subset and extract magnitudes
a <- subsetcircle(NZ, minday=julian(1,1,1966), maxday=julian(1,1,2006),
                  minmag=1.0,
                  maxradius=150, centrelat=-37.65, centrelong=179.49)
as.catalogue(a, catname="EastCape")
Mi <- EastCape$magnitude
maxmag <- ceiling(max(Mi))
# Set list of magnitude cutoffs
incr <- 0.1
Mc <- seq(1, maxmag, incr)
# Loop over all magnitude cutoffs to calculate
b-value, standard error and save
to ANS
ans <- NULL
for (i in 1:length(Mc)) {
  j <- which(Mi>=Mc[i])
  rate.mle <- 1/mean(Mi[j] - Mc[i])
  b.mle <- rate.mle/log(10)
  b.sterr <- b.mle/sqrt(length(j))
  ans <- rbind(ans, c(Mc[i], b.mle, b.sterr))
}
colnames(ans) <- c("Mc", "b", "sterr")
# Plot b-value vs magnitude cutoff with error bars
plot(ans[,"Mc"], ans[,"b"], xlab="Mc", ylab="b-value est.", ylim=c(0,4),
     cex=0.2)
error.bar(ans[,"Mc"], ans[,"b"], ans[,"sterr"])