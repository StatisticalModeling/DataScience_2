# AHRLR modified errors to all have mean 0 and sd of 1.
set.seed(34)
# Normal - no change - N(0, 1)
ne <- rnorm(1000000, 0, 1)
# Skewed error Exp(1) - 1
se <- rexp(1000000, 1) -1
# Uniform error - U[-sqrt(12)/2, sqrt(12)/2]
ue <- runif(1000000, -sqrt(12)/2, sqrt(12)/2)
# Heavy tail - t_3/sqrt(3)
te <- rt(1000000, 3)/sqrt(3)

ME <- cbind(ne, se, ue, te)
head(ME)
apply(ME, 2, mean)
apply(ME, 2, sd)