# Made up ages

ages <- c(22, 20, 25, 20, 21, 22, 21, 20, 26, 24, 22, 21, 21, 21)
qqnorm(ages)
qqline(ages)
hist(ages)

xbar <- mean(ages)
S <- sd(ages)
Tstat <- (xbar - 20)/(S/sqrt(14))
c(xbar, S, Tstat)
1 - pt(Tstat, 13)
pt(Tstat, 13, lower = FALSE) -> pvalue
pvalue
CI <- c(xbar - qt(.975, 13)*S/sqrt(14),
        xbar + qt(0.975, 13)*S/sqrt(14))
CI
t.test(ages, mu = 20, alternative = "greater")


t.test(ages, conf.level = .90)
