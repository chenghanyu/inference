# Coursera Data Science: Statistical Inference
# Course project
# Cheng-Han Yu
###############################################################################
# Part 1: Simulation of exponential distribution and CLT
########################################################
library(ggplot2)
lambda <- 0.2
n <- 40
nsim <- 1000
set.seed(123)
sim_sample <- matrix(rexp(n*nsim, rate = lambda), nrow = nsim, ncol = n)



# 1. Show the sample mean and compare it to the theoretical 
# mean of the distribution.
mean(sim_sample)
mu <- 1 / lambda

# 2. Show how variable the sample is (via variance) and compare it to 
# the theoretical variance of the distribution.
sig2 <- (1 / lambda ^ 2) / n 
sd(sim_sample_mean) ^ 2



# 3. Show that the distribution is approximately normal.
par(mfrow = c(1, 1))
# hist(sim_sample_mean)
ggplot(data.frame(sim_sample_mean), aes (x = sim_sample_mean)) + 
    geom_histogram(aes(y = ..density..), colour = "#00458c", fill = "#F1B521") +
    geom_vline(xintercept = c(mean(sim_sample_mean), 5), size = c(1,1), 
               colour = c("darkgreen", "red")) +
    xlab(expression(bar(X))) + geom_density(colour = "darkgreen", size = 1) +
    ggtitle(expression(paste("Distribution of averages of samples drawn from exponential distribution with ", lambda, " = 0.2"))) +
    theme(axis.text.x = element_text(colour = "grey20", size = 12), 
          axis.text.y = element_text(colour = "grey20", size = 14),
          axis.title.x = element_text(size = 18, hjust = .5),
          axis.title.y = element_text(size = 18, hjust = .5, vjust = 1),
          title = element_text(size = 20, hjust = .5)) +
    scale_x_continuous(breaks = c(seq(0, 8, 1))) +
    stat_function(fun = dnorm, arg = list(mean = 5, sd = sd(sim_sample_mean)),
                  colour = "red", size = 1)

# analyze the ToothGrowth data in the R datasets package
########################################################
library(datasets)
# Load the ToothGrowth data and perform some basic exploratory data analyses 
data(ToothGrowth)
tooth <- ToothGrowth
dim(tooth)
str(tooth)
tooth$dose <- factor(tooth$dose)
head(tooth)
apply(tooth[, 2:3], 2, table)
# h <- ggplot(tooth, aes(x = len))
# h + geom_histogram(binwidth = 0.1) + 
#     facet_grid(supp ~ dose)
ggplot(tooth, aes(supp, len, fill = supp)) + geom_boxplot()
ggplot(tooth, aes(dose, len, fill = dose)) + geom_boxplot()
ggplot(tooth, aes(interaction(supp, dose), len, 
                  fill = interaction(supp, dose))) + geom_boxplot()
ggplot(aes(y = len, x = supp, fill = dose), data = tooth) + geom_boxplot()
ggplot(aes(y = len, x = dose, fill = supp), data = tooth) + geom_boxplot()

# Provide a basic summary of the data.
summary(tooth$len) 
summary(tooth[tooth$supp == "VC", ]$len) 
summary(tooth[tooth$supp == "OJ", ]$len)
summary(tooth[tooth$dose == 0.5, ]$len) 
summary(tooth[tooth$dose == 1, ]$len)
summary(tooth[tooth$dose == 2, ]$len) 



# Use confidence intervals and/or hypothesis tests to compare tooth growth 
# by supp and dose. (Only use the techniques from class, even if there's 
# other approaches worth considering)
# df <- data.frame(supp = rep(c("VC", "OJ"), each = 3), 
#                  dose = rep(c("0.5", "1", "2")))

df <- data.frame(pvalue = 1:m, compare = 1:m)
df$lower <- rep(0, m)
df$upper <- rep(0, m)
supp <- rep(c("VC", "OJ"), each = 3)
dose <- rep(c("0.5", "1", "2"))
supp_dose <- interaction(supp, dose)
m <- choose(6, 2)
pvalue <- rep(0, m)
k <- 1
for (i in 1:5) {
    for (j in (i+1):6) {
        result <- t.test(x = tooth[(10*(i-1) + (1:10)), ]$len, 
                         y = tooth[(10*(j-1) + (1:10)), ]$len,
                         alternative = "two.sided",
                         paired = FALSE, var.equal = FALSE,
                         conf.level = 0.95)
        df$pvalue[k] <- result$p.value
        df$lower[k] <- round(result$conf.int[1], 3)
        df$upper[k] <- round(result$conf.int[2], 3)
        df$compare[k] <- paste(supp_dose[i], "vs", supp_dose[j])
        k <- k + 1
    }
}
df$pBon <- p.adjust(df$pvalue, method = "bonferroni")
df$pBH <- p.adjust(df$pvalue, method = "BH")
df$sig <- as.numeric(df$pvalue < 0.05)
df$sigBon <- as.numeric(df$pBon < 0.05)
df$sigBH <- as.numeric(df$pBH < 0.05)
round(df[, -2], 4)

# State your conclusions and the assumptions needed for your conclusions.