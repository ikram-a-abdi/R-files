secstats <- na.omit(secstats$`IPO total proceeds by U.S. issuers (US$ Millions)`)
secstats <- sec_stats_ipos_20260317
secstats <- na.omit(secstats)
mean(secstats$`IPO median proceeds by U.S. issuers (US$ Millions)`)
par(mar = c(4, 4, 2, 1))
hist(x, probability = TRUE, col = 'blue', main = "Normal Distribution - IPO offerings", xlab = "IPO total proceeds by U.S. issuers (US$ Millions)", yaxt = 'n')
axis(2, at = axTicks(2),
       +      labels = paste0(round(axTicks(2) * 100, 1), "%"))

curve(dnorm(x, mean(x), sd(x)),
          +       add = TRUE, col = "red", lwd = 2)
mean <- mean(x) 
t.test(x, mu = 82200)
prob_1 <- 1 - pnorm(82200, mean = mean, sd = sd(x))
print(prob) #returns a value of 0 - suggesting how unlikely it is that an IPO would ever return such a ridiculously high number
new_stats <- quantile(secstats$`IPO total proceeds by U.S. issuers (US$ Millions)`, 0.90) #find the top 10% of IPO issues to run the same hypothesis test
new_stats_top <- secstats$`IPO total proceeds by U.S. issuers (US$ Millions)`[secstats$`IPO total proceeds by U.S. issuers (US$ Millions)` >= 52835]
prob_2 <- 1 - pnorm(82200, mean = 52835, sd = sd(new_stats_top)) #returns a probability of 0.3 - suggesting that this achievement was possible when looking at only the top 10% of companies
ttest_1 <- t.test(x, mu = 82200)
ttest_2 <- t.test(new_stats_top, mu = 82200)
table <- map_df(list(ttest_1, ttest_2), tidy)