library("ggplot2")

ww <- read.csv("ww.csv")
names(ww)[names(ww) == "X..of.iterations"] <- "n"
fit <- lm(time ~ n, ww, subset = framework == "freer")
fit2 <- lm(time ~ n + I(n^2), ww, subset = framework == "mtl")
p <- qplot(n, time, data = ww, col = framework)
predicted <- data.frame(n = seq(1000, 10000, 100))
predicted$time <- predict(fit2, predicted)
p <- p + geom_line(data=predicted, aes(x = n, y = time), col = "black")

