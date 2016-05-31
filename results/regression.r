library("ggplot2")

# For each benchmark:
#  For each framework:
#    * Have a guess of polynomial degree
#    * Fit polynomial
#    * Print R^2
#  Plot data points + fitted curve

benchmarks <- c("exc","cd","cdr","rt",
                "ras", "rbs",
                "ss","sr","sw","se",
                "rs","rr","rw","re",
                "ws","wr","ww","we",
                "es","er","ew","ee")
params <- data.frame(framework = rep(c("freer", "monad-classes", "mtl"), 22),
                     benchmark = rep(benchmarks, each = 3),
                     degree = rep(c(1, 1, 1), 22))

# params[params$framework == "mtl" & params$benchmark == "exc",]

ww <- read.csv("ww.csv")
names(ww)[names(ww) == "X..of.iterations"] <- "n"

ww <- transform(ww, n = log10(n), time = log10(time))

p <- qplot(n, time, data = ww, col = framework)

# fit <- lm(time ~ n, ww, subset = framework == "mtl")
# predicted <- data.frame(n = seq(min(ww$n), max(ww$n), length.out = 100), framework = "mtl")
# predicted$time <- predict(fit, predicted)
# p <- p + geom_line(data=predicted, aes(x = n, y = time))

# params = data.frame(framework = c("freer", "monad-classes", "mtl"),
#                     degree = c(1, 2, 2))


by(params, 1:nrow(params), function(row) {
  fit <- lm(time ~ n, ww, subset = framework == row$framework)
  predicted <- data.frame(n = seq(min(ww$n), max(ww$n), length.out = 100), framework = row$framework)
  predicted$time <- predict(fit, predicted)
  p <<- p + geom_line(data=predicted, aes(x = n, y = time))
  print(fit$coefficients)
})
