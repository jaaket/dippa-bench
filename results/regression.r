library("ggplot2")
library("grid")

source("multiplot.r")

frameworks = c("freer", "monad-classes", "mtl")

benchmarks <- c("exc","cd","cdr","rt",
                "ras", "rbs",
                "ss","sr","sw","se",
                "rs","rr","rw","re",
                "ws","wr","ww","we",
                "es","er","ew","ee")

params <- expand.grid(benchmark = benchmarks, framework = frameworks, degree = 1)

set_degree <- function(params, benchmark, framework, degree) {
  params$degree[params$benchmark == benchmark & params$framework == framework] <- degree
  params
}

params[] <- set_degree(params, "ras", "mtl", 2)
params[] <- set_degree(params, "ras", "monad-classes", 2)
params[] <- set_degree(params, "rbs", "mtl", 2)
params[] <- set_degree(params, "rbs", "monad-classes", 2)
params[] <- set_degree(params, "rr", "freer", 2)
params[] <- set_degree(params, "rw", "freer", 2)
params[] <- set_degree(params, "wr", "freer", 2)
params[] <- set_degree(params, "ww", "mtl", 2)
params[] <- set_degree(params, "ww", "monad-classes", 2)

plots <- data.frame(benchmark = benchmarks, plot = NA)
fits <- subset(params, select = c(benchmark, framework))

results <- sapply(benchmarks, function(bname) {
  bdata <- read.csv(paste(bname, "csv", sep = "."))
  names(bdata)[2] <- "n"

  p <- ggplot() +
      geom_point(data = bdata, mapping = aes(x = n, y = time, col = framework, shape = framework), size = 5, stroke = 1.5) +
      # scale_shape_discrete(solid = F) +
      scale_shape_manual(values = c(4, 3, 1))

  fits <- sapply(frameworks, function(fw) {
    d <- params$degree[params$benchmark == bname & params$framework == fw]
    fit <- lm(time ~ poly(n, d), bdata, subset = framework == fw)

    predicted <- data.frame(n = seq(min(bdata$n), max(bdata$n), length.out = 100), framework = fw)
    predicted$time <- predict(fit, predicted)
    p <<- p + geom_line(data=predicted, aes(x = n, y = time, col = framework), size = 1)

    list(fit = fit)
  }, simplify = FALSE, USE.NAMES = TRUE)

  pdf(paste(bname, "pdf", sep = "."))
  print(p)
  dev.off()

  list(benchmark = bname, plot = p, fits = fits)
}, simplify = FALSE, USE.NAMES = TRUE)

results.final <- expand.grid(benchmark = benchmarks, framework = frameworks)

results.final[c("r2", "coeff", "degree")] <- t(mapply(function(benchmark, framework, degree) {
  fit <- results[[benchmark]]$fits[[framework]]$fit
  r2 <- summary(fit)$r.squared
  coeff <- tail(unname(coef(fit)), n = 1)
  c(r2, coeff, degree)
}, params$benchmark, params$framework, params$degree))

print(results.final[order(results.final$benchmark),])
print(subset(results.final, r2 < 0.99))

# Normalize leading coefficients?
