## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4
)

## ----setup--------------------------------------------------------------------
library(ggh4x)

## -----------------------------------------------------------------------------
df <- faithful
df$group <- ifelse(df$eruptions > 3, "High", "Low")

ggplot(df, aes(eruptions, colour = group)) +
  stat_theodensity(distri = "gamma") +
  geom_rug()

## -----------------------------------------------------------------------------
ggplot(df, aes(eruptions, colour = group)) +
  stat_theodensity(distri = "gamma", 
                   aes(linetype = "Theoretical")) +
  stat_density(aes(linetype = "Kernel Estimates"), 
               geom = "line", position = "identity") +
  geom_rug()

## ----fig.width = 3, fig.show='hold'-------------------------------------------
tdist <- data.frame(
  x = c(rt(1000, df = 2), rt(1000, df = 4)),
  group = rep(LETTERS[1:2], each = 1000)
)

ggplot(tdist, aes(x, colour = group)) +
  stat_theodensity(distri = "t", start.arg = list(df = 3))

fdist <- data.frame(
  x = c(rf(1000, df1 = 4, df2 = 8), rf(1000, df1 = 8, df2 = 16)),
  group = rep(LETTERS[1:2], each = 1000)
)

ggplot(fdist, aes(x, colour = group)) +
  stat_theodensity(distri = "f", start.arg = list(df1 = 3, df2 = 3))

## ----error = TRUE, fig.show='hold'--------------------------------------------
try({
correct <- data.frame(
  x = c(rpois(1000, 5), rnbinom(1000, 2, mu = 5)),
  group = rep(LETTERS[1:2], each = 1000)
)

incorrect <- correct
# Change a number to non-integer
incorrect$x[15] <- sqrt(2)

ggplot(incorrect, aes(x, colour = group)) +
  stat_theodensity(distri = "nbinom")

ggplot(correct, aes(x, colour = group)) +
  stat_theodensity(distri = "nbinom")
})

## -----------------------------------------------------------------------------
ggplot(correct, aes(x, colour = group)) +
  stat_theodensity(distri = "nbinom", geom = "step",
                   position = position_nudge(x = -0.5))

## -----------------------------------------------------------------------------
ggplot(correct, aes(x, colour = group)) +
  stat_theodensity(distri = "nbinom", geom = "segment",
                   aes(xend = after_stat(x), yend = 0), alpha = 0.5) +
  stat_theodensity(distri = "nbinom", geom = "point",
                   aes(xend = after_stat(x), yend = 0))

## -----------------------------------------------------------------------------
set.seed(0)
df <- data.frame(x = rnorm(1000, 10, 1/rgamma(1000, 5, 0.2)))

ggplot(df, aes(x)) +
  stat_theodensity(aes(colour = "Normal"), distri = "norm") +
  stat_theodensity(aes(colour = "Cauchy"), distri = "cauchy") +
  geom_rug(alpha = 0.1)

## -----------------------------------------------------------------------------
ggplot(df, aes(x)) +
  geom_histogram(binwidth = 0.01, alpha = 0.5) +
  stat_theodensity(aes(colour = "Normal"), distri = "norm") +
  stat_theodensity(aes(colour = "Cauchy"), distri = "cauchy")

## -----------------------------------------------------------------------------
ggplot(df, aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.01, alpha = 0.5) +
  stat_theodensity(aes(colour = "Normal"), distri = "norm") +
  stat_theodensity(aes(colour = "Cauchy"), distri = "cauchy")

## -----------------------------------------------------------------------------
binwidth <- 0.01
ggplot(df, aes(x)) +
  geom_histogram(alpha = 0.5, binwidth = binwidth) +
  stat_theodensity(aes(y = after_stat(count * binwidth),
                       colour = "Normal"), 
                   distri = "norm") +
  stat_theodensity(aes(y = after_stat(count * binwidth),
                       colour = "Cauchy"), 
                   distri = "cauchy")

## ----echo = FALSE-------------------------------------------------------------
set.seed(0)
df <- data.frame(
  x = runif(100, -5, 5),
  facet = "Data"
)
df$y <- cos(df$x) + rnorm(100, sd = 0.5)

df2 <- data.frame(facet = "Weights")


ggplot(df, aes(x, y)) +

  geom_function(data = df2, fun = function(x){-dnorm(x, -2.5, 0.5)},
                inherit.aes = FALSE, colour = "dodgerblue") +
  geom_function(data = df2, fun = function(x){-dnorm(x, 1.5, 0.5)},
                inherit.aes = FALSE, colour = "limegreen") +
  geom_segment(aes(alpha = dnorm(x, -2.5, 0.5), xend = x, yend = -Inf), 
               colour = "dodgerblue") +
  geom_segment(data = ~ transform(.x, facet = "Weights"),
               aes(alpha = dnorm(x, -2.5, 0.5), y = -dnorm(x, -2.5, 0.5), yend = Inf, xend = x), 
               colour = "dodgerblue") +
  geom_segment(aes(alpha = dnorm(x, 1.5, 0.5), xend = x, yend = -Inf), 
               colour = "limegreen") +
  geom_segment(data = ~ transform(.x, facet = "Weights"),
               aes(alpha = dnorm(x, 1.5, 0.5), y = -dnorm(x, 1.5, 0.5), yend = Inf, xend = x), 
               colour = "limegreen") +
  geom_segment(data = df2, aes(x = -2.5, xend = 1.5, y = -0.2, yend = -0.2),
               arrow = arrow()) +
  geom_point(colour = "grey") +
  geom_point(data = data.frame(facet = rep("Data", 2)),
             aes(x = c(-2.5, 1.5), y = cos(c(-2.5, 1.5)) - c(0.05, 0.1)),
             size = 2, colour = "red") +
  stat_rollingkernel(colour = "red", bw = 0.5) +
  scale_alpha_continuous(range = c(0, 1), guide = "none") +
  facet_grid(facet ~ ., scales = "free_y", switch = "y") +
  scale_y_continuous(name = "") +
  facetted_pos_scales(y = list(NULL, scale_y_continuous(labels = abs))) +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing = unit(0, "mm"))

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  stat_rollingkernel()

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  stat_rollingkernel(aes(alpha = after_stat(scaled)))

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = class)) +
  stat_rollingkernel(aes(y = stage(hwy, after_stat = weight),
                         linetype = "Rolling\nKernel"),
                     bw = 0.3) +
  stat_density(aes(displ, colour = class,
                   y = after_stat(count),
                   linetype = "KDE"),
               bw = 0.3,
               inherit.aes = FALSE, geom = "line", position = "identity") +
  scale_linetype_manual(values = c(2, 1))

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  stat_rollingkernel(kernel = "mean", bw = 1)

## -----------------------------------------------------------------------------
g <- ggplot(economics, aes(date))

g + geom_ribbon(aes(ymin = pmin(psavert, uempmed), 
                    ymax = pmax(psavert, uempmed),
                    fill = uempmed > psavert),
                alpha = 0.8)

## -----------------------------------------------------------------------------
g + stat_difference(
  aes(ymin = psavert, ymax = uempmed),
  levels = c("More uempmed", "More psavert"),
  alpha  = 0.8
)

## ----fig.show='hold', fig.width = 3-------------------------------------------
df <- data.frame(
  x = c(1:4), ymin = c(0, 1, 2, 2.5), ymax = c(2.5, 2, 1, 0.5)
)

g <- ggplot(df, aes(x, ymin = ymin, ymax = ymax)) +
  guides(fill = 'none') +
  geom_point(aes(y = ymin)) +
  geom_point(aes(y = ymax))

g + geom_ribbon(aes(fill = ymax < ymin)) +
  ggtitle("Plain ribbon")

g + stat_difference() +
  ggtitle("stat_difference()")

## -----------------------------------------------------------------------------
df <- faithful
df$group <- ifelse(df$eruptions > 3, "High", "Low")

ggplot(df, aes(waiting, eruptions, group = group)) +
  geom_point() +
  stat_funxy(aes(colour = group),
             funx = range, funy = mean, geom = "line",
             arrow = arrow(ends = "both"))

## -----------------------------------------------------------------------------
ggplot(df, aes(waiting, eruptions, group = group)) +
  geom_point() +
  stat_centroid(aes(label = "Centroid"), colour = "dodgerblue",
                geom = "label") +
  stat_midpoint(aes(label = "Midpoint"), colour = "limegreen",
                geom = "label")

## -----------------------------------------------------------------------------
ggplot(df, aes(waiting, eruptions, group = group)) +
  stat_centroid(aes(xend = waiting, yend = eruptions, colour = group),
                geom = "segment", crop_other = FALSE) +
  geom_point(size = 0.25)

## ----echo = FALSE-------------------------------------------------------------
x <- rep(LETTERS[1:4], 4:1)
x <- rle(x)

df <- data.frame(
  run_id = seq_along(x$lengths),
  run_value = x$values,
  run_length = x$lengths,
  start_id = cumsum(x$lengths) - x$lengths + 1,
  end_id = cumsum(x$lengths)
)
knitr::kable(df)

## -----------------------------------------------------------------------------
df <- data.frame(
  x = seq(0, 10, length.out = 100)
)
df$y <- cos(df$x)

ggplot(df, aes(x, y)) +
  stat_rle(aes(label = cut(y, breaks = 3))) +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  stat_rle(aes(label = cut(y, breaks = 3)),
           align = "center") +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(df) +
  stat_rle(aes(stage(x, after_stat = run_id),
               after_stat(runlength),
               label = cut(y, breaks = 3),
               fill = after_stat(runvalue)),
           geom = "col")

