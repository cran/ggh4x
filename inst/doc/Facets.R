## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5
)

## ----setup--------------------------------------------------------------------
library(ggh4x)

## -----------------------------------------------------------------------------
df <- cbind(
  iris,
  Nester = ifelse(iris$Species == "setosa", "Short Leaves", "Long Leaves")
)

ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  facet_nested(~ Nester + Species)

## -----------------------------------------------------------------------------
ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  facet_nested(~ Nester + Species, nest_line = TRUE) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "blue"))

## -----------------------------------------------------------------------------
ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  geom_point(data = transform(iris, Species = NULL, Nester = "All")) +
  facet_nested(~ Nester + Species, switch = "x")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  facet_nested_wrap(vars(cyl, drv), dir = "v",
                    strip.position = "left",
                    axes = "full",
                    remove_labels = "rows",
                    bleed = TRUE)

## -----------------------------------------------------------------------------
scales <- list(
  scale_x_reverse(),
  scale_x_continuous(labels = scales::dollar,
                     minor_breaks = c(2.5, 4.5)),
  scale_x_continuous(breaks = c(2.945, 6),
                     limits = c(0, 10),
                     guide = "axis_minor")
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(vars(drv), scales = "free_x") +
  facetted_pos_scales(x = scales)

## ---- fig.show='hold', fig.width = 3------------------------------------------
set.seed(0)
df <- data.frame(
  x = rlnorm(100, 10)
)

# Normally data is transformed prior to stat calculations
ggplot(df, aes(x)) +
  geom_density() +
  scale_x_log10()

# This can give problems when combining stat calculations with facetted 
# position scale transformations.
ggplot(df, aes(x)) +
  geom_density() +
  facetted_pos_scales(x = list(scale_x_log10()))

## -----------------------------------------------------------------------------
g <- ggplot(faithful) +
  geom_point(aes(waiting, eruptions),
             data = cbind(faithful, facet = "Points")) +
  geom_density(aes(y = eruptions),
               data = cbind(faithful, facet = "Density")) +
  facet_grid(~ facet, scales = "free_x") +
  force_panelsizes(cols = c(1, 0.2), 
                   rows = c(0.5),
                   respect = TRUE)
g

## -----------------------------------------------------------------------------
g + facetted_pos_scales(x = list(NULL, scale_x_continuous(breaks = c(0, 0.2, 0.4))))

