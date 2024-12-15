## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4
)

## ----setup--------------------------------------------------------------------
library(ggh4x)

## -----------------------------------------------------------------------------
# Separating layers by species and declaring (yet) unknown aesthetics
g <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point(aes(swidth = Sepal.Width),
             data = ~ subset(., Species == "setosa")) +
  geom_point(aes(pleng = Petal.Length),
             data = ~ subset(., Species == "versicolor")) +
  geom_point(aes(pwidth = Petal.Width),
             data = ~ subset(., Species == "virginica")) +
  facet_wrap(~ Species, scales = "free_x")

# This generated quite some warnings, but this is no reason to worry!

g + scale_colour_multi(
  aesthetics = c("swidth", "pleng", "pwidth"),
  # Providing colours as a list distributes list-elements over different scales
  colours = list(c("black", "green"),
                 c("gray",  "red"),
                 c("white", "blue")),
  guide = list(guide_colourbar(barheight = unit(35, "pt")))
)

## -----------------------------------------------------------------------------
# Reshaping the iris dataset for heatmap purposes
iriscor <- cor(t(iris[, 1:4]))
iriscor <- data.frame(
  x = as.vector(row(iriscor)),
  y = as.vector(col(iriscor)),
  correlation = as.vector(iriscor)
)
iris_df <- transform(iris, id = seq_len(nrow(iris)))

# Setting up the heatmap
g <- ggplot(iris_df, aes(id, id)) +
  geom_tilemargin(aes(species = Species)) +
  geom_raster(aes(x, y, cor = correlation),
              data = iriscor) +
  coord_fixed()

g + scale_listed(scalelist = list(
  scale_fill_distiller(palette = "RdBu", aesthetics = "cor"),
  scale_fill_brewer(palette = "Set1", aesthetics = "species")
), replaces = c("fill", "fill"))

## -----------------------------------------------------------------------------
ggplot(diamonds, aes(price, carat, colour = clarity)) +
  geom_point(shape = ".") +
  scale_colour_brewer(palette = "Dark2", guide = "stringlegend")

## ----fig.show='hold', fig.width = 3-------------------------------------------
set.seed(0)
df <- data.frame(
  x = 1:10,
  y = cumsum(rnorm(10))
)

p <- ggplot(pressure, aes(temperature, pressure)) +
  geom_pointpath()

p + theme(aspect.ratio = 0.5)
p + theme(aspect.ratio = 2)

## -----------------------------------------------------------------------------
ggplot(pressure, aes(temperature, pressure)) +
  geom_pointpath(linesize = 2, size = 2, mult = 1)

## -----------------------------------------------------------------------------
p + coord_polar(theta = "y")

## ----fig.show='hold', fig.width = 3-------------------------------------------
df <- data.frame(
  x = as.vector(row(volcano)),
  y = as.vector(col(volcano)),
  value = as.vector(volcano)
)

g <- ggplot(df, aes(x, y, fill = value)) +
  scale_fill_viridis_c(guide = "none") +
  theme_void()

g + geom_polygonraster(position = position_lineartrans(shear = c(0.2, 0.2))) +
  coord_fixed()

g + geom_polygonraster(position = position_lineartrans(angle = 45)) +
  coord_fixed()

g + geom_polygonraster() + coord_polar()

## -----------------------------------------------------------------------------
ggplot(transform(mtcars, car = rownames(mtcars)), 
       aes(mpg, wt)) +
  geom_point(aes(colour = as.factor(cyl))) +
  geom_text_aimed(aes(label = car), 
                  hjust = -0.2, size = 3,
                  xend = sum(range(mtcars$mpg)) / 2,
                  yend = sum(range(mtcars$wt)) / 2) +
  coord_cartesian(clip = "off")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(1), fill = class)) +
  geom_bar(show.legend = FALSE, position = "fill") +
  geom_text_aimed(aes(x = 1.2, label = class, group = class),
                  position = position_fill(vjust = 0.5),
                  stat = "count") +
  coord_polar("y") +
  theme_void()

## -----------------------------------------------------------------------------
ggplot(diamonds, aes(cut, fill = clarity)) +
  geom_bar(width = 1) +
  geom_text_aimed(aes(label = cut, group = cut),
                  angle = 90,
                  stat = "count", nudge_y = 2000) +
  scale_x_discrete(labels = NULL) +
  coord_polar()

## -----------------------------------------------------------------------------
p <- ggplot(mpg, aes(displ - mean(displ), hwy - mean(hwy))) +
  geom_point() +
  theme(axis.line = element_line())

p + coord_axes_inside()

## -----------------------------------------------------------------------------
p + coord_axes_inside(labels_inside = TRUE) +
  scale_x_continuous(
    labels = ~ ifelse(.x == 0, "", .x),
    guide  = guide_axis(minor.ticks = TRUE)
  ) +
  scale_y_continuous(
    labels = ~ ifelse(.x == 0, "", .x),
    guide  = guide_axis(cap = "both")
  )

