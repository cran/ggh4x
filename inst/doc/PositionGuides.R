## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4
)

## ----setup--------------------------------------------------------------------
library(ggh4x)

## -----------------------------------------------------------------------------
g <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme(axis.line = element_line(colour = "black"))

g + guides(x = "axis_truncated")

## ---- fig.width = 3, fig.show='hold'------------------------------------------
g + guides(x = guide_axis_truncated(trunc_lower = unit(0.1, "npc"),
                                    trunc_upper = unit(0.9, "npc")))

g + guides(x = guide_axis_truncated(trunc_lower = 2.5,
                                    trunc_upper = 4.5))

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  scale_x_continuous(
    minor_breaks = seq(2, 4, by = 0.2),
    guide = "axis_minor"
  )

## ---- eval = FALSE------------------------------------------------------------
#  g <- ggplot(mtcars, aes(wt, mpg)) +
#    geom_point()
#  
#  # The following are equivalent:
#  g + guides(x = "axis_minor", y = "axis_minor")
#  
#  g + scale_x_continuous(guide = "axis_minor") +
#    scale_y_continuous(guide = "axis_minor")
#  
#  # If you need to set extra options you can use the `guide_axis_minor()` function.
#  g + guides(x = guide_axis_minor(angle = 45),
#             y = guide_axis_minor(title = "I am a Y-axis"))
#  
#  g + scale_x_continuous(guide = guide_axis_minor(angle = 45)) +
#    scale_y_continuous(guide = guide_axis_minor(title = "I am a Y-axis"))

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  guides(x = "axis_minor") +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))

## -----------------------------------------------------------------------------
ggplot(pressure, aes(temperature, pressure)) +
  geom_line() +
  scale_y_log10(guide = "axis_logticks") +
  theme(axis.ticks.length.y = unit(0.5, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5),
        ggh4x.axis.ticks.length.mini = rel(0.2))

## ---- fig.width = 3, fig.show='hold'------------------------------------------
g <- ggplot(pressure, aes(temperature, pressure)) +
  geom_line()

# Using annotation log-ticks
g + scale_y_log10() +
  annotation_logticks(sides = 'l', outside = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_text(margin = margin(r = 10)))

# Using axis_logticks, setting tick length equivalently
g + scale_y_log10(guide = "axis_logticks") +
  theme(axis.ticks.length.y = unit(0.3, "cm"))

## -----------------------------------------------------------------------------
g + scale_y_log10(guide = "axis_logticks") +
  theme(axis.ticks.length.y = unit(-0.3, "cm"),
        axis.text.y = element_text(margin = margin(r = 0.4, unit = "cm")))

## -----------------------------------------------------------------------------
df <- data.frame(
  item = c("Coffee", "Tea", "Apple", "Pear", "Car"),
  type = c("Drink", "Drink", "Fruit", "Fruit", ""),
  amount = c(5, 1, 2, 3, 1),
  stringsAsFactors = FALSE
)

ggplot(df, aes(interaction(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested")

## -----------------------------------------------------------------------------
ggplot(df, aes(paste0(item, "~nonsense~", type), amount)) +
  geom_col() +
  guides(x = guide_axis_nested(delim = "nonsense"))

## -----------------------------------------------------------------------------
ggplot(df, aes(weave_factors(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested")

## -----------------------------------------------------------------------------
ggplot(df, aes(weave_factors(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested") +
  theme(
    axis.ticks = element_line(colour = "red"),
    ggh4x.axis.nestline.x = element_line(size = 2),
    ggh4x.axis.nesttext.x = element_text(colour = "blue")
  )

## -----------------------------------------------------------------------------
df$type2 <- c(rep("Consumables", 4), "Vehicle")
df$appletea <- c("", rep("Ingredient of apple tea", 2), rep(NA, 2))

ggplot(df, aes(weave_factors(item, type, appletea, type2), amount)) +
  geom_col() +
  guides(x = "axis_nested")

## ---- fig.height = 5, fig.width = 5-------------------------------------------
clusters <- hclust(dist(USArrests), "ave")

# reshaping USArrests
df <- data.frame(
  State = rownames(USArrests)[row(USArrests)],
  variable = colnames(USArrests)[col(USArrests)],
  value = unname(do.call(c, USArrests))
)

g <- ggplot(df, aes(variable, State, fill = value)) +
  geom_raster()
g + scale_y_dendrogram(hclust = clusters)

## ---- fig.height = 5, fig.width = 5-------------------------------------------
g + scale_y_dendrogram(hclust = clusters) +
  theme(
  axis.ticks.y = element_line(colour = "forestgreen"),
  axis.ticks.length.y = unit(10, "pt")
)

## ---- fig.height = 5, fig.width = 5-------------------------------------------
g + scale_y_dendrogram(guide = guide_dendro(position = "right"), 
                       hclust = clusters)

## ---- fig.height = 5, fig.width = 5-------------------------------------------
# Don't do this
ggplot(df, aes(variable, State, fill = value)) +
  geom_raster() +
  guides(y = guide_dendro(dendro = ggdendro::dendro_data(clusters)))

