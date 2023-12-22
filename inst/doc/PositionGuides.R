## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4
)

## ----setup--------------------------------------------------------------------
library(ggh4x)

## ----example_guides-----------------------------------------------------------
g <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme(axis.line = element_line(colour = "black"))

g + guides(
  x = guide_none(title = "x"),
  x.sec = guide_axis(title = "x.sec"),
  y = "none",
  y.sec = "axis"
)

## ----example_scales-----------------------------------------------------------
g + scale_x_continuous(
  position = "bottom",
  guide = guide_axis(position = "top")
)

## ----example_secondary--------------------------------------------------------
g + scale_x_continuous(
  sec.axis = dup_axis(breaks = 2:4 + 0.5,
                      guide = guide_axis(angle = 45))
)

## ----coloured_axis, fig.keep='last'-------------------------------------------
# Setting all theme elements is a mild inconvenience.
g + theme(
  axis.line.x  = element_line(colour = "forestgreen"),
  axis.ticks.x = element_line(colour = "forestgreen"),
  axis.text.x  = element_text(colour = "forestgreen") 
)

# A little bit easier
g + guides(x = guide_axis_colour(colour = "forestgreen"))

## ----coloured_secondary-------------------------------------------------------
ggplot(economics, aes(date)) +
  geom_line(aes(y = unemploy)) +
  geom_line(aes(y = pop / 30), colour = "red") +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ .x * 30, name = "pop",
      guide = guide_axis_colour(colour = "red"))
  ) +
  theme(axis.line.y = element_line())

## ----truncated_guide----------------------------------------------------------
g + guides(x = "axis_truncated")

## ----truncated_args, fig.width = 3, fig.show='hold'---------------------------
# Using grid units to specify data-independent truncation points.
g + guides(x = guide_axis_truncated(trunc_lower = unit(0.1, "npc"),
                                    trunc_upper = unit(0.9, "npc")))

# Using atomic vectors are interpreted as data points that should be mapped.
g + guides(x = guide_axis_truncated(trunc_lower = 2.5,
                                    trunc_upper = 4.5))

## ----truncated_vectors--------------------------------------------------------
g + guides(x = guide_axis_truncated(trunc_lower = c(2, 4),
                                    trunc_upper = c(3, 5)),
           y = guide_axis_truncated(trunc_lower = ~ .x - 1,
                                    trunc_upper = ~ .x + 1))

## ----truncated_divergent------------------------------------------------------
df <- data.frame(x = seq(-3, 3, length.out = 6), y = LETTERS[1:6])

ggplot(df, aes(x, y)) +
  geom_col() +
  scale_x_continuous(
    breaks = -3:0, guide = "axis_truncated",
    sec.axis = dup_axis(
      breaks = 0:3, guide = "axis_truncated"
    )
  ) +
  theme(axis.line.x = element_line())

## ----manual_guide_example-----------------------------------------------------
g + guides(y.sec = guide_axis_manual(
  breaks = unit(c(1, 3), "cm"), 
  labels = expression("treshold"^2, "annotation"[3]),
  label_colour = c("red", "blue"), label_size = c(8, 12)
))

## ----manual_discrete----------------------------------------------------------
tab <- table(diamonds$cut)

ggplot(diamonds, aes(cut, price)) +
  geom_violin() +
  guides(x.sec = guide_axis_manual(
    breaks = names(tab),
    labels = paste0("n = ", tab)
  ))

## ----manual_highlight---------------------------------------------------------
highlight <- c("New York", "California", "Alabama", "Hawaii")

clust <- hclust(dist(USArrests))

# Melting USArrests
df <- data.frame(
  state = rownames(USArrests)[as.vector(row(USArrests))],
  crime = colnames(USArrests)[as.vector(col(USArrests))],
  value = as.vector(as.matrix(USArrests)), 
  row.names = NULL
)

ggplot(df, aes(crime, state, fill = value)) +
  geom_raster() +
  scale_y_dendrogram(hclust = clust, labels = NULL) +
  guides(y.sec = guide_axis_manual(breaks = highlight, labels = highlight))

## ----minor_example------------------------------------------------------------
g + guides(x = "axis_minor", y = "axis_minor")

## ----minor_positions----------------------------------------------------------
g + scale_x_continuous(
  minor_breaks = seq(2, 4, by = 0.2),
  guide = "axis_minor"
)

## ----minor_length-------------------------------------------------------------
g + guides(x = "axis_minor") +
  theme(axis.ticks.length.x = unit(0.5, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))

## ----logticks_example---------------------------------------------------------
pres <- ggplot(pressure, aes(temperature, pressure)) +
  geom_line()

pres + scale_y_log10(guide = "axis_logticks") +
  theme(axis.ticks.length.y = unit(0.5, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5),
        ggh4x.axis.ticks.length.mini = rel(0.2))

## ----logticks_comparison, fig.width = 3, fig.show='hold'----------------------
# Using annotation log-ticks
pres + scale_y_log10() +
  annotation_logticks(sides = 'l', outside = TRUE) +
  coord_cartesian(clip = "off") +
  theme(axis.text.y = element_text(margin = margin(r = 10)))

# Using axis_logticks, setting tick length equivalently
pres + scale_y_log10(guide = "axis_logticks") +
  theme(axis.ticks.length.y = unit(0.3, "cm"))

## ----logticks_inward----------------------------------------------------------
pres + scale_y_log10(guide = "axis_logticks") +
  theme(axis.ticks.length.y = unit(-0.3, "cm"),
        axis.text.y = element_text(margin = margin(r = 10)))

## ----nested_example-----------------------------------------------------------
df <- data.frame(
  item = c("Coffee", "Tea", "Apple", "Pear", "Car"),
  type = c("Drink", "Drink", "Fruit", "Fruit", ""),
  amount = c(5, 1, 2, 3, 1),
  stringsAsFactors = FALSE
)

ggplot(df, aes(interaction(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested")

## ----nested_stringbreaks------------------------------------------------------
ggplot(df, aes(paste0(item, "~nonsense~", type), amount)) +
  geom_col() +
  guides(x = guide_axis_nested(delim = "nonsense"))

## ----nested_weaving-----------------------------------------------------------
ggplot(df, aes(weave_factors(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested")

## ----nested_theming-----------------------------------------------------------
ggplot(df, aes(weave_factors(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested") +
  theme(
    axis.ticks = element_line(colour = "red"),
    ggh4x.axis.nestline.x = element_line(size = 2),
    ggh4x.axis.nesttext.x = element_text(colour = "blue")
  )

## ----nested_stacking----------------------------------------------------------
df$type2 <- c(rep("Consumables", 4), "Vehicle")
df$appletea <- c("", rep("Ingredient of apple tea", 2), rep(NA, 2))

ggplot(df, aes(weave_factors(item, type, appletea, type2), amount)) +
  geom_col() +
  guides(x = "axis_nested")

## ----dendrogram_scale, fig.height = 5, fig.width = 5--------------------------
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

## ----dendrogram_theming, fig.height = 5, fig.width = 5------------------------
g + scale_y_dendrogram(hclust = clusters) +
  theme(
  axis.ticks.y = element_line(size  = 2, lineend = "round"),
  axis.ticks.length.y = unit(10, "pt")
)

## ----dendrogram_guide, fig.height = 5, fig.width = 5--------------------------
g + scale_y_dendrogram(guide = guide_dendro(position = "right"), 
                       hclust = clusters)

## ----dendrogram_forbidden, fig.height = 5, fig.width = 5----------------------
# Don't do this
ggplot(df, aes(variable, State, fill = value)) +
  geom_raster() +
  guides(y = guide_dendro(dendro = ggdendro::dendro_data(clusters)))

