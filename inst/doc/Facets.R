## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5
)

## ----setup--------------------------------------------------------------------
library(ggh4x)

## ----wrap_mimick--------------------------------------------------------------
# Make a standard plot
p <- ggplot(mpg, aes(displ, hwy, colour = as.factor(cyl))) + geom_point() +
  labs(x = "Engine displacement", y = "Highway miles per gallon") +
  guides(colour = "none")

p + facet_wrap2(vars(class))

## ----wrap_axes----------------------------------------------------------------
p + facet_wrap2(vars(class), axes = "all", remove_labels = "x")

## ----wrap_trimming------------------------------------------------------------
p + facet_wrap2(vars(class), nrow = 4, ncol = 4, trim_blank = FALSE)

## ----grid_axes----------------------------------------------------------------
p + facet_grid2(vars(year), vars(drv), axes = "all", remove_labels = "y")

## ----grid_independent---------------------------------------------------------
p + facet_grid2(vars(year), vars(drv), scales = "free_x", independent = "x")

## ----grid_independent_space---------------------------------------------------
p + facet_grid2(vars(year), vars(drv), 
                scales = "free", independent = "y", space = "free_x")

## ----grid_render_empty--------------------------------------------------------
p + facet_grid2(vars(drv), vars(cyl), render_empty = FALSE)

## ----nested_grid--------------------------------------------------------------
new_iris <- transform(
  iris, 
  Nester = ifelse(Species == "setosa", "Short Leaves", "Long Leaves")
)

iris_plot <- ggplot(new_iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point()

iris_plot +
  facet_nested(~ Nester + Species)

## ----nested_nesting_lines-----------------------------------------------------
iris_plot +
  facet_nested(~ Nester + Species, nest_line = element_line(linetype = 2)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "blue"))

## ----nested_grid_difference---------------------------------------------------
iris_plot +
  geom_point(data = ~ transform(.x, Species = NULL, Nester = "All")) +
  facet_nested(~ Nester + Species, switch = "x")

## ----nested_wrap--------------------------------------------------------------
p + 
  facet_nested_wrap(
    vars(cyl, drv), dir = "v", strip.position = "left",
    axes = "all", remove_labels = "x"
  ) +
  theme(strip.placement = "outside")

## ----base_layout--------------------------------------------------------------
# Setting up a design for a layout
design <- matrix(c(1,2,3,2), 2, 2)
layout(design)
par(mar = c(2,2,1,1))

# Making a multi-panel plot based on the layout
df <- mpg
df$colours <- with(df, match(cyl, sort(unique(cyl))))
df$colours <- scales::hue_pal()(4)[df$colours]
splitted <- split(df, df$drv)
xlim <- range(df$displ)
ylim <- range(df$cty)
for (i in seq_along(splitted)) {
  with(splitted[[i]], plot(displ, cty, col = colours, pch = 19,
                           xlim = xlim, ylim = ylim, ))
}

## ----facet_manual-------------------------------------------------------------
# Use design from previous chunk
p + facet_manual(vars(factor(drv)), design = design)

## ----facet_manual_chardesign--------------------------------------------------
design <- "
  A##
  AB#
  #BC
  ##C
"
p + facet_manual(vars(drv), design = design)

## ----facet_manual_sizes-------------------------------------------------------
p + facet_manual(
  vars(drv), design = design,
  heights = 4:1, widths = unit(1:3, "cm")
)

## ----facet_manual_invalid-----------------------------------------------------
design <- "
  AA#
  ACB
  #BB
"
p + facet_manual(vars(drv), design = design)

## ----strip_clip, fig.show='hold', fig.width = 3-------------------------------
p2 <- p +
  theme(strip.background = element_rect(colour = "black", linewidth = 2),
        axis.line.y = element_line(colour = "black", linewidth = 2))

p2 + facet_wrap2(vars(year), strip = strip_vanilla(clip = "on")) +
  ggtitle('clip = "on"')

p2 + facet_wrap2(vars(year), strip = strip_vanilla(clip = "off")) +
  ggtitle('clip = "off"')

## ----strip_size_constant------------------------------------------------------
df <- data.frame(
  long = paste("A long title that is going to make the\n",
               "smaller title take up too much space"),
  short = LETTERS[1:3],
  x = 1:3, y = 1:3
)
p2 <- ggplot(df, aes(x, y)) +
  geom_point() +
  theme(strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        plot.title.position = "plot")

p2 + facet_grid2(long + short ~ ., switch = "y",
                strip = strip_vanilla(size = "constant")) +
  ggtitle('size = "constant"')

## ----strip_size_variable------------------------------------------------------
p2 + facet_grid2(long + short ~ ., switch = "y",
                strip = strip_vanilla(size = "variable")) +
  ggtitle('size = "variable"')

## ----strip_themed-------------------------------------------------------------
ridiculous_strips <- strip_themed(
     # Horizontal strips
     background_x = elem_list_rect(fill = c("limegreen", "dodgerblue")),
     text_x = elem_list_text(colour = c("dodgerblue", "limegreen"),
                             face = c("bold", "bold")),
     by_layer_x = TRUE,
     # Vertical strips
     background_y = elem_list_rect(
       fill = c("gold", "tomato", "deepskyblue")
     ),
     text_y = elem_list_text(angle = c(0, 90)),
     by_layer_y = FALSE
)

p + facet_grid2(class ~ drv + year, strip = ridiculous_strips)

## ----strip_themed_mix---------------------------------------------------------
p + facet_grid2(
   . ~ drv + year,
   strip = strip_themed(
     background_x = list(NULL, element_rect(colour = "black"), element_blank(),
                         element_rect(fill = "black")),
     text_x = list(NULL, NULL, NULL, element_text(colour = "white"))
   )
)

## ----strip_nested-------------------------------------------------------------
p + facet_wrap2(
  vars(cyl, drv), ncol = 4,
  strip = strip_nested(bleed = FALSE)
) +
  ggtitle('bleed = FALSE')

## ----strip_bleed--------------------------------------------------------------
p + facet_wrap2(
  vars(cyl, drv), ncol = 4,
  strip = strip_nested(bleed = TRUE)
) +
  ggtitle("bleed = TRUE")

## ----strip_nested_bottom, fig.show='hold', fig.width = 3----------------------
p + facet_grid2(
  cols = vars("Outer label", "Inner label"),
  switch = "x", strip = strip_vanilla()
) +
  ggtitle("strip_vanilla()")

p + facet_grid2(
  cols = vars("Outer label", "Inner label"),
  switch = "x", strip = strip_nested()
) +
  ggtitle("strip_nested()")

## ----position_scales_list-----------------------------------------------------
scales <- list(
  scale_x_reverse(),
  scale_x_continuous(labels = scales::dollar,
                     minor_breaks = c(2.5, 4.5)),
  scale_x_continuous(breaks = c(2.945, 6),
                     limits = c(0, 10),
                     guide = guide_axis(minor.ticks = TRUE))
)

p + facet_wrap(vars(drv), scales = "free_x") +
  facetted_pos_scales(x = scales)

## ----position_scales_formula--------------------------------------------------
red_axis <- guide_axis(theme = theme(
  axis.text = element_text(colour = "red"),
  axis.ticks = element_line(colour = "red")
))

p +
  facet_wrap(vars(class), nrow = 1, scales = "free_x") +
  xlim(range(mpg$displ)) +
  facetted_pos_scales(x = list(
    COL %% 2 == 0 ~ scale_x_continuous(labels = NULL, limits = xlim),
    class %in% c("midsize", "suv", "subcompact") ~ scale_x_continuous(
      guide = red_axis, 
      limits = xlim
    )
  ))

## ----eval=FALSE---------------------------------------------------------------
# p +
#   facet_wrap(vars(class), nrow = 1, scales = "free_x") +
#   xlim(range(mpg$displ)) +
#   scale_x_facet(
#     COL %% 2 == 0,
#     labels = NULL, limits = xlim
#   ) +
#   scale_x_facet(
#     class %in% c("midsize", "suv", "subcompact"),
#     limits = xlim,
#     guide = red_axis
#   )

## ----position_scales_stats, fig.show='hold', fig.width = 3--------------------
set.seed(0)
df <- data.frame(
  x = rlnorm(100, 10)
)

# Normally data is transformed prior to stat calculations
ggplot(df, aes(x)) +
  geom_density() +
  scale_x_log10() +
  ggtitle("standard log10 scale")

# This can give problems when combining stat calculations with facetted 
# position scale transformations.
ggplot(df, aes(x)) +
  geom_density() +
  facetted_pos_scales(x = list(scale_x_log10())) +
  ggtitle("facetted scale")

# Pre-transformed data
ggplot(df, aes(log10(x))) +
  geom_density() +
  facetted_pos_scales(x = list(scale_x_continuous())) +
  ggtitle("facetted scale +\npre-transformation")

## ----panel_size_null----------------------------------------------------------
p + force_panelsizes(rows = unit(2, "cm"), cols = unit(2, "in"))

## ----panel_sizes--------------------------------------------------------------
lvls <- factor(c("Points", "Density"), c("Points", "Density"))
g <- ggplot(faithful) +
  geom_point(aes(waiting, eruptions),
             data = ~ cbind(.x, facet = lvls[1])) +
  geom_density(aes(y = eruptions),
               data = ~ cbind(faithful, facet = lvls[2])) +
  facet_grid(~ facet, scales = "free_x")

g + force_panelsizes(cols = c(1, 0.3), rows = c(0.5), respect = TRUE)

## ----panel_sizes_total--------------------------------------------------------
g <- g + force_panelsizes(
  cols = c(1, 0.3), total_width = unit(6, "cm"), 
  total_height = unit(4, "cm")
)
g

## ----panel_sizes_2------------------------------------------------------------
g + scale_x_facet(facet == "Density", breaks = c(0, 0.2, 0.4))

