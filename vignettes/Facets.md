---
title: "Facets"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Facets}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(ggh4x)
```

The ggh4x package has some extended options for tweaking the appearance of facets. 

# Extended facets

This package offers two extensions to the vanilla `facet_wrap()` and `facet_grid()` that give you more control of the placement of axes at the inner facets.

## Wrap

The default behaviour of `facet_wrap2()` is to replicate exactly what `ggplot2::facet_wrap()` does.


```r
p <- ggplot(mpg, aes(displ, hwy, colour = as.factor(cyl))) + geom_point()

p + facet_wrap2(vars(class))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

The difference is even when `scales = "fixed"` (the default), you can draw the axes at (some or all) inner facets with the `axes` argument. Moreover, you can choose to omit the axis labels but keep the axis ticks of the inner facets by setting the `remove_labels` argument.


```r
p + facet_wrap2(vars(class), axes = "all", remove_labels = "x")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## Grid

Likewise, `facet_grid2()` is based on `ggplot2::facet_grid()` and by default behaves identically, but also supports the extended options for axes that `facet_wrap2()` has.


```r
p + facet_grid2(vars(drv), vars(year), axes = "all", remove_labels = "y")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

In addition, `facet_grid2()` also supports what the package calls 'independent' scales. This relieves the constraint that `ggplot2::facet_grid()` has that a scale can only be free between rows and columns of the layout, and instead allows scales to be free within rows and columns of the layout. This keeps the grid layout but preserves the flexibility of scales in wrapped facets. 


```r
p + facet_grid2(vars(drv), vars(year), scales = "free_x", independent = "x")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

One sacrifice that had to be made for independent scales, is that `facet_grid2()` cannot have independent scales *and* have `space = "free"` for the independent dimensions. You can however combine these in *different* dimensions.


```r
p + facet_grid2(vars(drv), vars(year), 
                scales = "free", independent = "y", space = "free_x")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

# Nested facets

Perhaps this package might be best known for generating nested facets; wherein outer strips can span inner strips if they belong the the same category. This can be especially useful if there is some hierarchical relations to the facets. 

In the example below, we'll categorise the Iris species for having long or short leaves.


```r
df <- cbind(
  iris,
  Nester = ifelse(iris$Species == "setosa", "Short Leaves", "Long Leaves")
)

ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  facet_nested(~ Nester + Species)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

## Nesting lines

If you prefer your strips to have blank backgrounds, you could still indicate the hierarchical nature by setting `nest_line = TRUE`. The appearance of the line is controlled by the theme element `ggh4x.facet.nestline`.


```r
ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  facet_nested(~ Nester + Species, nest_line = TRUE) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "blue"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

## Relation to facet_grid()

While `facet_nested()` is based on `facet_grid())`, there are a few differences. First, `facet_nested()` inherits from `facet_grid2()`, so that it inherits the axis features. More notably, `facet_nested()` doesn't require input data to have *all* the facet variables. In the example below, we remove the `Species` column, to prevent facetting on that variable. Note that if we didn't specify a new `Nester` variable, it would put the second set of points in all panels, just like `facet_grid()`.

Furthermore, when strips are placed at the bottom, it rearranges the strips so that the inner strips are closest to the panels and spanning strips are furthest from the panel.


```r
ggplot(df, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  geom_point(data = transform(iris, Species = NULL, Nester = "All")) +
  facet_nested(~ Nester + Species, switch = "x")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

## Variant for facet_wrap()

A similar variant exists for wrapping in facets. It can span the strips for every `strip.position` argument, and has a few nifty tricks for duplicating the axes or just the axis ticks. To explain the `bleed` argument, take a look at the lower left, where the lower "f" strips are merged, even though the strips on top are in different categories.


```r
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  facet_nested_wrap(vars(cyl, drv), dir = "v",
                    strip.position = "left",
                    axes = "full",
                    remove_labels = "rows",
                    bleed = TRUE)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


# Position scales

A second thing we might want to tweak about facets is the exact specifications of each facet's position scale. To set the scales individually, we can use `facetted_pos_scales()` in combination with a list of scales. This way, you can vary labels, breaks, limits, transformations and even axis guides for each panel individually. 

The list of scales follows the order of the facets, as long as they are set to 'free'. Tweaking the position scales works with many types of facets, such as wrap, grid and nested, but has to be called *after* facets are added.


```r
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
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

This works because `facetted_pos_scales()` makes an edit to the facet, which comes with an important limitation. Due to the way plots are build and where facets are involved, scale transformations are applied *after* calculations in the stat part of the layer. This differs from normal behaviour, where scale transformations are applied *before* stat calculations. Therefore, it is recommended to pre-transform the data in layers with non-identity statistics in the `aes()` mapping. An example of what could go wrong is shown below.


```r
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
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-2.png)

If you plan on using `facetted_pos_scales()` to tweak the axis of the plots, do take a look at the [position guides](PositionGuides.html)!

# Sizes

Lastly, we can also set the sizes of the panels to what we want. This can be especially convenient when you want different sizes for facets, for example when you want to add a density next to your plot. The function `force_panelsizes()` can let you set relative or absolute sizes for the rows and columns. 

The settings overrule the coordinates' or theme's aspect ratio and `space = "free"` facet arguments. By default, rows and columns are set relative within themselves only. When `respect = TRUE`, the rows and columns relative units become also relative between rows and columns, as you can see in the plot below. Alternatively, you can set them as absolute units with the `grid::unit()` function. Again, these need to be added *after* any facets.


```r
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
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

If you think the breaks of the density plot above are too packed, why not tweak these with `facetted_pos_scales()`? Note that `NULL` signals here that the default scale should be used.


```r
g + facetted_pos_scales(x = list(NULL, scale_x_continuous(breaks = c(0, 0.2, 0.4))))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

