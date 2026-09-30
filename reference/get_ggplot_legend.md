# Extract the Legend from a ggplot Object

This function extracts the legend from a ggplot object and returns it as
a grob (graphical object). The extracted legend can then be used
independently, for instance, to combine the legend with other plots.

## Usage

``` r
get_ggplot_legend(plot)
```

## Arguments

- plot:

  A ggplot object from which the legend should be extracted.

## Value

A grob representing the legend of the ggplot object, or an empty
[`grid::nullGrob()`](https://rdrr.io/r/grid/grid.null.html) when the
plot has no legend (e.g. a mapped layer with no rows, such as a
single-location flux network), so the result can always be passed to
[`gridExtra::grid.arrange()`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
or
[`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).

## Details

This function converts a ggplot object into a grob using
**`ggplotGrob()`** and extracts the legend. ggplot2 \>= 3.5 lays out one
guide-box slot per position (`"guide-box-right"`, `"guide-box-bottom"`,
...), leaving empty slots as zero grobs; the first non-empty slot is
returned, and the single `"guide-box"` grob of older ggplot2 versions is
matched too.

## Examples

``` r
# Create a simple ggplot object
p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(gear))) +
     ggplot2::geom_point() +
     ggplot2::scale_color_discrete(name = "Gear")

# Extract the legend from the plot
legend <- get_ggplot_legend(p)

# Display the extracted legend
grid::grid.draw(legend)

```
