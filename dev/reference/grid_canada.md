# Create grid across Canada

Create grid across Canada

## Usage

``` r
grid_canada(cell_size = 200, buffer = 500, crs = "ESRI:102001")
```

## Arguments

- cell_size:

  Numeric. Size of grid (km) to use when creating grid. If using this
  grid as input to
  [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md),
  should use default COSEWIC grid size of 2.

- buffer:

  Numeric. Extra buffer (km) to add around the outline of Canada before
  calculating grid.

- crs:

  Character. CRS for the grid to create.

## Value

sf data frame with polygon grid

## Examples

``` r
gc <- grid_canada(200)
gc_buff <- grid_canada(200, buffer = 0)

# Plot to illustrate
library(ggplot2)
ggplot() +
  geom_sf(data = gc) +
  geom_sf(data = map_canada(), fill = NA) +
  labs(caption = "200km buffer")


ggplot() +
  geom_sf(data = gc_buff) +
  geom_sf(data = map_canada(), fill = NA) +
  labs(caption = "No buffer")
```
