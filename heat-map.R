# Source file to create heatmaps for US counties based on data. Is a
# function to allow Raju to use it too

## Inputs ##
# @data : data that needs to be visualised
# @variable: variable within data to be used for colouring
# @fips: name of variable with numeic fips code
# @shape_file: Loacation of shapefile. Either a string with path, or
#   a fortified data frame
# @colour: colour scheme from viridis. Characters from A to D

## How to use it
# source("heat-map.R")
# temp.data %>%
#   county.heatmap("<name-of-variable>", "<name-of-fips-code>")

county.heatmap <- function(data, variable, fips = "county.fips",
  shape_file = "ShapeFile.xlsx", discrete = F,
  colour = "A") {
  # Checking for packages and stopping if not installed
  pkgs <- c('ggplot2', 'ggmap', 'dplyr', 'viridis', 'rio')
  loaded <- sapply(pkgs, require, character.only = T)

  if (any(loaded == F)) {
    pkgs_needed <- pkgs[!loaded]
    pkgs_message <- paste(pkgs_needed, collapse = "\n ")
    stop(paste("There is an error! The following packages are needed; \n",
      pkgs_message))
  }

  # Checking the shapefile
  if (is.character(shape_file)) {
    us.shp <- try(rio::import(shape_file))

    if (inherits(us.shp, "try-error")) stop("Shapefile not found!")

  } else if (is.data.frame(shape_file)){
    us.shp <- shape_file
  } else {
    stop("ShapeFile is Missing!! Please see your inputs")
  }

  # Merging to get the correct dataset
  temp.plot <- data %>%
    mutate_(FIPS = fips) %>%
    left_join(us.shp) %>%
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(aes_string(fill = variable), colour = "white", size = 0.1) +
    scale_fill_viridis(option = colour, discrete = discrete) +
    theme_minimal()

  return(temp.plot)
}
