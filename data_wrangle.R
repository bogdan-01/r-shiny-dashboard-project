library(googleAnalyticsR)
library(lubridate)
library(tidyverse)
library(tsibble)
library(tsibbledata)


ga_data <- read_csv("synthesized_ga_data.csv")


ga_data <- ga_data %>% 
  mutate(lat_numeric = as.numeric(latitude), 
         lng_numeric = as.numeric(longitude)) %>%
  filter(lat_numeric > 49.958956,  lat_numeric < 60.914919) %>%
  filter(lng_numeric > -8.017032,  lng_numeric < 1.782773)


ga_data <- ga_data %>% 
  mutate(scottish_city = case_when(lat_numeric > 55.4060 & 
                                   lat_numeric < 56.6450 & 
                                   lng_numeric < -2.3150 & 
                                   lng_numeric > -3.7393 ~ "Edinburgh",
                                   lat_numeric > 54.6141 & 
                                   lat_numeric < 56.6450 & 
                                   lng_numeric < -3.7393 & 
                                   lng_numeric > -5.2901 ~ "Glasgow",
                                   lat_numeric > 56.6450 & 
                                   lat_numeric < 60.8532 & 
                                   lng_numeric < -0.2994 & 
                                   lng_numeric > -9.1667 ~ "Inverness"))


ga_data <- ga_data %>% 
  rename(data_application = goal2Completions, 
         glasgow_info_session = goal3Completions, 
         ed_info_sessions = goal5Completions, 
         programming_application = goal9Completions)


ga_data_select <- ga_data %>% 
  select(date, 
         sessions,
         users,
         data_application,
         programming_application,
         ed_info_sessions, 
         glasgow_info_session, 
         scottish_city,
         latitude,
         longitude) %>%
  filter(!is.na(scottish_city)) %>% 
  group_by(scottish_city, date) %>% 
  summarise(total_sessions_per_day = sum(sessions),
  total_users_per_day = sum(users),
  total_da_apps = sum(data_application), 
  total_prog_apps = sum(programming_application), 
  total_ed_info = sum(ed_info_sessions),
  total_gl_info = sum(glasgow_info_session))

ga_tsibble <- as_tsibble(ga_data_select, index = date, key = scottish_city)

# codeclan corporate colour pallete
codeclan_colours <- c(
  `codeclan dark blue` = "#1b3445",
  `codeclan light blue` = "#4da2cd",
  `codeclan other blue` = "#4ca1cd",
  `codeclan gold`     = "#e1bf76",
  `codeclan pink` = "#e74b7d",
  `codeclan red` = "red",
  `codeclan light grey` = "#cccccc",
  `codeclan dark grey`  = "#8c8c8c")
# Any changes to these colours, or addition of new colours, are done in the above vector.
# Tip: use back ticks to remove naming restrictions (e.g. to include spaces for `light grey` and `dark grey`).
# Function that extracts the hex codes from this vector by name.
codeclan_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (codeclan_colours)
  codeclan_colours[cols]
}
# This allows us to get hex colors in a robust and flexible way. For example, you can have all colours returned as they are, specify certain colours, in a particular order, add additional function arguments and checks, and so on:
# codeclan_cols()
# codeclan_cols("codeclan gold", "codeclan light blue")
# Combine colours into palettes
# Codeclan has a few main colours, but the full list (above) includes other official colours used for a variety of purposes. So we can now create palettes (various combinations) of these colours. Similar to how we deal with colours, first define a list like such:
codeclan_palettes <- list(
  `main`  = codeclan_cols("codeclan light blue", "codeclan dark blue", "codeclan gold"),
  `cool`  = codeclan_cols("codeclan dark blue", "codeclan light blue", "codeclan other blue"),
  `hot`   = codeclan_cols("codeclan gold", "codeclan red", "codeclan pink"),
  `mixed` = codeclan_cols("codeclan gold","codeclan light blue", "codeclan other blue","codeclan dark blue", "codeclan dark grey", "codeclan light grey", "codeclan pink"),
  `grey`  = codeclan_cols("codeclan light grey", "codeclan dark grey")
)

codeclan_palettes
# Changes or new colour palettes are added in this list. We write a function to access and interpolate them like so:
# Return function to interpolate a example color palette
#' @param palette Character name of palette in example_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
codeclan_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- codeclan_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}
# This function gets a pallete by name from the list ("main" by default), has a boolean condition determining whether to reverse the order or not, and additional arguments to pass on to colorRampPallete() (such as an alpha value). This returns another function:
# This returned function will interpolate the palette colours for a certain number of levels, making it possible to create shades between our original colours.
# This is what we need to create custom ggplot2 scales.
# Scales for ggplot2
# We can now create custom colour and fill scale functions for ggplot2.
# Colour scale constructor for codeclan colours
#' @param palette Character name of palette in example_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
scale_color_codeclan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- codeclan_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("example_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
#' Fill scale constructor for codeclan colours
#' @param palette Character name of palette in codeclan_palettes
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
scale_fill_codeclan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- codeclan_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("example_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
# Each of these functions specifies a palette, whether the palette is being applied based on a discrete or numeric variable, whether to reverse the palette colors, and additional arguments to pass to the relevant ggplot2 function (which differs for discrete or numeric mapping).
# Examples.
#
# # Colour by discrete variable using default palette
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_codeclan()
#
# # Colour by numeric variable with cool palette
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_color_codeclan(discrete = FALSE, palette = "cool")
#
# # Fill by discrete variable with different palette + remove legend (guide)
# ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_codeclan(palette = "mixed", guide = "none")



