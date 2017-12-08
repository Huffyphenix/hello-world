library(gapminder)

# Note: This code creates data sets to use in each tab.
# It removes Kuwait since Kuwait distorts the gdp scale
all_data <- dplyr::filter(gapminder, country != "Kuwait")
africa_data <- dplyr::filter(gapminder, continent == "Africa")
americas_data <- dplyr::filter(gapminder, continent == "Americas")
asia_data <- dplyr::filter(gapminder, continent == "Asia", country != "Kuwait")
europe_data <- dplyr::filter(gapminder, continent == "Europe")
oceania_data <- dplyr::filter(gapminder, continent == "Oceania")