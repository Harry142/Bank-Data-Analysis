# packages required
library(ggplot2)
library(wbstats)
library(scales)
library(RColorBrewer)
# and I want to make it responsive
install.packages("plotly")
library(plotly)

# Part 1
# countries selected manually if you select the countries in one specific region that has
# the approximate size of the population and gdp the data would be more able to well-displayed
# but I want to test if the data is possible to have some outliers "US" and "China"
example_geos <- c("USA", "AUS", "GBR", "VNM", "CHN", "NLD", "THA", "JPN", "CAN", "DEU")

# fetch gdp data
gdp_data <- wb_data("NY.GDP.MKTP.CD", country = example_geos, 
                    start_date = 2010, end_date = 2021)

# covert the gdp data for readability
gdp_data$NY.GDP.MKTP.CD.billion <- gdp_data$NY.GDP.MKTP.CD / 1e9

# order by example_geos
gdp_data$country <- factor(gdp_data$country, levels = unique(gdp_data$country))

# define color palette
color_palette <- brewer.pal(n = min(length(unique(gdp_data$country)), 12), name = "Set3")

# Create a ggplot object 
gdp_plot <- ggplot(data = gdp_data, aes(x = date, y = NY.GDP.MKTP.CD.billion, 
                                        group = country, color = country,
                                        text = paste("Country: ", country, "<br>",
                                                     "Year: ", date, "<br>",
                                                     "GDP: $", scales::comma(NY.GDP.MKTP.CD.billion), " billion"))) +
  geom_line() +
  scale_color_manual(values = color_palette) +
  theme_minimal(base_size = 10) +
  labs(title = "GDP Changes by Year (in Billions USD)",
       x = "Year",
       y = "GDP (Billions USD)",
       color = "Country") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_y_continuous(labels = scales::label_number_auto()) +
  scale_x_continuous(breaks = 2010:2021)

# Convert to a plotly object
gdp_plot_interactive <- ggplotly(gdp_plot, tooltip = "text")

# Print the plot
gdp_plot_interactive

# Part 2

my_indicators = c("pop" = "SP.POP.TOTL",
                  "gdp" = "NY.GDP.MKTP.CD")

# Fetch the data 
pop_gdp_wide <- wb_data(my_indicators, country = example_geos, 
                        start_date = 2010, end_date = 2021, return_wide = TRUE)

# Convert GDP to billions and population to millions for readability
pop_gdp_wide$gdp_billions <- pop_gdp_wide$gdp / 1e9
pop_gdp_wide$pop_millions <- pop_gdp_wide$pop / 1e6

# Define color palette
color_palette <- RColorBrewer::brewer.pal(n = min(length(unique(pop_gdp_wide$country)), 12), 
                                          name = "Spectral")

# Create the plot
gdp_pop_plot <- ggplot(data = pop_gdp_wide, aes(x = pop_millions, y = gdp_billions, color = country)) +
  geom_point(size = 4, alpha = 0.7) + 
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "Relationship between Population (in millions) and GDP (in billions)",
       x = "Population (millions)",
       y = "GDP (billions USD)",
       color = "Country") +
  scale_x_continuous(labels = label_number_auto()) +
  scale_y_continuous(labels = label_number_auto()) +
  facet_wrap(~ date) +  
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 8))  

# Print the plot
print(gdp_pop_plot)

# I want to calculate the GDP per capita (in USD)
pop_gdp_wide$gdp_per_capita <- pop_gdp_wide$gdp / pop_gdp_wide$pop

# Convert GDP per capita to thousands
pop_gdp_wide$gdp_per_capita_thousands <- pop_gdp_wide$gdp_per_capita / 1000

# Create a ggplot object
gdp_per_capita_plot <- ggplot(data = pop_gdp_wide, 
                              aes(x = date, y = gdp_per_capita, group = country, color = country,
                                  text = paste("Country: ", country, "<br>",
                                                "Year: ", date, "<br>",
                                                "GDP per Capita: $", scales::comma(gdp_per_capita_thousands), "k<br>",
                                                "GDP: $", scales::comma(gdp / 1e9), " billion<br>",
                                                "Population: ", scales::comma(pop / 1e6), " million"))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  labs(title = "GDP per Capita over Time (in Thousands of USD)",
       x = "Year",
       y = "GDP per Capita (Thousands of USD)",
       color = "Country") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = label_number_auto())

# Convert to a plotly object
gdp_per_capita_interactive <- ggplotly(gdp_per_capita_plot, tooltip = "text")

# Print the interactive plot
gdp_per_capita_interactive
