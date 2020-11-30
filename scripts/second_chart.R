## second chart
library(dplyr)
library(plotly)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(formattable)



## this is the function of ploting pie chart which visually shows the top 15
## countries with the highest death rate of their own country.

second_chart_plot <- function(data_set) {
  data.frame(data_set)
  second_chart_15 <- data_set %>%
    select(Country.Region, Deaths...100.Cases) %>%
    arrange(-Deaths...100.Cases) %>%
    head(15)
  names(second_chart_15)[1] <- paste("Country")
  names(second_chart_15)[2] <- paste("Death_Rate")
  second_chart_processed_data <- second_chart_15 %>%
    mutate(percentage = Death_Rate / sum(Death_Rate))
  ggplot(data = second_chart_processed_data, aes(x = "",
                                                 y = percentage,
                                                 fill = Country)) +
    geom_col(color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(percentage * 100), "%")),
              position = position_stack(vjust = 0.5)) +
    theme(panel.background = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18)) +
    ggtitle("Pie chart of Top 15 Highest Death Rate Country")
}