# Load data
CO2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)
final_CO2_df <- CO2_df %>%
  filter(year >= "1958")

library("ggplot2")
library("plotly")
library("dplyr")
library("styler")

# code for summary info

## global average of co2 emission per capita
avg_co2_per_capita <- final_CO2_df %>%
  filter(year == "2021") %>%
  summarize(avg_emission = mean(co2_per_capita, na.rm = TRUE)) %>%
  pull(avg_emission)

## country with highest co2 emission per capita as of 2021
max_co2_country <- final_CO2_df %>%
  filter(year == "2021") %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>%
  pull("country")

max_co2_emission <- final_CO2_df %>%
  filter(year == "2021") %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>%
  pull("co2_per_capita")

## country with lowest co2 emission per capita as of 2021
min_co2_country <- final_CO2_df %>%
  filter(year == "2021") %>%
  filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE)) %>%
  pull("country")

min_co2_emission <- final_CO2_df %>%
  filter(year == "2021") %>%
  filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE)) %>%
  pull("co2_per_capita")

## how much did global average co2 emission change over time (from 1958 to 2021)
co2_emission_change <- final_CO2_df %>%
  filter(year %in% c("1958", "2021")) %>%
  group_by(year) %>%
  summarize(avg_emission = mean(co2_per_capita, na.rm = TRUE)) %>%
  summarize(emission_change = avg_emission[year == "2021"] - avg_emission[year == "1958"]) %>%
  pull(emission_change)



# server
server <- function(input, output) {
  output$summary <- renderUI({
    p1 <- paste("Since my analysis focuses on comparing CO2 emissions per capita between country over the years, it is necessary to include some basic summary information for better grasp of the magnitude of values. As of 2021, the global average CO2 emission per capita is", avg_co2_per_capita, ". Country with the highest average CO2 emission per capita is", max_co2_country, ", with an average of", max_co2_emission, "while", min_co2_country, "has the lowest average CO2 emission per capita of", min_co2_emission, "as of 2021. With the global average, we are able to conclude that", max_co2_country, "has an average that is way higher than global average, indicating further need for investigation on reasons for their extremely high CO2 emission per capita.")

    p2 <- paste("Looking at data over the years (1985-2021), the difference between global average CO2 emission per capita in 2021 and 1985 is", co2_emission_change, ". Note that this does not include changes between. Although the number indicate a decline in average CO2 emission per capita, it should be taken into account that less countries had sufficient tools to collect data on their CO2 emission per capita, which may lead to NA or NAN values, hence altering the averages.")
    HTML(paste(p1, p2, sep = "<br/>"))
  })

  output$co2_plot <- renderPlotly({
    selected_df <- final_CO2_df %>%
      filter(country %in% input$check) %>%
      select(country, year, co2_per_capita) %>%
      filter(year == input$year)

    co2_plot <- ggplot(selected_df) +
      geom_col(aes(
        x = co2_per_capita,
        y = reorder(country, +co2_per_capita),
        fill = country,
        text = paste("Country, CO2 emission per capita:", input$check, ",", co2_per_capita)
      )) +
      labs(
        title = "CO2 Emission Per Capita of Countries in Selected Year",
        x = "CO2 Emissions per capita (in tonnes)",
        y = "Country"
      )
    return(ggplotly(co2_plot, tooltip = "text"))
  })
  output$plotSummary <- renderUI({
    p("This interactive chart is included to visualize CO2 emission per capita over the years. By selecting different years and countries one wants to look at, the chart is more personalized and can visualize differences in emissions between countries in a given year. This helps if we want to look at impacts that specific historical events may have on CO2 emission of countries in that year, such as natural disasters. Some patterns I noticed are [developed] countries are more likely to have data of CO2 emission per capita and China, United States stay high in the chart throughout the years.")
  })
}
