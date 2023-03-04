CO2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)
final_CO2_df <- CO2_df %>%
  filter(year >= "1958")


library("ggplot2")
library("plotly")
library("dplyr")
library("styler")
library("markdown")



intro_panel <- tabPanel(
  "Introduction",
  includeMarkdown("Introduction.md"),
  htmlOutput("summary")
)


plot_panel <- tabPanel(
  "CO2 Emissions Per Capita Between 1985 and 2021",
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # first widget
      selectInput("year",
        label = h3("Select Year"),
        choices = final_CO2_df$year,
        selected = "2021"
      ),

      # selectInput (second widget here)
      checkboxGroupInput("check",
        label = h3("Select countries to be compared:"),
        choices = unique(final_CO2_df$country),
        selected = "United States"
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("co2_plot"),
      htmlOutput("plotSummary")
    )
  )
)



# Define UI for application that draws a histogram
ui <- navbarPage(
  "A4",
  intro_panel,
  plot_panel
)
