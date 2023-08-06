library(shiny)
library(ggplot2)
library(shinydashboard)
library(gapminder)


ui <- dashboardPage(
  dashboardHeader(title = "Gapminder Data Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Charts", tabName = "chartsTab"),
      menuItem("Description", tabName = "DesciptionTab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "chartsTab",
              fluidRow(
                column(width = 6,
                       sliderInput("life_exp_slider", "Set Life Expectancy:",
                                   min = min(gapminder$lifeExp),
                                   max = max(gapminder$lifeExp),
                                   value = mean(gapminder$lifeExp),
                                   step = 1)
                ),
                column(width = 6,
                       plotOutput("bar_chart")
                )
              ),
              fluidRow(
                column(width = 6,
                       plotOutput("scatter_plot")
                ),
                column(width = 6,
                       plotOutput("box_plot")
                )
              ),
              fluidRow(
                column(width = 12,
                       selectInput("continent_selector", "Select Continent:",
                                   choices = c("All", unique(gapminder$continent)))
                )
              ),
              fluidRow(
                column(width = 12,
                       plotOutput("line_chart")
                )
              )
      ),
      tabItem(tabName = "DesciptionTab",
              h2("Project Description with code"),
              p("This is a Shiny app showcasing different types of interactive charts for Gapminder data."),
              p("The 'Charts' page consists of four interactive charts and a slider input. The charts are updated based on the selected life expectancy value using the slider input."),
              p("Bar Chart: This chart displays the population of different continents. Each continent is represented by a colored bar, and the height of the bar represents the population."),
              p("Scatter Plot: This chart shows the relationship between life expectancy and GDP per capita for different countries. Each country is represented by a point on the scatter plot, and the color of the point corresponds to its continent."),
              p("Box Plot: This chart displays the distribution of GDP per capita for different continents. Each continent is represented by a box plot, showing the median, quartiles, and outliers of GDP per capita."),
              p(" Line Chart: This chart illustrates the trend of GDP per capita over the years for different continents. Each continent is represented by a different colored line, allowing users to compare the economic growth of continents over time."),
              h2("Code Available On"),
              p("https://github.com/runal-pal?tab=repositories")
      )
    )
  )
)




server <- function(input, output) {
  output$bar_chart <- renderPlot({
    filtered_data <- gapminder[gapminder$lifeExp >= input$life_exp_slider, ]
    ggplot(filtered_data, aes(x = continent, fill = continent)) +
      geom_bar() +
      labs(title = "Bar Chart - Population by Continent",
           x = "Continent",
           y = "Population") +
      theme_minimal()
  })
  
  output$scatter_plot <- renderPlot({
    filtered_data <- gapminder[gapminder$lifeExp >= input$life_exp_slider, ]
    ggplot(filtered_data, aes(x = gdpPercap, y = lifeExp, color = continent)) +
      geom_point() +
      labs(title = "Scatter Plot - Life Expectancy vs. GDP per Capita",
           x = "GDP per Capita",
           y = "Life Expectancy") +
      theme_minimal()
  })
  
  output$box_plot <- renderPlot({
    filtered_data <- gapminder[gapminder$lifeExp >= input$life_exp_slider, ]
    ggplot(filtered_data, aes(x = continent, y = gdpPercap)) +
      geom_boxplot() +
      labs(title = "Box Plot - GDP per Capita by Continent",
           x = "Continent",
           y = "GDP per Capita") +
      theme_minimal()
  })
  
  output$continent_table <- renderTable({
    if (input$continent_selector == "All") {
      filtered_data <- gapminder
    } else {
      filtered_data <- gapminder[gapminder$continent == input$continent_selector, ]
    }
    filtered_data
  })
  
  output$line_chart <- renderPlot({
    ggplot(gapminder, aes(x = year, y = gdpPercap, color = continent)) +
      geom_line() +
      labs(title = "Line Chart - GDP per Capita of Continents over Years",
           x = "Year",
           y = "GDP per Capita",
           color = "Continent") +
      theme_minimal()
  })
}

shinyApp(ui, server)






