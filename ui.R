library("shiny")
shinyUI(fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel(":P",
            h2("More Developed, Happier People?"),
            h5("In this section we will explor the relationship between the factor of regions and happiness rate in 2016. Are there
              any relationship between the region that a country belongs to and their happiness score? We will first devided the countries
              based on their happiness score so that we can have a basic idea about the data, and then we will use the ranking to analyze
              the relationship between the regions and their ranking...."),
            tabsetPanel(
              tabPanel("Basic Info",
                       h2("Ranking by Happiness Score"),
                       
                       selectInput("Boundary", label =  h4("Select Boundary:"),
                                   c("Score > 7.00" = "4",
                                     "6.00 < Score <= 7.00" = "3",
                                     "5.00 < Score <= 6.00" = "2",
                                     "4.00 < Score <= 5.00" = "1",
                                     "Score <= 4.00" = "0"
                                   )),
                       h5("This tab will show the Happiness ranking based one happiness score inside the selected 
                         boundary with a decreasing order."),
                       plotOutput("plots"),
                       h5("Here is a bar plot for the number of countries and where region they belongs to. More detailed
                         data is provided through the table under the plot."),
                       tableOutput("table")),
              tabPanel("Advance Analysis",
                       selectInput("Ranking", label =  h4("Select Ranking:"),
                                   c("Top 20" = "20",
                                     "Top 50" = "50",
                                     "Top 100" = "100",
                                     "Full list" = "157"
                                   )),
                       plotOutput("table_3"),
                       tableOutput("table_2"),
                       
                       h4("Analysis:"),
                       h5(textOutput("full_report"))),
              tabPanel("Conclusion",
                     h5(textOutput("finally")))
              )
      )
    )
  )
))