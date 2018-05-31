library(shiny)
library(ggplot2)
library(dplyr)

world_happiness_data <- read.csv("./2016.csv", stringsAsFactors = FALSE)
world_happiness_data <- select(world_happiness_data, Country, Happiness.Score, Economy..GDP.per.Capita.)
country <- world_happiness_data$Country


ui <- fluidPage(
  titlePanel("2016 World Happiness Data"),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Economy Graph',
               p("This graph plots points of different countries' happiness score
                 vs GDP per Capita. You can filter which countries you want to focus
                 on and see the relationship between GDP per Capita and happiness."),
               plotOutput("GDPGraph"),
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput('country', 
                                      'Country',
                                      selected = 'Denmark',
                                      inline = TRUE,
                                      choices = country)
                 ),
               p(em("Commonly, it is said that money does not buy happiness. We are 
                    looking further into the relationship between a country's happiness
                    score and a country’s Gross Domestic Product (GDP) per Capita. 
                    Often, GDP is a significant indicator of the economic stability in a
                    country as well as a means to gauge a country’s standard of living. 
                    Hence, the higher the GDP in a country, the higher standard of living 
                    and wealth in a country. Looking at the graph, we can see a general trend
                    of positive correlation between GDP per Capita and happiness score.
                    We know that countries that are more economically stable have people
                    with higher happiness levels."))
          )
    
      
      )
  )
  )
)



#server

server <- function(input, output) {
  
  
  filtered_country <- reactive({
    filter(world_happiness_data, Country %in% input$country)
  })
  
  
  output$GDPGraph = renderPlot({
    ggplot(filtered_country(), aes(x = Happiness.Score,
                                   y = Economy..GDP.per.Capita.,
                                   color = Country)) +
      geom_point(stat = "identity", size = 3) +
      xlim(2.5, 7.8) +
      ylim(0.05, 1.5) +
      labs(x = "Happiness Score") +
      labs(y = "GDP per Capita") +
      ggtitle("GDP per Capita vs Happiness") +
      theme(plot.title = element_text(family = "Trebuchet MS", color = "#666666", 
                                      face = "bold", size = 32, hjust = 0))
      
  })

}


shinyApp(ui = ui, server = server)

