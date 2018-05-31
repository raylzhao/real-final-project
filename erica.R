library("shiny")
library("dplyr")
library("ggplot2")
library("rsconnect")
library("rworldmap")

#read .csv files
happiness_info <- read.csv('2016.csv', stringsAsFactors = FALSE)
government_info <- read.csv('government_type_data.csv', stringsAsFactors = FALSE)

#creating the dataframe I need
happiness_info <- left_join(happiness_info, government_info) %>%
  select(Country, Region, Happiness.Rank, Happiness.Score, Freedom,
         Trust..Government.Corruption., Government.Type)
happiness_info$Country[happiness_info$Country == "United States"] <- "USA"
happiness_info$Country[happiness_info$Country == "United Kingdom"] <- "UK"

#filtering the types of government
happiness_info$Government.Type[grepl("democracy", happiness_info$Government.Type, ignore.case = TRUE)] <- "Democracy"
happiness_info$Government.Type[grepl("republic", happiness_info$Government.Type, ignore.case = TRUE)] <- "Republic"
happiness_info$Government.Type[grepl("monarchy",happiness_info$Government.Type, ignore.case = TRUE)] <- "Monarchy"
happiness_info$Government.Type[grepl("communist", happiness_info$Government.Type, ignore.case = TRUE)] <- "Communism"
happiness_info$Government.Type[grepl("dictatorship", happiness_info$Government.Type, ignore.case = TRUE)] <- "Dictatorship"
happiness_info$Government.Type[grepl("in transition", happiness_info$Government.Type, ignore.case = TRUE)] <- "In Transition"
happiness_info$Government.Type[grepl("federation", happiness_info$Government.Type, ignore.case = TRUE)] <- "Federation"


#happiness rank range
happiness_rank_range <- range(happiness_info$Happiness.Rank)

#happiness score range
happiness_score_range <- range(happiness_info$Happiness.Score)

#freedom score range
freedom_range <- range(happiness_info$Freedom)

#trust government corruption score range
tgc_range <- range(happiness_info$Trust..Government.Corruption.)

#world map
world_map <- map_data(map = "world")
happiness_info <- left_join(world_map, happiness_info, by = c("region" = "Country"))
colnames(world_map)[colnames(world_map) == "region"] <- "Country"


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("What Determines Happiness?"),
  tabsetPanel(tabPanel("Freedom and Trust",
                       p(em("Two other factors that affect the happiness score are 
                            trust (as measured by a perceived absense of corruption
                            in government and business) and perceived freedom to make
                            life decisions. Perceptions of corruption are the average
                            of binary answers to two questions: 'Is corruption widespread
                            throughout the government or not?' and 'Is corruption widespread
                            within businesses or not?'. Freedom to make life choices are the
                            average of binary answers to the question 'Are you satisfied or
                            dissatisfied with your freedom to choose what you do with your
                            life?'.")),
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput('happiness_score', label = "Happiness Score",
                                       min = happiness_score_range[1],
                                       max = happiness_score_range[2],
                                       value = happiness_score_range),
                           sliderInput('freedom_score', label = "Freedom Score",
                                       min = freedom_range[1],
                                       max = freedom_range[2],
                                       value = freedom_range),
                           sliderInput('tgc', label = "Trust Government Corruption",
                                       min = tgc_range[1],
                                       max = tgc_range[2],
                                       value = tgc_range),
                           checkboxGroupInput('government_type', label = "Government Type",
                                              choices = c('Democracy', 'Republic', 'Monarchy',
                                                          'Communism', 'Dictatorship', 'In Transition',
                                                          'Federation'),
                                              selected = c('Democracy', 'Republic', 'Monarchy',
                                                           'Communism', 'Dictatorship'))
                         ),
                         
                         mainPanel(plotOutput('world_map', hover = 'plot_hover'),
                                   plotOutput('graph'),
                                   verbatimTextOutput('info'))
                          )
                          )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  freedom_changes <- reactive({
    freedom <- happiness_info %>%
      filter(Freedom > input$freedom_score[1] &
               Freedom < input$freedom_score[2]) %>%
      filter(Happiness.Score > input$happiness_score[1] &
               Happiness.Score < input$happiness_score[2]) 
    
    return(freedom)
    
  })
  
  happiness_vs_gov <- reactive({
    happiness_gov <- happiness_info %>%
      filter(Happiness.Score > input$happiness_score[1] &
               Happiness.Score < input$happiness_score[2]) %>%
      filter(Trust..Government.Corruption. > input$tgc[1] &
               Trust..Government.Corruption. < input$tgc[2]) %>%
      filter(Government.Type == input$government_type)
    
    return(happiness_gov)
    
  })
  
  
  output$world_map <- renderPlot({
    caption <- "This world map shows the different levels of freedom each country has.
    As the color gets lighter, the higher the freedom score the country has. All can be
    adjusted to see more specific countries using the side widgets labeled 'Freedom Score'
    and 'Happniess Score'."
    
    ggplot() +
      geom_polygon(data = world_map, mapping = aes(x = long, y = lat, group = group), fill = "grey") +
      geom_polygon(data = freedom_changes(),mapping = aes(x = long, y = lat, group = group, fill = Freedom)) +
      scale_fill_continuous(low = "darkred", high = "thistle2", 
                            guide = "colorbar", na.value = "white") +
      labs(title = "Freedom Score in Each Country",
           x = "longitude",
           y = "latitude",
           caption = caption)
    
  })
  
  
  output$graph <- renderPlot({
    caption <- "This graph show the different types of government and how it
    affects the trust in government corruption in relation to the happiness score.
    This graph data can be manipulated by adjusting the 'Trust Government Corruption'
    and 'Happiness Score' and by selecting the specific type of government."
    
    ggplot(data = happiness_vs_gov(), mapping = aes(x = Happiness.Score,
                                                    y = Trust..Government.Corruption.,
                                                    color = Government.Type)) +
      geom_point() +
      labs(title = "Trust in Government Corruption in Comparison to Happiness Score",
           x = "Happiness Score",
           y = "Trust in Government Corruption",
           caption = caption) +
      facet_wrap(~Government.Type)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

