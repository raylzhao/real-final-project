library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("leaflet")

country_map <- map_data("world")
data <- read.csv(file = "2016.csv", stringsAsFactors = FALSE)
hdi <- read.csv(file = "hdi_data.csv", stringsAsFactors = FALSE)

hdi <- hdi %>%
  select(Country, X2015)
colnames(hdi) <- c("Country", "hdi_score")

hdi$Country <- trimws(hdi$Country, "l")
hdi[hdi == "United States"] = "USA"
hdi[hdi == "United Kingdom"] = "UK"
hdi[hdi == "Russian Federation"] = "Russia"
hdi[hdi == "Bolivia (Plurinational State of)"] = "Bolivia"
hdi[hdi == "Venezuela (Bolivarian Republic of)"] = "Venezuela"
hdi[hdi == "Tanzania (United Republic of)"] = "Tanzania"
hdi[hdi == "Syrian Arab Republic"] = "Syria"
hdi[hdi == "Iran (Islamic Republic of)"] = "Iran"
hdi[hdi == "Viet Nam"] = "Vietnam"
hdi_map_data <- left_join(country_map, hdi, by = c("region" = "Country"))

happy_hdi_data <- left_join(data, hdi, by = c("Country" = "Country")) %>%
  select(Country, Region, Happiness.Score, hdi_score)

##############
##    UI    ##
##############

ui <- fluidPage(
  includeCSS("style.css"),
  
  titlePanel("What Makes People Happy?"),
  tabsetPanel(
    tabPanel("Overview", 
            br("We are using a dataset called World Happiness Report from Kaggle. The dataset is comprised of 
                happiness scores for 155 countries from the Gallup World Poll. The poll asked people to think of 
                their best possible life as a 10 and their worst possible life being a 0 and to rate their own current 
                life based on that scale. The columns after the happiness score estimate include different factors that 
                may be affecting the overall happiness score; which include, economic production, social support, life 
                expectancy, freedom, absence of corruption, and generosity. The data we chose to use is from the year 2016. 
                The data uses the Gallup weights to make the estimate representative for each happiness score for each country."),
             
             br("This report was released to the United Nations at an event and continues to gain global recognition and is 
             used to improve public policy. We will be comparing the GDP per capita, life expectancy, and freedom 
             levels of each country to their happiness scores to see the relationship between the factors and 
             happiness score. We will use different visuals to represent the relationships and have the analysis 
             used to make better decisions for overall happiness to increase"),
            
             br("One of the reasons we chose to use this dataset is because happiness is invaluable to all people. As humans, we
                like to say things like, money doesn't make you happy. While this may be true for some individuals, but through our
                analysis we intend to figure out, for the general population, what factors help contribute to the overall happiness of
                a country."),
             
             br("In our report, we will be analyzing how different factors contribute to a country's happiness. The four
             main factors we will analyze is Economic State, Level of Freedom, Development of a country, and Region."),
             
            br("For More Information, visit the following links:"),
            
            p(a(href = "https://www.kaggle.com/aida1alim/happiness-data/data", "Happiness Data Set")),
            p(a(href = "http://worldhappiness.report", "World Happiness Report Website")),
            p(a(href = "http://hdr.undp.org/en/content/human-development-index-hdi", "Human Development Index")),
            p(a(href = "https://www.cia.gov/library/publications/the-world-factbook/fields/2128.html", "Government Types Among Countries"))
             
                  ),
    tabPanel("Happiness Vs Development", textOutput("question"), "", plotOutput("plot"),
                  "", plotOutput("map"), "", plotOutput("graph", click = "plot_click"), verbatimTextOutput("info"), 
                  "", textOutput("analysis"), "", plotOutput("graph2"),"", 
                  plotOutput("graph3"), "", textOutput("conclusion")
    )
  )
)

##############
##  SERVER  ##
##############

server <- function(input,output) {
  
  output$question <- renderText({
    question <- "One question we will be analyzing is how the development of a country affects the 
    happiness of individials in a country. To do this, we have obtained data for each countries
    human development index, which measures a country development in relation to their life
    expectancy, standard of living, access to knowledge/eduation, and gross national income per 
    capita. The level of development is a measure of average achievement in key dimensions of human
    development: a long and healthy life, being knowledgable, and have a decent standard of living.
    Each country is given a Human Development Index score from 0 to 1, where the higher the number,
    the higher achievement of human development in the given year. In this section, we will compare
    the relationship between Happiness Scores and HDI in countries to see if the development of a 
    country affects the happiness of its citizens."
    
    question
    
  })
  
  output$plot <- renderPlot({
    country_map <- mutate(country_map, country_code = iso.alpha(country_map$region, n = 3))
    data[data == "United States"] = "USA"
    data[data == "United Kingdom"] = "UK"
    data <- mutate(data, country_code = iso.alpha(data$Country, n = 3))
    data_map <- left_join(country_map, data)
    caption <- "This map shows the data for happiness scores around the world for the year 2016."
    denmark_data <- data_map %>%
      filter(Country == "Denmark")
    
    ggplot(data = denmark_data) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
      coord_quickmap()
    
    happy_map <- ggplot(data = data_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Happiness.Score)) +
      coord_quickmap() +
      ggtitle("World Happiness Levels") +
      labs(x = "Longitude", y = "Latitude", caption = caption) +
      theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
    
    happy_map
  })
  
  output$map <- renderPlot({
    hdi <- hdi %>%
      select(Country, hdi_score)
    hdi_map_data <- left_join(country_map, hdi, by = c("region" = "Country"))
    caption <- "This map shows the data for Human Development Index scores around the world for the year 2015"
      
    hdi_map <- ggplot(data = hdi_map_data) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = hdi_score)) +
      coord_quickmap()  +
      ggtitle("Human Development Index Levels") +
      labs(x = "Longitude", y = "Latitude", caption = caption) +
      theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
    
    hdi_map
  })
  
  output$graph <- renderPlot({
    happy_hdi_data <- left_join(data, hdi, by = c("Country" = "Country")) %>%
      select(Country, Region, Happiness.Score, hdi_score)
  caption <- "This graph shows the relationship between Happiness Score among countries and their scores in Human
  Development Index. Click on any point in the scatter plot to view specific data."
  
  graph <- ggplot(data = happy_hdi_data) +
    geom_smooth(mapping = aes(x = hdi_score, y = Happiness.Score)) +
    geom_point(mapping = aes(x = hdi_score, y = Happiness.Score, color = hdi_score)) +
    ggtitle("Development versus Happiness") +
    labs(x = "Human Development Index Score", y = "Happiness Score", caption = caption) +
    theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
  
  graph
  })
  
  output$info <- renderPrint({
    row <- nearPoints(happy_hdi_data, input$plot_click)
    row
  })
  
  output$analysis <- renderText({
    analysis <- "From the data presented above, there is a positive correlation between Happiness and Human Development Index
          scores among countries around the world. Hence, the more developed a country is, the higher the happiness scores
          tend to be in that country. In this data, we can also see a few outliers, many of which can be attributed to extremely low/high
          scores in other characteristics that tend to have positive correlations to happiness. For example, Syria yield a low Happiness 
          score (3.069) for a relatively high HDI score (0.536). This can be attributed to their government system, for which they yield a 
          score of 0.06912, one of the lowest freedom scores, and a score of 0.17233 in Trust in Government. Another outlier we can examine
          is Costa Rica, which has a relatively high Happiness score (7.087) for a relatively low HDI score (0.776). As compared to a country
          such as Israel, with about the same Happiness score (7.267) and a higher HDI score (0.899). This can also be explained by the fact 
          that Costa Rica yields a Freedom Score of 0.55225 and a score of 0.10547 in Trust in Government. On the other hand, Israel has a 
          score of 0.36432 in Freedom and 0.08728 in Trust in Government. With a notably higher score in government related characteristics,
          which are seen to have a positive correlation with Happiness, it is reasonable that Costa Rica is an outlier. But even with these
          outliers, it is clear that the common trend for most countries is that higher Human Development leads to higher Happiness"
    
    analysis
    
  })
  
  output$graph2 <- renderPlot({
    caption <- "This graph shows the relationship between Happiness Score and Economic GDP per Capita in Countries"
    
    graph <- ggplot(data = data) +
      geom_smooth(mapping = aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
      ggtitle("GDP versus happiness") +
      labs(x = "GDP Per Capita", y = "Happiness Score", caption = caption) +
      theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
    
    graph
  })
  
  output$graph3 <- renderPlot({
    caption <- "This graph shows the relationship between Happiness Score and Quality of Health and Life Expectancy in Countries"
    
    graph <- ggplot(data = data) +
      geom_smooth(mapping = aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
      ggtitle("Health & Life Expectancy versus Happiness") +
      labs(x = "Health & Life Expectancy Score", y = "Happiness Score", caption = caption) +
      theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5))
    
    graph
  })  
  
  output$conclusion <- renderText({
    conclusion <- "Since Human Index Score factors in Economic Prosperity in a Country and their health and life expectancy,
    we analyzed those factors alone against happiness scores. From the plots above, we can also see that, in general, there is
    a positive correlation between Health & Life Expectancy and Happiness. Moreover, There is also a positive correlation between
    GDP Per Capita and Happiness Scores. Since all three of these results yield positive correlations with Happiness, this suggests that
    the factor of Human Development alone, with no regards to Health/Life Expectancy and Economic Propserity, also yields a positive
    correlation to Happiness. Thus, we can conclute that the development in a country contributes greatly to the
    happiness of individuals within that Country. For this reason, it is clear why many of the countries who ranked high on
    overall happiness tend to be highly developed countries."
    
    conclusion
    
  })
}

shinyApp(ui = ui, server = server)

