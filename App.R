library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(rworldmap)
library(rsconnect)

#Erica's Data
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

#Anastasia's Data
world_happiness_data <- read.csv("./2016.csv", stringsAsFactors = FALSE)
world_happiness_data <- select(world_happiness_data, Country, Happiness.Score, Economy..GDP.per.Capita.)
country <- world_happiness_data$Country

#Steve's Data
full_data <- read.csv("2016.csv", stringsAsFactors = F)
selected_data <- select(full_data, Country, Region, Happiness.Rank)

#Raymond's Data
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
        tabPanel("Freedom and Trust",
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
                           
                           mainPanel(plotOutput('world_map'),
                                     plotOutput('graph0'))
                         )
    ),
    
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
               ),
    tabPanel("Regions & Happiness",
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
                                    )
                        ),
                        h5("This tab will show the Happiness ranking based one happiness score inside the selected 
                           boundary with a decreasing order."),
                        plotOutput("plots"),
                        h5("Here is a bar plot for the number of countries and where region they belongs to. More detailed
                           data is provided through the table under the plot."),
                        tableOutput("table")
                        ),
               tabPanel("Advance Analysis",
                        selectInput("Ranking", label =  h4("Select Ranking:"),
                                    c("Top 20" = "20",
                                      "Top 50" = "50",
                                      "Top 100" = "100",
                                      "Full list" = "157"
                                    )
                        ),
                        plotOutput("table_3"),
                        tableOutput("table_2"),
                        
                        h4("Analysis:"),
                        h5(textOutput("full_report"))
               ),
               tabPanel("Conclusion",
                        h5(textOutput("finally")))
               )
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
  
  required_data <- reactive({
    return(select(full_data, Country, Region, Happiness.Rank, Happiness.Score))
  })
  
  filtered_data <- reactive({
    if(input$Boundary == 0){
      required_data() %>% filter(Happiness.Score <= 4)
    }
    else if(input$Boundary == 1){
      required_data() %>% filter(Happiness.Score > 4 & Happiness.Score <= 5)
    }
    else if(input$Boundary == 2){
      required_data() %>% filter(Happiness.Score > 5 & Happiness.Score <= 6)
    }
    else if(input$Boundary == 3){
      required_data() %>% filter(Happiness.Score > 6 & Happiness.Score <= 7)
    }
    else if(input$Boundary == 4){
      required_data() %>% filter(Happiness.Score > 7)
    }
  })
  
  all_data <- reactive({
    return(filter(selected_data, Happiness.Rank <= as.numeric(input$Ranking)) %>% group_by(Region) %>% summarise(
      Number = length(Happiness.Rank),
      Percentage = round(Number / as.numeric(input$Ranking) * 100, digits = 2))
    )
  })
  
  
  
  output$table <- renderTable({
    filtered_data()
  })
  
  output$table_2 <- renderTable({
    all_data() 
  })
  
  output$finally <- renderText({
    c("The conclusion that i come up with the qustion about the relationship between region and happiness score is that
      there are definately a huge relationship between them, but the data provided can not support the idea, and did not
      show a strong relationship bwtewwn them. Since the data set was based on countries, regions with more counties such
      as Europe will have huge advantage against regions such as North America. In one hand you can say that generally
      people are happier than people in NA, since the ranking they got is higher and the number of countries in their
      region also outweigh NA, but not all the countries are as happy as, for example, Denmark. Regions that are more
      developed tend to have happier people, but developing countries also dominated the lower half of the ranking.
      Too many factors will influence the result, so the answer is not clear.")
  })
  
  output$plots <- renderPlot({
    ggplot(filtered_data()) + theme_grey(base_size = 18)+ 
      geom_bar(mapping = aes(x = Region), stat="count", color = "white", fill = "steelblue") + theme(
        axis.text.y = element_text(color = "red", angle = 10, size = 12, vjust = 0.5),
        axis.title.x = element_text(color="forestgreen", size = 20, vjust = -0.35),
        axis.title.y = element_text(color="cadetblue", size = 20, vjust = 0.35)   
      ) + geom_text(stat='count',aes(x = Region, label=..count..), color = "white", hjust = 1.5) +
      labs(x = "Region Names", y = "Number of Countries") + coord_flip()
  })
  
  output$table_3 <- renderPlot({
    ggplot(data = all_data()) + theme_grey(base_size = 18) + 
      geom_bar(mapping = aes(x = Region, y = Percentage), stat="identity") + coord_flip() + theme(
        axis.text.y = element_text(color = "steelblue", angle = 10, size = 12, vjust = 0.5),
        axis.title.x = element_text(color="cadetblue", size = 20, vjust = -0.35),
        axis.title.y = element_text(color="forestgreen", size = 20, vjust = 0.35)   
      ) + labs(x = "Region Names", y = "Percentage Included") +
      geom_text(stat = "identity", aes(x = Region, y = Percentage, label = Number), color = "white", hjust = 1.5)
    
  })
  
  output$full_report <- renderText({
    if(input$Ranking == 20){
      c("Based on the top 20 countries, it is obviously that Western Europe dominate the ranking. 
        Most of the countries in western europe are wealthy. They have EU to support them, and most of the countries
        are captalism. US and Canada are also on the list. Most of the top 20 countries have well developed social 
        security and health insurence.")
    } 
    else if (input$Ranking == 50){
      c("Based on the top 50 countries, we can see that even though Western Europe region are still the happiest one,
        Latin America and Caribbean region is catching up. These countries also have well developed social security 
        and health insurence. Their GDP maybe lower than top 20 countries, but their good location and the special 
        culture did give them some advantage. It is reasonable that they are happy.")
    }
    else if (input$Ranking == 100){
      c("If we take more data from the table and look at the top 100 ranking, Central and Eastern Europe and Middle 
        East and Northern Africa regions started to grow up. Some of the countries had been though the world wars 
        but they are certainly developing fast. People will be satisfied because of the fast developing nation and 
        their better social security and insurance system.")
    }
    else if (input$Ranking == 157){
      c("The existing regions did not change much but Sub-Saharan Africa claimed the happiest region if we look on a 
        percentage base. It means that most of the countries are on the list of happiness, but on a fairly low ranking.
        New countries are formed and maybe wars are stopping them from peacefully develope, but they do have a chance to
        takeover the places from the so called 'First World Countries'.")
    }
    })
  
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
  
  
  output$graph0 <- renderPlot({
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

shinyApp(ui = ui, server = server)

