library("shiny")
library("dplyr")
library("ggplot2")
source("ui.R")

full_data <- read.csv("data/2016.csv", stringsAsFactors = F)
selected_data <- select(full_data, Country, Region, Happiness.Rank)



shinyServer(function(input, output) {
  
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
  

})          