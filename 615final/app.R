

# Define UI for application that draws a histogram
ui <- navbarPage("Final Report - the movie data base",

  
 #First tab
  tabPanel("Explore the dataset",
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "1.Width of bins:", min = 20, max = 100, value = 50),
      br(),
      selectInput("var","2.Select the variable form the TMDB dataset", choices = c("budget", "popularity","revenue","vote_count"), selected = "budget"),
      br(),
      radioButtons("color", "3. Select the colour of histogram", choices = c("Green", "sky blue", "Yellow"), selected = "sky blue")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("myhist")
    )
  )
),
#Second tab
tabPanel("Display countries with more than 20 movies",
           
           # Show a plot of the generated distribution
           mainPanel("Countries with more than 20 movies",
             plotOutput("maps")
             
           )
         
),
#Third tab
tabPanel("Display the beford distribution",
         
         sidebarLayout(
           sidebarPanel(
             selectInput("var1","Select the variable form the TMDB dataset", choices = c("budget", "popularity","revenue","vote_count"), selected = "budget")
           ),
           
           # Show a plot of the generated distribution
           mainPanel("Check the Benford plot showing below",
             plotOutput("benford")
           
           )
         )
),
#Fourth tab
tabPanel("Benford Analysis Summary",
         
         sidebarLayout(
           sidebarPanel(
             selectInput("var2","Select the variable form the TMDB dataset", choices = c("budget", "popularity","revenue","vote_count"), selected = "budget")
           ),
           
           # Show a plot of the generated distribution
           mainPanel("Benford analysis summary",
            
             verbatimTextOutput("summary")
           )
         )
)
)



# Define server logic
server <- function(input, output) {
  #load all the packages
  library(benford.analysis)
  library(readr)
  library(dplyr)
  library(tidyverse)
  library(shiny)
  library(ggplot2)
  library(knitr)
  library(jsonlite) 
  library(RColorBrewer)
  #readRin data and data cleaning
  tmdb <- read_csv("tmdb_5000_movies.csv")
  movie <- tmdb %>%
    select(budget, popularity, revenue,vote_count,production_countries)%>%
    filter(budget>0 & revenue>0)
  
    data <- reactive({
      movie %>%
      select(input$var)
    })
    
    country_data <- movie %>%filter(nchar(production_countries)>2)%>% mutate(cntry=lapply(production_countries,fromJSON)) %>% unnest(cntry) %>% select(production_countries=name)
    country_data$production_countries<- recode(country_data$production_countries,"United States of America" = "USA", "United Kingdom" = "UK")
    
    map.world = map_data("world")
    
    country<-country_data %>%
      group_by(production_countries)%>%
      count(production_countries)%>%
      arrange(desc(n))%>%
      filter(n >20)
    
    map.world_joined <- left_join(map.world, country, by = c('region' = 'production_countries'))
    map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(n),F,T))
    
    #output that will be displayed in the shiny
  
    output$maps <- renderPlot(
      ggplot() +
        geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
        scale_fill_manual(values = c("#CCCCCC","#e60000")) +
        theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
              ,panel.background = element_rect(fill = "#444444")
              ,plot.background = element_rect(fill = "#444444")
              ,panel.grid = element_blank()
              ,plot.title = element_text(size = 30)
              ,plot.subtitle = element_text(size = 10)
              ,axis.text = element_blank()
              ,axis.title = element_blank()
              ,axis.ticks = element_blank()
              ,legend.position = "none")
    )
    
   output$myhist <- renderPlot(

      if(input$var =="budget"){

        ggplot(data(),aes(x = budget)) + geom_histogram(bins = input$bins, fill =input$color,color = "black")+ ggtitle("The distribution of budget")
      }

     else if (input$var =="revenue"){

     ggplot(data(),aes(x = revenue)) + geom_histogram(bins = input$bins, fill =input$color,color = "black") + ggtitle("The distribution of revenue")
     }

     else if (input$var == "popularity"){
       ggplot(data(),aes(x = popularity)) + geom_histogram(bins = input$bins, fill =input$color, color = "black") + ggtitle("The distribution of popularity")

     }
      else if (input$var == "vote_count"){
        ggplot(data(),aes(x = vote_count)) + geom_histogram(bins = input$bins, fill =input$color, color = "black") + xlab("number of vote")+ggtitle("The distribution of number of vote")

      }
     )
 

      output$benford <- renderPlot(
        
        if(input$var1 =="budget"){
          
          plot(benford(movie$budget), number.of.digits = 2)
        }
        
        else if (input$var1 =="revenue"){
          
          plot(benford(movie$revenue), number.of.digits = 2)
        }
        
        else if (input$var1 == "popularity"){
          plot(benford(movie$popularity), number.of.digits = 2)
          
        }
        else if (input$var1 == "vote_count"){
          plot(benford(movie$vote_count), number.of.digits = 2)
        })
     

         output$summary <- renderPrint(
        if(input$var2 =="budget"){

          print(benford(movie$budget))
        }

        else if (input$var2 =="revenue"){

          print(benford(movie$revenue))
        }

        else if (input$var2 == "popularity"){
          print(benford(movie$popularity))

        }
        else if (input$var2 == "vote_count"){
          print(benford(movie$vote_count))
        }
      )
 
  
}

# Run the application 

shinyApp(ui = ui, server = server)

