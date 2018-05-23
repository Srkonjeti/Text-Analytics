install.packages("tm")
install.packages("wordcloud")
install.packages("memoise")
install.packages("dtplyr")
install.packages("maps")


library(rgeos)
library(rgdal)
library(shiny)
library(data.table)

library(leaflet)

library(tm)
library(wordcloud)
library(memoise)
library(dtplyr)
library(dplyr)
# library(maps)
# library(geojsonio)
# library(geojson)

Hdata <- fread("TweetDatabyState.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

Statenames <- fread("StateNames.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

States<-setNames(Statenames$Ab,Statenames$Name)


ui<-fluidPage(
  # Application title
  titlePanel("RETROSPECTIVE STUDY OF U.S. HEALTHCARE COSTS"),
  
  sidebarLayout(
    # Sidebar with a slidelibrary(shiny)r and selection inputs
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #EAF2F8; }")
      ),
      tags$style(".well{background-color:grey;}"),
      
      selectInput("State", "Select State:",
                  choices = States),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 25, value = 10),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 100,  value = 30)
    ),
    
    # Show Word Cloud
    mainPanel(
      # Output: Header + summary of distribution ----
      
      h4("Health Costs Snapshot by State"),
      leafletOutput("map","100%",300),
      
      h4("Health Cost Information by State"),
      tableOutput("table"),
      
      h4("Word Cloud By State"),
      plotOutput("plot")
    )
  )
)



server<- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$State)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(3,0.5),
                  min.freq = input$freq, max.words=input$max,colors=brewer.pal(5, "Set1"))
    
    
    output$table <- renderTable({
      df<- Hdata %>%
        select(SumofTotalCost,SumofAverageCoveredCharges,Variance) %>%
        filter(Hdata$State %in% input$State)
      df
    })
    
    output$map <- renderLeaflet({
      mapStates = map("state", fill = TRUE, plot = FALSE)
      
      df <- Hdata %>%
        filter(Hdata$State %in% input$State)
      df1<- Hdata %>%
        select(SumofTotalCost,SumofAverageCoveredCharges,Variance) %>%
        filter(Hdata$State %in% input$State)
      content<-paste(sep="<br/>",
                     paste("SumofTotalCost:",df1$SumofTotalCost), 
                     paste("SumofAverageCoveredCharges:",df1$SumofAverageCoveredCharges),
                     paste("Variance:",df1$Variance))
     
      leaflet() %>% addTiles() %>%
        setView(-93.65, 42.0285, zoom = 4) %>%
        addMarkers(df$Long,df$Lat, popup = content
                   ,labelOptions = labelOptions(noHide = T, textsize = "15px"))
      
    })
  })
}


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(instate) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(instate %in% States))
    stop("Unknown State")
  data2 <- Hdata %>% filter(Hdata$State==instate)
  text1 <- data2$NontrumpcareTweet2
  text2 <- data2$TrumpcareTweet1
  
  myCorpus = Corpus(VectorSource(list(text1, text2)))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "a", "an", "and", "but", "is", "the", "will"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})



#Create Shiny app ----
shinyApp(ui, server)
