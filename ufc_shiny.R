library(shiny)
#library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

ufc_basics <- read.csv("/Users/VickyWu/Desktop/R datasets/UFC stats/ufcbasics.csv",
                       blank.lines.skip = TRUE, na.strings=TRUE)

#Strike Accuracy and Takedown Accuracy should be numerics instaead of factors
#Coerce factors to numerics (first coerce to character and replace "%" and "." with " ")
ufc_basics$Total.Strike.Accuracy<-gsub("%", "",as.character(ufc_basics$Total.Strike.Accuracy), fixed=TRUE)
ufc_basics$Total.Strike.Accuracy<-gsub(".", "",as.character(ufc_basics$Total.Strike.Accuracy), fixed=TRUE)
ufc_basics$Total.Strike.Accuracy<-gsub("-", "0", ufc_basics$Total.Strike.Accuracy, fixed=TRUE)
#Now coerce the characters to numerics and divide the number by 1000
ufc_basics$Total.Strike.Accuracy<-as.numeric(ufc_basics$Total.Strike.Accuracy)/10000

#Do the same with takedown accuracy
ufc_basics$Take.Down.Accuracy<-gsub("%", "", as.character(ufc_basics$Take.Down.Accuracy), fixed=TRUE)
ufc_basics$Take.Down.Accuracy<-gsub(".", "", ufc_basics$Take.Down.Accuracy, fixed=TRUE)
ufc_basics$Take.Down.Accuracy<-gsub("-", "0", ufc_basics$Take.Down.Accuracy, fixed=TRUE)
#Now coerce the characters to numerics and divide the number by 1000
ufc_basics$Take.Down.Accuracy<-as.numeric(ufc_basics$Take.Down.Accuracy)/10000

#Coerce Name, Lastname and Firstname to characters
ufc_basics$Name <- as.character(ufc_basics$Name)
ufc_basics$Last.Name <- as.character(ufc_basics$Last.Name)
ufc_basics$Firstst.Name<-as.character(ufc_basics$Firstst.Name)
colnames(ufc_basics)[2] <- "Last.Name"
colnames(ufc_basics)[3] <- "First.Name"
colnames(ufc_basics)[10] <- "Pass"
colnames(ufc_basics)[12] <- "Submission"

# Define UI for application that plots features of movies
ui <- fluidPage(
     #Add a theme
     #theme=shinytheme("journal"),
     # App title
     titlePanel("UFC Fighters Stats", windowTitle = "UFC Fighters"),
     
     # Sidebar layout with a input and output definitions
     sidebarLayout(
          
          # Inputs
          sidebarPanel(
               
               h3("Plotting"),      # Third level header: Plotting
               
               # Select variable for y-axis 
               selectInput(inputId = "y", 
                           label = "Y-axis:",
                           choices = c("Number of fights" = "Fights", 
                                       "Strikes" = "Strikes", 
                                       "Take Downs" = "Take.Down", 
                                       "Knock Downs" = "Knock.Down", 
                                       "Passes" = "Pass",
                                       "Reversals" ="Reversal",
                                       "Submissions" = "Submission" ), 
                           selected = "Fights"),
               
               # Select variable for x-axis 
               selectInput(inputId = "x", 
                           label = "X-axis:",
                           choices = c("Number of fights" = "Fights", 
                                       "Strikes" = "Strikes", 
                                       "Take Downs" = "Take.Down", 
                                       "Knock Downs" = "Knock.Down", 
                                       "Passes" = "Pass",
                                       "Reversals" ="Reversal",
                                       "Submissions" = "Submission" ), 
                           selected = "Strikes"),
               
               selectInput(inputId = "z", 
                           label = "Fighter-names:",
                           choices = ufc_basics$Name, 
                           selected = ufc_basics$Name[1]),
               
               # Enter text for plot title
               textInput(inputId = "plot_title", 
                         label = "Plot title", 
                         placeholder = "Enter text to be used as plot title"),
               
               hr(),
               
               h3("Subsetting"),    # Third level header: Subsetting
               
               hr(),
               
               # Show data table
               checkboxInput(inputId = "show_data",
                             label = "Show data table",
                             value = TRUE),
               
               # Built with Shiny by RStudio
               br(), br(),
               h5("Built with",
                  img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                  "by",
                  img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                  ".")
               
          ),
          
          # Output:
          mainPanel(
               
               tabsetPanel(type = "tabs",
                           id = "tabsetpanel",
                           tabPanel(title = "Plot", 
                                    plotOutput(outputId = "scatterplot"),
                                    br(),
                                    h5(textOutput("description"))),
                           tabPanel(title = "Data", 
                                    br(),
                                    DT::dataTableOutput(outputId = "ufcfightstable")),
                           tabPanel(title = "FightStats", 
                                    br(),
                                    DT::dataTableOutput(outputId = "selectedfightertable")),
                           tabPanel(title = "Bar Chart",
                                    plotOutput(outputId = "barchart"),
                                    br())
               ) #tabsetPanel
          ) #mainPanel
     ) #sidebarlayout
) #fluidpage

# Define server function required to create the scatterplot
server <- function(input, output, session) {
     
     
     # x and y as reactive expressions
     x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
     y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
     z <- reactive({ toTitleCase(str_replace_all(input$z, "_", " ")) })
     
     # Create scatterplot object the plotOutput function is expecting 
     output$scatterplot <- renderPlot({
          ggplot(data = ufc_basics, aes_string(x = input$x, y = input$y)) +
               geom_point() +
               labs(x = x(),
                    y = y(),
                    #color = toTitleCase(str_replace_all(input$z, "_", " ")),
                    title = toTitleCase(input$plot_title))
     })
     
     output$description <- renderText({
          paste("The plot above shows the relationship between",
                x(),
                "and",
                y(),
                "for UFC fighters")
     })
     
     # Print data table if checked
     output$ufcfightstable <- DT::renderDataTable(
          DT::datatable(data = ufc_basics[, c(1,4,5,7,9,10,11,12)], 
                        options = list(pageLength = 10), 
                        rownames = FALSE)
     )
     
     #selected fighter stats
     output$selectedfightertable <- DT::renderDataTable(
          DT::datatable(data = ufc_basics[ufc_basics$Name==input$z,], 
                        #options = list(pageLength = 10), 
                        rownames = FALSE)
     )
     
     #fighter scores barchart for selected fighter
     output$barchart <- renderPlot({
          barplot(height=data.matrix(ufc_basics[ufc_basics$Name==input$z,c(5,7,9,10,11,12)]),
                  main=paste(input$z,"scores"),
                  xlab="method",
                  ylab="scores")
          #text(x = barchart, y = scores, label = scores, pos = 3, cex = 0.8, col = "black")
                  #color = "red")
                    #title = toTitleCase(input$plot_title))
     })
     
     # Display data table tab only if show_data is checked
     observeEvent(input$show_data, {
          if(input$show_data){
               showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
          } else {
               hideTab(inputId = "tabsetpanel", target = "Data")
          }
     })
     
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
