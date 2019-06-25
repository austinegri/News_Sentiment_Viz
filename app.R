#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lubridate)
library(scales)

# define list of publications
# publications <- list("All", "Atlantic", "Reuters", "New_York_Post",
#                      "Breitbart", "New_York_Times", "Buzzfeed_News",
#                      "National_Review", "Business_Insider", "Talking_Points_Memo",
#                      "Fox_News", "CNN", "Guardian", "NPR", "Washington_Post")
publications <- list("All", "Breitbart")


# load data using new tidy format data
load_data <- function(filename) {
   
   ## Read data from CSV
   df <- read.csv(filename, header= TRUE, 
                  row.names=1, sep = ',', na.strings= "None", stringsAsFactors = TRUE)
   #change date column to be processed as dates
   df$dates <- as.Date(parse_date_time(df$dates, "%m%y"))
   df
}

# data_ <- read.csv("../app/data/Tidy_Data/tidy_sent_data_All.csv", header= TRUE, 
#                   row.names=1, sep = ',', na.strings= "None", stringsAsFactors = TRUE)
# data_$dates <- as.Date(parse_date_time(data_$dates, "%m%y"))#as.Date(parse_date_time(data_$dates, "%m%y"))
# 
# occ_by_time <- read.csv("../app/data/Tidy_Data/tidy_occ_by_time_data.csv", header= TRUE, 
#                         row.names=1, sep = ',', na.strings= "None", stringsAsFactors = TRUE)
# occ_by_time$dates <- as.Date(parse_date_time(occ_by_time$dates, "%m%y"))

# "All", "Atlantic", "Reuters", "New_York_Post",
# "Breitbart", "New_York_Times", "Buzzfeed_News",
# "National_Review", "Business_Insider", "Talking_Points_Memo",
# "Fox_News", "CNN", "Guardian", "NPR", "Washington_Post")
data_ <- load_data("Data/Tidy_Data/tidy_sent_data_All.csv")
data_atl <- load_data("Data/Tidy_Data/tidy_sent_data_Atlantic.csv")
data_rtr <- load_data("Data/Tidy_Data/tidy_sent_data_Reuters.csv")
data_NYP <- load_data("Data/Tidy_Data/tidy_sent_data_New_York_Post.csv")
data_bb <- load_data("Data/Tidy_Data/tidy_sent_data_Breitbart.csv")
data_NYT <- load_data("Data/Tidy_Data/tidy_sent_data_New_York_Times.csv")
data_Buzz <- load_data("Data/Tidy_Data/tidy_sent_data_Buzzfeed_News.csv")
data_ntl <- load_data("Data/Tidy_Data/tidy_sent_data_National_Review.csv")
data_busin <- load_data("Data/Tidy_Data/tidy_sent_data_Business_Insider.csv")
data_tpm <- load_data("Data/Tidy_Data/tidy_sent_data_Talking_Points_Memo.csv")
data_FOX <- load_data("Data/Tidy_Data/tidy_sent_data_Fox_News.csv")
data_CNN <- load_data("Data/Tidy_Data/tidy_sent_data_CNN.csv")
data_gdn <- load_data("Data/Tidy_Data/tidy_sent_data_Guardian.csv")
data_NPR <- load_data("Data/Tidy_Data/tidy_sent_data_NPR.csv")
data_WaPo <- load_data("Data/Tidy_Data/tidy_sent_data_Washington_Post.csv")

occ_by_time <- load_data("Data/Tidy_Data/tidy_occ_by_time_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel("News Entity Sentiment Data", windowTitle = "News Sentiment"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
         # Select publication to plot
         selectInput(inputId = "pub", label = strong("Publication"),
                     choices = unique(publications),
                     selected = "All"),
         
         ## TODO make input id lists vary based on publication choice
         # Select Entity 1 to plot
         selectInput(inputId = "e1", label = strong("Entity 1"),
                     # choices = list("Trump", "Donald.Trump"), 
                     unique(colnames(data_)),
                     selected = "Trump"),
         # Select Entity 2 to plot
         selectInput(inputId = "e2", label = strong("Entity 2"),
                     # choices = list("Clinton", "Hillary.Clinton"), 
                     unique(colnames(data_)),
                     selected = "Clinton")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        # plotOutput("occurences_plot"),
         textOutput("testvar1"),
         textOutput("testvar2"),
         plotOutput("sent_plot"),
         plotOutput("occ_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
   #Debug print value of string being passed
   e1 <- reactive({input$e1})
   e2 <- reactive({input$e2})
   output$testvar1 = renderText(input$e1)
   output$testvar2 = renderText(input$e2)
   title_ <- reactive({paste("Sentiment over Time (", input$e1, " and ", input$e2, ")", sep= "")})
   
   # "All", "Atlantic", "Reuters", "New_York_Post",
   # "Breitbart", "New_York_Times", "Buzzfeed_News",
   # "National_Review", "Business_Insider", "Talking_Points_Memo",
   # "Fox_News", "CNN", "Guardian", "NPR", "Washington_Post")
   df <- reactive({
      if ( input$pub == "All" ) {
         data_
      } else if ( input$pub == "Atlantic") {
         data_atl
      } else if ( input$pub == "Reuters") {
         data_rtr
      } else if ( input$pub == "New_York_Post") {
         data_NYP
      } else if ( input$pub == "Breitbart") {
         data_bb
      } else if ( input$pub == "New_York_Times") {
         data_NYT
      } else if ( input$pub == "Buzzfeed_News") {
         data_Buzz
      } else if ( input$pub == "National_Review") {
         data_ntl
      } else if ( input$pub == "Business_Insider") {
         data_busin
      } else if ( input$pub == "Talking_Points_Memo") {
         data_tpm
      } else if ( input$pub == "Fox_News") {
         data_FOX
      } else if ( input$pub == "CNN") {
         data_CNN
      } else if ( input$pub == "Guardian") {
         data_gdn
      } else if ( input$pub == "NPR") {
         data_NPR
      } else if ( input$pub == "Washington_Post") {
         data_WaPo
      }
      
   })

   # ggplot initializer
   # sentiment plot
   output$sent_plot <- renderPlot({
      # p <- ggplot(data= data_) +
      p <- ggplot(data= df()) + 
         # use white background
         theme_bw() +

         # plot lines for entities
         geom_line( aes_string(x= "dates", y= e1(), color= factor(e1()))) +
         geom_line( aes_string(x= "dates", y= e2(), color= factor(e2()))) +

         # plot points for entities
         geom_point( aes_string(x= "dates", y= e1(),  colour = factor(e1()))) +
         geom_point( aes_string(x= "dates", y= e2(),  colour = factor(e2()))) +


        
         scale_color_manual(values = c('blue', 'orange')) +
   
         # y range from [-1,+1]
         scale_y_continuous(limits= c(-.6,+.6)) +
   
         # label axes and Title
         labs(x= "Date",
              y= expression(paste("Sentiment Value [-1,+1]")), #expression(paste("Sentiment Value (range [-1,+1])")),
              # title= title_,
              color= "Entities") +
   
         # format x-axis ticks
         theme(axis.text.x= element_text(size= 6,
                                         vjust= .5,
                                         angle= 45)) +
         # x-axis scaling (labels every x months, formatted e.g. Jan '14)
         scale_x_date(date_breaks= "2 months",
                      labels= date_format("%b '%y"))
   
      # finish the plot for output
      print(p)
   })
   
   # occurences plot
   output$occ_plot <- renderPlot({
      p2 <- ggplot(data= occ_by_time) +
         
         # use white background
         theme_bw() +
         
         # plot lines for entities
         geom_line( aes_string(x= "dates", y= e1(), color= factor(e1()))) +
         geom_line( aes_string(x= "dates", y= e2(), color= factor(e2()))) +
         #
         # plot points for entities
         geom_point( aes_string(x= "dates", y= e1(),  colour = factor(e1()))) +
         geom_point( aes_string(x= "dates", y= e2(),  colour = factor(e2()))) +
         
         scale_color_manual(values = c('blue4', 'darkorange3')) +
         
         
         # label axes and Title: TODO NORMALIZE the Data
         labs(x= "Date",
              y= expression(paste("Number of Occurences")), #expression(paste("Sentiment Value (range [-1,+1])")),
              # title= title_,
              color= "Entities") +
         
         # format x-axis ticks
         theme(axis.text.x= element_text(size= 6,
                                         vjust= .5,
                                         angle= 45)) +
         
         # x-axis scaling (labels every x months, formatted e.g. Jan '14)
         scale_x_date(date_breaks= "2 months",
                      labels= date_format("%b '%y"))
      
      
      print(p2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

