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
library(tidyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Line Plot"),

    # Sidebar with four slider inputs for a1, a2, b1, b2
    sidebarLayout(
        sidebarPanel(
            helpText("Select values on the sliders"),
            sliderInput("a1",
                        "Select value for a1",
                        min = -0.8,
                        max = 0.8,
                        value = 0,
                        step = 0.1),
            sliderInput("b1",
                        "Select value for b1",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = 0.1),
            
            sliderInput("a2",
                        "Select value for a2",
                        min = -0.8,
                        max = 0.8,
                        value = 0,
                        step = 0.1),
            sliderInput("b2",
                        "Select value for b2",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = 0.1),
            sliderInput("size",
                        "Select the sample size",
                        min = 5,
                        max = 20,
                        value = 5,
                        step = 1),
            sliderInput("sd",
                        "Select the standard deviation",
                        min = 0.1,
                        max = 2.0,
                        value = 0.1,
                        step = 0.1)
        ),
            # Show a plot of the generated distribution
            mainPanel(
                plotlyOutput("linePlot"),
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$linePlot <- renderPlotly({
        
        x<- seq(-10,10,0.1)
        y1<- x*input$b1+input$a1
        y2<- 1/(1+exp(-input$a2-input$b2*x))
        #data frame for the line plot 
        df1<-data.frame(x,y1,y2)
    
        random_x<- round(runif(input$size,min=-10, max=10),1)
        random_num<- round((rnorm(input$size, mean=0, sd=input$sd)),1)
        y1_data<- random_num+random_x*input$b1+input$a1
        y2_data<- 1/(1+exp(random_num-input$a2-input$b2*random_x))
        #data frame for the scatter plot 
        df2<-data.frame(random_x,y1_data,y2_data)
        
        #merging two data frames
        df3<-merge(df1, df2, by.x = "x", by.y = "random_x", all.x = TRUE,all.y = TRUE)
        
        #plot 
        p<-plot_ly(df3)%>%
            add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y=a1+b1*x",line = list(color = 'rgb(255, 129, 10)')) %>% 
            add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))%>%
            add_trace(x=random_x,y= y1_data, name = 'y1_data',type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(255, 129, 10)')) %>%
            add_trace(x=random_x,y= y2_data, name = 'y2_data',type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(22, 96, 167)'))
        
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



