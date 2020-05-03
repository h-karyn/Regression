#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(plotly)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regression Line Simulator"),

    # Sidebar with four slider inputs for a1, a2, b1, b2
    sidebarLayout(
        sidebarPanel(
            # checkboxGroupInput("regression", label = "Type of regression", 
            #                    choices = list("Simple Linear" = 1, "Logistic" = 2),
            #                    selected = c(1,2)),
            # 
            checkboxInput("simple","Simple Regression", value = TRUE),
            checkboxInput("logistic", "Logistic Regression", value = TRUE),
            helpText("Select values on the sliders"),
            
            conditionalPanel(
                condition = "input.simple==true",
                sliderInput("a1","Select value for a1",min = -0.8,max = 0.8,value = 0,step = 0.1),
                sliderInput("b1","Select value for b1",min = -3,max = 3,value = 0,step = 0.1),
                sliderInput("sd1","Select the standard deviation for y1_data",min = 0.1,max = 2.0,value = 0.1,step = 0.1),
            ),
            
            conditionalPanel(
                condition = "input.logistic==true",
                sliderInput("a2", "Select value for a2",min = -0.8,max = 0.8,value = 0,step = 0.1),
                sliderInput("b2","Select value for b2",min = -3,max = 3,value = 0,step = 0.1),
                sliderInput("sd2","Select the standard deviation for y2_data",min = 0.1, max = 2.0,value = 0.1,step = 0.1)
                
            ),
                
            conditionalPanel(
                condition = "input.simple==true||input.logistic==true",
                sliderInput("size", "Select the sample size",min = 5,max = 20,value = 5,step = 1),
                actionButton("simulate", "Simulate!"),
                actionButton("clear", "Clear"))
            
        ),
            # Show a plot of the generated distribution
            mainPanel(
                plotlyOutput("linePlot")
            )
        )
    )
server <- function(input, output) {
    observeEvent(input$simple||input$logistic,{
        output$linePlot <- renderPlotly({
            x<- seq(-10,10,0.1)
            y1<- x*input$b1+input$a1
            y2<- 1/(1+exp(-input$a2-input$b2*x))
            #data frame for the line plot
            df1<-data.frame(x,y1,y2)

            random_x<- rnorm(input$size,mean = 0, 3)
            random_num1<- rnorm(input$size, mean=0, sd=input$sd1)
            random_num2<- rnorm(input$size, mean=0, sd=input$sd2)

            y1_data<- random_num1 + random_x*input$b1+input$a1
            y2_data<- 1/(1+exp(random_num2-input$a2-input$b2 * random_x))
            #data frame for the scatter plot
            df2<-data.frame(random_x,y1_data,y2_data)

            #merging two data frames
            df3<-merge(df1, df2, by.x = "x", by.y = "random_x", all.x = TRUE,all.y = TRUE)

            p<-plot_ly(df3)%>%
                layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
            

            #plot
            if (input$simple){
                p<-p%>%
                    add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)')) 
                
            }
            
            if (input$logistic) {
                p<-p%>%
                    add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))
            }
            
            p
            })
    })
    
    # observeEvent(input$logistic,{
    #     output$linePlot <- renderPlotly({
    #         
    #         x<- seq(-10,10,0.1)
    #         y2<- 1/(1+exp(-input$a2-input$b2*x))
    #         
    #         l1<-list(x,y2)
    #         names(l1)<-c("x","y2")
    #         df<- as.data.frame(l1)
    #         
    #         #plot
    #         p<-plot_ly(df)%>%
    #             add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))
    #         
    #         p
    #     })
    # })
}
#------- GOD VERSION---------------------------------------------------------------------------
# 
# server <- function(input, output) {
#     output$linePlot <- renderPlotly({
#         
#         x<- seq(-10,10,0.1)
#         y1<- x*input$b1+input$a1
#         y2<- 1/(1+exp(-input$a2-input$b2*x))
#         #data frame for the line plot 
#         df1<-data.frame(x,y1,y2)
#         
#         random_x<- rnorm(input$size,mean = 0, 3)
#         random_num1<- rnorm(input$size, mean=0, sd=input$sd1)
#         random_num2<- rnorm(input$size, mean=0, sd=input$sd2)
#         
#         y1_data<- random_num1 + random_x*input$b1+input$a1
#         y2_data<- 1/(1+exp(random_num2-input$a2-input$b2 * random_x))
#         #data frame for the scatter plot 
#         df2<-data.frame(random_x,y1_data,y2_data)
#         
#         #merging two data frames
#         df3<-merge(df1, df2, by.x = "x", by.y = "random_x", all.x = TRUE,all.y = TRUE)
#         
#         #plot 
#         p<-plot_ly(df3)%>%
#             add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)')) %>% 
#             add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))%>%
#             add_trace(x=round(random_x,4),y= round(y1_data,4), name = 'y1_data',
#                       type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(255, 129, 10)')) %>%
#             add_trace(x=round(random_x,4),y= round(y2_data,4), name = 'y2_data',
#                       type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(22, 96, 167)'))%>%
#             layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
#         
#         p
#     })
# }

#------------------------------------------GOD_VERSOIN_IS_OVER----------------------------------



# Run the application 
shinyApp(ui = ui, server = server)