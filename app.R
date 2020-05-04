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
                sliderInput("a1","Select value for a1",min = -0.8,max = 0.8,value = NULL,step = 0.1),
                sliderInput("b1","Select value for b1",min = -3,max = 3,value = NULL,step = 0.1),
                sliderInput("sd1","Select the standard deviation for y1_data",min = 0.1,max = 2.0,value = NULL,step = 0.1),
            ),
            
            conditionalPanel(
                condition = "input.logistic==true",
                sliderInput("a2", "Select value for a2",min = -0.8,max = 0.8,value = NULL,step = 0.1),
                sliderInput("b2","Select value for b2",min = -3,max = 3,value = NULL,step = 0.1),
                sliderInput("sd2","Select the standard deviation for y2_data",min = 0.1, max = 2.0,value = NULL,step = 0.1)
                
            ),
                
            conditionalPanel(
                condition = "input.simple==true||input.logistic==true",
                sliderInput("size", "Select the sample size",min = 5,max = 10000,value = NULL,step = 1),
                actionButton("simulate", "Simulate!"),
                actionButton("clear", "Clear"))
            
        ),
            # Show a plot of the generated distribution
            mainPanel(
                plotlyOutput("linePlot")
            )
        )
    )

# #regression line 
# x<- seq(-10,10,0.1)
# y1<- reactive({x*input$b1+input$a1})
# y2<- reactive({1/(1+exp(-input$a2-input$b2*x))})
# 
# #simulation data
# # size <- reactive({input$size})  
# # sd1 <- reactive({input$sd1})  
# # sd2 <- reactive({input$sd2})  
# 
# random_x<- reactive({rnorm(input$size,mean = 0, 3)})
# #random_num1<- reactive({rnorm(input$size, mean=0, sd=input$sd1)})
# #random_num2<- reactive({rnorm(input$size, mean=0, sd=input$sd2)})
# # 
# y1_data<- reactive({rnorm(input$size, mean=0, sd=input$sd1) + random_x*input$b1+input$a1})
# y2_data<- reactive({1/(1+exp(rnorm(input$size, mean=0, sd=input$sd2)-input$a2-input$b2 * random_x))})


# if (input$logistic){
#     p<-p%>%
#         add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", 
#                   line = list(color = 'rgb(22, 96, 167)'))
#         
# }
# 
# if (input$simple){
#     p<-p%>%
#         add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))
# }
# 
# p

server <- function(input, output) {
    
    v <- reactiveValues(data = NULL)
    

    observeEvent(input$simulate, {

        random_x<- rnorm(input$size,mean = 0, 3)
        random_num1<- rnorm(input$size, mean=0, sd=input$sd1)
        random_num2<- rnorm(input$size, mean=0, sd=input$sd2)

        y1_data<- random_num1 + random_x*input$b1+input$a1
        y2_data<- 1/(1+exp(random_num2-input$a2-input$b2 * random_x))
        
        
        x<- seq(-10,10,0.1)
        y1<- x*input$b1+input$a1
        y2<- 1/(1+exp(-input$a2-input$b2*x))
        
        #plot
        p<-plot_ly()%>%
            layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
        
        if (input$simple){
            p<-p%>%
                add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))%>%
                add_trace(x=round(random_x,4),y= round(y1_data,4), name = 'y1_data',
                          type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(255, 129, 10)'))
            }

        if (input$logistic) {
            p<-p%>%
                add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))%>%
                add_trace(x=round(random_x,4),y= round(y2_data,4), name = 'y2_data',
                          type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(22, 96, 167)'))
        }
        
        #p 
        
        v$data <- p
       
    })

    observeEvent(input$clear, {
        
            
        x<- seq(-10,10,0.1)
        y1<- x*input$b1+input$a1
        y2<- 1/(1+exp(-input$a2-input$b2*x))
        
        #plot
        p<-plot_ly()%>%
            layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
        
        if (input$logistic){
            p<-p%>%
                add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", 
                          line = list(color = 'rgb(22, 96, 167)'))
        }
        
        if (input$simple){
            p<-p%>%
                add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))
        }
        
        
        #p
        
        v$data <- p
        
    })
    
    output$linePlot <- renderPlotly({
        if (is.null(v$data)) return()
        v$data
    })
    
    # output$linePlot <- renderPlotly({
    #     
    #     x<- seq(-10,10,0.1)
    #     y1<- x*input$b1+input$a1
    #     y2<- 1/(1+exp(-input$a2-input$b2*x))
    #     
    #     #plot
    #     p<-plot_ly()%>%
    #         layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
    #     
    #     if (input$logistic){
    #         p<-p%>%
    #             add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", 
    #                       line = list(color = 'rgb(22, 96, 167)'))
    #     }
    #     
    #     if (input$simple){
    #         p<-p%>%
    #             add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))
    #     }
    #    
    #     
    #     p
    # })
}

<<<<<<< Updated upstream
#-----------------------------------Using plotlyProxy----------------------------------------------------
# server <- function(input, output) {
#     
#     size <- reactive({input$size})  
#     sd1 <- reactive({input$sd1})  
#     sd2 <- reactive({input$sd2})  
#     
#     random_x<- reactive({rnorm(input$size,mean = 0, 3)})
#     random_num1<- reactive({rnorm(input$size, mean=0, sd=input$sd1)})
#     random_num2<- reactive({rnorm(input$size, mean=0, sd=input$sd2)})
#     # 
#     y1_data<- reactive({random_num1 + random_x*input$b1+input$a1})
#     y2_data<- reactive({1/(1+exp(random_num2-input$a2-input$b2 * random_x))})
# 
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
#             layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
# 
#         if (input$simple){
#             p<-p%>%
#                 add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))
#         }
# 
#         if (input$logistic) {
#             p<-p%>%
#                 add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))
#         }
#     })
#     
#     # random_x<- reactive({rnorm(input$size,mean = 0, 3)})
#     # random_num1<- reactive({rnorm(input$size, mean=0, sd=input$sd1)})
#     # random_num2<- reactive({rnorm(input$size, mean=0, sd=input$sd2)})
#     # # 
#     # y1_data<- reactive({random_num1 + random_x*input$b1+input$a1})
#     # y2_data<- reactive({1/(1+exp(random_num2-input$a2-input$b2 * random_x))})
#     
#     # observeEvent(input$simulate, {
#     # 
#     #     # random_x<- rnorm(input$size,mean = 0, 3)
#     #     # random_num1<- rnorm(input$size, mean=0, sd=input$sd1)
#     #     # random_num2<- rnorm(input$size, mean=0, sd=input$sd2)
#     #     # 
#     #     # y1_data<- random_num1 + random_x*input$b1+input$a1
#     #     # y2_data<- 1/(1+exp(random_num2-input$a2-input$b2 * random_x))
#     # 
#     #     # plotlyProxy("linePlot") %>%
#     #     #     plotlyProxyInvoke("addTraces", list(x = random_x,y =y1_data,
#     #     #                                         type = "scatter",mode = "markers",
#     #     #                                         name = 'y1_data',line = list(color = 'rgb(255, 129, 10)')))
#     #     if (input$simple){
#     #         plotlyProxy("linePlot") %>%
#     #             plotlyProxyInvoke("addTraces", list(x = random_x,y =y1_data,
#     #                                                 type = "scatter",mode = "markers",
#     #                                                 name = 'y1_data',line = list(color = 'rgb(255, 129, 10)')))
#     #         }
#     # 
#     #     if (input$logistic) {
#     #         plotlyProxy("linePlot") %>%
#     #             plotlyProxyInvoke("addTraces", list(x = random_x,y =y2_data,
#     #                                                 type = "scatter",mode = "markers",
#     #                                                 name = 'y1_data',line = list(color = 'rgb(22, 96, 167)')))
#     #         }
#     # 
#     #     
#     # })
#    
# }
#-----------------------------------OVER: Using plotlyProxy----------------------------------------------------


#----------------------------------------Using ObserveEvent----------------------------------------------------
# server <- function(input, output) {
#     
#     x<- seq(-10,10,0.1)
#     y1<- x*input$b1+input$a1
#     y2<- 1/(1+exp(-input$a2-input$b2*x))
#     #data frame for the line plot
#     df1<-data.frame(x,y1,y2)
# 
#     random_x<- rnorm(input$size,mean = 0, 3)
#     random_num1<- rnorm(input$size, mean=0, sd=input$sd1)
#     random_num2<- rnorm(input$size, mean=0, sd=input$sd2)
# 
#     y1_data<- random_num1 + random_x*input$b1+input$a1
#     y2_data<- 1/(1+exp(random_num2-input$a2-input$b2 * random_x))
#     #data frame for the scatter plot
#     df2<-data.frame(random_x,y1_data,y2_data)
#     df3<-merge(df1, df2, by.x = "x", by.y = "random_x", all.x = TRUE,all.y = TRUE)
#     
#     df <- reactive({data = df3})
#         
#     
#     observeEvent(input$clear, {
#         
#         output$linePlot <- renderPlotly({
#         p<-plot_ly(df3)%>%
#             layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
#         
#         if (input$simple){
#             p<-p%>%
#                 add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))
#             }
#         
#         if (input$logistic) {
#             p<-p%>%
#                 add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))
#             }
#         
#         })
#     })
#     
#     observeEvent(input$simulate, {
#         output$linePlot <- renderPlotly({
#             p<-plot_ly(df3)%>%
#                 layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
# 
#             if (input$simple){
#                 p<-p%>%
#                     add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)'))%>%
#                     add_trace(x=round(random_x,4),y= round(y1_data,4), name = 'y1_data',
#                               type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(255, 129, 10)')) 
#             }
#             
#             if (input$logistic) {
#                 p<-p%>%
#                     add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))%>%
#                     add_trace(x=round(random_x,4),y= round(y2_data,4), name = 'y2_data',
#                               type = 'scatter',mode = 'markers',marker = list(size = 5, color ='rgb(22, 96, 167)'))
#             }
#             p
#         })
#     })  
# 
# 
# }
#----------------------------------OVER: Using ObserveEvent----------------------------------------------------



#------- GOD VERSION---------------------------------------------------------------------------
# 
# server <- function(input, output) {
#     
#     # observeEvent(input$simple||input$logistic,{
#     
#     
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
#             layout(xaxis = list(range=c(-10,10)), yaxis = list(range=c(-32,32)))
#         
#         if (input$simple){
#             p<-p%>%
#                 add_trace(x=x,y=y1,type = 'scatter', mode = 'lines', name = "y1=a1+b1*x",line = list(color = 'rgb(255, 129, 10)')) 
#         }
#         
#         if (input$logistic) {
#             p<-p%>%
#                 add_trace(x=x,y=y2,type = 'scatter', mode = 'lines', name = "y2=1/(exp(-a2-b2*x))", line = list(color = 'rgb(22, 96, 167)'))
#         }
#         
#         p
#     })
#     # })
# }

#------------------------------------------GOD_VERSOIN_IS_OVER----------------------------------



=======
>>>>>>> Stashed changes
# Run the application 
shinyApp(ui = ui, server = server)