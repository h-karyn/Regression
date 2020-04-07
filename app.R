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
                        step = 0.1)
        ),
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("linePlot"),
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$linePlot <- renderPlot({
        
        x<- seq(-10,10,0.1)
        
        y1<- x*input$b1+input$a1
        y2<- 1/(1+exp(-input$a2+input$b2*x))
        
        #Create a data frame and tidy it  
        df1<-data.frame(x,y1,y2)
        df2<-gather(df1,key=type,value=value,y1,y2)
       
        ggplot(df2,aes(y = value,x = x,color = type)) + 
            geom_line() +
            coord_cartesian(xlim = c(-3,3),ylim=c(-0.5,1.5))+
            scale_color_discrete(name = "Function", labels = c("y=a1+b1*x", "y=1/(exp(a2+b2*x))"))+
            labs(x="X", y="Y")
            
        
        
        ### The current codes which use gather() seems tedious, 
        ### but I am not sure why the following code is problematic 
        
        # data<-data.frame(x,y1,y2)
        # ggplot() + 
        # geom_line(data, aes(x = x, y = y1), color = "blue") +
        # geom_line(data, aes(x = x, y = y2), color = "red") 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



