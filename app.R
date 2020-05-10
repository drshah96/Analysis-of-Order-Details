#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("DT")

# Define UI for application that draws a histogram
ui <- 
    navbarPage(
        "Analysis of Order Details",
        tabPanel(
            "Intoduction",
            fluidPage(
                
                # Application title
                titlePanel("Order Details Data Visualization"),
                
                # Sidebar with a slider input for number of bins 
#                sidebarLayout(
#                    sidebarPanel(
#                        sliderInput("bins",
#                                    "Number of bins:",
#                                    min = 1,
#                                    max = 50,
#                                    value = 30)
#                    ),
                    
                    # Show a plot of the generated distribution
#                    mainPanel(
#                        plotOutput("distPlot")
#                    )
#                )
            )
        ),
        
        tabPanel(
            "Heatmap of Sales of Sub-Category according to Region",
            fluidPage(
                plotOutput("Plot1")
            )
        ),
        
        tabPanel(
            "Sales of Category",
            fluidPage(
                plotOutput("Plot2")
            )
        ),
        
        tabPanel(
            "Sales and Profit of Product according to region and Order Priority",
            fluidPage(
                fluidRow(
                    column(
                        selectInput("selected_type",
                                    label = "Select Type",
                                    choices = c("Order Priority",
                                                "Region")),
                        width = 3
                    ),
                    
                    column(
                        selectInput("selected_data",
                                    label = "Select Data",
                                    choices = c("Sales",
                                                "Profit")),
                        width = 3
                    ),
                ),
                plotOutput("Plot3")
            )
        ),
        collapsible = TRUE
    )
            

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    suppressWarnings(source("RCode.R"))

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$Plot1 <- renderPlot({
        Plot1
    })
    
    output$Plot2 <- renderPlot({
        Plot2
    })
    
    output$Plot3 <- renderPlot({
        if(input$selected_type == "Order Priority" & input$selected_data == "Sales"){
            Plot3_Sales1
        }
        
        else if(input$selected_type == "Order Priority" & input$selected_data == "Profit"){
            Plot3_Profit1
        }
        
        else if(input$selected_type == "Region" & input$selected_data == "Sales"){
            Plot3_Sales2
        }
        
        else if(input$selected_type == "Region" & input$selected_data == "Profit"){
            Plot3_Profit2
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
