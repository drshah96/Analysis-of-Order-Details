##### Order Details Shiny Web Application #####
### By: Dhruvin Shah 

library(shiny)
library("DT")

# Define UI for application
ui <- 
    navbarPage(
        "Analysis of Order Details",
        tabPanel(
            "Intoduction",
            fluidPage(
                
                # Application title
                titlePanel("Order Details Data Visualization"),
                
                # Main Panel 
                mainPanel(
                    h3("Data Description", align = "left"),
                    p("The data contains information about orders placed from 2011 to 2014. 
                      The variables of dataset depict the vital information like Order ID, Order Date, Return Product Details, Shipping Details, Priority of Order, Countries along with Sales Quantity as well as Profit."),
                )
                
            )
        ),
        
        tabPanel(
            "Sales of Sub-Category according to Region",
            fluidPage(
                mainPanel(
                    h3("HeatMap (Matrix)", align = "left"),
                    p("Heatmaps visualise data through variations in colouring. 
                      When applied to a tabular format, Heatmaps are useful for cross-examining multivariate data, through placing variables in the rows and columns and colouring the cells within the table."),
                    p(br()),
                ),
                plotOutput("Plot1")
            )
        ),
        
        tabPanel(
            "Sales of Sub-Category",
            fluidPage(
                mainPanel(
                    h3("BarPlot", align = "left"),
                    p("A bar plot represents an estimate of central tendency for a numeric variable with the height of each rectangle and provides some indication of the uncertainty around that estimate using error bars. 
                      Bar plots include 0 in the quantitative axis range, and they are a good choice when 0 is a meaningful value for the quantitative variable, and you want to make comparisons against it."),
                    p(br()),
                ),
                plotOutput("Plot2")
            )
        ),
        
        tabPanel(
            "Sales and Profit of Product according to region and Order Priority",
            fluidPage(
                mainPanel(
                    h3("Ridgeline Plot", align = "left"),
                    p("A Ridgeline plot (sometimes called Joyplot) shows the distribution of a numeric value for several groups. 
                      Distribution can be represented using histograms or density plots, all aligned to the same horizontal scale and presented with a slight overlap."),
                    p(br()),
                ),
                
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

        tabPanel(
            "References",
            fluidPage(
                titlePanel("References"),
                mainPanel(
                    p(br()),
                    
                    h5("1) Data:", align = "left"),
                    p("Elearning.tableau.com. 2020. [online] Available at: <https://elearning.tableau.com/desktop-i-fundamentals-for-tableau-10/48419/scorm/2s83epjqb5dy4#> [Accessed 27 April 2020"),
                    
                    p(br()),
                    
                    h5("2) Heatmap (Matrix):", align = "left"),
                    p("Heatmap (Matrix). (n.d.). Retrieved from https://datavizcatalogue.com/methods/heatmap.html"),
                    
                    p(br()),
                    
                    h5("3) Bar Plot:", align = "left"),
                    p("seaborn.barplot. (n.d.). Retrieved from https://seaborn.pydata.org/generated/seaborn.barplot.html"),
                    
                    p(br()),
                    
                    h5("4) Ridgeline Plot:", align = "left"),
                    p("Holtz, Y., & Healy, C. (n.d.). Ridgeline plot. Retrieved from https://www.data-to-viz.com/graph/ridgeline.html"),
                )
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
