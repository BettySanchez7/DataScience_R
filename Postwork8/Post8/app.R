#POSTWORK 8
#EQUIPO 3

library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinythemes)

match<-read.csv("Documentos/BEDU_DataScience/8_Shiny_Dashboard/Postwork8/match.data.csv")

#UI
ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Dashboard Football"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Gráfico de Barras", tabName = "barras", icon = icon("dashboard")),
                    menuItem("Gráficas Post-work 3", tabName = "graph", icon = icon("area-chart")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                    menuItem("Gráficas ganancia", tabName = "img", icon = icon("file-picture-o"))
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Gráfico de barras
                    tabItem(tabName = "barras",
                            fluidRow(
                                titlePanel("Gráfica de Barras"), 
                                selectInput("x", "Seleccione el valor de X",
                                            choices = names(match)),
                                
                                selectInput("zz", "Selecciona la variable del grid", 
                                            choices = c("cyl", "vs", "gear", "carb")),
                                
                                box(plotOutput("plot1", height = 250)),
                                
                                box(
                                    title = "Controls",
                                    sliderInput("bins", "Number of observations:", 1, 30, 15)
                                )
                            )
                    ),
                    
                    # Gráficas Post-work 3 (Imágenes)
                    tabItem(tabName = "graph", 
                            fluidRow(
                                titlePanel(h3("Gŕaficas Post-work 3")),
                                img( src = "goles_casa.png", 
                                     height = 350, width = 500),
                                img( src = "goles_visitante.png", 
                                     height = 350, width = 500),
                                img( src = "probabilidad_conjunta.png", 
                                     height = 350, width = 500)
                                
                            )
                    ),
                    
                    
                    #Data table
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    #Gráficas ganancia (Imágenes)
                    tabItem(tabName = "img",
                            fluidRow(
                                titlePanel(h3("Momios")),
                                img( src = "momiosmax.png", 
                                     height = 350, width = 500),
                                img( src = "momiosprom.png", 
                                     height = 350, width = 500)
                            )
                    )
                    
                )
            )
        )
    )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    
    #Gráfico de Histograma
    output$output_plot <- renderPlot({
        
        ggplot(match, aes(x=match[,input$x])) + 
            geom_bar() +
            xlab(input$x)
        
        
    })
    
    # Gráficas de dispersión
    output$output_plot <- renderPlot({ 
        
        ggplot(mtcars, aes(x =  mtcars[,input$a] , y = mtcars[,input$y], 
                           colour = mtcars[,input$z] )) + 
            geom_point() +
            ylab(input$y) +
            xlab(input$x) + 
            theme_linedraw() + 
            facet_grid(input$z)  #selección del grid
        
    })   
    
    #Data Table
    output$data_table <- renderDataTable( {match}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
