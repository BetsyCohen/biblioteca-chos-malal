library(shiny)

ui <- fluidPage(
  titlePanel("Biblioteca Chos Malal"),
  
  h3("MVP Relevamiento Patrimonial"),
  
  p("Primera versión de la aplicación")
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)