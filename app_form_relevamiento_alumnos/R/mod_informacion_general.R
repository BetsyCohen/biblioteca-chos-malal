# Módulo: Información General
# Paso 1 del wizard de relevamiento

# library(shiny)
# library(bslib)
# library(shinyvalidate)
# 
# Este módulo:
#   
#   Captura la información básica del libro (título, ISBN, ID de biblioteca, procedencia)
# Muestra condicionalmente el campo "Nombre de la institución" solo cuando se selecciona "Institución" como procedencia
# Valida que los campos requeridos estén completos usando shinyvalidate
# Guarda automáticamente los datos en app_data$relevamiento
# Restaura valores cuando el usuario vuelve a este paso (permite edición)
# Está preparado para PostgreSQL: la estructura de datos es compatible con una tabla relacional


informacion_general_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    p("Completa la información básica del libro a relevar."),
    
    textInput(
      ns("titulo"),
      "Título del libro",
      placeholder = "Ingresa el título completo"
    ),
    
    textInput(
      ns("isbn"),
      "ISBN (si está disponible)",
      placeholder = "ISBN"
    ),
    
    textInput(
      ns("id_biblioteca"),
      "ID Biblioteca",
      placeholder = "Código o número de inventario"
    ),
    
    radioButtons(
      ns("procedencia"),
      "Procedencia",
      choices = c("Privada", "Institución"),
      selected = character(0)
    ),
    
    uiOutput(ns("institucion_condicional"))
  )
}

informacion_general_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # Validación
    iv <- InputValidator$new()
    iv$add_rule("titulo", sv_required())
    iv$add_rule("id_biblioteca", sv_required())
    iv$add_rule("procedencia", sv_required())
    
    # Validación condicional para institución
    iv$add_rule("institucion_nombre", function(value) {
      if (!is.null(input$procedencia) && input$procedencia == "Institución") {
        if (is.null(value) || value == "") {
          "Debes ingresar el nombre de la institución"
        }
      }
    })
    
    iv$enable()
    
    # Mostrar campo de institución condicionalmente
    output$institucion_condicional <- renderUI({
      req(input$procedencia)
      
      if (input$procedencia == "Institución") {
        textInput(
          session$ns("institucion_nombre"),
          "Nombre de la institución",
          placeholder = "Ingresa el nombre de la institución"
        )
      }
    })
    
    # Observar cambios y actualizar app_data
    observe({
      app_data$relevamiento$titulo <- input$titulo
      app_data$relevamiento$isbn <- input$isbn
      app_data$relevamiento$id_biblioteca <- input$id_biblioteca
      app_data$relevamiento$procedencia <- input$procedencia
      
      # Solo guardar institución si la procedencia es "Institución"
      if (!is.null(input$procedencia) && input$procedencia == "Institución") {
        app_data$relevamiento$institucion_nombre <- input$institucion_nombre
      } else {
        app_data$relevamiento$institucion_nombre <- NULL
      }
    })
    
    # Restaurar valores guardados cuando se vuelve a este paso
    observe({
      if (!is.null(app_data$relevamiento$titulo)) {
        updateTextInput(session, "titulo", value = app_data$relevamiento$titulo)
      }
      if (!is.null(app_data$relevamiento$isbn)) {
        updateTextInput(session, "isbn", value = app_data$relevamiento$isbn)
      }
      if (!is.null(app_data$relevamiento$id_biblioteca)) {
        updateTextInput(session, "id_biblioteca", value = app_data$relevamiento$id_biblioteca)
      }
      if (!is.null(app_data$relevamiento$procedencia)) {
        updateRadioButtons(session, "procedencia", selected = app_data$relevamiento$procedencia)
      }
      if (!is.null(app_data$relevamiento$institucion_nombre)) {
        updateTextInput(session, "institucion_nombre", value = app_data$relevamiento$institucion_nombre)
      }
    })
    
  })
}

#shinyApp(informacion_general_ui, informacion_general_server)
