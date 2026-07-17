# Módulo: Evaluación de Urgencia
# Paso 2 del wizard de relevamiento

# Este módulo:
#   
#   Evalúa condiciones de urgencia mediante dos preguntas críticas sobre agentes biológicos
# Muestra una alerta visual prominente cuando se detecta algún problema que requiere intervención prioritaria
# Proporciona contexto educativo sobre por qué estas condiciones son importantes
# Valida que ambas preguntas sean respondidas
# Guarda automáticamente las respuestas en app_data$relevamiento
# Restaura valores cuando el usuario regresa al paso
# Usa diseño visual claro con colores (info para instrucciones, danger para alertas) apropiados para estudiantes
# La alerta condicional solo aparece cuando hay un problema real, haciendo la interfaz más limpia y efectiva.

urgencia_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "alert alert-info",
      icon("info-circle"),
      " Identifica si el libro requiere atención prioritaria por presencia de agentes biológicos activos."
    ),
    
    radioButtons(
      ns("microorganismos"),
      "¿Posee microorganismos o moho activo?",
      choices = c("Sí", "No"),
      selected = character(0)
    ),
    
    radioButtons(
      ns("insectos"),
      "¿Posee insectos vivos?",
      choices = c("Sí", "No"),
      selected = character(0)
    ),
    
    uiOutput(ns("alerta_urgencia"))
  )
}

urgencia_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # Validación
    iv <- InputValidator$new()
    iv$add_rule("microorganismos", sv_required())
    iv$add_rule("insectos", sv_required())
    iv$enable()
    
    # Mostrar alerta si hay algún problema de urgencia
    output$alerta_urgencia <- renderUI({
      req(input$microorganismos, input$insectos)
      
      if (input$microorganismos == "Sí" || input$insectos == "Sí") {
        div(
          class = "alert alert-danger mt-3",
          style = "border-left: 5px solid #dc3545;",
          h5(
            icon("exclamation-triangle"),
            " ¡Atención: esta obra requiere intervención prioritaria!"
          ),
          p("Este libro presenta agentes biológicos activos que pueden dañar el material y propagarse a otros ejemplares."),
          tags$ul(
            if (input$microorganismos == "Sí") tags$li("Presencia de microorganismos o moho activo"),
            if (input$insectos == "Sí") tags$li("Presencia de insectos vivos")
          ),
          p(strong("Recomendación: "), "Aísla este ejemplar y notifica al responsable de conservación.")
        )
      }
    })
    
    # Observar cambios y actualizar app_data
    observe({
      app_data$relevamiento$microorganismos <- input$microorganismos
      app_data$relevamiento$insectos <- input$insectos
    })
    
    # Restaurar valores guardados cuando se vuelve a este paso
    observe({
      if (!is.null(app_data$relevamiento$microorganismos)) {
        updateRadioButtons(session, "microorganismos", selected = app_data$relevamiento$microorganismos)
      }
      if (!is.null(app_data$relevamiento$insectos)) {
        updateRadioButtons(session, "insectos", selected = app_data$relevamiento$insectos)
      }
    })
    
  })
}