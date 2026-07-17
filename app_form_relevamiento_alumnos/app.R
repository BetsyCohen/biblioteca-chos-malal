library(shiny)
library(bslib)
library(shinyvalidate)

# La carga de los modulos se debería tomar automaticamente desde la carpeta R


ui <- page_navbar(
  title = "Relevamiento Biblioteca Patrimonial - Chos Malal",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    base_font = font_google("Roboto")
  ),
  fillable = FALSE,
  
  nav_panel(
    "Formulario de Relevamiento",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        h4("Progreso"),
        uiOutput("progress_indicator"),
        hr(),
        h5("Navegación"),
        actionButton("btn_prev", "← Anterior", width = "100%", class = "mb-2"),
        actionButton("btn_next", "Siguiente →", width = "100%", class = "mb-2"),
        hr(),
        actionButton("btn_save_draft", "💾 Guardar borrador", width = "100%", class = "btn-warning"),
        hr(),
        p("Paso actual:", textOutput("current_step_name", inline = TRUE), class = "small text-muted")
      ),
      
      card(
        card_header(
          class = "bg-primary text-white",
          uiOutput("step_title")
        ),
        card_body(
          uiOutput("step_content")
        )
      )
    )
  ),
  
  nav_panel(
    "Ayuda",
    card(
      card_header("Instrucciones de uso"),
      card_body(
        h4("Bienvenido al sistema de relevamiento"),
        p("Este formulario te permitirá registrar el estado de conservación de los libros de la biblioteca patrimonial."),
        h5("Pasos a seguir:"),
        tags$ol(
          tags$li("Completa cada sección del formulario"),
          tags$li("Puedes guardar borradores en cualquier momento"),
          tags$li("Navega entre secciones usando los botones"),
          tags$li("Revisa toda la información antes de enviar"),
          tags$li("Al finalizar, presiona 'Enviar relevamiento'")
        ),
        hr(),
        h5("Consejos:"),
        tags$ul(
          tags$li("Lee cuidadosamente cada pregunta"),
          tags$li("Si una obra presenta moho o insectos vivos, márcalo inmediatamente"),
          tags$li("Toma fotografías claras y bien iluminadas"),
          tags$li("No olvides revisar todos los datos antes de enviar")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Estado global de la aplicación
  # TODO: Migrar a PostgreSQL
  app_data <- reactiveValues(
    current_step = 1,
    max_steps = 10,
    
    # Datos del relevamiento (estructura compatible con PostgreSQL)
    relevamiento = list(
      # Paso 1: Información general
      titulo = NULL,
      isbn = NULL,
      id_biblioteca = NULL,
      procedencia = NULL,
      institucion_nombre = NULL,
      
      # Paso 2: Urgencia
      microorganismos = NULL,
      insectos = NULL,
      
      # Paso 3: Evaluación general
      estado_cubierta = NULL,
      sellos_inscripciones = NULL,
      etiquetas = NULL,
      cinta_adhesiva = NULL,
      suciedad = NULL,
      marcas_mojadura = NULL,
      intervenciones_anteriores = NULL,
      
      # Paso 4: Tapa
      posee_tapa = NULL,
      tapa_estado = NULL,
      tapa_desprendida = NULL,
      tapa_rigida = NULL,
      
      # Paso 5: Lomo
      posee_lomo = NULL,
      lomo_estado = NULL,
      lomo_cuero = NULL,
      
      # Paso 6: Hojas de guarda
      posee_hojas_guarda = NULL,
      hojas_guarda_estado = NULL,
      
      # Paso 7: Costura
      reunion_hojas = NULL,
      costura_estado = NULL,
      
      # Paso 8: Bloque de papel
      bloque_problemas = NULL,
      
      # Paso 9: Fotografías (futuro: almacenar rutas)
      foto_portada = NULL,
      foto_lomo = NULL,
      foto_interior = NULL,
      fotos_adicionales = NULL,
      
      # Metadatos
      fecha_creacion = Sys.time(),
      fecha_modificacion = Sys.time(),
      estado = "borrador" # borrador, enviado
    )
  )
  
  # Nombres de los pasos
  step_names <- c(
    "Información General",
    "Evaluación de Urgencia",
    "Evaluación General",
    "Estado de la Tapa",
    "Estado del Lomo",
    "Hojas de Guarda",
    "Costura",
    "Bloque de Papel",
    "Fotografías",
    "Revisión Final"
  )
  
  # Título del paso actual
  output$step_title <- renderUI({
    h3(paste("Paso", app_data$current_step, "de", app_data$max_steps, "-", step_names[app_data$current_step]))
  })
  
  # Nombre del paso actual
  output$current_step_name <- renderText({
    step_names[app_data$current_step]
  })
  
  # Indicador de progreso
  output$progress_indicator <- renderUI({
    progress_pct <- (app_data$current_step / app_data$max_steps) * 100
    
    tagList(
      div(
        class = "progress mb-3",
        div(
          class = "progress-bar bg-success",
          role = "progressbar",
          style = paste0("width: ", progress_pct, "%"),
          paste0(round(progress_pct), "%")
        )
      ),
      tags$small(paste("Paso", app_data$current_step, "de", app_data$max_steps))
    )
  })
  
  # Contenido del paso actual
  output$step_content <- renderUI({
    switch(app_data$current_step,
           "1" = informacion_general_ui("info_general"),
           "2" = urgencia_ui("urgencia"),
           "3" = evaluacion_general_ui("eval_general"),
           "4" = tapa_ui("tapa"),
           "5" = lomo_ui("lomo"),
           "6" = hojas_guarda_ui("hojas_guarda"),
           "7" = costura_ui("costura"),
           "8" = bloque_papel_ui("bloque_papel"),
           "9" = fotos_ui("fotos"),
           "10" = revision_ui("revision")
    )
  })
  
  # Llamar a los módulos (servidores)
  informacion_general_server("info_general", app_data)
  urgencia_server("urgencia", app_data)
  evaluacion_general_server("eval_general", app_data)
  tapa_server("tapa", app_data)
  lomo_server("lomo", app_data)
  hojas_guarda_server("hojas_guarda", app_data)
  costura_server("costura", app_data)
  bloque_papel_server("bloque_papel", app_data)
  fotos_server("fotos", app_data)
  revision_result <- revision_server("revision", app_data, step_names)
  
  # Navegación: Anterior
  observeEvent(input$btn_prev, {
    if (app_data$current_step > 1) {
      app_data$current_step <- app_data$current_step - 1
    }
  })
  
  # Navegación: Siguiente
  observeEvent(input$btn_next, {
    if (app_data$current_step < app_data$max_steps) {
      app_data$current_step <- app_data$current_step + 1
    }
  })
  
  # Guardar borrador
  observeEvent(input$btn_save_draft, {
    # TODO: Guardar en PostgreSQL
    # Código futuro:
    # dbWriteTable(con, "relevamientos_borrador", as.data.frame(app_data$relevamiento))
    
    app_data$relevamiento$fecha_modificacion <- Sys.time()
    app_data$relevamiento$estado <- "borrador"
    
    showNotification(
      "Borrador guardado exitosamente",
      type = "message",
      duration = 3
    )
  })
  
  # Observar el evento de envío final desde el módulo de revisión
  observeEvent(revision_result$enviar, {
    # TODO: Guardar en PostgreSQL como registro final
    # Código futuro:
    # dbWriteTable(con, "relevamientos_finales", as.data.frame(app_data$relevamiento))
    
    app_data$relevamiento$fecha_modificacion <- Sys.time()
    app_data$relevamiento$estado <- "enviado"
    
    showModal(
      modalDialog(
        title = "✅ Relevamiento enviado",
        "El relevamiento ha sido registrado exitosamente. Gracias por tu contribución al proyecto de conservación patrimonial.",
        footer = modalButton("Cerrar"),
        easyClose = TRUE
      )
    )
  })
}

shinyApp(ui, server)