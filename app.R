
library(shiny)
library(usethis)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(bslib)
library(thematic)

data("diamonds")
thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  h1("Exploration des Diamants"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "filtre_couleurs",
        choices = unique(diamonds$color),
        label = "Choisir une couleur à filtrer :",
        selected = "D"
      ),
      sliderInput(
        inputId = "prix",
        label = "Prix maximum :",
        min = 300,
        max = 20000,
        value = 5000
      ),
      actionButton(
        inputId = "boutton", 
        label = "Afficher une notification"
      )
    ),
    mainPanel(
      plotOutput("diamondsPlot"),
      DTOutput("diamonds_table")
    )
  )
)

server <- function(input, output, session) {
  data_filtered <- reactive({
    diamonds %>%
      filter(color == input$filtre_couleurs, price <= input$prix)
  })
  
  output$diamondsPlot <- renderPlot({
    ggplot(data_filtered(), aes(x = price, y = carat, color = color)) +
      geom_point(alpha = 0.5) +
      labs(
        title = glue("prix : {input$prix} & color: {input$filtre_couleurs}"),
        x = "Prix",
        y = "Carat"
      ) +
      theme_minimal()
  })
  
  output$diamonds_table <- renderDT({
    datatable(data_filtered())
  })
  
  observeEvent(input$boutton, {
    showNotification(glue("Filtrage sur la couleur {input$filtre_couleurs} avec un prix max de {input$prix}"), type = "message")
  })
}

shinyApp(ui = ui, server = server)
