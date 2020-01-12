# Paquetes -------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(ggridges)
library(lubridate)
library(plotly)
library(ggthemes)
library(shiny)
library(shinythemes)

# Cargando el ambiente de trabajo para el app
source("load_ws.R")

# Cuerpo de la aplicación ----------------------------------------------------------
shinyUI(fluidPage(
  theme = shinytheme("lumen"),
  sidebarLayout(
    
    # Barra lateral
    sidebarPanel(
      width = 3,
      # widget para seleccionar el tipo de vehículo
      selectInput(
        inputId = "tipo_vehiculo",
        label = "Tipo de vehículo",
        choices =c("Todos", "Carro", "Camion"),
        selected = "Camion",
        multiple = F),
      
      # widget reactivo, depende de tipo de vehículo seleccionado
      # este se crea en el server
      uiOutput("size"),
      
      # Controles para la ventana del análisis de precios
      h4("Tabla de precio"),
      wellPanel(
        # una fila para colocar dos widgets uno al lado del otro
        fluidRow(
          # columna izquierda
          column(
            # Para controlar los meses
            selectInput(
              inputId = "mes",
              label = "Mes",
              choices = unique(month(tbr_pcr1$fecha, label = TRUE)),
              selected = "Jan"
              ),
            width = 6),
          # columna derecha
          column(
            # Para elegir el año
            selectInput(
              inputId = "year",
              label = "Año",
              choices = c(2018, 2019),
              selected = 2018),
            width = 6)
          
          ),
        
        # este widget permite elergir el step de la tabla del precio
        numericInput(
          inputId = "rango",
          label = "Amplio de intervalo",
          value = 50)
        ),
      
      # Para importar nueva base de datos
      h4("Procesamiento de archivo externo"),
      # Controles
      wellPanel(
        # importar archivo externo
        fileInput(inputId = "file", label = "Base original DGA"),
        # descargar TBR-PCR
        downloadButton("download_data.csv", label = "TBR-PCR"),
        # descargar OTR
        downloadButton("download_otr.csv", label = "OTR"),
        # observaciones no utilizadas
        downloadButton("download_exclude.csv", label = "obs excluidas")
        )  
    ),
    
    # Ventana principal de la aplicación
    mainPanel(
      width = 9,
      # Pestañas en el panel principal
      tabsetPanel(
        
        tabPanel(
          title = "Análisis gráfico",
          fluidRow(
            h4("Gráfico del valor FOB unitario"),
            column(width = 6, plotlyOutput(outputId = "plot1")),
            
            h4("Evolución de la cantidad importada"),
            column(width = 6, plotlyOutput(outputId = "plot2"))
            ),
          fluidRow(
            h4("Distribución del precio"),
            column(width = 6, plotOutput(outputId = "plot3")),
            column(width = 6, plotOutput(outputId = "plot4"))
            )
          ),
        
        tabPanel(
          title = "Tablas del precio y cantidad",
          h4("Distribución de las cantidades importadas"),
          tableOutput(outputId = "tabla1"),
          
          h4("Análisis del precio de multiples tamaños, según rangos de precios"),
          tableOutput(outputId = "tabla2")
          ),
        
        tabPanel(
          title = "Procesamiento de archivo externo",
          tabsetPanel(
            tabPanel(
              title = "Tabla Size cantidad",
              dataTableOutput( outputId = "tabla")),
            
            tabPanel(
              title = "Análisis del precio",
              tableOutput(outputId = "tabla3"))
            )
          )
        )
      )
    )
  )
)
