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

shinyUI(fluidPage(
  theme = shinytheme("lumen"),
  sidebarLayout(
    
    sidebarPanel( width = 3,
                  
                  selectInput(
                    inputId = "tipo_vehiculo",
                    label = "Tipo de vehículo",
                    choices =c("Todos", "Carro", "Camion"),
                    selected = "Camion",
                    multiple = F
                  ),
                  
                  uiOutput("size")
                  ,
                  
                  h4("Tabla de precio"),
                  wellPanel(
                    fluidRow(
                      column(
                    selectInput(
                      inputId = "mes",
                      label = "Mes",
                      choices = unique(month(tbr_pcr1$fecha, label = TRUE)),
                      selected = "Jan"
                      
                    ), width = 6), column(
                      selectInput(
                      inputId = "year",
                      label = "Año",
                      choices = c(2018, 2019),
                      selected = 2018), width = 6)),
                    
                    numericInput(inputId = "rango", label = "Amplio de intervalo", value = 50)
                  ),
                  
                  h4("Procesamiento de archivo externo"),
                  wellPanel(
                    fileInput(inputId = "file", label = "Base original DGA"),
                    downloadButton("download_data.csv", label = "TBR-PCR"),
                    downloadButton("download_otr.csv", label = "OTR"),
                    downloadButton("download_exclude.csv", label = "obs excluidas")
                  )#,
                  
                  #selectInput(inputId = "size",
                  #            label = "Tamaño",
                  #            choices = sort(unique(tbr_pcr1$size_2)), 
                  #            multiple = T,
                  #            selectize = T, 
                  #            selected = tbr_pcr1 %>%
                  #              filter(tipo_vehiculo == "Camion") %>%
                  #              group_by(size_2) %>%
                  #              summarise(cantidad = sum(cantidad)) %>%
                  #              top_n(5) %>%
                  #              #filter(cantidad > quantile(cantidad, 0.99)) %>%
                  #              select(size_2) %>%
                  #              table() %>%
                  #              names())
                  
                  
    ),
    
    mainPanel( width = 9,
               tabsetPanel(
                 tabPanel(title = "Análisis gráfico",
                          fluidRow(h4("Gráfico del valor FOB unitario"),
                                   column(width = 6, plotlyOutput(outputId = "plot1")),
                                   h4("Evolución de la cantidad importada"),
                                   column(width = 6, plotlyOutput(outputId = "plot2"))),
                          fluidRow(h4("Distribución del precio"),
                                   column(width = 6, plotOutput(outputId = "plot3")),
                                   column(width = 6, plotOutput(outputId = "plot4")))),
                 
                 tabPanel(title = "Tablas del precio y cantidad",
                          h4("Distribución de las cantidades importadas"),
                          tableOutput(outputId = "tabla1"),
                          h4("Análisis del precio de multiples tamaños, según rangos de precios"),
                          tableOutput(outputId = "tabla2")),
                 
                 tabPanel(title = "Procesamiento de archivo externo",
                          tabsetPanel(
                            tabPanel(title = "Tabla Size cantidad", dataTableOutput( outputId = "tabla")),
                            tabPanel(title = "Análisis del precio", tableOutput(outputId = "tabla3"))
                          ))
                 
                 
               )
               
    )
    
    
  )
)
)
