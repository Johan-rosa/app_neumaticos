



shinyServer(function(input, output) {
   
  
  data <- reactive({
    
    if(input$tipo_vehiculo == "Todos"){
      
      data <- tbr_pcr1 %>%
        mutate(cuts = cut(valor_fob_unitario,
                          breaks = seq(to = max(valor_fob_unitario), by = input$rango)))
      
    } else {
      
      data <- tbr_pcr1 %>%
        filter(tipo_vehiculo %in% input$tipo_vehiculo ) %>%
        mutate(cuts = cut(valor_fob_unitario,
                          breaks = seq(to = max(valor_fob_unitario), by = input$rango)))
    }
    
  })
  
  
  output$size <- renderUI({
    
    selectInput(inputId = "size",
                label = "Tamaño",
                choices = sort(unique(data()$size_2)), 
                multiple = T,
                selectize = T, 
                selected = data() %>%
                  group_by(size_2) %>%
                  summarise(cantidad = sum(cantidad)) %>%
                  top_n(5) %>%
                  #filter(cantidad > quantile(cantidad, 0.99)) %>%
                  select(size_2) %>%
                  table() %>%
                  names())
  })
  
  
  
  datos_externos <- reactive({
    
    validate(
      need(input$file$datapath, warning("Favor cargar una base de datos para poder usar esta función")
      ))
      
    read.csv(input$file$datapath, stringsAsFactors = F, encoding = "latin1")
  }) 
  
  
  
  output$plot1 <- renderPlotly({
    
    data() %>%
      filter(size_2 %in% input$size) %>%
      group_by(fecha, size_2) %>%
      summarise(precio_meido = mean(valor_fob_unitario),
                cantidad = sum(cantidad)) %>%
      plot_ly(x = ~fecha, y = ~precio_meido, color = ~size_2, type = "scatter", mode = "line") %>%
      layout(legend = list(orientation = 'h'),
             xaxis = list(title = ""),
             yaxis = list(title = "Valor FOB unitario"))
    
    
  })
  
  output$plot2 <- renderPlotly({
    
    data() %>%
      filter(size_2 %in% input$size) %>%
      group_by(fecha, size_2) %>%
      summarise(precio_meido = mean(valor_fob_unitario),
                cantidad = sum(cantidad)) %>%
      plot_ly(x = ~fecha, y = ~cantidad, color = ~size_2, type = "scatter", mode = "line") %>%
      layout(legend = list(orientation = 'h'),
             xaxis = list(title = ""),
             yaxis = list(title = "Unidades"),
             title = "")
    
    
    
  })
  
  output$plot3 <- renderPlot({
    
    data() %>%
      filter(size_2 %in% input$size) %>%
      group_by(fecha, size_2) %>%
      summarise(precio_meido = mean(valor_fob_unitario),
                cantidad = sum(cantidad)) %>%
      ggplot(aes(y = size_2, x = precio_meido, fill = size_2)) +
      geom_density_ridges() +
      #theme_few() +
      theme(legend.position = "none") +
      labs(color = "",
           x = "",
           y = "Tamaños") 
    
  })  
  
  output$plot4 <- renderPlot({
    
  })
  
  output$tabla1 <- renderTable({ 
    
    data() %>%
      mutate(mes = month(fecha, label = TRUE),
             mes = paste0(mes, year(fecha)),
             mes = fct_inorder(mes)) %>%
      filter(size_2 %in% input$size) %>%
      group_by(mes, size_2) %>%
      summarise(cantidad = sum(cantidad)) %>%
      spread(size_2, cantidad) %>%
      rename(Mes = mes)
  },  striped = TRUE, digits = 0)
  
  output$tabla2 <- renderTable({
    
    data() %>%
      mutate(mes = month(fecha, label = TRUE),
             year = year(fecha)) %>%
      filter(size_2 %in% input$size, mes == input$mes, year == input$year) %>%
      mutate(cuts = cut(valor_fob_unitario,
                        breaks = c(seq(0, 300, by = input$rango), 400, 500, 2000))) %>%
      group_by(cuts, mes, size_2) %>%
      summarise(Cantidad = sum(cantidad)) %>%
      spread(size_2, Cantidad) %>%
      ungroup() %>%
      rename(Rangos = cuts) %>%
      select(-mes)
    
  }, striped = TRUE, digits = 0)
  
  
  
  
  tbr_pcr <- reactive({
    
    datos_externos() %>%
      set_names(tolower(names(datos_externos()))) %>%  
      filter(arancel_codigo %in% c( "4011.20.00", "4011.10.00")) %>% 
      mutate(producto_nombre =  toupper(producto_nombre)) %>%
      filter(!str_detect(producto_nombre, "MOTOC")) %>%
      mutate(pattern = ifelse(str_detect(producto_nombre, pattern0), pattern0,
                       ifelse(str_detect(producto_nombre, pattern1), pattern1,
                       ifelse(str_detect(producto_nombre, pattern2), pattern2,
                       ifelse(str_detect(producto_nombre, pattern3), pattern3,
                       ifelse(str_detect(producto_nombre, pattern4), pattern4,
                       ifelse(str_detect(producto_nombre, pattern5), pattern5,
                       ifelse(str_detect(producto_nombre, pattern6), pattern6,
                       ifelse(str_detect(producto_nombre, pattern7), pattern7,
                       ifelse(str_detect(producto_nombre, pattern8), pattern8,
                       ifelse(str_detect(producto_nombre, pattern9), pattern9, NA))))))))))) %>%
      
      mutate(size = str_extract(producto_nombre, pattern)) %>%
      mutate(size_2 = ifelse(pattern == pattern0, str_replace(size, pattern0, replace0),
                      ifelse(pattern == pattern1, str_replace(size, pattern1, replace1),
                      ifelse(pattern == pattern2, str_replace(size, pattern2, replace2),
                      ifelse(pattern == pattern3, str_replace(size, pattern3, replace3),
                      ifelse(pattern == pattern4, str_replace(size, pattern4, replace4),
                      ifelse(pattern == pattern5, str_replace(size, pattern5, replace5),
                      ifelse(pattern == pattern6, str_replace(size, pattern6, replace6),
                      ifelse(pattern == pattern7, str_replace(size, pattern7, replace7),
                      ifelse(pattern == pattern8, str_replace(size, pattern8, replace8),
                      ifelse(pattern == pattern9, str_replace(size, pattern9, replace9), NA))))))))))) %>%
      
      mutate(tipo_vehiculo = ifelse(pattern == pattern0  | pattern == pattern2, "Carro",
                                    ifelse(pattern == pattern1, "Camion",
                                    ifelse(size_2 == "325/95R24", "Camion",
                                    ifelse(pattern == pattern3 | pattern == pattern4 | pattern == pattern5 |
                                    pattern == pattern7 | pattern == pattern8 | pattern == pattern9, "Camion",
                                    ifelse(pattern == pattern6, "Carro", "NA"))))))   
    
  })
  
  otr <- reactive({
    
    datos_externos() %>%
    setNames(., tolower(names(.))) %>%
      filter(arancel_codigo %in% c( "4011.70.00", "4011.80.00", "4011.90.00"),
             !str_detect(producto_nombre, "MOTOC"),
             !str_detect(producto_nombre, "SOPORTE"),
             !str_detect(producto_nombre, "BICIC"),
             !str_detect(producto_nombre, "BIBIC"), 
             str_detect(producto_nombre, DIGIT)
      ) %>%
      mutate(producto_nombre = toupper(str_trim(str_squish(producto_nombre)))) %>%
      
      
      mutate(pattern = ifelse(str_detect(producto_nombre, pattern_otr1), pattern_otr1,
                       ifelse(str_detect(producto_nombre, pattern_otr2), pattern_otr2,
                       ifelse(str_detect(producto_nombre, pattern_otr3), pattern_otr3,
                       ifelse(str_detect(producto_nombre, pattern_otr4), pattern_otr4,
                       ifelse(str_detect(producto_nombre, pattern_otr5), pattern_otr5,
                       ifelse(str_detect(producto_nombre, pattern_otr6), pattern_otr6,
                       ifelse(str_detect(producto_nombre, pattern_otr7), pattern_otr7,
                       ifelse(str_detect(producto_nombre, pattern_otr8), pattern_otr8,
                       ifelse(str_detect(producto_nombre, pattern_otr9), pattern_otr9,
                       ifelse(str_detect(producto_nombre, pattern_otr10), pattern_otr10,       
                       ifelse(str_detect(producto_nombre, pattern_otr11), pattern_otr11,
                       ifelse(str_detect(producto_nombre, pattern_otr12), pattern_otr12,
                       ifelse(str_detect(producto_nombre, pattern_otr13), pattern_otr13,
                       ifelse(str_detect(producto_nombre, pattern_otr14), pattern_otr14,
                       ifelse(str_detect(producto_nombre, pattern_otr15), pattern_otr15,
                       ifelse(str_detect(producto_nombre, pattern_otr16), pattern_otr16,
                       ifelse(str_detect(producto_nombre, pattern_otr17), pattern_otr17,
                       ifelse(str_detect(producto_nombre, pattern_otr18), pattern_otr18,
                       ifelse(str_detect(producto_nombre, pattern_otr19), pattern_otr19,
                       ifelse(str_detect(producto_nombre, pattern_otr20), pattern_otr20,
                       ifelse(str_detect(producto_nombre, pattern_otr21), pattern_otr21,
                       ifelse(str_detect(producto_nombre, pattern_otr22), pattern_otr22,
                       ifelse(str_detect(producto_nombre, pattern_otr23), pattern_otr23,
                       ifelse(str_detect(producto_nombre, pattern_otr24), pattern_otr24,
                       NA))))))))))))))))))))))))) %>%
      mutate(size = str_extract(producto_nombre, pattern)) %>% 
      mutate(size_2 = ifelse(pattern == pattern_otr1, str_replace(size, pattern_otr1, replace_otr1),
                      ifelse(pattern == pattern_otr2, str_replace(size, pattern_otr2, replace_otr2),       
                      ifelse(pattern == pattern_otr3, str_replace(size, pattern_otr3, replace_otr3),
                      ifelse(pattern == pattern_otr4, str_replace(size, pattern_otr4, replace_otr4),
                      ifelse(pattern == pattern_otr5, str_replace(size, pattern_otr5, replace_otr5),
                      ifelse(pattern == pattern_otr6, str_replace(size, pattern_otr6, replace_otr6),
                      ifelse(pattern == pattern_otr7, str_replace(size, pattern_otr7, replace_otr7),
                      ifelse(pattern == pattern_otr8, str_replace(size, pattern_otr8, replace_otr8),
                      ifelse(pattern == pattern_otr9, str_replace(size, pattern_otr9, replace_otr9),
                      ifelse(pattern == pattern_otr10, str_replace(size, pattern_otr10, replace_otr10),
                      ifelse(pattern == pattern_otr11, str_replace(size, pattern_otr11, replace_otr11),
                      ifelse(pattern == pattern_otr12, str_replace(size, pattern_otr12, replace_otr12),
                      ifelse(pattern == pattern_otr13, str_replace(size, pattern_otr13, replace_otr13),
                      ifelse(pattern == pattern_otr14, str_replace(size, pattern_otr14, replace_otr14),
                      ifelse(pattern == pattern_otr15, str_replace(size, pattern_otr15, replace_otr15),
                      ifelse(pattern == pattern_otr16, str_replace(size, pattern_otr16, replace_otr16),
                      ifelse(pattern == pattern_otr17, str_replace(size, pattern_otr17, replace_otr17),
                      ifelse(pattern == pattern_otr18, str_replace(size, pattern_otr18, replace_otr18),
                      ifelse(pattern == pattern_otr19, str_replace(size, pattern_otr19, replace_otr19),
                      ifelse(pattern == pattern_otr20, str_replace(size, pattern_otr20, replace_otr20),
                      ifelse(pattern == pattern_otr21, str_replace(size, pattern_otr21, replace_otr21),
                      ifelse(pattern == pattern_otr22, str_replace(size, pattern_otr22, replace_otr22),
                      ifelse(pattern == pattern_otr23, str_replace(size, pattern_otr23, replace_otr23),
                      ifelse(pattern == pattern_otr24, str_replace(size, pattern_otr24, replace_otr24),
                      NA)))))))))))))))))))))))))
  })
  
  excluidas <- reactive({
    
    datos_externos() %>%
      setNames(., tolower(names(.))) %>%
      filter(!arancel_codigo %in% c( "4011.70.00", "4011.80.00", "4011.90.00"),
             str_detect(producto_nombre, "MOTOC"),
             str_detect(producto_nombre, "SOPORTE"),
             str_detect(producto_nombre, "BICIC"),
             str_detect(producto_nombre, "BIBIC"), 
             !str_detect(producto_nombre, DIGIT)) %>%
      filter(!arancel_codigo %in% c( "4011.20.00", "4011.10.00"))
      
    
  })
  
  
    
  output$tabla <- renderDataTable({
    
    if(input$tipo_vehiculo != "Todos"){
      
      tbr_pcr() %>%
        filter(tipo_vehiculo == input$tipo_vehiculo) %>%
        group_by(size_2) %>%
        summarise(Cantidad = sum(cantidad))
    } else {
      
      tbr_pcr() %>%
        group_by(size_2) %>%
        summarise(Cantidad = sum(cantidad))
    }
    
  })
  
  output$tabla3 <- renderTable({
    
    tbr_pcr() %>%
      filter(size_2 == input$size) %>%
      mutate(cuts = cut(valor_fob_unitario,
                        breaks = c(seq(0, 300, by = input$rango), 400, 500, 2000))) %>%
      group_by(cuts) %>%
      summarise(Cantidad = sum(cantidad), precio_promedio = mean(valor_fob_unitario, na.rm = T))
    
  })
  
  
  
  
  output$download_data.csv <- downloadHandler(
    
    filename = "tbr_pcr.csv",
    content = function(file){
      
      data <- tbr_pcr() %>%
        select(-pattern, -size)
      
      write.csv(data, file, row.names = F)
      
    }
    
  )
  

  output$download_otr.csv <- downloadHandler(
    
    filename = "otr.csv",
    content =  function(file) {
      data2 <- otr() %>%
        select(-pattern, -size)
      
      write.csv(data2, file, row.names = FALSE)
    }
    
   )  
  
  
})
