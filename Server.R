#Librerias
library(readr)
library(xts)
library(forecast)
library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(tseries)
library(dygraphs)
library(TTR)
library(plotly)
library(DBI)
library(readxl)
library(shinythemes)
library(shinycssloaders)
library(openxlsx)
library(shinyWidgets)
library(DT)
library(shiny)
library(shinydashboard)
library(formattable)
library(stringr)
require(reshape)
library(stats)

#Server shiny app, codigo de todas los outputs
shinyServer(function(input, output,session) {
  #Importanción datos histórico
  reporte_diario_campaña <- read_excel("reporte_diario_campaña_limpio.xlsx")[ , -1]
  
  #Configuración y modificaciones a datos historica 
  data = arrange(bind_rows(reporte_diario_campaña), fecha)
  data$fecha<-as.Date(data$fecha)
  data = cbind(data, fecha_inicio = as.Date(cut(data$fecha, "week")))
  
  data$fecha = as.Date(data$fecha, format="%Y-%m-%d")
  data$mes_b = format(data$fecha,"%B")
  data$out_real_calls<-as.factor(data$out_real_calls)
  levels(data$out_real_calls) <- c("no", "si")
  data$out_real_aht<-as.factor(data$out_real_aht)
  levels(data$out_real_aht) <- c("no", "si")
  data$linea<- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", data$linea)
  data$negocio<- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", data$negocio)
  data$id <- paste(data$negocio, data$linea, sep = "-")
  
  # Importación datos validación
  validacion_diaria_calls <- read_excel("validacion_diaria_calls.xlsx")
  validacion_diaria_calls = dplyr::rename(validacion_diaria_calls, real = interpolado_real_calls)
  validacion_diaria_calls$target <- "Trafico"
  validacion_diaria_aht <- read_excel("validacion_diaria_aht.xlsx")
  validacion_diaria_aht = dplyr::rename(validacion_diaria_aht, real =interpolado_real_aht)
  validacion_diaria_aht$target <- "AHT"
  
  # Configuración y modificaciones a datos validación
  validacion_diaria <- rbind(validacion_diaria_calls, validacion_diaria_aht)
  validacion_diaria$negocio <- "campaña_1"
  validacion_diaria$target<-as.factor(validacion_diaria$target)
  validacion_diaria$pred_ts <- ifelse(validacion_diaria$target == "Trafico", round(validacion_diaria$pred_ts, 0),
                                      ifelse(validacion_diaria$target == "AHT", round(validacion_diaria$pred_ts, 2), validacion_diaria$pred_ts))
  
  validacion_diaria$real <- ifelse(validacion_diaria$target == "Trafico", round(validacion_diaria$real, 0),
                                   ifelse(validacion_diaria$target == "AHT", round(validacion_diaria$real, 2), validacion_diaria$real))
  validacion_diaria$pred_regr <- ifelse(validacion_diaria$target == "Trafico", round(validacion_diaria$pred_regr, 0),
                                        ifelse(validacion_diaria$target == "AHT", round(validacion_diaria$pred_regr, 2), validacion_diaria$pred_regr))
  validacion_diaria$pred_promedio <- ifelse(validacion_diaria$target == "Trafico", round(validacion_diaria$pred_promedio, 0),
                                            ifelse(validacion_diaria$target == "AHT", round(validacion_diaria$pred_promedio, 2), validacion_diaria$pred_promedio))
  validacion_diaria$pred_fb <- ifelse(validacion_diaria$target == "Trafico", round(validacion_diaria$pred_fb, 0),
                                      ifelse(validacion_diaria$target == "AHT", round(validacion_diaria$pred_fb, 2), validacion_diaria$pred_fb))
  validacion_diaria$pred_gru <- ifelse(validacion_diaria$target == "Trafico", round(validacion_diaria$pred_gru, 0),
                                                ifelse(validacion_diaria$target == "AHT", round(validacion_diaria$pred_gru, 2), validacion_diaria$pred_gru))
  
  #Importación datos predicción
  pred_diaria_calls <- read_excel("prediccion_calls.xlsx")[,-1]
  pred_diaria_calls$target <- "Trafico"
  pred_diaria_aht <- read_excel("prediccion_aht.xlsx")[,-1]
  pred_diaria_aht$target <- "AHT"
  pred_diaria_aht <- pred_diaria_aht %>% select(-interpolado_real_calls)
  
  # Configuración y modificaciones a datos prediccion
  pred_diaria <- rbind(pred_diaria_calls, pred_diaria_aht)
  pred_diaria$negocio <- "campaña_1"

  pred_diaria$combinado <- paste(pred_diaria$año, pred_diaria$mes, pred_diaria$dia, pred_diaria$negocio, pred_diaria$linea,pred_diaria$target, sep = "-")
  pred_diaria <- subset(pred_diaria, !duplicated(combinado, fromLast = TRUE))
  pred_diaria$fecha <- paste(pred_diaria$año, pred_diaria$mes, pred_diaria$dia, sep="-") %>% ymd() %>% as.Date()
  pred_diaria$combinado <- paste(pred_diaria$fecha, pred_diaria$target, pred_diaria$negocio, pred_diaria$linea, sep = "-")
  pred_diaria <- subset(pred_diaria, !duplicated(combinado, fromLast = TRUE))
  pred_diaria$prediccion <- ifelse(pred_diaria$target == "Trafico", round(pred_diaria$prediccion, 0),
                                   ifelse(pred_diaria$target == "AHT", round(pred_diaria$prediccion, 2), pred_diaria$prediccion))
  
  pred_diaria$id <- paste(pred_diaria$negocio, pred_diaria$linea, sep = " - ")
  pred_diaria$mes_abr<- format(pred_diaria$fecha, "%b", locale = "es_ES")
  pred_diaria <- subset(pred_diaria, select = -combinado)
  #####################################################
  
  #########################################################################
  
  
  #Creación funciones error wape y mae 
  wape <- function(actuals, preds, na.rm=TRUE) {
    error <- actuals - preds
    return((sum(abs(error), na.rm = na.rm) / sum(abs(actuals), na.rm = na.rm)) * 100)
  }
  mae <- function(actuals, preds, na.rm = FALSE) {
    error <- actuals - preds
    return(mean(abs(error), na.rm = na.rm))
  }
  
  
  week_of_month <- function(dt) {
    # Obtener el primer día del mes
    first_day <- as.Date(format(dt, "%Y-%m-01"))
    
    # Obtener el día del mes
    dom <- as.numeric(format(dt, "%d"))
    
    # Calcular el día ajustado
    adjusted_dom <- dom + as.numeric(format(first_day, "%u")) - 1
    
    # Calcular la semana del mes
    week <- ceiling(adjusted_dom / 7)
    
    return(week)
  }
  
  
  #Elementos observe para sincronizar los filtros de las 3 páginas
  observeEvent(
    input$linea3,
    updatePickerInput(session, "linea", selected = input$linea3)
  )
  observeEvent(
    input$camp3,
    updatePickerInput(session, "camp", selected = input$camp3)
  )
  observeEvent(
    input$tag3,
    updatePickerInput(session, "tag", selected = input$tag3)
  )
  
  #Titulo grafica serie de tiempo con outliers
  output$title_ts_plot <- renderText({
    paste("Valores atípicos", "-", input$camp, input$linea, sep = " ")
  })
  
  #Titulo grafica serie de tiempo interpolada vs original
  output$title_ts_plot2 <- renderText({
    paste("Serie de tiempo original VS serie de tiempo interpolada", "-", input$camp, input$linea, sep = " ")
  })
  
  #Nombre ylab para todas las gráficas de la pagina 1
  tipo_dato <- reactive({
    if (input$tag=="Trafico"){
        as.character('Tráfico')
      }else if (input$tag=="AHT")
      {
        as.character('AHT')
      }
    
  })
  
  # CONFIGURAICÓN DATOS
  #Dataframe filtrado por camapaña y línea
  rv_predata <- reactive({
    data %>%
      subset(negocio == input$camp) %>%
      subset(linea == input$linea) 
    
  })
  
  
  #dataframe anterior filtrado por rango de fecha
  rv_data <- reactive({
    if (input$amount == 'Todos'){
      rv_predata()
      
    } else if (input$amount == 'Rango'){
      rv_predata() %>%
        subset(fecha >= input$range[1] & fecha <= input$range[2])
    }
  })
  
  #Serie de tiempo por periodicidad con rango 
  ts_df_rv <- reactive({
    if (input$per == "diaria") {
      rv_data() %>%arrange(fecha)%>%
        select(fecha,real_calls,interpolado_real_calls, real_aht,interpolado_real_aht)
    } 
    
    else if (input$per == "mensual"){
      if(input$tag =="Trafico"){
        rv_data() %>%
          group_by(año, mes_b) %>%
          summarise(real_calls = sum(real_calls),interpolado_real_calls = sum(interpolado_real_calls), 
                    fecha = first(fecha)) %>%
          arrange(fecha)%>%
          ungroup()%>%
          select(fecha,real_calls,interpolado_real_calls)
      }else if(input$tag =="AHT"){
        rv_data() %>%
          group_by(año, mes_b) %>%
          summarise( fecha = first(fecha),
                     call_aht_real=sum(real_calls*real_aht),
                     call_aht_off=sum(interpolado_real_calls*interpolado_real_aht),
                     interacciones_real=sum(real_calls),
                     interacciones_off=sum(interpolado_real_calls)
                     #real_aht = mean(real_aht),
                     #interpolado_real_aht = mean(interpolado_real_aht)
          ) %>%
          mutate(real_aht=(call_aht_real/interacciones_real),
                 interpolado_real_aht=(call_aht_off/interacciones_off)) %>%
          arrange(fecha)%>%
          ungroup()%>%
          select(fecha, real_aht,interpolado_real_aht)
      }
    }else if (input$per == "semanal"){
      if(input$tag =="Trafico"){
        rv_data() %>% 
          group_by(fecha_inicio) %>%
          summarise(real_calls = sum(real_calls),interpolado_real_calls = sum(interpolado_real_calls),) %>%
          mutate(año = year(fecha_inicio), mes_b = month(fecha_inicio)) %>%
          arrange(fecha_inicio)%>%
          ungroup() %>%
          select(fecha_inicio,real_calls,interpolado_real_calls)
      }else if (input$tag =="AHT"){
        rv_data() %>% 
          group_by(fecha_inicio) %>%
          summarise(
            #real_aht = mean(real_aht),interpolado_real_aht = mean(interpolado_real_aht)
            call_aht_real=sum(real_calls*real_aht),
            call_aht_off=sum(interpolado_real_calls*interpolado_real_aht),
            interacciones_real=sum(real_calls),
            interacciones_off=sum(interpolado_real_calls)) %>%
          mutate(año = year(fecha_inicio), mes_b = month(fecha_inicio),
                 real_aht=(call_aht_real/interacciones_real),
                 interpolado_real_aht=(call_aht_off/interacciones_off)) %>%
          arrange(fecha_inicio)%>%
          ungroup() %>%
          select(fecha_inicio,real_aht,interpolado_real_aht)
      }
    }
  })
  
  #Serie de tiempo por periodicidad sin rango
  ts_df_rv2 <- reactive({
    if (input$per == "diaria") {
      rv_predata() %>%arrange(fecha)%>%
        select(fecha,real_calls,interpolado_real_calls, real_aht,interpolado_real_aht)
    } 
    
    else if (input$per == "mensual"){
      if(input$tag =="Trafico"){
        rv_predata() %>%
          group_by(año, mes_b) %>%
          summarise(real_calls = sum(real_calls),interpolado_real_calls = sum(interpolado_real_calls), 
                    fecha = first(fecha)) %>%
          arrange(fecha)%>%
          ungroup()%>%
          select(fecha,real_calls,interpolado_real_calls)
      }else if(input$tag =="AHT"){
        rv_predata() %>%
          group_by(año, mes_b) %>%
          summarise( fecha = first(fecha),
                     call_aht_real=sum(real_calls*real_aht),
                     call_aht_off=sum(interpolado_real_calls*interpolado_real_aht),
                     interacciones_real=sum(real_calls),
                     interacciones_off=sum(interpolado_real_calls)
                     #real_aht = mean(real_aht),
                     #interpolado_real_aht = mean(interpolado_real_aht)
          ) %>%
          mutate(real_aht=(call_aht_real/interacciones_real),
                 interpolado_real_aht=(call_aht_off/interacciones_off)) %>%
          arrange(fecha)%>%
          ungroup()%>%
          select(fecha, real_aht,interpolado_real_aht)
      }
    }else if (input$per == "semanal"){
      if(input$tag =="Trafico"){
        rv_predata() %>% 
          group_by(fecha_inicio) %>%
          summarise(real_calls = sum(real_calls),interpolado_real_calls = sum(interpolado_real_calls),) %>%
          mutate(año = year(fecha_inicio), mes_b = month(fecha_inicio)) %>%
          arrange(fecha_inicio)%>%
          ungroup() %>%
          select(fecha_inicio,real_calls,interpolado_real_calls)
      }else if (input$tag =="AHT"){
        rv_predata() %>% 
          group_by(fecha_inicio) %>%
          summarise(
            #real_aht = mean(real_aht),interpolado_real_aht = mean(interpolado_real_aht)
            call_aht_real=sum(real_calls*real_aht),
            call_aht_off=sum(interpolado_real_calls*interpolado_real_aht),
            interacciones_real=sum(real_calls),
            interacciones_off=sum(interpolado_real_calls)) %>%
          mutate(año = year(fecha_inicio), mes_b = month(fecha_inicio),
                 real_aht=(call_aht_real/interacciones_real),
                 interpolado_real_aht=(call_aht_off/interacciones_off)) %>%
          arrange(fecha_inicio)%>%
          ungroup() %>%
          select(fecha_inicio,real_aht,interpolado_real_aht)
      }
    } 
    
  })
  
  #DESCRIPCIÓN
  # Gráfica con outliers
  output$outliers_plot <- renderPlotly({
    if (input$per != "intervalos") {
        Sys.setlocale("LC_TIME", "es_ES.utf8")
        df=rv_data()
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        if (input$tag == "Trafico") {
          plot_ly(df, x = ~fecha, y = ~real_calls, color = ~out_real_calls,
                  colors = c('#5D0664', '#05ABAB'),
                  type = "scatter", mode = "markers") %>%
            layout(xaxis = list(title = "fecha",dtick = 300),
                   yaxis = list(title = tipo_dato()),
                   showlegend = TRUE) 
        } else if (input$tag == "AHT") {
          plot_ly(df, x = ~fecha, y = ~real_aht, color = ~out_real_aht,
                  colors = c('#5D0664', '#05ABAB'),
                  type = "scatter", mode = "markers") %>%
            layout(xaxis = list(title = "fecha",dtick = 300),
                   yaxis = list(title = tipo_dato()),
                   showlegend = FALSE) 
        }
        
    }
    
  })
  
  # Gráfica de la serie de tiempo interpolada vs original
  output$serie_tiempo_plot <- renderPlotly({
    if(input$tag == "Trafico"){
      df <- ts_df_rv() %>%
        pivot_longer(cols = c(real_calls,interpolado_real_calls), names_to = "Serie", values_to = "Valor")
      df$Serie<-as.factor(df$Serie)
      df$Serie <- factor(df$Serie, levels = c("real_calls","interpolado_real_calls"))
      levels(df$Serie) <- c("Real","Interpolada")
      if (input$per == "diaria"){
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        plot_ly(df, x = ~fecha, y = ~Valor, color = ~Serie, type = "scatter",
                mode = "lines",colors = c('#5D0664', '#05ABAB')) %>%
          layout(xaxis = list(title = "fecha",dtick = 290),
                 yaxis = list(title = tipo_dato()),
                 showlegend = TRUE) 
      }else if (input$per == "mensual"){
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        plot_ly(df, x = ~fecha, y = ~Valor, color = ~Serie, type = "scatter",
                mode = "lines",colors = c('#5D0664', '#05ABAB')) %>%
          layout(xaxis = list(title = "fecha",dtick = 10),
                 yaxis = list(title = tipo_dato()),
                 showlegend = TRUE) 
      }else if (input$per == "semanal"){
        df$fecha_inicio <-format(df$fecha_inicio, "%d %b %Y")
        df$fecha_inicio <- factor(df$fecha_inicio, levels = unique(df$fecha_inicio))
        plot_ly(df, x = ~fecha_inicio, y = ~Valor, color = ~Serie, type = "scatter",
                mode = "lines",colors = c('#5D0664', '#05ABAB')) %>%
          layout(xaxis = list(title = "fecha",dtick = 25),
                 yaxis = list(title = tipo_dato()),
                 showlegend = TRUE) 
      }}else if(input$tag == "AHT"){
        df <- ts_df_rv() %>%
          pivot_longer(cols = c(real_aht,interpolado_real_aht), names_to = "Serie", values_to = "Valor")
        df$Serie<-as.factor(df$Serie)
        df$Serie <- factor(df$Serie, levels = c("real_aht","interpolado_real_aht"))
        levels(df$Serie) <- c("Real","Interpolado")
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        df$fecha_inicio <-format(df$fecha_inicio, "%d %b %Y")
        df$fecha_inicio <- factor(df$fecha_inicio, levels = unique(df$fecha_inicio))
        if (input$per == "diaria"){
          plot_ly(df, x = ~fecha, y = ~Valor, color = ~Serie, type = "scatter",
                  mode = "lines",colors = c('#5D0664', '#05ABAB')) %>%
            layout(xaxis = list(title = "fecha",dtick = 290),
                   yaxis = list(title = tipo_dato()),
                   showlegend = TRUE) 
        }else if (input$per == "mensual"){
          plot_ly(df, x = ~fecha, y = ~Valor, color = ~Serie, type = "scatter",
                  mode = "lines",colors = c('#5D0664', '#05ABAB')) %>%
            layout(xaxis = list(title = "fecha",dtick = 10),
                   yaxis = list(title = tipo_dato()),
                   showlegend = TRUE) 
        }else if (input$per == "semanal"){
          plot_ly(df, x = ~fecha_inicio, y = ~Valor, color = ~Serie, type = "scatter",
                  mode = "lines",colors = c('#5D0664', '#05ABAB')) %>%
            layout(xaxis = list(title = "fecha",dtick = 25),
                   yaxis = list(title = tipo_dato()),
                   showlegend = TRUE) 
        }
      }
  })
  
  #Gráfica de descomposición
  output$descompose_plot <- renderPlotly({
  if((input$linea== "linea_4" || input$linea== "linea_3" )&& input$per == "mensual"&& input$tag == "AHT"){
      showNotification("¡Advertencia! La línea no cuenta con la cantidad de datos necesarios para realizar la descomposición de los datos.",closeButton=FALSE,
                       type = "error")
    }else if(input$linea== "linea_4" && input$per == "semanal"){
        showNotification("¡Advertencia! La línea no cuenta con la cantidad de datos necesarios para realizar la descomposición de los datos.",closeButton=FALSE,
                         type = "error")
    }else{
      if(input$tag == "Trafico"){
        if (input$per == "diaria"){
          df <- ts_df_rv2() %>%
            select(fecha,interpolado_real_calls)
          df = na.omit(df)
          fechamin<-df$fecha %>%
            min() %>%
            as.Date()
          
          
          año<-fechamin%>%
            format('%Y') %>%
            as.numeric()
          
          dia<-fechamin%>%
            yday()
          
          
          mes<-fechamin%>%
            format('%m') %>%
            as.numeric()
          
          df_ts<-ts(df$interpolado_real_calls, start=c(año,mes),frequency = 30)
          df_des <- stats::decompose(df_ts, type = "multiplicative")
          
          
          real <-df_des$x
          trend <-df_des$trend
          seasonal <-df_des$seasonal
          random <-df_des$random
          
          
          df$fecha <- format(df$fecha, "%d %b %Y")
          fecha <- factor(df$fecha, levels = unique(df$fecha))
          datos_des<-data.frame(fecha,real,trend,seasonal,random)
          
          
          plot1 <- plot_ly(datos_des, x = ~fecha, y = ~real, name = "Serie origianal") %>%
            add_lines(line = list(color = '#5D0664'))%>% 
            layout(yaxis = list(title = tipo_dato()),xaxis = list(dtick = 290))
          
          plot2 <- plot_ly(datos_des, x = ~fecha, y = ~trend, name = 'Tendencia') %>%
            add_lines(line = list(color = '#05ABAB'))%>% 
            layout(yaxis = list(title = 'Tendencia'),xaxis = list(dtick = 290))
          
          plot3 <- plot_ly(datos_des, x = ~fecha, y = ~seasonal, name = 'Estacionalidad') %>%
            add_lines(line = list(color = '#5D0664'))%>% 
            layout(yaxis = list(title = 'Estacionalidad'),xaxis = list(dtick = 290))
          
          plot4 <- plot_ly(datos_des, x = ~fecha, y = ~random, name = 'Random') %>%
            add_lines(line = list(color = '#05ABAB'))%>% 
            layout(yaxis = list(title = 'Random'),xaxis = list(dtick = 290))
          
          
          fig <- subplot(plot1, plot2, plot3,plot4, nrows = 4,titleY = TRUE)
          
          
          fig
          
          
        }else if (input$per == "mensual"){
          df <- ts_df_rv2() %>%
            select(fecha,interpolado_real_calls)
          df = na.omit(df)
          fechamin<-df$fecha %>%
            min() %>%
            as.Date()
          
          
          año<-fechamin%>%
            format('%Y') %>%
            as.numeric()
          
          dia<-fechamin%>%
            yday()
          
          
          mes<-fechamin%>%
            format('%m') %>%
            as.numeric()
          
          df_ts<-ts(df$interpolado_real_calls, start=c(año,mes),frequency = 12)
          df_des <- stats::decompose(df_ts, type = "multiplicative")
          
          
          real <-df_des$x
          trend <-df_des$trend
          seasonal <-df_des$seasonal
          random <-df_des$random
          
          df$fecha <- format(df$fecha, "%d %b %Y")
          fecha <- factor(df$fecha, levels = unique(df$fecha))
          
          datos_des<-data.frame(fecha,real,trend,seasonal,random)
          
          plot1 <- plot_ly(datos_des, x = ~fecha, y = ~real, name = "Serie origianal") %>%
            add_lines(line = list(color = '#5D0664'))%>% 
            layout(yaxis = list(title = tipo_dato()),xaxis = list(dtick = 10))
          
          plot2 <- plot_ly(datos_des, x = ~fecha, y = ~trend, name = 'Tendencia') %>%
            add_lines(line = list(color = '#05ABAB'))%>% 
            layout(yaxis = list(title = 'Tendencia'),xaxis = list(dtick = 10))
          
          plot3 <- plot_ly(datos_des, x = ~fecha, y = ~seasonal, name = 'Estacionalidad') %>%
            add_lines(line = list(color = '#5D0664'))%>% 
            layout(yaxis = list(title = 'Estacionalidad'),xaxis = list(dtick = 10))
          
          plot4 <- plot_ly(datos_des, x = ~fecha, y = ~random, name = 'Random') %>%
            add_lines(line = list(color = '#05ABAB'))%>% 
            layout(yaxis = list(title = 'Random'),xaxis = list(dtick = 10))
          
          fig <- subplot(plot1, plot2, plot3,plot4, nrows = 4,titleY = TRUE)
          
          
          fig
        }else if (input$per == "semanal"){
          df <- ts_df_rv2()   %>%
            select(fecha_inicio,interpolado_real_calls)
          df = na.omit(df)
          fechamin<-df$fecha_inicio %>%
            min() %>%
            as.Date()
          
          
          año<-fechamin%>%
            format('%Y') %>%
            as.numeric()
          
          dia<-fechamin%>%
            yday()
          
          
          mes<-fechamin%>%
            format('%m') %>%
            as.numeric()
          
          df_ts<-ts(df$interpolado_real_calls, start=c(año,mes),frequency = 30)
          df_des <- stats::decompose(df_ts, type = "multiplicative")
          
          
          real <-df_des$x
          trend <-df_des$trend
          seasonal <-df_des$seasonal
          random <-df_des$random
          df$fecha_inicio <- format(df$fecha_inicio, "%d %b %Y")
          fecha <- factor(df$fecha_inicio, levels = unique(df$fecha_inicio))
          
          datos_des<-data.frame(fecha,real,trend,seasonal,random)
          
          plot1 <- plot_ly(datos_des, x = ~fecha, y = ~real, name = "Serie origianal") %>%
            add_lines(line = list(color = '#5D0664'))%>% 
            layout(yaxis = list(title = tipo_dato()),xaxis = list(dtick = 25))
          
          plot2 <- plot_ly(datos_des, x = ~fecha, y = ~trend, name = 'Tendencia') %>%
            add_lines(line = list(color = '#05ABAB'))%>% 
            layout(yaxis = list(title = 'Tendencia'),xaxis = list(dtick = 25))
          
          plot3 <- plot_ly(datos_des, x = ~fecha, y = ~seasonal, name = 'Estacionalidad') %>%
            add_lines(line = list(color = '#5D0664'))%>% 
            layout(yaxis = list(title = 'Estacionalidad'),xaxis = list(dtick = 25))
          
          plot4 <- plot_ly(datos_des, x = ~fecha, y = ~random, name = 'Random') %>%
            add_lines(line = list(color = '#05ABAB'))%>% 
            layout(yaxis = list(title = 'Random'),xaxis = list(dtick = 25))
          
          fig <- subplot(plot1, plot2, plot3,plot4, nrows = 4,titleY = TRUE)
          
          
          fig
        }}else if(input$tag == "AHT"){
          if (input$per == "diaria"){
            df <- ts_df_rv2() %>%
              select(fecha,interpolado_real_aht)
            df = na.omit(df)
            fechamin<-df$fecha %>%
              min() %>%
              as.Date()
            
            
            año<-fechamin%>%
              format('%Y') %>%
              as.numeric()
            
            dia<-fechamin%>%
              yday()
            
            
            mes<-fechamin%>%
              format('%m') %>%
              as.numeric()
            
            df_ts<-ts(df$interpolado_real_aht, start=c(año,mes),frequency = 30)
            df_des <- stats::decompose(df_ts, type = "multiplicative")
            
            
            real <-df_des$x
            trend <-df_des$trend
            seasonal <-df_des$seasonal
            random <-df_des$random
            
            df$fecha <- format(df$fecha, "%d %b %Y")
            fecha <- factor(df$fecha, levels = unique(df$fecha))
            
            datos_des<-data.frame(fecha,real,trend,seasonal,random)
            
            
            plot1 <- plot_ly(datos_des, x = ~fecha, y = ~real, name = "Serie origianal") %>%
              add_lines(line = list(color = '#5D0664'))%>% 
              layout(yaxis = list(title = tipo_dato()),xaxis = list(dtick = 290))
            
            plot2 <- plot_ly(datos_des, x = ~fecha, y = ~trend, name = 'Tendencia') %>%
              add_lines(line = list(color = '#05ABAB'))%>% 
              layout(yaxis = list(title = 'Tendencia'),xaxis = list(dtick = 290))
            
            plot3 <- plot_ly(datos_des, x = ~fecha, y = ~seasonal, name = 'Estacionalidad') %>%
              add_lines(line = list(color = '#5D0664'))%>% 
              layout(yaxis = list(title = 'Estacionalidad'),xaxis = list(dtick = 290))
            
            plot4 <- plot_ly(datos_des, x = ~fecha, y = ~random, name = 'Random') %>%
              add_lines(line = list(color = '#05ABAB'))%>% 
              layout(yaxis = list(title = 'Random'),xaxis = list(dtick = 290))
            
            
            fig <- subplot(plot1, plot2, plot3,plot4, nrows = 4,titleY = TRUE)
            
            
            fig
            
            
          }else if (input$per == "mensual"){
            df <- ts_df_rv2()  %>%
              select(fecha,interpolado_real_aht)
            df = na.omit(df)
            fechamin<-df$fecha %>%
              min() %>%
              as.Date()
            
            
            año<-fechamin%>%
              format('%Y') %>%
              as.numeric()
            
            dia<-fechamin%>%
              yday()
            
            
            mes<-fechamin%>%
              format('%m') %>%
              as.numeric()
            
            df_ts<-ts(df$interpolado_real_aht, start=c(año,mes),frequency = 12)
            
              df_des <- stats::decompose(df_ts, type = "multiplicative")
            
            
            real <-df_des$x
            trend <-df_des$trend
            seasonal <-df_des$seasonal
            random <-df_des$random
            
            df$fecha <- format(df$fecha, "%d %b %Y")
            fecha <- factor(df$fecha, levels = unique(df$fecha))
            
            datos_des<-data.frame(fecha,real,trend,seasonal,random)
            
            plot1 <- plot_ly(datos_des, x = ~fecha, y = ~real, name = "Serie origianal") %>%
              add_lines(line = list(color = '#5D0664'))%>% 
              layout(yaxis = list(title = tipo_dato()),xaxis = list(dtick = 10))
            
            plot2 <- plot_ly(datos_des, x = ~fecha, y = ~trend, name = 'Tendencia') %>%
              add_lines(line = list(color = '#05ABAB'))%>% 
              layout(yaxis = list(title = 'Tendencia'),xaxis = list(dtick = 10))
            
            plot3 <- plot_ly(datos_des, x = ~fecha, y = ~seasonal, name = 'Estacionalidad') %>%
              add_lines(line = list(color = '#5D0664'))%>% 
              layout(yaxis = list(title = 'Estacionalidad'),xaxis = list(dtick = 10))
            
            plot4 <- plot_ly(datos_des, x = ~fecha, y = ~random, name = 'Randomn') %>%
              add_lines(line = list(color = '#05ABAB'))%>% 
              layout(yaxis = list(title = 'Random'),xaxis = list(dtick = 10))
            
            fig <- subplot(plot1, plot2, plot3,plot4, nrows = 4,titleY = TRUE)
            
            
            fig
          }else if (input$per == "semanal"){
            df <- ts_df_rv2()   %>%
              select(fecha_inicio,interpolado_real_aht)
            df = na.omit(df)
            fechamin<-df$fecha_inicio %>%
              min() %>%
              as.Date()
            
            
            año<-fechamin%>%
              format('%Y') %>%
              as.numeric()
            
            dia<-fechamin%>%
              yday()
            
            
            mes<-fechamin%>%
              format('%m') %>%
              as.numeric()
            
            df_ts<-ts(df$interpolado_real_aht, start=c(año,mes),frequency = 30)
            
            df_des <- stats::decompose(df_ts, type = "multiplicative")
            
            
            real <-df_des$x
            trend <-df_des$trend
            seasonal <-df_des$seasonal
            random <-df_des$random
            
            df$fecha_inicio <- format(df$fecha_inicio, "%d %b %Y")
            fecha <- factor(df$fecha_inicio, levels = unique(df$fecha_inicio))
            
            datos_des<-data.frame(fecha,real,trend,seasonal,random)
            
            plot1 <- plot_ly(datos_des, x = ~fecha, y = ~real, name = "Serie origianal") %>%
              add_lines(line = list(color = '#5D0664'))%>% 
              layout(yaxis = list(title = tipo_dato()),xaxis = list(dtick = 25))
            
            plot2 <- plot_ly(datos_des, x = ~fecha, y = ~trend, name = 'Tendencia') %>%
              add_lines(line = list(color = '#05ABAB'))%>% 
              layout(yaxis = list(title = 'Tendencia'),xaxis = list(dtick = 25))
            
            plot3 <- plot_ly(datos_des, x = ~fecha, y = ~seasonal, name = 'Estacionalidad') %>%
              add_lines(line = list(color = '#5D0664'))%>% 
              layout(yaxis = list(title = 'Estacionalidad'),xaxis = list(dtick = 25))
            
            plot4 <- plot_ly(datos_des, x = ~fecha, y = ~random, name = 'Random') %>%
              add_lines(line = list(color = '#05ABAB'))%>% 
              layout(yaxis = list(title = 'Random'),xaxis = list(dtick = 25))
            
            fig <- subplot(plot1, plot2, plot3,plot4, nrows = 4,titleY = TRUE)
            
            
            fig
          }
          
        }
    }
  })
  
  
  #Titulo grafico de descomposición
  output$title_decompose <- renderText({
    paste(input$camp, input$linea, sep = " ")
  })
  
  # Gráficas de analisis estacional mensual
  output$boxmes_plot <- renderPlotly({
    monthorder<-c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                  "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    if(input$tag== "Trafico"){
      if (input$ts_season == "Por año") {
        df <- rv_data() %>%
          group_by(año, mes_b) %>%
          summarise(interpolado_real_calls = sum(interpolado_real_calls))
        df$año<-as.factor(df$año)
        df<-df[order(match(df$mes_b,monthorder)),]
        plot1 <- plot_ly(df, x = ~mes_b, y = ~interpolado_real_calls, color = ~factor(año), 
                         colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB'), 
                         type = 'scatter', mode = 'lines') %>%
         layout(
            xaxis = list(title = "fecha",categoryorder = "array",
                         categoryarray =monthorder),
            yaxis = list(title = tipo_dato()))
        
        plot1%>% layout(legend=list(title=list(text="año")))
        
      } else if (input$ts_season == "Por mes") {
        df2 <- rv_data() %>%
          group_by(mes_b, año) %>%
          summarise(interpolado_real_calls = sum(interpolado_real_calls))
        df2$año <- factor(df2$año)
        df2$mes_b<-factor(df2$mes_b,levels = monthorder)
        plot2 <- plot_ly(df2, x = ~año, y = ~interpolado_real_calls, color = ~mes_b, colors = c('#142066','#5D0664','#05ABAB'), type = 'bar') %>%
          layout(
            xaxis = list(title = "fecha"),
            yaxis = list(title = tipo_dato()),
            barmode = 'group')
        plot2 %>% layout(legend=list(title=list(text="mes_b")))
        
      } else if (input$ts_season == "Box-plot Meses") {
        df3 <- rv_data()
        df3$mes_b<-factor(df3$mes_b,levels = monthorder)
        plot3<-plot_ly(df3, x = ~mes_b, y = ~interpolado_real_calls, type = "box", jitter = 0.3, color = ~mes_b,marker = list(color = '#000000'),
                       colors = c('#142066','#5D0664','#05ABAB')) %>%
          layout(
            xaxis = list(title = "mes_b"),
            yaxis = list(title = tipo_dato())
          )
        plot3%>% layout(legend=list(title=list(text="mes_b")))
      }}else if(input$tag == "AHT"){
        if (input$ts_season == "Por año") {
          df <- rv_data() %>%
            group_by(año, mes_b)  %>%
            summarise(
              #interpolado_real_aht = mean(interpolado_real_aht)
              call_aht_off=sum(interpolado_real_calls*interpolado_real_aht),
              interacciones_off=sum(interpolado_real_calls)) %>%
            mutate(interpolado_real_aht=(call_aht_off/interacciones_off))
          df$año<-as.factor(df$año)
          df<-df[order(match(df$mes_b,monthorder)),]
          plot1 <- plot_ly(df, x = ~mes_b, y = ~interpolado_real_aht, color = ~factor(año), colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB'), type = 'scatter', mode = 'lines') %>%
            layout(
              xaxis = list(title = "fecha",categoryorder = "array",
                           categoryarray =monthorder),
              yaxis = list(title = tipo_dato()))
          
          plot1%>% layout(legend=list(title=list(text="año")))
          
        } else if (input$ts_season == "Por mes") {
          
          df2 <- rv_data() %>%
            group_by(mes_b, año)  %>%
            summarise(
              #interpolado_real_aht = mean(interpolado_real_aht)
              call_aht_off=sum(interpolado_real_calls*interpolado_real_aht),
              interacciones_off=sum(interpolado_real_calls)) %>%
            mutate(interpolado_real_aht=(call_aht_off/interacciones_off))
          df2$mes_b<-factor(df2$mes_b,levels = monthorder)
          plot2 <- plot_ly(df2, x = ~año, y = ~interpolado_real_aht, color = ~mes_b, colors = c('#142066','#5D0664','#05ABAB'), type = 'bar') %>%
            layout(
              xaxis = list(title = "fecha"),
              yaxis = list(title = tipo_dato()),
              barmode = 'group')
          plot2%>% layout(legend=list(title=list(text="mes_b")))
        } else if (input$ts_season == "Box-plot Meses") {
          df3 <- rv_data()
          df3$mes_b<-factor(df3$mes_b,levels = monthorder)
          plot3<-plot_ly(df3, x = ~mes_b, y = ~interpolado_real_aht, type = "box", jitter = 0.3, color = ~mes_b,marker = list(color = '#000000'),
                         colors = c('#142066','#5D0664','#05ABAB')) %>%
            layout(
              xaxis = list(title = "mes_b"),
              yaxis = list(title = tipo_dato()))
          plot3%>% layout(legend=list(title=list(text="mes_b")))
          
        }}
  })
  
  # Graficas analisis estacional por día de la semana
  output$boxweek_plot <- renderPlotly({
    day_labels <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo", "Festivo")
    monthorder<-c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                  "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    
    if(input$tag == "Trafico"){
      if (input$ts_season_dia == "Por año") {
        df <- rv_data() %>%
          group_by(dia_semana, año) %>%
          summarise(interpolado_real_calls = sum(interpolado_real_calls))
        
        df$dia_semana <- factor(df$dia_semana, labels = day_labels)
        
        plot1 <- plot_ly(df, x = ~dia_semana, y = ~interpolado_real_calls, color = ~factor(año), 
                         colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB'), 
                         type = 'scatter', mode = "lines+markers") %>%
          layout(
            xaxis = list(title = "Día de la semana", categoryorder = "array",
                         categoryarray = day_labels),
            yaxis = list(title = tipo_dato())
          )
        
        
      } else if (input$ts_season_dia == "Box-plot día semana") {
        df2 <- rv_data()
        df2$dia_semana <- factor(df2$dia_semana,labels=day_labels )
        plot2 <- plot_ly(df2, x = ~dia_semana, y = ~interpolado_real_calls, type = "box", jitter = 0.3, color = ~dia_semana, 
                         marker = list(color = '#000000'),
                         colors = c('#142066','#5D0664','#05ABAB')) %>%
          layout(
            xaxis = list(title = "Día de la semana"),
            yaxis = list(title = tipo_dato())
          )
        plot2 %>% layout(legend = list(title = list(text = "Día de la semana")))
      } else if (input$ts_season_dia == "Por mes") {
        df <-rv_data() %>%
          group_by(mes_b, dia_semana) %>%
          summarise(interpolado_real_calls = sum(interpolado_real_calls))
        
        df$mes_b<-factor(df$mes_b,levels = monthorder)
        df$dia_semana <- factor(df$dia_semana, labels = day_labels)
        
        plot2 <- plot_ly(df, x = ~mes_b, y = ~interpolado_real_calls, color = ~dia_semana, colors = c('#142066','#5D0664','#05ABAB'), type = 'bar') %>%
          layout(
            xaxis = list(title = "mes"),
            yaxis = list(title = tipo_dato()),
            barmode = 'group')
      }
    } else if(input$tag == "AHT"){
      if (input$ts_season_dia == "Por año") {
        df <- rv_data() %>%
          group_by(dia_semana, año)  %>%
          summarise(
            call_aht_off = sum(interpolado_real_calls * interpolado_real_aht),
            interacciones_off = sum(interpolado_real_calls)) %>%
          mutate(interpolado_real_aht = (call_aht_off / interacciones_off))
        
        df$dia_semana <- factor(df$dia_semana, labels = day_labels)
        
        plot1 <- plot_ly(df, x = ~dia_semana, y = ~interpolado_real_aht, color = ~factor(año), 
                         colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB'), 
                         type = 'scatter', mode = "lines+markers") %>%
          layout(
            xaxis = list(title = "Día de la semana", categoryorder = "array",
                         categoryarray = day_labels),
            yaxis = list(title = tipo_dato())
          )
        
      } else if (input$ts_season_dia == "Por mes") {
        df <- rv_data() %>%
          group_by(dia_semana, mes_b)  %>%
          summarise(
            call_aht_off = sum(interpolado_real_calls * interpolado_real_aht),
            interacciones_off = sum(interpolado_real_calls)) %>%
          mutate(interpolado_real_aht = (call_aht_off / interacciones_off))
        
        df$mes_b<-factor(df$mes_b,levels = monthorder)
        df$dia_semana <- factor(df$dia_semana, labels = day_labels)
        
        plot2 <- plot_ly(df, x = ~mes_b, y = ~interpolado_real_aht, color = ~dia_semana, colors = c('#142066','#5D0664','#05ABAB'), type = 'bar') %>%
          layout(
            xaxis = list(title = "mes"),
            yaxis = list(title = tipo_dato()),
            barmode = 'group')
      }else if (input$ts_season_dia == "Box-plot día semana") {
        df <- rv_data() 
        df$dia_semana <- factor(df$dia_semana,labels=day_labels )
        plot2 <- plot_ly(df, x = ~dia_semana, y = ~interpolado_real_aht, type = "box", jitter = 0.3, color = ~dia_semana, 
                         marker = list(color = '#000000'),
                         colors = c('#142066','#5D0664','#05ABAB')) %>%
          layout(
            xaxis = list(title = "Día de la semana"),
            yaxis = list(title = tipo_dato())
          )
        plot2 %>% layout(legend = list(title = list(text = "Día de la semana")))
      }
    }
  })
  
  
  
  #Titulo analisis estacional
  output$title_esta <- renderText({
    paste("Análisis de estacionalidad mensual", "-", input$camp, input$linea, sep = " ")
  })
  
  output$title_esta_dia <- renderText({
    paste("Análisis de estacionalidad día de la semana", "-", input$camp, input$linea, sep = " ")
  })
  
  
  # VALIDACION
  #Elementos observe para sincronizar los filtros de las 3 páginas
  observeEvent(
    input$linea,
    updatePickerInput(session, "linea2", selected = input$linea)
  )
  observeEvent(
    input$camp,
    updatePickerInput(session, "camp2", selected = input$camp)
  )
  observeEvent(
    input$tag,
    updatePickerInput(session, "tag2", selected = input$tag)
  )
  
  #Nombre ylab para todas las gráficas de la pagina 2
  tipo_dato2 <- reactive({
    if (input$tag2=="Trafico"){
      as.character('Tráfico')
    }else if (input$tag2=="AHT")
    {
      as.character('AHT')
    }
    
  })
  
  # Creación validación mensual
  val_mes <-reactive({
    if (input$tag2=="Trafico"){
      validacion_diaria %>%
        subset(target == "Trafico")   %>% 
        mutate(año = format(fecha, "%Y"),mes = format(fecha, "%m")) %>% 
        group_by(año,mes,negocio,linea,target) %>% summarise(real = sum(real),
                                                             pred_ts=sum(pred_ts),
                                                             pred_regr=sum(pred_regr),
                                                             pred_fb=sum(pred_fb),
                                                             pred_promedio=sum(pred_promedio),
                                                             pred_gru = sum(pred_gru),
                                                             fecha = min(fecha),
                                                             target = first(target))%>%
        arrange(fecha)%>%
        ungroup()
    }else if (input$tag2=="AHT"){
      validacion_diaria %>%
        subset(target == "AHT")   %>% 
        mutate(año = format(fecha, "%Y"),mes = format(fecha, "%m")) %>% 
        group_by(año,mes,negocio,linea,target) %>% summarise(real = mean(real),
                                                             pred_ts=mean(pred_ts),
                                                             pred_regr=mean(pred_regr),
                                                             pred_fb=mean(pred_fb),
                                                             pred_promedio=mean(pred_promedio),
                                                             pred_gru=mean(pred_gru),
                                                             fecha = min(fecha),
                                                             target = first(target))%>%
        ungroup()
    }
  })
  
  #Creación validación semanal
  val_semanal <-reactive({
    if (input$tag2=="Trafico"){
      validacion_diaria %>%
        subset(target == "Trafico")   %>% 
        mutate(año = format(fecha, "%Y"),dia_semana_num = format(fecha, "%W")) %>% 
        group_by(año,dia_semana_num,negocio,linea,target) %>% summarise(real= sum(real),
                                                                         pred_ts=sum(pred_ts),
                                                                         pred_regr=sum(pred_regr),
                                                                         pred_fb=sum(pred_fb),
                                                                         pred_promedio=sum(pred_promedio),
                                                                         pred_gru=sum(pred_gru),
                                                                         fecha = min(fecha),
                                                                         target = first(target))%>%
        ungroup()
    }else if (input$tag2=="AHT"){
      validacion_diaria %>%
        subset(target == "AHT")   %>% 
        mutate(año = format(fecha, "%Y"),dia_semana_num = format(fecha, "%W")) %>% 
        group_by(año,dia_semana_num,negocio,linea,target) %>% summarise(real= mean(real),
                                                                         pred_ts=mean(pred_ts),
                                                                         pred_regr=mean(pred_regr),
                                                                         pred_fb=mean(pred_fb),
                                                                         pred_promedio=mean(pred_promedio),
                                                                         pred_gru=mean(pred_gru),
                                                                         fecha = min(fecha),
                                                                         target = first(target))%>%
        ungroup()
    }
  })
  
  #Dataframe validación mensual filtrado por camapaña,línea y target
  rv_val_mes <- reactive({
    val_mes() %>%
      subset(negocio == input$camp2)%>%
      subset(linea == input$linea2) %>%
      subset(target == input$tag2) 
    
  })
  
  #Dataframe validación semanal filtrado por camapaña,línea y target
  rv_val_semanal <- reactive({
    val_semanal() %>%
      subset(negocio == input$camp2)%>%
      subset(linea == input$linea2)%>%
      subset(target == input$tag2) 
    
  })
  
  #Dataframe validación diaria filtrado por camapaña,línea y target
  rv_val_dia <- reactive({
    validacion_diaria %>%
      subset(negocio == input$camp2)%>%
      subset(linea == input$linea2)%>%
      subset(target == input$tag2) 
  })
  
  #Gráfico validación todos los modelos
  output$validacion_plot <- renderPlotly({
    Sys.setlocale("LC_TIME", "es_ES.utf8")
    if (input$per2 == "diaria"){
      df <- rv_val_dia() %>%
        pivot_longer(cols = c(real, pred_ts,pred_regr,pred_promedio,pred_fb,pred_gru), names_to = "modelo", values_to = "Valor")
      df$modelo<-as.factor(df$modelo)
      df$modelo <- factor(df$modelo, levels = c("real","pred_ts","pred_regr","pred_promedio","pred_fb","pred_gru"))
      levels(df$modelo) <- c("Real","Time Series","Regresión","Promedio TS-R","Facebook Prophet","GRU")
      df$fecha <- format(df$fecha, "%d %b %Y")
      df$fecha <- factor(df$fecha, levels = unique(df$fecha))
      plot_ly(df,x = ~fecha, y = ~Valor, color = ~modelo, type = 'scatter', mode = 'lines+markers', colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB', '#81CBF5')) %>% 
        layout(legend=list(title=list(text='modelo')), xaxis = list(title = "fecha",dtick=15), yaxis = list(title = tipo_dato2()))
    }else if (input$per2 == "mensual"){
      df <- rv_val_mes() %>%
        pivot_longer(cols = c(real, pred_ts,pred_regr,pred_promedio,pred_fb, pred_gru), names_to = "modelo", values_to = "Valor")
      df$modelo<-as.factor(df$modelo)
      df$modelo <- factor(df$modelo, levels = c("real","pred_ts","pred_regr","pred_promedio","pred_fb","pred_gru"))
      levels(df$modelo) <- c("Real","Time Series","Regresión","Promedio TS-R","Facebook Prophet","GRU")
      df$fecha <- format(df$fecha, "%d %b %Y")
      df$fecha <- factor(df$fecha, levels = unique(df$fecha))
      plot_ly(df,x = ~fecha, y = ~Valor, color = ~modelo, type = 'scatter', mode = 'lines+markers', colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB', '#81CBF5')) %>% 
        layout(legend=list(title=list(text='modelo')), xaxis = list(title = "fecha"), yaxis = list(title = tipo_dato2()))
    }
    else if (input$per2 == "semanal"){
      df <- rv_val_semanal() %>%
        pivot_longer(cols = c(real, pred_ts,pred_regr,pred_promedio,pred_fb, pred_gru), names_to = "modelo", values_to = "Valor")
      df$modelo<-as.factor(df$modelo)
      df$modelo <- factor(df$modelo, levels = c("real","pred_ts","pred_regr","pred_promedio","pred_fb","pred_gru"))
      levels(df$modelo) <- c("Real","Time Series","Regresión","Promedio TS-R","Facebook Prophet","GRU")
      df$fecha <- format(df$fecha, "%d %b %Y")
      df$fecha <- factor(df$fecha, levels = unique(df$fecha))
      plot_ly(df,x = ~fecha, y = ~Valor, color = ~modelo, type = 'scatter', mode = 'lines+markers', colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB', '#81CBF5')) %>% 
        layout(legend=list(title=list(text='modelo')), xaxis = list(title = "fecha",dtick=4), yaxis = list(title = tipo_dato2()))
    }
  })
  #Titulo página validación
  output$title_validacion_plot <- renderText({
    paste("Validación", "-", input$camp2, input$linea2, sep = " ")
  })
  
  #Tabla errores mejor modelo
  tabla_val<-reactive({
    if (input$per2 == "mensual"){
    TS<-round(wape(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_ts),2)
    Reg<-round(wape(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_regr),2)
    Promedio<-round(wape(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_promedio),2)
    Facebook<-round(wape(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_fb),2)
    Promedio_movil<-round(wape(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_gru),2)
    
    TS2<-round(mae(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_ts),2)
    Reg2<-round(mae(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_regr),2)
    Promedio2<-round(mae(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real,preds= val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_promedio),2)
    Facebook2<-round(mae(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_fb),2)
    Promedio_movil2<-round(mae(actuals=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$real, preds=val_mes()[val_mes()$linea == input$linea2 & val_mes()$target == input$tag2,]$pred_gru),2)
    
    valores <- c(TS, Reg, Promedio, Facebook)
    nombres <- c("TS", "Reg", "Promedio TS-R", "Facebook", "GRU")
    indice_minimo <- which.min(valores)
    valores2 <- c(TS2, Reg2, Promedio2, Facebook2, Promedio_movil2)
    indice_minimo2 <- which.min(valores)
    data.frame(linea = input$linea2, target = input$tag2,modelo = nombres[indice_minimo], WAPE = paste(valores[indice_minimo], "%"), MAE = valores2[indice_minimo2])
    }else if(input$per2 == "diaria"){
      TS<-round(wape(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_ts),2)
      Reg<-round(wape(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_regr),2)
      Promedio<-round(wape(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_promedio),2)
      Facebook<-round(wape(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_fb),2)
      Promedio_movil<-round(wape(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_gru),2)
      
      TS2<-round(mae(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_ts),2)
      Reg2<-round(mae(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_regr),2)
      Promedio2<-round(mae(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real,preds= validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_promedio),2)
      Facebook2<-round(mae(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_fb),2)
      Promedio_movil2<-round(mae(actuals=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$real, preds=validacion_diaria[validacion_diaria$linea == input$linea2 & validacion_diaria$target == input$tag2,]$pred_gru),2)
      
      valores <- c(TS, Reg, Promedio, Facebook)
      nombres <- c("TS", "Reg", "Promedio TS-R", "Facebook", "GRU")
      indice_minimo <- which.min(valores)
      valores2 <- c(TS2, Reg2, Promedio2, Facebook2, Promedio_movil2)
      indice_minimo2 <- which.min(valores)
      data.frame(linea = input$linea2, target = input$tag2,modelo = nombres[indice_minimo], WAPE = paste(valores[indice_minimo], "%"), MAE = valores2[indice_minimo2])
      
    }else{
      TS<-round(wape(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_ts),2)
      Reg<-round(wape(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_regr),2)
      Promedio<-round(wape(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_promedio),2)
      Facebook<-round(wape(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_fb),2)
      Promedio_movil<-round(wape(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_gru),2)
      
      TS2<-round(mae(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_ts),2)
      Reg2<-round(mae(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_regr),2)
      Promedio2<-round(mae(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real,preds= val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_promedio),2)
      Facebook2<-round(mae(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_fb),2)
      Promedio_movil2<-round(mae(actuals=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$real, preds=val_semanal()[val_semanal()$linea == input$linea2 & val_semanal()$target == input$tag2,]$pred_gru),2)
      
      valores <- c(TS, Reg, Promedio, Facebook)
      nombres <- c("TS", "Reg", "Promedio TS-R", "Facebook", "GRU")
      indice_minimo <- which.min(valores)
      valores2 <- c(TS2, Reg2, Promedio2, Facebook2, Promedio_movil2)
      indice_minimo2 <- which.min(valores)
      data.frame(linea = input$linea2, target = input$tag2,modelo = nombres[indice_minimo], WAPE = paste(valores[indice_minimo], "%"), MAE = valores2[indice_minimo2])
      
    }
    })
  output$table_val <- renderTable({
    tabla_val()
  })
  
  #Titulo tabla errores mejor modelo
  output$title_validacion_table <- renderText({
    paste("Errores mejor modelo")
  })
  
  # Titulo gráfica mejor modelo
  output$title_val_plot_m <- renderText({
    paste("Comparación de modelos")
  })
  
  #Gráfica validación mejor modelo
  
  output$val_plot <- renderPlotly({
      if (input$per2 == "diaria"){
        tabla <- tabla_val()
        df <- rv_val_dia() 
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        fig<-plot_ly(df,x = ~fecha, y = ~real, name = 'Real', type = 'scatter', mode = 'lines',line = list(color = '#5D0664'),marker=list(color = '#5D0664')) %>%
          layout(legend=list(title=list(text='modelo')), xaxis = list(title = "fecha",dtick=15 ), yaxis = list(title = tipo_dato2()))
        if(tabla$modelo == "TS"){
          fig %>% add_trace(y = ~pred_ts, name = 'Time Series', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB'))
        }else if (tabla$modelo == "Reg") {
          fig %>% add_trace(y = ~pred_regr, name = 'Regresión', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "Promedio TS-R"){
          fig %>% add_trace(y = ~pred_promedio, name = 'Promedio TS-R', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "Facebook"){
          fig %>% add_trace(y = ~pred_fb, name = 'Facebook Prophet', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "GRU"){
          fig %>% add_trace(y = ~pred_gru, name = 'GRU', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }
      }else if (input$per2 == "mensual"){
        tabla <- tabla_val()
        df <- rv_val_mes()
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        fig<-plot_ly(df,x = ~fecha, y = ~real, name = 'Real', type = 'scatter', mode = 'lines+markers',line = list(color = '#5D0664'),marker=list(color = '#5D0664')) %>% 
          layout(legend=list(title=list(text='modelo')), xaxis = list(title = "fecha"), yaxis = list(title = tipo_dato2()))
        if(tabla$modelo == "TS"){
          fig %>% add_trace(y = ~pred_ts, name = 'Time Series', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if (tabla$modelo == "Reg") {
          fig %>% add_trace(y = ~pred_regr, name = 'Regresión', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "Promedio TS-R"){
          fig %>% add_trace(y = ~pred_promedio, name = 'Promedio TS-R', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "Facebook"){
          fig %>% add_trace(y = ~pred_fb, name = 'Facebook Prophet', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "GRU"){
          fig %>% add_trace(y = ~pred_gru, name = 'GRU', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }
      }
      else if (input$per2 == "semanal"){
        tabla <- tabla_val()
        df <- rv_val_semanal()
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha <- factor(df$fecha, levels = unique(df$fecha))
        fig<-plot_ly(df,x = ~fecha, y = ~real, name = 'Real', type = 'scatter', mode = 'lines+markers',line = list(color = '#5D0664'),marker=list(color = '#5D0664')) %>% 
          layout(legend=list(title=list(text='modelo')), xaxis = list(title = "fecha",dtick=4), yaxis = list(title = tipo_dato2()))
        if(tabla$modelo == "TS"){
          fig %>% add_trace(y = ~pred_ts, name = 'Time Series', type = 'scatter',mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if (tabla$modelo == "Reg") {
          fig %>% add_trace(y = ~pred_regr, name = 'Regresión', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "Promedio TS-R"){
          fig %>% add_trace(y = ~pred_promedio, name = 'Promedio TS-R', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "Facebook"){
          fig %>% add_trace(y = ~pred_fb, name = 'Facebook', type = 'scatter', mode = 'lines',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }else if(tabla$modelo == "GRU"){
          fig %>% add_trace(y = ~pred_gru, name = 'GRU', type = 'scatter', mode = 'lines+markers',line=list(color = '#05ABAB'), marker=list(color = '#05ABAB')) 
        }
      }
  })
  
  #Titulo grafica validación mejor modelo
  output$title_val_plot <- renderText({
    paste("Mejor modelo")
  })
  
  #PREDICCION

  #Elementos observe para sincronizar los filtros de las 3 páginas
  observeEvent(
    input$linea2,
    updatePickerInput(session, "linea3", selected = input$linea2)
  )
  observeEvent(
    input$camp2,
    updatePickerInput(session, "camp3", selected = input$camp2)
  )
  observeEvent(
    input$tag2,
    updatePickerInput(session, "tag3", selected = input$tag2)
  )
  
  # Creación predición mensual
  pred_mes <-reactive({
    if (input$tag3=="Trafico"){
      pred_diaria[is.na(pred_diaria)] <- 0
      pred_diaria  %>% subset(target == "Trafico")   %>% 
        mutate(año = format(fecha, "%Y"),mes = format(fecha, "%m")) %>% 
        group_by(año,mes,negocio,linea,target) %>% summarise(prediccion=sum(prediccion),
                                                             fecha = min(fecha),
                                                             target = first(target))%>%
        ungroup()
    }else if (input$tag3=="AHT"){
      pred_aht<-filter(pred_diaria,target == "AHT") %>%
        select(fecha,linea,negocio,target,prediccion)
      pred_inter<-filter(pred_diaria,target == "Trafico") %>%
        select(fecha,linea,negocio,prediccion)
      
      df_pred<-merge(pred_aht,pred_inter,by =c("fecha","linea","negocio"))
      df_pred[is.na(df_pred)] <- 0
      df_pred_mes<- pred_aht %>% 
        mutate(año = format(fecha, "%Y"),mes = format(fecha, "%m")) %>% 
        group_by(año,mes,negocio,linea,target) %>% summarise(calls_aht = sum(prediccion.x*prediccion.y),
                                                             Interacciones=sum(prediccion.y),
                                                             target = first(target),
                                                             fecha = min(fecha))%>%
        mutate(prediccion=(calls_aht/Interacciones)) %>%
        ungroup() %>%
        select(fecha,año,mes,negocio,linea,target,prediccion)
      
      return(df_pred_mes)
    }
  })
  
  #Dataframe mensual filtrado por camapaña,línea y target
  rv_pre_mes <- reactive({
    pred_mes() %>%
      subset(negocio == input$camp3)%>%
      subset(linea == input$linea3) %>%
      subset(target == input$tag3) 
    
  })
  
  #Dataframe diario filtrado por camapaña,línea y target
  rv_Pre_dia <- reactive({
    pred_diaria %>%
      subset(negocio == input$camp3)%>%
      subset(linea == input$linea3) %>%
      subset(target == input$tag3) 
  })
  
  #Titulo pagina pronostico
  output$title_pred_plot <- renderText({
    paste("Predicción", "-", input$camp3, input$linea3, sep = " ")
  })
  
  #Titulo grafica pronostico
  output$title_pred_plot_mes <- renderText({
    paste("Real vs Predicción Mensual")
  })
  
  #Titulo tabla de desviaciones
  output$title_pred_table_desv <- renderText({
    paste("Tabla de Desviaciones")
  })
  
  #Titulo de tabla resumen de predicciones
  output$title_pred_table_f <- renderText({
    paste("Resumen Predicciones")
  })
  
  #Botón de prediccion mensual/diaria
  estado <- reactiveVal(TRUE)
  observeEvent(input$btn_diario, {
    # Cambiar el estado del botón al contrario del estado actual
    estado(!estado())
  })
  
  #Cambio tabla a mensual/diaria según el botón
  pred <- reactive({
    if(estado()) {
      predi <- rv_pre_mes()
      predi = rename(predi, c(mes="mes",negocio= "negocio",target ="Target",
                              linea = "linea",prediccion= "Prediccion",fecha = "fecha"))
      predi %>% mutate(mes = format(predi$fecha, "%B", locale = "es_ES"))%>%
        select(año, mes, negocio, linea, Target, Prediccion)
      
    } else {
      predi<- rv_pred_dia_f()
      #predi<-predi[-c(4,5,11)]
      predi = rename(predi, c(mes = "mes",negocio= "negocio",target ="Target",
                              linea = "linea",prediccion= "Prediccion",fecha = "fecha"))
      predi%>% select(fecha,negocio, linea, Target, Prediccion)
    }
  })
  
  output$table_pred <- DT::renderDT({
    pred()
  })
  
  #Titulo tabla pronostico
  output$title_pred_table <- renderText({
    paste("Predicciones", "-", input$camp3, input$linea3, sep = " ")
  })
  
  #Botón para descargar tabla de predicciones
  output$exportButton <- downloadHandler(
    filename = function() {
      "predicciones.xlsx"
    },
    content = function(file) {
      write.xlsx(pred(), file)
    }
  )
  
  #Concatenación historico diario con predicción diaria
  df_pre_ajus_dia<-reactive({
    
    
    df_concat<- data %>%
      select(fecha, linea, negocio, real_aht, real_calls, año, mes, dia)
    
    pred_aht<-filter(pred_diaria,target == "AHT") %>%
      select(fecha,linea,negocio,prediccion)
    names(pred_aht)[which(names(pred_aht) == "prediccion")] <- "real_aht"
    pred_aht <- na.omit(pred_aht)
    pred_inter<-filter(pred_diaria,target == "Trafico") %>%
      select(fecha, linea, negocio, prediccion, año, mes, dia)
    names(pred_inter)[which(names(pred_inter) == "prediccion")] <- "real_calls"
    pred_inter <- na.omit(pred_inter)
    df_pred<-merge(pred_inter,pred_aht,by =c("fecha","linea","negocio"), all = TRUE)
    df_pred <- na.omit(df_pred)
    df_real_frc<- rbind(df_concat, df_pred)
    df_real_frc$combinado <- paste(df_real_frc$fecha, df_real_frc$negocio, df_real_frc$linea, sep = "-")
    df_sindupl <- subset(df_real_frc, !duplicated(combinado, fromLast = TRUE))
    df_sindupl$mes_name<-month.name[df_sindupl$mes]
    df_sindupl$mes = format(df_sindupl$fecha,"%B")
    df_sindupl$mes_abr <- format(df_sindupl$fecha, "%b", locale = "es_ES")
    
    
    df_pre_aju<-filter(df_sindupl, negocio ==input$camp3 & linea== input$linea3)
    
    return(df_pre_aju)
    
  })
  
  ## Filtrar dataframe predicción por rango
  rv_pred_dia_f <- reactive({
    if (input$amount3 == 'Todos'){
      rv_Pre_dia()
      
    } else if (input$amount3 == 'Rango'){
      rv_Pre_dia() %>%
        subset(fecha >= input$range3[1] & fecha <= input$range3[2])
    }
  })
  
  # Filtrar df concatenado por rango
  rv_pred_dia <- reactive({
    if (input$amount3 == 'Todos'){
      df_pre_ajus_dia()
      
    } else if (input$amount3 == 'Rango'){
      df_pre_ajus_dia() %>%
        subset(fecha >= input$range3[1] & fecha <= input$range3[2])
    }
  })  
  
  # Creación df concatenado mensual
  df_pre_ajus_mes<-reactive({
    
    df_pre_aju_mes<-df_pre_ajus_dia() %>%
      group_by(año,mes,negocio,linea) %>%
      summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                fecha=min(fecha)) %>%
      mutate(real_aht=(aht_calls_m/real_calls_f))%>%
      ungroup()
    
    
  })
  
  # Creación df concatenado semanal
  df_pre_ajus_sem<-reactive({
    
    df_pre_aju_sem<-df_pre_ajus_dia() %>%
      mutate(año = format(fecha, "%Y"),dia_semana_num = format(fecha, "%W")) %>%
      group_by(año,dia_semana_num,negocio,linea) %>%
      summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                fecha=min(fecha)) %>%
      mutate(real_aht=(aht_calls_m/real_calls_f))%>%
      ungroup()
    
  })
  
  

  #Gráfica predicción junto a historico
  output$pred_plot <- renderPlotly({
    
    if(input$tag3 == "Trafico"){
      if (input$per3 == "diaria"){
        df_real<-filter(data,  negocio ==input$camp3 & linea== input$linea3)
        df=rv_pred_dia()
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha<- factor(df$fecha, levels = unique(df$fecha))
        df_real$fecha<-format(df_real$fecha, "%d %b %Y")
        df_real$fecha<-factor(df_real$fecha, levels = unique(df_real$fecha))
        fecha_min<-tail(df_real$fecha, n = 1)
        fecha_max<-tail(df$fecha, n = 1)
        fig <- plot_ly(df, x = ~fecha, y = ~real_calls) %>%
          add_lines(line = list(color = '#05ABAB'))
        
        # Personalizar los colores en el layout
        fig <- fig %>% layout(
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 3,
                  label = "3 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 yr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "YTD",
                  step = "year",
                  stepmode = "todate"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = tipo_dato()),
          
          shapes = list(
            list(
              type = "rect",
              xref = "x",
              yref = "paper",
              x0 = fecha_min,
              x1 = fecha_max,
              y0 = 0,
              y1 = 1,
              fillcolor = "#5D0664",
              opacity = 0.2,
              layer = "below",
              line = list(width = 0)
            )
          )
        )
        
        fig%>% layout(xaxis = list(dtick = 290))
        
      }else if (input$per3 == "mensual"){
        
        df_real<-filter(data, negocio ==input$camp3 & linea== input$linea3)
        
        df_real_mes <- df_real %>%
          group_by(año,mes,negocio,linea) %>%
          summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                    fecha=min(fecha)) %>%
          mutate(real_aht=(aht_calls_m/real_calls_f))%>%
          ungroup()
        df=df_pre_ajus_mes()
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha<- factor(df$fecha, levels = unique(df$fecha))
        df_real_mes$fecha<-format(df_real_mes$fecha, "%d %b %Y")
        df_real_mes$fecha<-factor(df_real_mes$fecha, levels = unique(df_real_mes$fecha))
        fecha_min<-tail(df_real_mes$fecha, n = 1)
        fecha_max<-tail(df$fecha, n = 1)
        
        fig <- plot_ly(df, x = ~fecha, y = ~real_calls_f) %>%
          add_lines(line = list(color = '#05ABAB'))
        
        # Personalizar los colores en el layout
        fig <- fig %>% layout(
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 3,
                  label = "3 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 yr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "YTD",
                  step = "year",
                  stepmode = "todate"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = tipo_dato()),
          
          shapes = list(
            list(
              type = "rect",
              xref = "x",
              yref = "paper",
              x0 = fecha_min,
              x1 = fecha_max,
              y0 = 0,
              y1 = 1,
              fillcolor = "#5D0664",
              opacity = 0.2,
              layer = "below",
              line = list(width = 0)
            )
          )
        )
        
        fig%>% layout(xaxis = list(dtick = 10))
        
        
      }else if (input$per3 == "semanal"){
        df_real<-filter(data, negocio ==input$camp3 & linea== input$linea3)
        
        df_real_sem <- df_real %>%
          mutate(año = format(fecha, "%Y"),dia_semana_num = format(fecha, "%W")) %>%
          group_by(año,dia_semana_num,negocio,linea) %>%
          summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                    fecha=min(fecha)) %>%
          mutate(real_aht=(aht_calls_m/real_calls_f))%>%
          ungroup()
        df=df_pre_ajus_sem()
        df$fecha <- format(df$fecha, "%d %b %Y")
        df$fecha<- factor(df$fecha, levels = unique(df$fecha))
        df_real_sem$fecha<-format(df_real_sem$fecha, "%d %b %Y")
        df_real_sem$fecha<-factor(df_real_sem$fecha, levels = unique(df_real_sem$fecha))
        fecha_min<-tail(df_real_sem$fecha, n = 1)
        fecha_max<-tail(df$fecha, n = 1)
        
        
        fig <- plot_ly(df, x = ~fecha, y = ~real_calls_f) %>%
          add_lines(line = list(color = '#05ABAB'))
        
        # Personalizar los colores en el layout
        fig <- fig %>% layout(
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 3,
                  label = "3 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 yr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "YTD",
                  step = "year",
                  stepmode = "todate"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = tipo_dato()),
          
          shapes = list(
            list(
              type = "rect",
              xref = "x",
              yref = "paper",
              x0 = fecha_min,
              x1 = fecha_max,
              y0 = 0,
              y1 = 1,
              fillcolor = "#5D0664",
              opacity = 0.2,
              layer = "below",
              line = list(width = 0)
            )
          )
        )
        
        fig%>% layout(xaxis = list(dtick = 30))
      }}else if(input$tag3 == "AHT"){
        if (input$per3 == "diaria"){
          df_real<-filter(data,  negocio ==input$camp3 & linea== input$linea3)
          df=rv_pred_dia()
          df$fecha <- format(df$fecha, "%d %b %Y")
          df$fecha<- factor(df$fecha, levels = unique(df$fecha))
          df_real$fecha<-format(df_real$fecha, "%d %b %Y")
          df_real$fecha<-factor(df_real$fecha, levels = unique(df_real$fecha))
          fecha_min<-tail(df_real$fecha, n = 1)
          fecha_max<-tail(df$fecha, n = 1)
          
          fig <- plot_ly(df, x = ~fecha, y = ~real_aht) %>%
            add_lines(line = list(color = '#05ABAB'))
          
          # Personalizar los colores en el layout
          fig <- fig %>% layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 3,
                    label = "3 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 6,
                    label = "6 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "1 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "YTD",
                    step = "year",
                    stepmode = "todate"),
                  list(step = "all"))),
              
              rangeslider = list(type = "date")),
            
            yaxis = list(title = "AHT"),
            
            shapes = list(
              list(
                type = "rect",
                xref = "x",
                yref = "paper",
                x0 = fecha_min,
                x1 = fecha_max,
                y0 = 0,
                y1 = 1,
                fillcolor = "#5D0664",
                opacity = 0.2,
                layer = "below",
                line = list(width = 0)
              )
            )
          )
          
          fig%>% layout(xaxis = list(dtick = 290))
        }else if (input$per3 == "mensual"){
          df_real<-filter(data, negocio ==input$camp3 & linea== input$linea3)
          
          df_real_mes <- df_real %>%
            group_by(año,mes,negocio,linea) %>%
            summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                      fecha=min(fecha)) %>%
            mutate(real_aht=(aht_calls_m/real_calls_f))%>%
            ungroup()
          df=df_pre_ajus_mes()
          df$fecha <- format(df$fecha, "%d %b %Y")
          df$fecha<- factor(df$fecha, levels = unique(df$fecha))
          df_real_mes$fecha<-format(df_real_mes$fecha, "%d %b %Y")
          df_real_mes$fecha<-factor(df_real_mes$fecha, levels = unique(df_real_mes$fecha))
          fecha_min<-tail(df_real_mes$fecha, n = 1)
          fecha_max<-tail(df$fecha, n = 1)
          
          fig <- plot_ly(df, x = ~fecha, y = ~real_aht) %>%
            add_lines(line = list(color = '#05ABAB'))
          
          # Personalizar los colores en el layout
          fig <- fig %>% layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 3,
                    label = "3 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 6,
                    label = "6 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "1 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "YTD",
                    step = "year",
                    stepmode = "todate"),
                  list(step = "all"))),
              
              rangeslider = list(type = "date")),
            
            yaxis = list(title = "AHT"),
            
            shapes = list(
              list(
                type = "rect",
                xref = "x",
                yref = "paper",
                x0 = fecha_min,
                x1 = fecha_max,
                y0 = 0,
                y1 = 1,
                fillcolor = "#5D0664",
                opacity = 0.2,
                layer = "below",
                line = list(width = 0)
              )
            )
          )
          
          fig%>% layout(xaxis = list(dtick = 10))
        }else if (input$per3 == "semanal"){
          df_real<-filter(data, negocio ==input$camp3 & linea== input$linea3)
          
          df_real_sem <- df_real %>%
            mutate(año = format(fecha, "%Y"),dia_semana_num = format(fecha, "%W")) %>%
            group_by(año,dia_semana_num,negocio,linea) %>%
            summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                      fecha=min(fecha)) %>%
            mutate(real_aht=(aht_calls_m/real_calls_f))%>%
            ungroup()
          
          
          df=df_pre_ajus_sem()
          df$fecha <- format(df$fecha, "%d %b %Y")
          df$fecha<- factor(df$fecha, levels = unique(df$fecha))
          df_real_sem$fecha<-format(df_real_sem$fecha, "%d %b %Y")
          df_real_sem$fecha<-factor(df_real_sem$fecha, levels = unique(df_real_sem$fecha))
          fecha_min<-tail(df_real_sem$fecha, n = 1)
          fecha_max<-tail(df$fecha, n = 1)
          
          fig <- plot_ly(df, x = ~fecha, y = ~real_aht) %>%
            add_lines(line = list(color = '#05ABAB'))
          
          # Personalizar los colores en el layout
          fig <- fig %>% layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 3,
                    label = "3 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 6,
                    label = "6 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "1 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "YTD",
                    step = "year",
                    stepmode = "todate"),
                  list(step = "all"))),
              
              rangeslider = list(type = "date")),
            
            yaxis = list(title = "AHT"),
            
            shapes = list(
              list(
                type = "rect",
                xref = "x",
                yref = "paper",
                x0 = fecha_min,
                x1 = fecha_max,
                y0 = 0,
                y1 = 1,
                fillcolor = "#5D0664",
                opacity = 0.2,
                layer = "below",
                line = list(width = 0)
              )
            )
          )
          
          fig%>% layout(xaxis = list(dtick = 30))
        }
      }
    
    
  })
  
  #Grafica comparación mensual de historico con prediccion
  output$Ajuste_pre_plot_mes <- renderPlotly({
    monthorder<-c("enero","febrero","marzo", "abril","mayo", "junio","julio",
                  "agosto", "septiembre","octubre","noviembre","diciembre")
    
    df <- df_pre_ajus_dia() %>%
      group_by(año,mes) %>%
      summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht),
                fecha = first(fecha)) %>%
      mutate(real_aht=(aht_calls_m/real_calls_f))%>%
      ungroup()
    df$año<-as.factor(df$año)
    df<-df[order(match(df$mes,monthorder)),]
    
    
    if(input$tag3 == "Trafico"){
      
      plot_ly(df, x = ~mes, y = ~real_calls_f, color = ~factor(año), colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB', '#81CBF5'), type = 'scatter', mode = 'lines')  %>%
        layout(
          xaxis = list(title = "fecha",categoryorder = "array",
                       categoryarray =monthorder),
          yaxis = list(title = tipo_dato()))
      
    }else if(input$tag3 == "AHT"){
      
      plot_ly(df, x = ~mes, y = ~real_aht, color = ~factor(año), colors = c('#DA261E','#D1B2D1','#09B56B', '#5D0664', '#05ABAB', '#81CBF5'), type = 'scatter', mode = 'lines')  %>%
        layout(
          xaxis = list(title = "fecha",categoryorder = "array",
                       categoryarray =monthorder),
          yaxis = list(title = tipo_dato()))
    }
    
    
  })
  
  #Calculo desviaciones trafico
  df_pre_ajus_desv_cls<-reactive({
    
    df<-df_pre_ajus_dia() %>%
      group_by(año,mes,mes_abr) %>%
      summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht)
      ) %>%
      mutate(real_aht=(aht_calls_m/real_calls_f))%>%
      ungroup() 
    
    df$desv_inter <- c(NA, round((df$real_calls_f[-1] / df$real_calls_f[-length(df$real_calls_f)]-1)*100,2))
    df$desv_aht <- c(NA, round((df$real_aht[-1] / df$real_aht[-length(df$real_aht)]-1)*100,2))
    df1<-df %>% select(año, mes_abr,desv_inter)
    
  })
  
  #Calculo desviaciones aht
  df_pre_ajus_desv_aht<-reactive({
    
    df<-df_pre_ajus_dia() %>%
      group_by(año,mes,mes_abr) %>%
      summarise(real_calls_f=sum(real_calls),aht_calls_m=sum(real_calls*real_aht)
      ) %>%
      mutate(real_aht=(aht_calls_m/real_calls_f))%>%
      ungroup()
    
    
    df$desv_inter <- c(NA, round((df$real_calls_f[-1] / df$real_calls_f[-length(df$real_calls_f)]-1)*100,2))
    df$desv_aht <- c(NA, round((df$real_aht[-1] / df$real_aht[-length(df$real_aht)]-1)*100,2))
    df1<-df %>% select(año, mes_abr,desv_aht)
    
  })

  # output$tablita  <- renderDataTable({
  #   df_pre_ajus_desv_cls()
  #  })
  
  #Creacion tabla de desviaciones
  df_desv<-reactive({
    
    if(input$tag3 == "Trafico"){
      
      df_pre <- df_pre_ajus_desv_cls() %>%
        pivot_wider(names_from = mes_abr, values_from = desv_inter)
      df_pre <- arrange(df_pre, año)
      df_pre1<- select(df_pre, año, ene., feb., mar., abr., may., jun.,
                       jul., ago., sep., oct., nov., dic.)
      return(df_pre1)
      
    }else if(input$tag3 == "AHT"){
      df_pre_aht <- df_pre_ajus_desv_aht() %>%
        pivot_wider(names_from = mes_abr, values_from = desv_aht)
      df_pre_aht <- arrange(df_pre_aht, año)
      df_pre_aht1<- select(df_pre_aht, año, ene., feb., mar., abr., may., jun.,
                           jul., ago., sep., oct., nov., dic.)
      return(df_pre_aht1)
      
    }
    
  })
  
  #Configuración tabla de desviaciones
  output$Ajuste_pre_tabla <- renderDT({
    datos=df_desv()
    datatable(
      datos,
      options = list(
        columnDefs = list(
          list(
            targets = 1:ncol(datos),  # Columnas afectadas (todas en este caso)
            render = JS(
              "function(data, type, row, meta) {",
              "  if (parseFloat(data) > 10.0 || parseFloat(data)< -10.0) {",
              "    return '<span style=\"color: red;\">' + data + '</span>';",
              "  } else {",
              "    return data;",
              "  }",
              "}"
            )
          )
        )
      )
    )
    
  })
  
  
})