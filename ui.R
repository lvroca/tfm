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

# Importanción datos histórico
reporte_diario_campaña <- read_excel("reporte_diario_campaña_limpio.xlsx")[ , -1]

# Configuración y modificaciones a datos historica 
data = arrange(bind_rows(reporte_diario_campaña), fecha)
data$FECHA<-as.Date(data$fecha)
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
validacion_diaria_calls = dplyr::rename(validacion_diaria_calls, real =interpolado_real_calls)
validacion_diaria_calls$target <- "Trafico"
validacion_diaria_aht <- read_excel("validacion_diaria_aht.xlsx")
validacion_diaria_aht = dplyr::rename(validacion_diaria_aht, real =interpolado_real_aht)
validacion_diaria_aht$target <- "AHT"

# Configuración y modificaciones a datos validación
validacion_diaria <- rbind(validacion_diaria_calls, validacion_diaria_aht)
validacion_diaria$negocio <- "camapaña_1"
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

# Importacion validacion wfm
validacion_diaria_wfm <- read_excel("validacion_diaria_wfm.xlsx")

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

# UI (INTERFAZ SHINY APP)
shinyUI(fluidPage(
  tags$head(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$style(
      HTML(
        "
        .navbar-default {background-color: #222D32;border-color: #e7e7e7;}
        
        .navbar-default .navbar-nav>li>a {color: white;}
        
        .navbar-default .navbar-toggle .icon-bar {background-color: white;}
        
        .navbar-default .navbar-brand {color: white;font-size: 20px;font-weight: bold;}
          
        body {background-color: #ECF0F5;}
        
        .spacer {background-color: #ECF0F5;height: 20px;}
        
        .picker-label { font-weight: normal; }
        
        "
      )
    )
  ),
  titlePanel(
    title = "",
    windowTitle = "ABC PROPHET"
  ),
  navbarPage(
    # Application title.
    title ="ABC PROPHET" ,

    tabPanel(
      "Descripción",
      fluidRow(
        style = "background-color: white;",
        column(
          width = 2,
          pickerInput("camp", "Seleccione campaña", sort(unique(data$negocio)))
        ),
        column(
          width = 2,
          pickerInput("linea", "Seleccione linea", choices = sort(unique(data$linea)))
        ),
        column(
          width = 2,
          pickerInput("tag", "Seleccione target", choices = c("Trafico", "AHT"))
        ),
        conditionalPanel(
          condition = 'input.tabs == "Serie" || input.tabs == "Descomposición" ',
        column(
          width = 2,
          pickerInput("per", "Seleccione periodicidad", c("Diaria" = "diaria","Mensual" = "mensual"))
          )
        ),
        conditionalPanel(
          condition = 'input.tabs == "Serie"',
        column(
          width = 2,
          radioGroupButtons("amount", "Cantidad de datos: ", list("Todos", "Rango"), 
                              checkIcon = list(yes = icon("ok", lib = "glyphicon")), status = "primary")
          )
          
        ),
        conditionalPanel(
          condition = "input.amount == 'Rango'",
        column(
          width = 2,
         dateRangeInput("range", "Rango:")
          )
        ),
        conditionalPanel(
          condition = 'input.tabs == "Serie"',
        ),
      ),
      fluidRow(
        tags$div(class = "spacer"),
        style = "background-color: white;",
        tabsetPanel(
          id = "tabs",
          tabPanel(
            status = "primary",
            title = "Serie",
            align = "center",
            textOutput("title_ts_plot2"),
            tags$head(tags$style('#title_ts_plot2{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("serie_tiempo_plot", height = "200px",width = "100%"),
            plotlyOutput("separadorr", height = "20px"),
            textOutput("title_ts_plot"),
            tags$head(tags$style('#title_ts_plot{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("outliers_plot", height = "200px",width = "100%")
          ),
          tabPanel(
            "Descomposición",
            align = "center",
            textOutput("title_decompose"),
            tags$head(tags$style('#title_decompose{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("descompose_plot", height = "600px",width = "100%")
          ),
          tabPanel(
            "Análisis estacional mensual",
            align = "center",
            textOutput("title_esta"),
            tags$head(tags$style('#title_esta{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("separadorrr", height = "20px"),
            prettyRadioButtons("ts_season", "Gráficos:", c("Por año", "Por mes", "Box-plot Meses"), inline = TRUE),
            plotlyOutput("boxmes_plot", height = "400px",width = "100%")
          ),
          tabPanel(
            "Análisis estacional dia-semana",
            align = "center",
            textOutput("title_esta_dia"),
            tags$head(tags$style('#title_esta_dia{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("separadorrr_dia", height = "20px"),
            prettyRadioButtons("ts_season_dia", "Gráficos:", c("Por año", "Por mes", "Box-plot día semana"), inline = TRUE),
            plotlyOutput("boxweek_plot", height = "400px",width = "100%")
          ),
        )
    )
      ),
    tabPanel(
      "Validación",
      fluidRow(
        style = "background-color: white;",
        column(
          width = 2,
          pickerInput("camp2", "Seleccione campaña", sort(unique(data$negocio)))
        ),
        column(
          width = 2,
          pickerInput("linea2", "Seleccione linea", choices = sort(unique(data$linea)))
        ),
        column(
          width = 2,
          pickerInput("tag2", "Seleccione target", choices = c("Trafico", "AHT"))
        ),
        column(
          width = 2,
          pickerInput("per2", "Seleccione periodicidad", c("Diaria" = "diaria", "Mensual" = "mensual"))
        )
      ),
      fluidRow(
        tags$div(class = "spacer"),
        style = "background-color: white;",
        tabsetPanel(
          tabPanel(
            "Comparación de modelos",
            align= "center",
            textOutput("title_validacion_plot"),
            tags$head(tags$style('#title_validacion_plot{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("sep", height = "20px"),

            textOutput("title_validacion_table"),
            tags$head(tags$style('#title_validacion_table{font-size:18px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            tableOutput(outputId = "table_val"),

            textOutput("title_val_plot"),
            tags$head(tags$style('#title_val_plot{font-size:18px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("val_plot", height = "200px",width = "100%"),
            plotlyOutput("separador3", height = "20px"),
            textOutput("title_val_plot_m"),
            tags$head(tags$style('#title_val_plot_m{font-size:18px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("validacion_plot", height = "200px",width = "100%")
          ),
          tabPanel(
           "Validación del modelo",
             align= "center",
             textOutput("title_validacion_wfm"),
             tags$head(tags$style('#title_validacion_wfm{font-size:20px;font-style: normal;
                                        font-family: "Calibri";font-weight: bold;}')),
             plotlyOutput("seppppp", height = "20px"),
             tableOutput(outputId = "table_val_wfm"),
             plotlyOutput("val_plot_wfm", height = "200px",width = "100%")
          ),
        )
      )
    ),
    tabPanel(
      "Pronóstico M. y L.P",
      fluidRow(
        style = "background-color: white;",
        column(
          width = 2,
          pickerInput("camp3", "Seleccione campaña", choices = sort(unique(data$negocio)))
        ),
        column(
          width = 2,
          pickerInput("linea3", "Seleccione linea", choices = sort(unique(data$linea)))
        ),
        column(
          width = 2,
          pickerInput("tag3", "Seleccione target", choices = c("Trafico", "AHT"))
        ),
        conditionalPanel(
          condition = 'input.tabs1 == "Predicción"',
          column(
            width = 2,
            pickerInput("per3", "Seleccione periodicidad", c("Diaria" = "diaria", "Mensual" = "mensual"))
          )
        ),
        conditionalPanel(
          condition = 'input.tabs1 == "Ajuste de predicción"',
          column(
            width = 1,
            passwordInput("clave", "Contraseña")
          )
        ),
        conditionalPanel(
          condition = "input.clave == 'fabi'",
          column(
            width = 1,
            actionButton("save", "Guardar")
          )
        ),
        conditionalPanel(
          condition = 'input.tabs1 == "Ajuste de predicción"',
          column(
            width = 2,
            numericInput("Part_t", "Ajustar todo", value = 0.0, min = -1, max = 1,step = 0.01)
          )
        ),
        column(
          width = 2,
          radioGroupButtons("amount3", "Cantidad de datos: ", list("Todos", "Rango"), 
                            checkIcon = list(yes = icon("ok", lib = "glyphicon")), status = "primary"),
        ),
        conditionalPanel(
          condition = "input.amount3 == 'Rango'",
          column(
            width = 2,
            dateRangeInput("range3", "Rango:")
          )
        )
      ),
      fluidRow(
        tags$div(class = "spacer"),
        style = "background-color: white;",
        tabsetPanel(
          id = "tabs1",
          tabPanel(
            "Predicción",
            align= "center",
            textOutput("title_pred_plot"),
            tags$head(tags$style('#title_pred_plot{font-size:20px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("pred_plot", height = "200px",width = "100%"),
            plotlyOutput("separar",height = "10px"),
            textOutput("title_pred_plot_mes"),
            tags$head(tags$style('#title_pred_plot_mes{font-size:18px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("Ajuste_pre_plot_mes", height = "200px",width = "100%"),
            textOutput("title_pred_table_desv"),
            tags$head(tags$style('#title_pred_table_desv{font-size:18px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            DTOutput(outputId = "Ajuste_pre_tabla"),
            plotlyOutput("separar1",height = "10px"),
            textOutput("title_pred_table_f"),
            tags$head(tags$style('#title_pred_table_f{font-size:18px;font-style: normal;
                                       font-family: "Calibri";font-weight: bold;}')),
            actionButton("btn_diario", "Cambiar a vista diaria"),
            downloadButton("exportButton", "Exportar"),
            DTOutput(outputId = "table_pred")
          ),
          tabPanel(
            "Ajuste de predicción",
            align= "center",
            textOutput("title_pred_plot_ajmes"),
            tags$head(tags$style('#title_pred_plot_ajmes{font-size:20px;font-style: normal;
                                                    font-family: "Calibri";font-weight: bold;}')),
            plotlyOutput("plot", height = "200px",width = "100%"),
            textOutput("title_tabla_aj"),
            tags$head(tags$style('#title_tabla_aj{font-size:18px;font-style: normal;
                                                    font-family: "Calibri";font-weight: bold;}')),
            checkboxInput("aplicar_a_todos", "Aplicar a todos los meses en adelante"),
            DTOutput("tabla", width= "100%"),
            textOutput("title_tabladesv_aj"),
            tags$head(tags$style('#title_tabladesv_aj{font-size:18px;font-style: normal;
                                                    font-family: "Calibri";font-weight: bold;}')),
            DTOutput(outputId = "Ajuste_pre_tabladesv", width= "100%"),
            textOutput("title_resultado_aj"),
            tags$head(tags$style('#title_resultado_aj{font-size:18px;font-style: normal;
                                                    font-family: "Calibri";font-weight: bold;}')),
            actionButton("btn_diario1", "Cambiar a vista diaria"),
            downloadButton("exportButton1", "Exportar"),
            DTOutput(outputId = "resultado", width= "100%")
          )
        )
      )
    ),
  ),
)

  
)