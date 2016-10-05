
########################Aplicacion SAT########################

library(shiny)
library(shinydashboard)
library(networkD3)
library(dplyr)
library(ggvis)
library(DT)
library(knitr)
library(plotly)
library(Hmisc)
library(reshape2)



#### Para ponintar el Dashboard, necesitamos 3 archivos csv, 1 html y un rds
# 1. catalogo_aplicacion_servidor.csv
# 2. catalogo_app_general_capacidad.csv
# 3. informacion_servidores.csv
# 4. catalogo_aplicacion_servidor_capacidad

### Cargamos la informacion de los datos :

# 1. catalogo_aplicacion_servidor.csv
catalogo_aplicacion_servidor<-read.csv("data/catalogo_aplicacion_servidor.csv",stringsAsFactors = F)%>%distinct()
catalogo_aplicacion_servidor<-catalogo_aplicacion_servidor%>%filter(aplicacion!="sin aplicacion")%>%arrange(aplicacion)
# 2. catalogo_app_general_capacidad.csv
catalogo_app_general_capacidad<-read.csv("data/catalogo_app_general_capacidad.csv",stringsAsFactors = F)%>%distinct()
# 3. informacion_servidores.csv
informacion_servidores<-read.csv("data/informacion_servidores.csv",stringsAsFactors = F)%>%distinct()
# 4. catalogo_aplicacion_servidor_capacidad
catalogo_aplicacion_servidor_capacidad<-read.csv("data/catalogo_aplicacion_servidor_capacidad.csv",stringsAsFactors = F)


############################# LISTA DE GENERALES ##########################################
## creamos un dataset de las aplicaciones y sus generales, con el fin de cuantificar el consumo por general de sus aplicaciones
app_general<-catalogo_app_general_capacidad%>%select(aplicacion,General)%>%distinct()
app_general<-app_general%>%filter(aplicacion!="sin aplicacion")%>%arrange(General)
####Obtenemos una lista de todas las direcciones para desplegar al usuario
list_direc<-lapply(unique(app_general$General),function(x) x)
names(list_direc)<-unique(app_general$General)

############################# LISTA DE APLICACIONES ##########################################
### lista de aplicacion servidor
server_app<-catalogo_aplicacion_servidor_capacidad%>%select(servidor,aplicacion)%>%distinct()
server_app<-server_app%>%filter(aplicacion!="sin aplicacion")%>%arrange(aplicacion)
server_app<-server_app[complete.cases(server_app),]
####Obtenemos una lista de todas las aplicaciones para desplegar al usuario
para_app<-unique(server_app$aplicacion[order(server_app$aplicacion)])
lista_app<-lapply(para_app, function(x) x)
names(lista_app)<-para_app

# datos que viene informacion del oversuscription

buscar_db <- function(servidor){
  if(grepl("sql",servidor)){
    return("SQL")
  }else if(grepl("db2",servidor)){
    return("DB2")
  }else if(grepl("orcl",servidor)){
    return("ORCL")
  }else{
    return("Sin_base")
  }
}

informacion_servidores$base_datos <- sapply(informacion_servidores$servidor, buscar_db)
informacion_servidores[,c(1,2)] <- NULL



shinyUI(dashboardPage(skin = "green",
                      dashboardHeader(title = "Servidores SAT"),
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Administraciones Generales", tabName = "generales", icon = icon("users")),
                          menuItem("Aplicaciones", tabName = "aplicaciones", icon = icon("laptop")),
                          menuItem("Sobreasignacion", tabName = "oversuscription", icon = icon("adjust")),
                          menuItem("Base de datos", tabName = "datos", icon = icon("database"),
                                   menuSubItem("Aplicacion-Servidor", tabName = "app_serv"),
                                   menuSubItem("Direccion-Aplicacion", tabName = "dir_app"),
                                   menuSubItem("Servidores", tabName = "server"))
                        ) #cierra sidebarMenu
                      ), #cierra dashboardSidebar
                      dashboardBody(skin = "red",
                                    tabItems(
                                      
                                      
                                      # Primer apartado
                                      tabItem(tabName = "generales",
                                              tabsetPanel(
                                                tabPanel("Pag 1",
                                                         fluidRow(
                                                           box(tableOutput("tabla_general")),
                                                           box(plotOutput("pie_general")),
                                                           box(plotOutput("pie_general2"))
                                                         )
                                                ),
                                                tabPanel("Pag 2",
                                                         fluidRow(selectInput("direcciones",label = "Direccion General:",
                                                                              choices = list_direc, width = 400)),
                                                         fluidRow(valueBoxOutput("num_app_gen"),
                                                                  valueBoxOutput("consumo_gen_asig"),
                                                                  valueBoxOutput("consumo_gen_usad")),
                                                         fluidRow(plotOutput("direc")),
                                                         sankeyNetworkOutput("sankey")
                                                )
                                              )
                                      ),
                                      
                                     
                                      # Segundo apartado
                                      
                                      
                                      tabItem(tabName = "aplicaciones",
                                              
                                              fluidRow(selectInput("aplicaciones",label = "Aplicacion :",
                                                                   choices = lista_app, width = 800),
                                                       #valueBoxOutput("num_server"),
                                                       valueBoxOutput("num_otrapp"),
                                                       valueBoxOutput("capacidad_asig"),
                                                       valueBoxOutput("capacidad_usada"),
                                                       valueBoxOutput("fisico"),
                                                       valueBoxOutput("virtual"),
                                                       box(forceNetworkOutput("simplenet"),height = 900),
                                                       box(plotOutput("tipo_status_f"),height = 450),
                                                      box(plotOutput("tipo_status_v"),height = 450)
                                                       
                                              )
                                      ),
                                      

                                      # Tercer apartado oversuscription
                                      tabItem(tabName = "oversuscription",
                                              fluidRow(column(8, selectInput("tipo",label = "Tipo :",
                                                                             choices = unique(datos_limpios_over$tipo), 
                                                                             selected = "PRODUCCION", width = 300)
                                              ),
                                              column(8,sliderInput("rangoamb", label = h3("Rango Gigabytes"), 
                                                                   min = 1, max = 5000, value = 100,
                                                                   step = 100)
                                              )
                                              ),
                                              fluidRow(plotlyOutput('over_tipo', height = "300px")),
                                              fluidRow(column(8, selectInput("db",label = "Base de datos :",
                                                                             choices = unique(datos_limpios_over$base_datos), 
                                                                             width = 300)
                                              ),
                                              column(8,sliderInput("rangodb", label = h3("Rango Gigabytes"),
                                                                   min = 1, max = 5000, value = 100,
                                                                   step = 100)
                                              )
                                              ),
                                              fluidRow(plotlyOutput('over_db', height = "300px"))
                                      ),
                                      
                                      # Cuarto Apartado : Base de datos
                                      
                                
                                      tabItem(tabName = "app_serv",
                                              dataTableOutput("tabla1")
                                      ),
                                      
                                      tabItem(tabName = "dir_app",
                                              dataTableOutput("tabla2")
                                      ),
                                      
                                      tabItem(tabName = "server",
                                              dataTableOutput("tabla3")
                                      )
                                
                                      
                                      
                                      
                                    ) #cierra tabItems
                      ) #cierra dashboardBody
) #cierra dashboardPage
) #cierra shinyUI
