######################Aplicacion SAT###################

## app.R ##
library(shiny)
library(shinydashboard)
library(networkD3)
library(dplyr)
library(ggvis)
library(ggplot2)
library(scales)
library(DT)
library(RColorBrewer)
library(plotly)
library(tidyr)
library(Hmisc)
library(reshape2)
#papalord

#### Para ponintar el Dashboard, necesitamos 3 archivos csv, 1 html y un rds
# 1. catalogo_aplicacion_servidor.csv
# 2. catalogo_app_general_capacidad.csv
# 3. informacion_servidores.csv
# 4. catalogo_aplicacion_servidor_capacidad

### Cargamos la informacion:

# 1. catalogo_aplicacion_servidor.csv
catalogo_aplicacion_servidor<-read.csv("data/catalogo_aplicacion_servidor.csv",stringsAsFactors = F)%>%distinct()
catalogo_aplicacion_servidor<-catalogo_aplicacion_servidor%>%filter(aplicacion!="sin aplicacion")%>%arrange(aplicacion)
# 2. catalogo_app_general_capacidad.csv
catalogo_app_general_capacidad<-read.csv("data/catalogo_app_general_capacidad.csv",stringsAsFactors = F)%>%distinct()

# 3. informacion_servidores.csv
informacion_servidores<-read.csv("data/informacion_servidores.csv",stringsAsFactors = F)%>%distinct()
informacion_servidores[,c(1,2)] <- NULL
# 4. catalogo_aplicacion_servidor_capacidad
catalogo_aplicacion_servidor_capacidad<-read.csv("data/catalogo_aplicacion_servidor_capacidad.csv",stringsAsFactors = F)

## creamos un dataset de las aplicaciones y sus generales, con el fin de cuantificar el consumo por general de sus aplicaciones
app_general<-catalogo_app_general_capacidad%>%select(aplicacion,General,consumo_x_general_asignado,consumo_x_general_usado)%>%distinct()
app_general<-app_general%>%filter(aplicacion!="sin aplicacion")%>%arrange(General)
nodo_app<-app_general[complete.cases(app_general),] ###nos quedamos con las observaciones sin NA 

## hacer un join del catalogo de servidor, aplicacion con la informacion de servidores
#catalogo_aplicacion_servidor$servidor<-tolower(catalogo_aplicacion_servidor$servidor)
#informacion_servidores$servidor<-tolower(informacion_servidores$servidor)
#server_app_capacidad<-left_join(catalogo_aplicacion_servidor,informacion_servidores,by="servidor")
#server_app_capacidad<-server_app_capacidad[complete.cases(server_app_capacidad),]
#colnames(server_app_capacidad)[3]<-"aplicacion"


###Datos con informacion del oversuscription
#datos_limpios_over<-readRDS("data/datos_limpios_over.rds")


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


shinyServer(function(input, output) {
  
  #########Primer apartado del dashboard##########
  #######Informacion por direccion general#########

  
  ##################Pagina 1######################
  ##############Grafica de Pie################
  
  ####Sumarisamos los datos
  consumo_gen<-app_general%>%
    group_by(General)%>%
    filter(General != "TODAS" ) %>%
    summarise(Consumo_asignado =( sum(consumo_x_general_asignado,na.rm = T)/1024),Consumo_usado = (sum(consumo_x_general_usado,na.rm = T))/1024)%>%
    arrange(desc(General))
  
  #####Hacemos la grafica de pastel
  output$pie_general<-renderPlot({
    #consumo_gen<-consumo_gen[c(1,11,2,10,3,9,4,8,5,7,6),]
    par(mar=c(0,0,0,0))
    pie(consumo_gen$Consumo_usado, labels = consumo_gen$General, border = .1, cex = 1,
        col = sort(brewer.pal(length(consumo_gen$General), "Paired")),main="Distribucion del consumo usado")
  })
  
  output$pie_general2<-renderPlot({
    #consumo_gen<-consumo_gen[c(1,11,2,10,3,9,4,8,5,7,6),]
    par(mar=c(0,0,0,0))
    pie(consumo_gen$Consumo_asignado, labels = consumo_gen$General, border = .1, cex = .8,
        col = sort(brewer.pal(length(consumo_gen$General), "Paired")),main="Distribucion del consumo asignado")
  })
  
  
  
  ##################Pagina 2######################
  
  ########Para filtrar los datos por la direccion seleccionada############
  datos_general<-reactive({
    data<-app_general[app_general$General==input$direcciones,]
  })
  
  
  
  ###Caja con informacion de la direccion sobre numero de aplicaciones que tiene
  output$num_app_gen<-renderValueBox({
    valueBox(paste(nrow(datos_general())),
             "Aplicaciones", icon = icon("desktop"), color = "purple")
  })
  
  ###Caja con informacion de la direccion sobre consumo en Teras
  output$consumo_gen_asig<-renderValueBox({
    valueBox(paste(round(sum(datos_general()$consumo_x_general_asignado, na.rm = T)/1024,1)," TB"),
             "Consumo asignado", icon = icon("floppy-o"), color = "yellow")
  })
  
  output$consumo_gen_usad<-renderValueBox({
    valueBox(paste(round(sum(datos_general()$consumo_x_general_usado, na.rm = T)/1024,1)," TB"),
             "Consumo usado", icon = icon("floppy-o"), color = "yellow")
  })
  
  ##################Pagina 2######################
  #############Grafico de consumo por direccion general########
  ###Hacer el grafico de barras de consumo por aplicacion y direccion 
  output$direc<-renderPlot({
    
    data<-datos_general()%>%
      group_by(aplicacion)%>%
      summarise(Consumo=sum(consumo_x_general_asignado, na.rm = T))
    
    
    ggplot(data, aes(x=reorder(aplicacion,Consumo), y = Consumo , fill = Consumo))+
      geom_bar(stat = "identity")+theme_bw()+
      theme(plot.background = element_rect(fill = "gray93"),
            panel.background = element_blank(),
            legend.position=c(.1, .7), axis.title.x = element_blank(),
            axis.text.x  = element_text(angle=90, hjust=1, size=10, colour="black"))+
      scale_y_continuous(labels = comma) +
      scale_fill_continuous(name="Gigabytes", labels = comma)
    
  })
  
  ##################Pagina 3######################
  
  ####Este proceso es para la grafica de SANKEY para ubicar relaciones entre
  ###aplicaciones y otras direcciones
  output$sankey <- renderSankeyNetwork({
    
    ###Filtramos las aplicaciones por la direccion que se seleccionan
    ###Lo hacemos as???? porque luego vemos si esas aplicaciones pueden 
    ###estar en otras direcciones
    
    apps<-nodo_app$aplicacion[nodo_app$General == input$direcciones]
    
    ###Filtramos la informaci??????n de las direcciones para solo aquellas
    ###en la seleccion anterior
    direcciones<-nodo_app$General[nodo_app$aplicacion %in% apps]
    
    app_general<-nodo_app[nodo_app$aplicacion %in% apps,]
    
    ######Creamos la tabla de relaciones de los nodos, apps y servidores
    ######Asignamos a los servidores el grupo 2 (por esos son grises)
    ######y a las aplicaciones un id del 3 para adelante, el tama????o 
    ######tambien se define aqui, los servidores son 1 y las apps 100
    nodos = data.frame(names=c(unique(apps), unique(direcciones)))
    
    ####Este proceso es para crear los id para los nodos
    target<-sapply(app_general$aplicacion, function(x){
      match(x, nodos$names)
    })
    
    ####Este proceso es para crear los id para los nodos
    from<-sapply(app_general$General, function(x){
      match(x, nodos$names)
    })
    
    ####Aqui ya hacemos la relacion entre los servidores y las apps pero por
    ####su id
    links = data.frame(from = as.numeric(unlist(from))-1,
                       target = as.numeric(unlist(target))-1,
                       value = 20)
    
    
    
    ###Finalmente hacemos la grafica
    sankeyNetwork(Links = links, Nodes = nodos,
                  Source = "from", Target = "target",
                  Value = "value", NodeID = "names",
                  width = 700, fontSize = 12, nodeWidth = 15)
    
  }) # cierra renderSankeyNetwork
  
  
  output$tabla_general<-renderTable({
    #consumo_gen$Consumo<-prettyNum(consumo_gen$Consumo,big.mark=",",scientific=FALSE)
    consumo_gen
  })
  

  #########Segundo apartado del dashboard##########
  #######Informacion por aplicacion #########
  
  
  
  #######Cajas con valores de servidores, otras aplicaciones relacionadas a 
  #######la seleccionada y consumo de la aplicacion
  

  
  #######Cajas con valores de servidores, otras aplicaciones relacionadas a 
  #######la seleccionada y consumo de la aplicacion
#  output$num_server<-renderValueBox({
 #   valueBox(paste(length(servidor_aplicacion[servidor_aplicacion$aplicacion==input$aplicaciones,]$servidor)),
  #           "Servidores", icon = icon("cubes"), color = "blue")
#  })
  
  output$fisico<-renderValueBox({
    fis<-catalogo_aplicacion_servidor_capacidad%>%select(fv,aplicacion)%>%filter(aplicacion==input$aplicaciones)%>%filter(fv=="fisico")
    valueBox(paste(nrow(fis)),
             "Servidores fisicos", icon = icon("cubes"), color = "green")
  })
  
  output$virtual<-renderValueBox({
    virt<-catalogo_aplicacion_servidor_capacidad%>%select(fv,aplicacion)%>%filter(aplicacion==input$aplicaciones)%>%filter(fv=="virtual")
    valueBox(paste(nrow(virt)),
             "Servidores virtuales", icon = icon("cubes"), color = "aqua")
  })
  
  
  output$num_otrapp<-renderValueBox({
    data<-catalogo_aplicacion_servidor_capacidad$servidor[catalogo_aplicacion_servidor_capacidad$aplicacion %in% input$aplicaciones] #servidores de la aplicacion seleccionada
    data_2<-catalogo_aplicacion_servidor_capacidad[catalogo_aplicacion_servidor_capacidad$servidor %in% data,c("aplicacion","servidor")]
    num_apps<-length(unique(data_2$aplicacion))-1  
    
    valueBox(paste(num_apps),
             "Otras aplicaciones", icon = icon("desktop"), color = "purple")
  })
  
  output$capacidad_asig<-renderValueBox({
    data<-catalogo_aplicacion_servidor_capacidad%>%select(aplicacion,fv,tipo,consumo_dividido_asignado)%>%
      group_by(aplicacion,fv,tipo)%>%summarise(consumo_dividido_asignado=sum(consumo_dividido_asignado))%>%
      filter(aplicacion==input$aplicaciones)
    
    valueBox(paste(round(sum(data$consumo_dividido_asignado, na.rm = T)/1024,1)," TB"),
             "Capacidad asignada", icon = icon("floppy-o"), color = "yellow")
  })
  
  output$capacidad_usada<-renderValueBox({
    data<-catalogo_aplicacion_servidor_capacidad%>%select(aplicacion,fv,tipo,consumo_dividido_usado)%>%
      group_by(aplicacion,fv,tipo)%>%summarise(consumo_dividido_usado=sum(consumo_dividido_usado))%>%
      filter(aplicacion==input$aplicaciones)
    
    valueBox(paste(round(sum(data$consumo_dividido_usado, na.rm = T)/1024,1)," TB"),
             "Capacidad usada", icon = icon("floppy-o"), color = "yellow")
  })
  

  
  output$tipo_status_f<-renderPlot({
    
    data1<-catalogo_aplicacion_servidor_capacidad%>%select(aplicacion,fv,tipo,consumo_dividido_usado,consumo_dividido_asignado=consumo_dividido_fantasia)%>%group_by(aplicacion,fv,tipo)%>%summarise(consumo_dividido_usado=sum(consumo_dividido_usado),consumo_dividido_asignado=sum(consumo_dividido_asignado))%>%filter(fv=="fisico")%>%filter(aplicacion==input$aplicaciones)
    data<-melt(data1,id.vars = c("aplicacion","tipo","fv"))
    
    ggplot(data, aes(x = tipo, y = value, fill = variable )) +
      geom_bar(stat = "identity")+xlab("Tipo de ambiente")+ylab("consumo")+scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+
      ggtitle("Tipo de ambiente de servidores fisicos")+
      theme(plot.background = element_rect(fill = "gray93"),
            panel.background = element_blank(), axis.title.x = element_blank(),
            axis.text.x  = element_text(angle=0, hjust=1, size=8, colour="black"))
    
    
  })
  
  output$tipo_status_v<-renderPlot({
    
    data1<-catalogo_aplicacion_servidor_capacidad%>%select(aplicacion,fv,tipo,consumo_dividido_usado,consumo_dividido_asignado=consumo_dividido_fantasia)%>%group_by(aplicacion,fv,tipo)%>%summarise(consumo_dividido_usado=sum(consumo_dividido_usado),consumo_dividido_asignado=sum(consumo_dividido_asignado))%>%filter(fv=="virtual")%>%filter(aplicacion==input$aplicaciones)
    data<-melt(data1,id.vars = c("aplicacion","tipo","fv"))
    
    ggplot(data, aes(x = tipo, y = value, fill = variable )) +
      geom_bar(stat = "identity")+xlab("Tipo de ambiente")+ylab("consumo")+scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+
      ggtitle("Tipo de ambiente de servidores virtuales")+
      theme(plot.background = element_rect(fill = "gray93"),
            panel.background = element_blank(), axis.title.x = element_blank(),
            axis.text.x  = element_text(angle=0, hjust=1, size=8, colour="black"))
    
  })
  
  
  
  #######Grafo de relacion entre aplicaciones y servidores#######
  output$simplenet<-renderForceNetwork({
    
    ######Servidores que funcionan a la aplicacion
    server<-catalogo_aplicacion_servidor_capacidad[catalogo_aplicacion_servidor_capacidad$aplicacion==input$aplicaciones,]$servidor
    
    ######Aplicaciones que tienen relacion con los servidores anteriores
    apps<-unique(catalogo_aplicacion_servidor_capacidad[catalogo_aplicacion_servidor_capacidad$servidor %in% server,]$aplicacion)
    
    ######Creamos la tabla de relaciones de los nodos, apps y servidores
    ######Asignamos a los servidores el grupo 2 (por esos son grises)
    ######y a las aplicaciones un id del 3 para adelante, el tama?o 
    ######tambien se define aqui, los servidores son 1 y las apps 100
    nodos = data.frame(nodos=c(server, apps), 
                       group = c(rep(2,length(server)),seq(length(apps))+2),
                       tamano = c(rep(1,length(server)),rep(100,length(apps)))
    )
    
    ####Este proceso es para crear los id para los nodos
    target<-sapply(catalogo_aplicacion_servidor_capacidad$aplicacion[catalogo_aplicacion_servidor_capacidad$servidor %in% server], function(x){
      match(x, nodos$nodos)
    })
    
    ####Este proceso es para crear los id para los nodos
    from<-sapply(catalogo_aplicacion_servidor_capacidad$servidor[catalogo_aplicacion_servidor_capacidad$servidor %in% server], function(x){
      match(x, nodos$nodos)
    })
    
    ####Aqu? ya hacemos la relacion entre los servidores y las apps pero por
    ####su id
    links = data.frame(from = as.numeric(unlist(from))-1,
                       target = as.numeric(unlist(target))-1)
    
    ####Finalmente hacemos la grafica
    forceNetwork(Links = links, Nodes = nodos, Source = "from",
                 Target = "target", NodeID = "nodos",Nodesize = "tamano",
                 Group = "group", opacity = 1,
                 colourScale = JS("d3.scale.category10()"), fontSize = 20)
  })  
  
  
 

  
  
  #########Tercer apartado del dashboard##########
  #######Informacion de oversuscription#########
  
  output$over_tipo<-renderPlotly({
    datos_graf<-informacion_servidores%>%
      filter(tipo==input$tipo)%>%
      filter(capacidad_asignada > quantile(capacidad_asignada, probs = c(0.05), na.rm = T) &
               capacidad_asignada < quantile(capacidad_asignada, probs = c(0.95), na.rm = T))
    
    
    
    datos <- datos_graf$capacidad_asignada
    datos_graf$rango_capacidad <- cut2(datos,cuts = seq(0,max(datos),by = input$rangoamb))
    
    datos_graf %>%
      group_by(rango_capacidad) %>%
      summarise(capacidad_total = sum(capacidad_fantasia)/sum(capacidad_asignada), 
                capacidad_real = sum(capacidad_usada)/sum(capacidad_asignada)) %>%
      ungroup() %>%
      gather(Tipo, Capacidad, capacidad_total:capacidad_real) %>%
      mutate(Tipo = factor(Tipo, c("capacidad_real","capacidad_total"))) %>%
      arrange(Tipo) %>%
      ggplot(aes(x = rango_capacidad, y = Capacidad, fill = Tipo)) + geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#005b96", "#777777")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  output$over_db<-renderPlotly({
    datos_graf<-informacion_servidores%>%
      #filter(base_datos==input$db)%>%
      filter(base_datos=="SQL")%>%
      filter(capacidad_asignada > quantile(capacidad_asignada, probs = c(0.05), na.rm = T) &
               capacidad_asignada < quantile(capacidad_asignada, probs = c(0.95), na.rm = T))
    
    
    
    max(datos_graf$capacidad_asignada)
    datos <- datos_graf$capacidad_asignada
    
    datos_graf$rango_capacidad <- cut2(datos,cuts = seq(0,max(datos),by = input$rangodb))
    
    datos_graf %>%
      group_by(rango_capacidad) %>%
      summarise(capacidad_total = sum(capacidad_fantasia)/sum(capacidad_asignada), 
                capacidad_real = sum(capacidad_usada)/sum(capacidad_asignada)) %>%
      ungroup() %>%
      gather(Tipo, Capacidad, capacidad_total:capacidad_real) %>%
      mutate(Tipo = factor(Tipo, c("capacidad_real","capacidad_total"))) %>%
      arrange(Tipo) %>%
      ggplot(aes(x = rango_capacidad, y = Capacidad, fill = Tipo)) + geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#005b96", "#777777")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  
  
  
  
  #########Cuarto apartado del dashboard##########
  #######Informacion en tablas#########
  
  #####Relacion aplicacion servidor
  output$tabla1<-renderDataTable({ #falta agregar almacenamiento GB
    datatable(catalogo_aplicacion_servidor_capacidad%>%select(aplicacion,servidor,capacidad_asignada,capacidad_usada,consumo_dividido_asignado,consumo_dividido_usado)%>%
                distinct(),filter = 'top')
    #datatable(servidores%>%select(servidor,aplicacion=APP,capacidad,tipo,fv,Powerstate)%>%distinct()%>%arrange(servidor),filter = 'top')
    })
  
  #####Relacion direccion aplicacion 
  output$tabla2<-renderDataTable({
    datatable(app_general%>%distinct()%>%arrange(General),
              filter = 'top')
  })
  
  
  
  ## informacion de los servidores
  
  output$tabla3<-renderDataTable({
    datatable(informacion_servidores%>%select(servidor,DataCenter,tipo,fv,Powerstate,capacidad_asignada,capacidad_usada)%>%distinct()%>%arrange(servidor),
              filter = 'top')
  })
  
  
  
  
  
}) # cierra shinyServer



