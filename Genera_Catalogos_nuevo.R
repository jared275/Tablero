##### Para el DASHBOARD NECESITAMOS:
# 1. catalogo_aplicacion_servidor.csv  (fijo, no se mueve a menos que se agreguen aplicaciones)
# 2. catalogo_aplicacion_capacidad.csv
# 3. catalogo_aplicacion_general_capacidad.csv
# 4. catalogo_servidor_tipo_disco.csv
# 5.Informacion servidores
library(dplyr)

app_server<-read.csv("/Users/Yes/Desktop/Tablero/data/catalogo_aplicacion_servidor.csv",stringsAsFactors = F)
app_server$servidor<-tolower(app_server$servidor)

servidores_fisicos<-read.csv("/Users/Yes/Desktop/Tablero/servidores_fisicos.csv",stringsAsFactors = F)
servidores_virtuales<-read.csv("/Users/Yes/Desktop/Tablero/servidores_virtuales.csv",stringsAsFactors = F)
servidores_todos<-rbind(servidores_fisicos,servidores_virtuales)

servidores_todos$servidor<-tolower(servidores_todos$servidor)
colnames(servidores_todos)[7:9]<-c("capacidad_asignada","capacidad_usada","capacidad_fantasia")
write.csv(servidores_todos,"/Users/Yes/Desktop/Tablero/data/informacion_servidores.csv")


app_server$servidor<-tolower(app_server$servidor)

servidores_todos<-left_join(servidores_todos,app_server,by="servidor")
servidores_todos<-servidores_todos%>%select(servidor,DataCenter,tipo,Powerstate,fv,capacidad_fantasia,capacidad_usada,capacidad_asignada,aplicacion)




#prueba<-servidores_todos%>%filter(is.na(aplicacion))
## hay servidores virtuales que no tiene aplicacion??????



### generar aplicacion_capacidad


###### calcular capacidad dividida

consumo_dividido<-left_join(app_server,servidores_todos)

## hay servidores que originalmente fueron asociados a una app, sim embargo con la info nueva, no hay datos de estos
# servidores, los vamos a quitar, puede ser que hayan desaparecido.

consumo_dividido<-consumo_dividido%>%filter(fv!="")


# ¿Cuantas aplicaciones tiene cada servidor?
# el objetivo es calcular que parte de la capacidad total del servidor ocupa esa aplciacion
# (consumo dividido ) ya que si un servidores tiene 3 apps , cada app no ocupa la capacidad
# total del servidor, sino que ocupa e.g 1/3 del servidor 
# i.e que cachito de capacidad ocupa cada aplicacion de cada servdior

app_x_server<-consumo_dividido%>%select(servidor)%>%group_by(servidor)%>%summarise(n_app=n())
consumo_dividido<-left_join(consumo_dividido,app_x_server)%>%arrange(servidor)
consumo_dividido$consumo_dividido_asignado<-consumo_dividido$capacidad_asignada/consumo_dividido$n_app
consumo_dividido$consumo_dividido_usado<-consumo_dividido$capacidad_usada/consumo_dividido$n_app
consumo_dividido$consumo_dividido_fantasia<-consumo_dividido$capacidad_fantasia/consumo_dividido$n_app

write.csv(consumo_dividido,"/Users/Yes/Desktop/Tablero/data/catalogo_aplicacion_servidor_capacidad.csv")

# para sacar la capacidad total que ocupa la aplicacion hay que sumaer los cachitos de 
# capacidad que ocupa de cada servidor

capacidad_x_app<-consumo_dividido%>%select(aplicacion,consumo_dividido_asignado,consumo_dividido_fantasia,consumo_dividido_usado)%>%
                  group_by(aplicacion)%>%summarise(consumo_asignado_app=sum(consumo_dividido_asignado),
                                                   consumo_usado_app=sum(consumo_dividido_usado),
                                                   consumo_fantasia_app=sum(consumo_dividido_fantasia))

# pegar las centrales para agrupar por centrales cuanto consumo hay.

generales<-read.csv("/Users/Yes/Desktop/Code_Dashboard_SAT/Code_Dashboard_SAT/app_general.csv")
generales<-generales%>%select(aplicacion=Aplicacion,General)%>%distinct()%>%arrange(aplicacion)
generales$aplicacion<-as.character(generales$aplicacion)

cat_generales<-left_join(capacidad_x_app,generales)
# puede pasar que una aplicacion tenga 3 centrales , pero el consumo que hace de cada central,
# no solo proviene de una, entonces por convencion, asignamos que ocupa 1/3 de su capacidad
# de cada central

n_centrales<-cat_generales%>%select(aplicacion)%>%group_by(aplicacion)%>%summarise(n_centrales=n())

cat_generales<-left_join(cat_generales,n_centrales)
cat_generales$consumo_x_general_asignado<-cat_generales$consumo_asignado_app/cat_generales$n_centrales
cat_generales$consumo_x_general_usado<-cat_generales$consumo_usado_app/cat_generales$n_centrales

cat_generales$General[is.na(cat_generales$General)]<-"Sin Direccion"


# capacidad por cada aplicacion
#aplicacion_capacidad<-cat_generales%>%select(aplicacion,consumo)%>%distinct()

write.csv(cat_generales,"/Users/Yes/Desktop/Tablero/data/catalogo_app_general_capacidad.csv")







# generar aplicacion_general _capacidad


