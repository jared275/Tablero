
library(plotly)
library(ggplot2)
library(xlsx)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
options(scipen=999)

setwd("/Users/Yes/GIT_RICH/datascience/gobierno_datos")

#leo el archivo de HBAs con el objetivo de extraer el nombre del host y el WWN
qro<-read.csv("datos/RVTools/RVTools_export_all_5Qro_2016-05-18_09.27.29/RVTools_export_all_2016-05-18_09.27.29/RVTools_tabvHBA.csv",stringsAsFactors=FALSE)
qro$WWN2 <- as.character(lapply(strsplit(as.character(qro$WWN), "\\ "), "[", 2))
qro$WWN <- as.character(lapply(strsplit(as.character(qro$WWN), "\\ "), "[", 1))
qro<-select(qro,Host, Cluster,Datacenter,WWN,WWN2)
cpn<-read.xlsx("datos/RVTools/RVTools_export_all_CPN_201624may.xls", sheetName="tabvHBA",stringsAsFactors=FALSE)
cpn$WWN2 <- as.character(lapply(strsplit(as.character(cpn$WWN), "\\ "), "[", 2))
cpn$WWN <- as.character(lapply(strsplit(as.character(cpn$WWN), "\\ "), "[", 1))
cpn<-select(cpn,Host, Cluster,Datacenter,WWN,WWN2)
mty<-read.xlsx("datos/RVTools/RVTools_export_all_5Mty_2016-05-18_09.20.07.xls", sheetName="tabvHBA",stringsAsFactors=FALSE)
mty$WWN2 <- as.character(lapply(strsplit(as.character(mty$WWN), "\\ "), "[", 2))
mty$WWN <- as.character(lapply(strsplit(as.character(mty$WWN), "\\ "), "[", 1))
mty<-select(mty,Host, Cluster,Datacenter,WWN,WWN2)

hba_RVtools<- union(qro,cpn)%>%union(mty)

#leo el archivo de IG devices con el objetivo de extraer el nombre del host y el WWN
ig_dev<-read.csv("datos/storage/DEV_SID_IG/dev_sid_ig.csv",stringsAsFactors=FALSE,header=FALSE)
names(ig_dev)<-c("IG","DEV","SYM","WWN","WWN2")
ig_dev$WWN<-toupper(ig_dev$WWN)
ig_dev$WWN2<-toupper(ig_dev$WWN2)
ig_dev$IG<-toupper(ig_dev$IG)
ig_dev$SYM<-as.numeric(ig_dev$SYM)
head(ig_dev)

#hago merge y junto todo en un archivo
ig_host_dev <- inner_join(hba_RVtools,ig_dev,by=c("WWN2"="WWN")) %>% select(Host,Cluster,Datacenter,IG,DEV,SYM) %>% union(inner_join(hba_RVtools,ig_dev,by=c("WWN2"="WWN2")) %>% select(Host,Cluster,Datacenter,IG,DEV,SYM)) %>% union(inner_join(hba_RVtools,ig_dev,by=c("WWN"="WWN")) %>% select(Host,Cluster,Datacenter,IG,DEV,SYM)) %>% union(inner_join(hba_RVtools,ig_dev,by=c("WWN"="WWN2")) %>% select(Host,Cluster,Datacenter,IG,DEV,SYM))

#el antijoin en este caso representa los que no son ESX y que estan en almenos 1 HBA
fisicos<-anti_join(ig_dev,ig_host_dev,by="IG")%>%filter(WWN!="")%>%select(IG,DEV,SYM)%>%distinct()
head(fisicos)  
#le calculo que tipo de ambiente es:
fisicos$tipo<-toupper(fisicos$IG)
fisicos$tipo[str_detect(fisicos$tipo,"PRO")]<-"PRODUCCION"
fisicos$tipo[str_detect(fisicos$tipo,"DEV")]<-"DESARROLLO"
fisicos$tipo[str_detect(fisicos$tipo,"UAT")]<-"UAT"
fisicos$tipo[str_detect(fisicos$tipo,"STR")]<-"STRESS"
fisicos$tipo[str_detect(fisicos$tipo,"ESX")]<-"ESX"
fisicos$tipo[!str_detect(fisicos$tipo,"ESX")&!str_detect(fisicos$tipo,"STRESS")&!str_detect(fisicos$tipo,"UAT")&!str_detect(fisicos$tipo,"DESARROLLO")&!str_detect(fisicos$tipo,"PRO")]<-"OTROS"

#Leo el archivo que tiene los distintos pools de disco
vmaxdata<-read.csv("datos/storage/VMAXData.csv",stringsAsFactors=FALSE)
colnames(vmaxdata)[3]<-"DEV"
colnames(vmaxdata)[8]<-"SYM"

#checar aqui
unique(ig_host_dev$SYM)
unique(vmaxdata$SYM)

#hago merge de los IGs que corresponden a servidores fisicos
fisicos_pool <- inner_join(fisicos,vmaxdata,by=c("SYM","DEV"))

#con esta operacion divido la capacidad cuando se trata de clusters y agrego el numero de veces que se repite el pool
declusteriza<-fisicos_pool %>% select(IG,SYM,DEV,Pool_Total_GBs,Technology,Pool_Allocated_GBs)%>%
            group_by(SYM,DEV,IG)%>%distinct()%>%summarise(Pool_total=sum(Pool_Total_GBs),
                                                          Pool_allocated=sum(Pool_Allocated_GBs))%>%
            mutate(capacidad=Pool_allocated/n(),n=n())


head(declusteriza)
fisicos_bien<- inner_join(fisicos_pool,declusteriza,By=c("SYM","DEV","IG"))
fisicos_bien_1<-fisicos_bien%>%select(IG,DEV,SYM,tipo,PoolName,Cap_total_DEV=Pool_total,Cap_allocated_DEV=Pool_allocated,capacidad,n_serv_x_dev=n)

#esta puede ser la tabla con mas detalle f??sico

## vamos a asociar las vmax correspondientes y los Datacenter.
fisicos_bien_1$SYM<-as.character(fisicos_bien_1$SYM)

fisicos_bien_1$vmax[substr(fisicos_bien_1$SYM,6,9)=="5225"]<-"vmax_5225"
fisicos_bien_1$DataCenter[substr(fisicos_bien_1$SYM,6,9)=="5225"]<-"QRO"

fisicos_bien_1$vmax[substr(fisicos_bien_1$SYM,6,9)=="2494"]<-"vmax_2494"
fisicos_bien_1$DataCenter[substr(fisicos_bien_1$SYM,6,9)=="2494"]<-"QRO"

fisicos_bien_1$vmax[substr(fisicos_bien_1$SYM,6,9)=="2293"]<-"vmax_2293"
fisicos_bien_1$DataCenter[substr(fisicos_bien_1$SYM,6,9)=="2293"]<-"QRO"

fisicos_bien_1$vmax[substr(fisicos_bien_1$SYM,6,9)=="0779"]<-"vmax_0779"
fisicos_bien_1$DataCenter[substr(fisicos_bien_1$SYM,6,9)=="0779"]<-"QRO"

fisicos_bien_1$vmax[substr(fisicos_bien_1$SYM,6,9)=="0669"]<-"vmax_0669"
fisicos_bien_1$DataCenter[substr(fisicos_bien_1$SYM,6,9)=="0669"]<-"QRO"




### si queremos saber la capacidad del servidor fisico solo sumamos los "cachitos" capacidades (capacidad) de todos sus dev asociados







#Unir F??sicos y Virtuales

# ESTE porcentaje es el porcentaje usado de cada device 
fisicos_bien_1$Porcentaje_real<-(fisicos_bien_1$Cap_allocated_DEV)/fisicos_bien_1$Cap_total_DEV
fisicos_bien_1$Porcentaje_real[is.na(fisicos_bien_1$Porcentaje_real)]<-0.00

# ahora de acuerdo al porcentaje usado de dev, hay que sacar la proporcion de la capacidad a nivel servidor
### CAPACIDAD ESTA EN GB , comnvertir a Tb
#fisicos_bien_1<-fisicos_bien_1%>%mutate(capacidad=(capacidad/1024))


fisicos_bien_1$capacidad_usada<-fisicos_bien_1$Porcentaje_real*fisicos_bien_1$capacidad
fisicos_bien_1$capacidad_asignada<-fisicos_bien_1$capacidad-fisicos_bien_1$capacidad_usada



### ahora vamos a sacar la capacidad de los servidores, sumando sus capacidades 

servidores_fisicos<-fisicos_bien_1%>%group_by(IG,tipo,vmax,DataCenter)%>%summarise(capacidad_total_serv=sum(capacidad),capacidad_usada_serv=sum(capacidad_usada),capacidad_asignada_serv=sum(capacidad_asignada))
# ahora pasa que un servidore tiene asoiada mas de una vmax entonces sumamos sus caacidades.

servidores_fisicos<-servidores_fisicos%>%group_by(IG,tipo,DataCenter)%>%
                    summarise(capacidad_total_serv=sum(capacidad_total_serv),
                              capacidad_usada_serv=sum(capacidad_usada_serv),
                              capacidad_asignada_serv=sum(capacidad_asignada_serv))%>%
                    mutate(fv="fisico",Powerstate="poweredOn")


servidores_fisicos<-servidores_fisicos%>%select(servidor=IG,DataCenter,tipo,Powerstate,fv,capacidad_total=capacidad_total_serv,capacidad_usada=capacidad_usada_serv,capacidad_asignada=capacidad_asignada_serv)

write.csv(servidores_fisicos,"/Users/Yes/Desktop/Tablero/servidores_fisicos.csv")
#AGRUPA<-servidores_fisicos%>%group_by(IG)%>%mutate(n=n())




