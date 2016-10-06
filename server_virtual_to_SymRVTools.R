
library(dplyr)
library(tidyr)
library(stringr)
options(scipen=9999)

## Obs. los archivos originalmente se descargan de los symetrics en un formato txt pero dificil de cargar
# correctamente en R , por eso antes de cargarlos se ejecuta el programa "prueba.sh" en bash para limpiarlos

#####Leemos la informacion de los vmax y le pegamos la columna de vmax para saber
#####despues de donde viene

dire = "/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/Symetrics"
setwd(dire)

# escanear los ficheros en el directorio existente
list.files(path = dire,recursive = F)
b<-as.vector(list.files()) 
archivos=c()

for (i in 1:length(b)){
if (str_detect(b[i],"txt_limpio")==TRUE){
  archivos=c(archivos,b[i])
 } # cierra if
}# cierra for
 


archivos
# crear dataset vacio para guardar la informacion de todos los vmax 
vmax_todos_size<-data.frame (Sym=character(),
                            Bound_Pool_Name=character(),
                            Flgs_EMPT=character(),
                            Total_GBs=numeric(),
                            Total_Allocated_GBs=numeric(),
                            Total_Allocated_GBs_pct=integer(),
                            Total_Written_GBs=numeric(),
                            Total_Written_GBs_pct= integer(),
                            Compressed_Size_Ratio_GBs=numeric(),
                            Compressed_Size_Ratio_GBs_pct=integer(),
                            vmax= character())


vmax_todos_wwn<- data.frame(Sym=character(),
                            Physical=character(),
                            Config=character(),
                            Attr=character(),
                            wwn=character() ,
                            vmax = character())

# recorrer el vector donde estan los nombres de los archivos y crear un dataset final
for (i in 1:length(archivos)){
  if (str_detect(archivos[i],"LUNS")==TRUE){ #####Juntamos toda la informacion de los vmax size en un solo archivo
      name = substr(archivos[i], 5, 8)
      name = paste("vmax_",name,sep="")
      
      dataset_vmax <- read.table(archivos[i], quote="\"",stringsAsFactors =F)
      names(dataset_vmax) <- c("Sym","Bound_Pool_Name","Flgs_EMPT",
                                 "Total_GBs","Total_Allocated_GBs",
                                 "Total_Allocated_GBs_pct", "Total_Written_GBs",
                                 "Total_Written_GBs_pct","Compressed_Size_Ratio_GBs",
                                 "Compressed_Size_Ratio_GBs_pct")
      
      dataset_vmax$vmax <- name
      vmax_todos_size <- rbind(vmax_todos_size,dataset_vmax)
    
  } # cierra if
  else {
    
    #####Leemos la informacion de los wwn, la leemos como delimitado por numero de caracteres
    #####por cada columna porque luego tenemos problema en leerlo en otro formato. Tambien
    #####le pegamos la informacion de los vmax de donde vienen.
    name = substr(archivos[i], 4, 7)
    name = paste("vmax_",name, sep="")
    
    dataset_vmax <- read.fwf(archivos[i],
                              width = c(5,15,20,6,35), stringsAsFactors = F, colClasses= "character")
    names(dataset_vmax) <-c("Sym","Physical","Config","Attr","wwn")
    
    dataset_vmax$vmax <- name
    dataset_vmax$wwn <- gsub(" ","",dataset_vmax$wwn)
    vmax_todos_wwn <- rbind(vmax_todos_wwn,dataset_vmax)
    
    
  }
  
}


vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_0669"]<-"QRO"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_5225"]<-"QRO"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_2494"]<-"QRO"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_0779"]<-"QRO"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_2293"]<-"QRO"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_1168"]<-"CPN"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_0840"]<-"CPN"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_3655"]<-"MTY"
vmax_todos_size$DataCenter[vmax_todos_size$vmax=="vmax_1439"]<-"AGS"

vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_0669"]<-"QRO"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_5225"]<-"QRO"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_2494"]<-"QRO"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_0779"]<-"QRO"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_2293"]<-"QRO"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_1168"]<-"CPN"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_0840"]<-"CPN"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_3655"]<-"MTY"
vmax_todos_wwn$DataCenter[vmax_todos_wwn$vmax=="vmax_1439"]<-"AGS"


#####Hacemos el join de la capacidad con los wwn
vmax_todos_size_wwn <- vmax_todos_size %>%
  left_join(vmax_todos_wwn) %>%
  ####Hay Sym que no tienen wwn y aparece como N/A
  filter(!wwn=="N/A") %>%
  group_by(wwn) %>%
  mutate(conteo = n()) %>%
  ungroup() %>%
  arrange(desc(conteo), wwn)

### NOTA: vmax_todos_size_wwn , solo contiene informacion a nivel Symetrics , la llave de 
# esta tabla es (Sym,vmax), y aque el Sym es unico a nivel vmax.



#### cargamos los DataStore para encontrar el naa (equivalente al WWN) y hacemos el join de
# vmax_todos_size_wwn con el datastore  vmax_todos_size_wwn$wwn==DataStore$naa

library(XLConnect)
require(XLConnect)
#vignette('XLConnect') # Manual (viñeta) es muy útil

##### 1. Datacenter  AGS

wb <- loadWorkbook('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/AGS/RVTools_export_all_AGS.xls', create = TRUE) 
ags_tabvDatastore <- readWorksheet(wb, sheet = 'tabvDatastore')
ags_tabvDatastore$DataCenter<-"AGS"
ags_tabvDatastore<-ags_tabvDatastore%>%select(Name, Address,Capacity.MB,Provisioned.MB,In.Use.MB,Free.MB,DataCenter)


##### 2. Datacenter  cpn

cpn_tabvDatastore <- read.csv('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/CPN/RVTools_tabvDatastore.csv') 
cpn_tabvDatastore$DataCenter<-"CPN"
cpn_tabvDatastore<-cpn_tabvDatastore%>%select(Name, Address,Capacity.MB,Provisioned.MB,In.Use.MB,Free.MB,DataCenter)


##### 2. Datacenter  mty

wb <- loadWorkbook('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/MTY/RVTools_export_all_Mty_spacv2.xls', create = TRUE) 
mty_tabvDatastore <- readWorksheet(wb, sheet = 'tabvDatastore')
mty_tabvDatastore$DataCenter<-"MTY"
mty_tabvDatastore<-mty_tabvDatastore%>%select(Name, Address,Capacity.MB,Provisioned.MB,In.Use.MB,Free.MB,DataCenter)


 ##### 2. Datacenter  qro

qro_tabvDatastore <- read.csv('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/QRO/RVTools_tabvDatastore.csv') 
qro_tabvDatastore$DataCenter<-"QRO"
qro_tabvDatastore<-qro_tabvDatastore%>%select(Name, Address,Capacity.MB,Provisioned.MB,In.Use.MB,Free.MB,DataCenter)

### JUNTAMOS DE TODOS LOS DATASTORE DE LOS DATACENTER Y SACAR EL naa

RVTools_tabvDatastore<-rbind(ags_tabvDatastore,cpn_tabvDatastore,mty_tabvDatastore,qro_tabvDatastore)
RVTools_tabvDatastore$wwn <- sub("na..","",RVTools_tabvDatastore$Address)

RVTools_tabvDatastore$Name<-tolower(RVTools_tabvDatastore$Name)


#### join de vmax_todos_size_wwn con RVTools_tabvDatastore 

#join_vmax_datastore<-left_join(vmax_todos_size_wwn,RVTools_tabvDatastore) # WWN



### ahora hay que juntar RVTools_tabvDatastore con RVTools_tabvDisk 


#####Leemos el archivo de tabvDisk para encontrar los hostnames de las maquinas virtuales

## TabvDisk 

setwd("/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS")

### AGS

library(XLConnect)
#vignette('XLConnect') # Manual (viñeta) es muy útil

##### 1. Datacenter  AGS

wb <- loadWorkbook('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/AGS/RVTools_export_all_AGS.xls', create = TRUE) 
ags_tabvdisk <- readWorksheet(wb, sheet = 'tabvDisk')
ags_tabvdisk$DataCenter<-"AGS"
ags_tabvdisk<-ags_tabvdisk%>%select(VM,Disk,Capacity.MB,Path,Cluster,OS,DataCenter)

##### 2. Datacenter  cpn

cpn_tabvdisk <- read.csv('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/CPN/RVTools_tabvDisk.csv') 
cpn_tabvdisk$DataCenter<-"CPN"
cpn_tabvdisk<-cpn_tabvdisk%>%select(VM,Disk,Capacity.MB,Path,Cluster,OS,DataCenter)

##### 2. Datacenter  mty

wb <- loadWorkbook('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/MTY/RVTools_export_all_Mty_spacv2.xls', create = TRUE) 
mty_tabvdisk <- readWorksheet(wb, sheet = 'tabvDisk')
mty_tabvdisk$DataCenter<-"MTY"
mty_tabvdisk<-mty_tabvdisk%>%select(VM,Disk,Capacity.MB,Path,Cluster,OS,DataCenter)


##### 2. Datacenter  qro

qro_tabvdisk <- read.csv('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/QRO/RVTools_tabvDisk.csv') 
qro_tabvdisk$DataCenter<-"QRO"
qro_tabvdisk<-qro_tabvdisk%>%select(VM,Disk,Capacity.MB,Path,Cluster,OS,DataCenter)


### JUNTAMOS DE TODOS LOS DATACENTER TabDisk

RVTools_tabvDisk<-rbind(ags_tabvdisk,cpn_tabvdisk,mty_tabvdisk,qro_tabvdisk)
colnames(RVTools_tabvDisk)[3]<-"Capacity.MB_tabvDisk"
RVTools_tabvDisk$aux<-RVTools_tabvDisk$Path
#####Los hostnames vienen en la columna Path entre corchetes, por lo que separo 
#####por el cierre de corchete
RVTools_tabvDisk <- RVTools_tabvDisk %>%
  filter(!aux == "") %>%
  separate(aux, c("Name","path_"),sep = "]")

#####substituyo el corchete que abre y lo convierto a minusculas
RVTools_tabvDisk$Name <- tolower(sub("\\[","",RVTools_tabvDisk$Name))
RVTools_tabvDisk$VM <- tolower(RVTools_tabvDisk$VM)

###########################################   cargar TabInfo:


##### 1. Datacenter  AGS

wb <- loadWorkbook('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/AGS/RVTools_export_all_AGS.xls', create = TRUE) 
ags_tabvInfo <- readWorksheet(wb, sheet = 'tabvInfo')
ags_tabvInfo$DataCenter<-"AGS"
ags_tabvInfo<-ags_tabvInfo%>%select(VM,Powerstate,Provisioned.MB,DataCenter)

##### 2. Datacenter  cpn

cpn_tabvInfo <- read.csv('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/CPN/RVTools_tabvInfo.csv') 
cpn_tabvInfo$DataCenter<-"CPN"
cpn_tabvInfo<-cpn_tabvInfo%>%select(VM,Powerstate,Provisioned.MB,DataCenter)

##### 2. Datacenter  mty

wb <- loadWorkbook('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/MTY/RVTools_export_all_Mty_spacv2.xls', create = TRUE) 
mty_tabvInfo <- readWorksheet(wb, sheet = 'tabvInfo')
mty_tabvInfo $DataCenter<-"MTY"
mty_tabvInfo<-mty_tabvInfo%>%select(VM,Powerstate,Provisioned.MB,DataCenter)


##### 2. Datacenter  qro

qro_tabvInfo<- read.csv('/Users/Yes/Desktop/RVTools/DATOS/2016/09-Septiembre/RVTOOLS/QRO/RVTools_tabvInfo.csv') 
qro_tabvInfo$DataCenter<-"QRO"
qro_tabvInfo<-qro_tabvInfo%>%select(VM,Powerstate,Provisioned.MB,DataCenter)

### juntar todas

RVTools_tabvInfo<-rbind(ags_tabvInfo,cpn_tabvInfo,mty_tabvInfo,qro_tabvInfo)
RVTools_tabvInfo$VM<-tolower(RVTools_tabvInfo$VM)
## la llave de esta tabla en VM pero hay unos casos en qro que no se cumple. por lo que nos vamos a quedar con la capacidad mas grande
RVTools_tabvInfo<-RVTools_tabvInfo%>%group_by(VM,DataCenter,Powerstate)%>%summarise(Provisioned.MB=max(Provisioned.MB))%>%distinct()
colnames(RVTools_tabvInfo)[4]<-"Provisioned.MB_tabvInfo"
#RVTools_tabvInfo<-RVTools_tabvInfo%>%select(VM,DataCenter,Provisioned.MB)%>%distinct()

### join de TabvInfo con TabvDisk
RVTools_tabvDisk$VM<-tolower(RVTools_tabvDisk$VM)

RVTools_tabvInfoDisk<-left_join(RVTools_tabvDisk,RVTools_tabvInfo)

## join de RVTools_tabvInfoDisk con RVTools_tabvDatastore

RVTools_tabvInfoDisk$Name<-tolower(RVTools_tabvInfoDisk$Name)
RVTools_tabvDatastore$Name<-tolower(RVTools_tabvDatastore$Name)
RVTools_tabvInfoDiskDatastote<-left_join(RVTools_tabvInfoDisk,RVTools_tabvDatastore,by=c("Name","DataCenter"))


## quitar letra del wwn

#RVTools_tabvInfoDiskDatastote$wwn<-gsub("[a-z]","",RVTools_tabvInfoDiskDatastote$wwn)



##### JOIN DE LA INFORMACION DE RVTOOLS CON LA DE SYMETRICS

RVTools_Sym<-left_join(RVTools_tabvInfoDiskDatastote,vmax_todos_size_wwn)
RVTools_Sym$Pct_Allocated_Sym<-RVTools_Sym$Total_Allocated_GBs/RVTools_Sym$Total_GBs
RVTools_Sym$Capacidad_disk_toSym_real<-RVTools_Sym$Pct_Allocated_Sym*RVTools_Sym$Capacity.MB_tabvDisk
RVTools_Sym$Capacidad_disk_toSym_fantasia<-RVTools_Sym$Capacity.MB_tabvDisk-RVTools_Sym$Capacidad_disk_toSym_real

### prueba para ver que la suma de los discos de RVTOOL TABVDISK debe de coindicir con la de TabvInfo


#prueba<-RVTools_Sym%>%select(VM,DataCenter,Capacity.MB_tabvDisk)%>%group_by(VM,DataCenter)%>%summarise(suma_discos=sum(Capacity.MB_tabvDisk))
#RVTools_Sym<-left_join(RVTools_Sym,prueba)
#prueba2<-prueba1%>%select(VM,Provisioned.MB_tabvInfo,suma_discos,Capacity.MB_tabvDisk)
#prueba2$diferencia<-prueba2$Provisioned.MB_tabvInfo-prueba2$suma_discos
##  NOOOOO CUADRAAAAAA!!!! D:
# SE TOMARA LA DE TABDISK!!!!!

## convertimos megas a GB
RVTools_Sym<-RVTools_Sym%>%mutate(Capacidad_disk_toSym_real=((Capacidad_disk_toSym_real/1024)),
                                  Capacidad_disk_toSym_fantasia=((Capacidad_disk_toSym_fantasia/1024)),
                                  Capacity.TB_tabvDisk=((Capacity.MB_tabvDisk/1024)))


RVTools_Sym_nonulos<-RVTools_Sym%>%filter(Sym!="")

servidores_virtuales<-RVTools_Sym_nonulos%>%select(VM,DataCenter,Powerstate,Capacidad_disk_toSym_real,
                                                   Capacidad_disk_toSym_fantasia,Capacity.TB_tabvDisk)%>%
                      group_by(VM,DataCenter,Powerstate)%>%
                      summarise(Cap_disk_toSym_usado=sum(Capacidad_disk_toSym_real),
                                Cap_disk_toSym_asignado=sum(Capacidad_disk_toSym_fantasia),
                                Cap_disk_total=sum(Capacity.TB_tabvDisk))%>%
                      mutate(fv="virtual")


servidores_virtuales$tipo<-toupper(servidores_virtuales$VM)
servidores_virtuales$tipo[str_detect(servidores_virtuales$tipo,"PRO")]<-"PRODUCCION"
servidores_virtuales$tipo[str_detect(servidores_virtuales$tipo,"DEV")]<-"DESARROLLO"
servidores_virtuales$tipo[str_detect(servidores_virtuales$tipo,"UAT")]<-"UAT"
servidores_virtuales$tipo[str_detect(servidores_virtuales$tipo,"STR")]<-"STRESS"
servidores_virtuales$tipo[str_detect(servidores_virtuales$tipo,"ESX")]<-"ESX"
servidores_virtuales$tipo[!str_detect(servidores_virtuales$tipo,"ESX")&!str_detect(servidores_virtuales$tipo,"STRESS")&!str_detect(servidores_virtuales$tipo,"UAT")&!str_detect(servidores_virtuales$tipo,"DESARROLLO")&!str_detect(servidores_virtuales$tipo,"PRO")]<-"OTROS"

# estandarizar nombres:

servidores_virtuales<-servidores_virtuales%>%select(servidor=VM,DataCenter,tipo,Powerstate,fv,capacidad_total=Cap_disk_total,capacidad_usada=Cap_disk_toSym_usado,capacidad_asignada=Cap_disk_toSym_asignado)


write.csv(servidores_virtuales,"/Users/Yes/Desktop/Tablero/servidores_virtuales.csv")
##### guardamos los VM que no tienen asociado ningun device! para mostrarlo a EMC

RVTools_Sym_nulos<-RVTools_Sym%>%filter(is.na(Sym))



write.csv(RVTools_Sym_nulos,"/Users/Yes/Desktop/Tablero/RVTools_Sym_nulos.csv")

