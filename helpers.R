# baseR<- select(base, contains("A5"))
# target<- names(baseR)
# table(base$A5E.OTR.OT)
# 
# hagomatch<- levels(base$A5E.TV.8)
# final<- NULL
# for(i in 1:length(target)){
#   if(levels(base[, target[i]])==hagomatch){
#     final<- paste(final,"'", target[i],"',", sep="")
#   }
# }
# final
# 
# 
# 
# levels(base$A5E.ME.7)
# filtro-------------------------------------------------------------------------------------------------------------------------------------------------

#  input: -mi base de datos
#         -lista con los filtros donde cada elemento es e.g. : 'sexo= list(var= "SEXO",valores= c(NA, 1, 2),etiquetas = c("Sin filtro", "Mujer", "Hombre"),actual = input$formSexo)'
#
# output: -una base de datos filtrada por cada elemento de la lista pasada

filtro <- function(filbase, filListaFiltros){
  for(ff in 1:length(filListaFiltros)){
    #     i<-2
    #     filbase<- base
    #     filListaFiltros<-listaFiltros
    
    mifiltroT <- filListaFiltros[[ff]]
    
    if (length(mifiltroT[["actual"]])==1){
      # Tengo un select, dado que sólo puede tomar un valor mi filtro...
      # Busco entre mis valores cual es el que corresponde a la etiqueta actual
      filTempo <- mifiltroT[["valores"]][ mifiltroT[["etiquetas"]] == mifiltroT[["actual"]] ]
      if(!is.na(filTempo)){
        filbase<-filbase %>% filter(as.numeric(filbase[,mifiltroT[["var"]]]) %in% filTempo)
      }
    }else{
      # Tengo multiples opciones, mi base se debe quedar con todos los que digan true
      filTempo <- mifiltroT[["valores"]][mifiltroT[["actual"]]]
      if(length(filTempo)!=0){
        for(t in 1: length(filTempo)){
          filbase<-filbase %>% filter(as.numeric(filbase[,mifiltroT[["var"]]]) %in% filTempo)
        }
      }
    }
  }
  if(nrow(filbase)==0){
    filbase<- filbase[0, ]
    filbase<- filbase[NA, ]
  }
  return(filbase)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------- filtro


# creameIntervalos-------------------------------------------------------------------------------------------------------------------------------------------------
#
#  input: -mi base de datos
#         -Un vector con los nombres de las variables que, de manera independiente, se tiene que agrupar; asumo que son NUMÉRICAS
#         -Una lista que me dice como voy a agrupar a cada una de las variables del vector anterior, E.G. list(
#           # Primer intervalo cerrado, segundo abierto
#           # Asumo nombre del conjunto como el nombre de la etiqueta, y el orden es el level
#           "De 0% a 3%" = c(0,3),
#           "De 3% a 4%" = c(3,4),
#           "De 4% a 5%" = c(4,5),
#           "De 5% a 6%" = c(5,6),
#           "De 6% a 7%" = c(6,7),
#           "De 7% a 8%" = c(7,8),
#           "De 8% a 9%" = c(8,9),
#           "De 9% a 10%" = c(9,10),
#           "De 10% a 11%" = c(10,11),
#           "Mayor a 11%" = c(11,98),
#           "Ns/Nc" = c(98,99)
#           )
#
# output: -una base de datos con las mismas variables que le di, pero con subnombre "RR" que ya están agrupadas

creameIntervalos<- function(cIntvlbase, cIntvlsucias, cIntvlintervalos){
  #   cIntvlbase<- base
  #   cIntvlsucias<- c("A18.1", "A18.2", "A18.3", "A18.4", "A18.5", "A18.6", "A18.7", "A18.8")
  #   cIntvlintervalos<- list(
  #     # Primer intervalo cerrado, segundo abierto
  #     # Asumo nombre del conjunto como el nombre de la etiqueta, y el orden es el level
  #     "De 0% a 3%" = c(0,3),
  #     "De 3% a 4%" = c(3,4),
  #     "De 4% a 5%" = c(4,5),
  #     "De 5% a 6%" = c(5,6),
  #     "De 6% a 7%" = c(6,7),
  #     "De 7% a 8%" = c(7,8),
  #     "De 8% a 9%" = c(8,9),
  #     "De 9% a 10%" = c(9,10),
  #     "De 10% a 11%" = c(10,11),
  #     "Mayor a 11%" = c(11,98),
  #     "Ns/Nc" = c(98,99)
  #   )
  # Limpio a cada sucia segun la lista
  for(cIntvli in 1: length(cIntvlsucias)){
    # cIntvli<- 1
    variableFinal<- paste(cIntvlsucias[cIntvli], "RR", sep="")
    cIntvlbase[,variableFinal]<-NA
    
    for(cIntvlt in 1: length(cIntvlintervalos)){
      # cIntvlt<- 1
      cIntvlintervalosSub<- cIntvlintervalos[cIntvlt]
      #Creo la condicion
      inferior<- cIntvlintervalosSub[[1]][1]
      superior<- cIntvlintervalosSub[[1]][2]
      #Doy el número t
      cIntvlbase[,variableFinal][cIntvlbase[,cIntvlsucias[cIntvli]] >= inferior & cIntvlbase[,cIntvlsucias[cIntvli]] < superior]<-cIntvlt
    }
    cIntvlbase[,variableFinal] <- factor(cIntvlbase[,variableFinal], levels = c(1:length(cIntvlintervalos)), labels = names(cIntvlintervalos))
  }
  return(cIntvlbase)   
}
# -------------------------------------------------------------------------------------------------------------------------------------------------creameIntervalos




# agrupaFreq1-------------------------------------------------------------------------------------------------------------------------------------------------

# input : -Mi base de datos
#         -Un vector con los nombres de las variables a agrupar y cuyos "levels" de respuesta (en la base) sean exactamente los mismos
#         -Un string que puede ser "freq", "casos" o "custom", para saber si los porcentajes se calculan sobre la suma de frecuencias o sobre el total de casos (rows de la base) o sobre un valor dado
#         -logical, ¿Ordeno los datos de mayor a menor?
#         -limite, ¿eliminar a partir de qué valor en pct (porcentaje) i.e. 1, elimina todos los row con porcentaje menor a 1
#         -custom: ¿cual es el valor sobre el que voy a dividir frecuencias, en caso de que se pida un "custom" ?
#         -colores: un vector de lenght igual al número de opciones de respuesta de mis variables a agrupar, que contenga los colores (que se le van a pasar a HighCharts)
#
# output: -Un data frame con cuatro columnas: respuesta, frecuencia, porcentaje (sobre frecuencias, redondeado a 0 decimals) y color

agrupaFreq1<-function(aFbase, afvector, afSumaCual, afsort, aflim, afCustom, afColores){
  #   aFbase<- base
  #   afvector<- c("F4CC.1", "F4CC.2", "F4CC.3")
  afvector <- match(afvector,names(aFbase))
  sub_base<-aFbase%>%select(afvector)
  
  if(length(sub_base)!=1){
    tabla<- table(sub_base[1])
    for(i in 2:length(sub_base)){
      tabla<-table(sub_base[i])+tabla
    }
  }else{
    tabla<- table(sub_base[1])
  }
  tabla<-as.data.frame(tabla)
  if(dim(tabla)[2]==1){
    tabla[,"frecuencias"]<- rep(0, nrow(tabla))
    tabla[,"pct"]<- rep(0, nrow(tabla))
    tabla[,"colores"]<- rep("A", nrow(tabla))
  }else{
    names(tabla)<-c("respuesta","frecuencias")
    if(afSumaCual=="casos"){
      tabla<-tabla%>%mutate(pct=frecuencias/dim(aFbase)[1])
    }else if (afSumaCual=="freq"){
      tabla<-tabla%>%mutate(pct=frecuencias/sum(frecuencias))    
    }else if (afSumaCual=="custom"){
      tabla[,"pct"]= tabla$frecuencias / afCustom
    }
    tabla$colores<- afColores
  }
  ### mi tabla está vacía?
  if(sum(tabla[,2])>0){
    
    # y si mis porcentajes son muy bajos? que hago ? no puedo quedarme con una tabla de puros 0
    if(all(round(tabla[,3]*100, 0)==0)){
      tabla[,3]<- tabla[,3]*100
    }else{
      tabla[,3]<- round(tabla[,3]*100, 0)
    }
    
    if(afsort){
      tabla<-tabla %>% arrange(desc(frecuencias))
    }
    
    if(dim(tabla %>% filter(pct >= aflim))[1] == 0){
      if(dim(tabla %>% filter(pct > 0))[1] == 0){
        
      }else{
        tabla<- tabla %>% filter(pct > 0)
      }
    }else{
      tabla<-tabla %>% filter(pct >= aflim)
    }
    return(tabla)
  }else{
    return(tabla) 
  }
}

# ------------------------------------------------------------------------------------------------------------------------------------------------- agrupaFreq1



# graficaVenn-------------------------------------------------------------------------------------------------------------------------------------------------
graficaVenn<-function(basevenn,medios){
  nombres<-levels(basevenn[,1])
  
  conteo<-list()
  for(i in 1:length(nombres)){
    cuales<-NULL
    for(j in 1:length(basevenn)){
      cuales<-c(cuales,which(basevenn[,j]==nombres[i]))
      cuales<-(unique(cuales))
    }
    conteo[[i]]<-cuales
  }
  names(conteo)<-nombres
  
  tabla<-as.numeric(table(unlist((conteo[as.numeric(unlist(medios))]))))
  tabla<-summary(as.factor(tabla))
  
  total<-length(unique(unlist(conteo[as.numeric(unlist(medios))])))
# tabla<-round(tabla/total*100,2)
  tabla2<-c(' recuerdan publicidad 1 vez',
            ' recuerdan publicidad 2 veces',
            ' recuerdan publicidad 3 veces',
            ' recuerdan publicidad 4 veces',
            ' recuerdan publicidad 5 veces')
  
  titulo<-paste(tabla,'%',tabla2,sep='')
  titulo<-paste(tabla,tabla2,sep='')
  
  titulo<-titulo[1:length(medios)]
  
  titulo<-paste('Total: ', total,'personas\n',paste(titulo[1:min(length(unlist(medios)),2)],collapse='\n'))
  
  g<-venn.diagram(x=conteo[as.numeric(unlist(medios))],
                  filename=NULL,
                  col=c('coral2','aquamarine3','azure3','goldenrod2','darkmagenta')[1:length(unlist(medios))],
                  fill=c('coral2','aquamarine3','azure3','goldenrod2','darkmagenta')[1:length(unlist(medios))],
                  lwd=1,
                  lty=2,
                  margin=0.03,
                  main.cex=.8,
                  cat.cex=.8,
                  cex = .8,
                  main.pos=c(.65,1),
                  main=titulo
  )
  return(grid.draw(g))
}

# ------------------------------------------------------------------------------------------------------------------------------------------------- graficaVenn


# mivnnnVector-------------------------------------------------------------------------------------------------------------------------------------------------

##### NOTA: esta es una función muy específica a este shiny

#### input: - mvVid : El subnombre que corresponde a la marca, e.g. Presta Prenda es el "1" de los conjuntos de respuesta de medios
####        - mvVCualid : ¿Qué resultado obtengo? Suma de Espontáneo más ayudado(1), Espontáneo (2), Ayudado(3)
#### output: -Un vector con los string que corresponden a las variables que quiero para mi resultado


mivnnnVector<- function(mvVid,mvVCualid){
  mvVVector<- NULL
  medio<- c("TV","RE","CA","ME","PAE","PO","IN","RS","RA","OTR")
  if(mvVid!=99){
    if(mvVCualid==1){
      #Sumados
      for(i in 1:10){
        if(i !=10){
          medioEspontaneo<- paste("A5E.",medio[i],".",mvVid, sep="")
          medioAyudado<- paste("A6A.",medio[i],".",mvVid, sep="")
          mvVVector<- c( mvVVector, medioEspontaneo, medioAyudado)
        }else{
          medioEspontaneo<- paste("A5E.",medio[i],".",mvVid, sep="")
          mvVVector<- c( mvVVector, medioEspontaneo)
        }
      }
    }else
      if(mvVCualid==2){
        # Espontaneo
        for(i in 1:10){
          medioEspontaneo<- paste("A5E.",medio[i],".",mvVid, sep="")
          mvVVector<- c( mvVVector, medioEspontaneo)
        }
      }else
        if(mvVCualid==3){
          # Ayudado
          for(i in 1:9){
            medioAyudado<- paste("A6A.",medio[i],".",mvVid, sep="")
            mvVVector<- c( mvVVector, medioAyudado)
          }
        }
    return(mvVVector)
  }else{
    if(mvVCualid==1){
      #Sumados
          medioEspontaneo<- c("A5E.TV.1","A5E.RE.1","A5E.CA.1","A5E.ME.1","A5E.PAE.1","A5E.PO.1","A5E.IN.1","A5E.RS.1","A5E.RA.1","A5E.OTR.1","A5E.TV.8","A5E.RE.8",
          "A5E.CA.8","A5E.ME.8","A5E.PAE.8","A5E.PO.8","A5E.IN.8","A5E.RS.8","A5E.RA.8","A5E.OTR.8","A5E.TV.9","A5E.RE.9","A5E.CA.9","A5E.ME.9",
          "A5E.PAE.9","A5E.PO.9","A5E.IN.9","A5E.RS.9","A5E.RA.9","A5E.OTR.9","A5E.TV.15","A5E.RE.15","A5E.CA.15","A5E.ME.15",
          "A5E.PAE.15","A5E.PO.15","A5E.IN.15","A5E.RS.15","A5E.RA.15","A5E.OTR.15","A5E.TV.17","A5E.RE.17","A5E.CA.17","A5E.ME.17","A5E.PAE.17","A5E.PO.17","A5E.IN.17","A5E.RS.17",
          "A5E.RA.17","A5E.OTR.17")
          medioAyudado<-  c("A6A.TV.1", "A6A.RE.1", "A6A.CA.1", "A6A.ME.1", "A6A.PAE.1","A6A.PO.1", "A6A.IN.1", "A6A.RS.1", "A6A.RA.1", "A6A.TV.8", "A6A.RE.8", "A6A.CA.8", "A6A.ME.8", "A6A.PAE.8","A6A.PO.8", "A6A.IN.8", "A6A.RS.8", "A6A.RA.8",
          "A6A.TV.9", "A6A.RE.9", "A6A.CA.9", "A6A.ME.9", "A6A.PAE.9","A6A.PO.9", "A6A.IN.9", "A6A.RS.9", "A6A.RA.9","A6A.TV.15","A6A.RE.15","A6A.CA.15","A6A.ME.15","A6A.PAE.15","A6A.PO.15",
          "A6A.IN.15","A6A.RS.15","A6A.RA.15","A6A.TV.17","A6A.RE.17","A6A.CA.17","A6A.ME.17","A6A.PAE.17","A6A.PO.17","A6A.IN.17","A6A.RS.17","A6A.RA.17")
          mvVVector<- c( mvVVector, medioEspontaneo, medioAyudado)
      }else if(mvVCualid==2){
        # Espontaneo
        medioEspontaneo<- c("A5E.TV.1","A5E.RE.1","A5E.CA.1","A5E.ME.1","A5E.PAE.1","A5E.PO.1","A5E.IN.1","A5E.RS.1","A5E.RA.1","A5E.OTR.1","A5E.TV.8","A5E.RE.8",
                            "A5E.CA.8","A5E.ME.8","A5E.PAE.8","A5E.PO.8","A5E.IN.8","A5E.RS.8","A5E.RA.8","A5E.OTR.8","A5E.TV.9","A5E.RE.9","A5E.CA.9","A5E.ME.9",
                            "A5E.PAE.9","A5E.PO.9","A5E.IN.9","A5E.RS.9","A5E.RA.9","A5E.OTR.9","A5E.TV.15","A5E.RE.15","A5E.CA.15","A5E.ME.15",
                            "A5E.PAE.15","A5E.PO.15","A5E.IN.15","A5E.RS.15","A5E.RA.15","A5E.OTR.15","A5E.TV.17","A5E.RE.17","A5E.CA.17","A5E.ME.17","A5E.PAE.17","A5E.PO.17","A5E.IN.17","A5E.RS.17",
                            "A5E.RA.17","A5E.OTR.17")
          mvVVector<- c( mvVVector, medioEspontaneo)
      }else if(mvVCualid==3){
          # Ayudado
        medioAyudado<-  c("A6A.TV.1", "A6A.RE.1", "A6A.CA.1", "A6A.ME.1", "A6A.PAE.1","A6A.PO.1", "A6A.IN.1", "A6A.RS.1", "A6A.RA.1", "A6A.TV.8", "A6A.RE.8", "A6A.CA.8", "A6A.ME.8", "A6A.PAE.8","A6A.PO.8", "A6A.IN.8", "A6A.RS.8", "A6A.RA.8",
                          "A6A.TV.9", "A6A.RE.9", "A6A.CA.9", "A6A.ME.9", "A6A.PAE.9","A6A.PO.9", "A6A.IN.9", "A6A.RS.9", "A6A.RA.9","A6A.TV.15","A6A.RE.15","A6A.CA.15","A6A.ME.15","A6A.PAE.15","A6A.PO.15",
                          "A6A.IN.15","A6A.RS.15","A6A.RA.15","A6A.TV.17","A6A.RE.17","A6A.CA.17","A6A.ME.17","A6A.PAE.17","A6A.PO.17","A6A.IN.17","A6A.RS.17","A6A.RA.17")
            mvVVector<- c( mvVVector, medioAyudado)
          }
        }
    return(mvVVector)
  }

# ------------------------------------------------------------------------------------------------------------------------------------------------- mivnnnVector



# agrupaFreqFiltro-------------------------------------------------------------------------------------------------------------------------------------------------

#  input: -mi base de datos
#         -la palabra que voy a filtrar en diferentes variables
#         -¿qué voy a sumar para los porcentajes? "personas" suma el número de personas que respondieron al filtro
#         -preparo la lista de filtro, cada elemento de la lista tiene por nombre LA VARIABLE DE FILTRO EN LA QUE VOY A BUSCAR EL VALOR DE FILTRO
#           cada nombre de variable de filtro tiene sus valores PARA NUBE y PARA TABLA DE FRECUENCIAS
#
# output: -Una lista con el vector para pasar a la función de dameJson nube, y un dataframe para pasar a la función de dame mi data frame json

agrupaFreqFiltro<- function(agrffbase, agrfffiltro, agrffsuma, agrffList, agrffOrden=F, agrffColorz= NA, agrfflimite=NA){
  
  #      agrffbase<- base
  #      agrfffiltro<- "Presta Prenda "
  #      agrffsuma<- "personas"
  
  #      agrffList<- list(
  #        A8.1= list(
  #          nube = "TA14.1NUBE",
  #          variables = c("A14.1.1.COD", "A14.1.2.COD", "A14.1.3.COD")
  #        ),
  #        
  #        A8.2= list(
  #          nube = "TA14.2NUBE",
  #          variables = c("A14.2.1.COD", "A14.2.2.COD", "A14.2.3.COD")
  #        ),
  #        
  #        A8.3= list(
  #          nube = "TA14.3NUBE",
  #          variables = c("A14.3.1.COD", "A14.3.2.COD", "A14.3.3.COD")
  #        ),
  #        
  #        A8.4= list(
  #          nube = "TA14.4NUBE",
  #          variables = c("A14.4.1.COD", "A14.4.2.COD", "A14.4.3.COD")
  #        ),
  #        
  #        A8.5= list(
  #          nube = "A14.5NUBE",
  #          variables = c("A14.5.1.COD", "A14.5.2.COD", "A14.5.3.COD")
  #        ),
  #        
  #        A8.6= list(
  #          nube = "TA14.6NUBE",
  #          variables = c("A14.6.1.COD", "A14.6.2.COD", "A14.6.3.COD")
  #        ),
  #        
  #        A8.7= list(
  #          nube = "TA14.7NUBE",
  #          variables = c("A14.7.1.COD", "A14.7.2.COD", "A14.7.3.COD")
  #        ),
  #        
  #        A8.8= list(
  #          nube = "TA14.8NUBE",
  #          variables = c("A14.8.1.COD", "A14.8.2.COD", "A14.8.3.COD")
  #        )
  #      )
  
  # Voy a hacer un data frame para cada respuesta, luego voy a sumar las columnas y me quedo sólo con la suma. Emulo un resultado de la función agrupaFreq1
  agrfftringVector <- NULL    
  # Obtengo mis respuestas de mi primer variable de filtro
  #agrffdataframe<- as.data.frame(levels(agrffbase[,agrffList[[1]]$variables[1]]))
  agrffdataframe<- as.data.frame(names(table(agrffbase[,agrffList[[1]]$variables[1]])))
  
  names(agrffdataframe)<- "respuesta"
  agrffN<- 0
  
  # Para cada elemento de la lista, obtengo las frecuencias
  for(agrffi in 1:length(agrffList)){
    #    agrffi<-1
    subagrffList<- agrffList[[agrffi]]
    agrffmeLlamo <-   names(agrffList)[agrffi]
    subagrffbase<- agrffbase[agrffbase[agrffmeLlamo] == agrfffiltro,]
    agrffN<- agrffN + sum(table(subagrffbase[,agrffmeLlamo]))
    agrffdataframe<-cbind(agrffdataframe,agrupaFreq1(subagrffbase, subagrffList$variables, "freq", F, 0, 0, rep("A", nrow(agrffdataframe)))[2])
    verdaderos<- !is.na(subagrffbase[,subagrffList$nube])
    agrfftringVector<- c(agrfftringVector,
                         as.character(subagrffbase[verdaderos,subagrffList$nube])
    )
  }
  
  agrffdataframe$freq<- rowSums(agrffdataframe[2:dim(agrffdataframe)[2]])
  agrffdataframe<- agrffdataframe[, c(1, dim(agrffdataframe)[2])]
  
  if(agrffOrden){
    agrffdataframe<-agrffdataframe[order(-agrffdataframe$freq),] 
  }
  
  if(agrffsuma=="personas"){
    agrffdataframe$pct<- round(agrffdataframe$freq/ agrffN * 100,0)  
  }else{
    agrffdataframe$pct<- round(agrffdataframe$freq/ sum(agrffdataframe$freq) * 100,0)
  }
  
  if(!is.na(agrffColorz)){
    agrffdataframe$colores<- agrffColorz
  }
  
  if(!is.na(agrfflimite)){
    agrffdataframe<- agrffdataframe %>% filter(pct > agrfflimite)
  }
  
  
  
  agrffFinal<- list(
    paNube= agrfftringVector,
    paTabla= agrffdataframe
  )
  
  return(agrffFinal)
  
}

# -------------------------------------------------------------------------------------------------------------------------------------------------agrupaFreqFiltro



# damemiJsonDFTable-------------------------------------------------------------------------------------------------------------------------------------------------

# input: -Un data frame, suponiendo que ya tiene cuatro columnas: respuesta, frecuencia, porcentaje, color
#
# output: -json [ respuesta, , frecuencia, Porcentaje (data es el % del data frame), color], listo para pasarse a la función de JavaScript que hace Tablas

damemiJsonDFTable<- function(dmJDataFrame){
  if(nrow(dmJDataFrame)>0){
    final<- list()
    previo<- list()
    for(tbb in 1:dim(dmJDataFrame)[1]){
      previo$Respuesta<- dmJDataFrame[tbb,1]
      previo$Frecuencia<- dmJDataFrame[tbb,2]
      previo$Porcentaje<- dmJDataFrame[tbb,3]
      final[[tbb]]<- previo
    }
    return(toJSON(final, asIs=T))    
  }else
  {
    final<- list()
    previo<- list()
    previo$Respuesta<- ""
    previo$Frecuencia<- ""
    previo$Porcentaje<- ""
    final[[1]]<- previo
    return(toJSON(final, asIs=T))    
  }
}

# ------------------------------------------------------------------------------------------------------------------------------------------------- damemiJsonDFTable



# miTablaA24-------------------------------------------------------------------------------------------------------------------------------------------------
#
#  input: -mi base de datos
#         -un string para saber qué tabla regresar
#
# output: -Un json listo para pasar a la función de JavaScript

miTablaA24<- function(A24Base, A24Cual){
  tablaPreferencias<- data.frame("Lugar"=NA, "Primera"=NA,"Segunda"=NA,"Tercera"=NA,"Cuarta"=NA,"Quinta"=NA,"Sexta"=NA)
  tablaPreferenciasTemporal<- data.frame("Marca"=NA, "Primera"=NA,"Segunda"=NA,"Tercera"=NA,"Cuarta"=NA,"Quinta"=NA,"Sexta"=NA)
  listaSucias<- c("A24.1", "A24.2", "A24.3", "A24.4", "A24.5", "A24.6", "A24.Otras")
  for(i in 1: length(listaSucias)){
    miTabla<- table(A24Base[,listaSucias[i]])
    for(t in 1: length(miTabla)){
      if(length(miTabla)>0){
        miValor<- miTabla[[t]]
        tablaPreferencias[i,t+1]<- miValor          
      }else{
        tablaPreferencias[i,t+1]<-0
      }
    }
    tablaPreferencias[i,][is.na(tablaPreferencias[i,])]<-0
  }
  tablaPreferencias$Lugar<- c("Presta Prenda", "Prenda Mex", "Nacional Monte de Piedad", "Prendinero", "Dondé Casa de Empeño", "First Cash", "Otro")
  
  tablaPreferenciasCasos<- tablaPreferencias[,1:4]
  tablaPreferenciasPct<- tablaPreferencias
  tablaPreferenciasPct[,2:7]<- round(prop.table(as.matrix(tablaPreferencias[,2:7]), margin=2)*100,0)  
  tablaPreferenciasPct<- tablaPreferenciasPct[,1:4]
  if(A24Cual==1){
    return(toJSON(tablaPreferenciasCasos, byrow=T, colNames=T, .na="0"))
  }else if(A24Cual==2) {
    return(toJSON(tablaPreferenciasPct, byrow=T, colNames=T, .na="0"))
  }
}
# -------------------------------------------------------------------------------------------------------------------------------------------------miTablaA24



# beneficios emocionales-------------------------------------------------------------------------------------------------------------------------------------------------
# Esta función es especial a este shiny
beneficiosEmocionales<- function(bemmbase, bemmOption){
  # bemmbase<- base
  bemmColor<- c("#53131E", "#F19A3E", "#45462A", "#98BDEE", "#5B8C5A")
  # 1: porcentaje, 2: casos
   # bemmOption<- 1
  
  bemmlistado<- list(
    MontePiedad = c("A27.MP.1", "A27.MP.2", "A27.MP.3", "A27.MP.4", "A27.MP.5", "A27.MP.6", "A27.MP.7", "A27.MP.8", "A27.MP.9"),
    PrestaPrenda = c("A27.PP.1", "A27.PP.2", "A27.PP.3", "A27.PP.4", "A27.PP.5", "A27.PP.6", "A27.PP.7", "A27.PP.8", "A27.PP.9"),
    FirstCash = c("A27.FC.1","A27.FC.2", "A27.FC.3", "A27.FC.4", "A27.FC.5", "A27.FC.6", "A27.FC.7", "A27.FC.8" ,"A27.FC.9"),
    PrendaMex = c("A27.PM.1", "A27.PM.2", "A27.PM.3", "A27.PM.4", "A27.PM.5", "A27.PM.6", "A27.PM.7", "A27.PM.8", "A27.PM.9"),
    RafaelDonde = c("A27.RD.1", "A27.RD.2", "A27.RD.3", "A27.RD.4", "A27.RD.5", "A27.RD.6", "A27.RD.7", "A27.RD.8", "A27.RD.9")
  )
  
  bemmNombres<- c("Nacional Monte de Piedad", "Presta Prenda", "First Cash", "Prendamex", "Fundación Rafael Dondé")
  
  maestro<- list(
    ejeX= c("Me hace sentir apoyado","Me hace sentir que cuento con un respaldo","Me permite estar aliviado en los momentos duros","Me hace sentir seguro","Me impulsa a ver otras posibilidades para alcanzar mis metas",
            "Me permite gozar de una relación cercana con él","Me ayuda a cumplir mis objetivos","Me da confianza","Entiendo lo que necesito y me ayuda a obtenerlo"),
    colores=bemmColor,
    cUBMversion=bemmOption,
    datos=list()
  )
  for(i in 1:length(bemmNombres)){
    final<- list(
      name= bemmNombres[i],
      data= NULL
    )
    bemmVariables <- bemmlistado[[i]]
    
    for(t in 1: length(bemmVariables)){
      tablaTemporal<- as.data.frame(table(bemmbase[, bemmVariables[t]]))
      tablaTemporal$pct<- round(tablaTemporal$Freq/sum(tablaTemporal$Freq)*100,0)
      if(bemmOption==1){
        final$data<- c(final$data,tablaTemporal[4,3]+tablaTemporal[3,3])
      }else{
        final$data<- c(final$data,tablaTemporal[4,2]+tablaTemporal[3,2])
      }
    }  
    maestro$datos[[i]]<- final
  }
  return(toJSON(maestro))
}
# -------------------------------------------------------------------------------------------------------------------------------------------------beneficios emocionales



# beneficios emocionales-------------------------------------------------------------------------------------------------------------------------------------------------
# Esta función es especial a este shiny
beneficiosEmocionales2<- function(bemmbase, bemmOption){
  # bemmbase<- base
  bemmColor<- c("#53131E", "#F19A3E", "#45462A", "#98BDEE", "#5B8C5A")
  # 1: porcentaje, 2: casos
  # bemmOption<- 1
  
  bemmlistado<- list(
    MontePiedad = c("A27.MP.1", "A27.MP.2", "A27.MP.3", "A27.MP.4", "A27.MP.5", "A27.MP.6", "A27.MP.7", "A27.MP.8", "A27.MP.9"),
    PrestaPrenda = c("A27.PP.1", "A27.PP.2", "A27.PP.3", "A27.PP.4", "A27.PP.5", "A27.PP.6", "A27.PP.7", "A27.PP.8", "A27.PP.9"),
    FirstCash = c("A27.FC.1","A27.FC.2", "A27.FC.3", "A27.FC.4", "A27.FC.5", "A27.FC.6", "A27.FC.7", "A27.FC.8" ,"A27.FC.9"),
    PrendaMex = c("A27.PM.1", "A27.PM.2", "A27.PM.3", "A27.PM.4", "A27.PM.5", "A27.PM.6", "A27.PM.7", "A27.PM.8", "A27.PM.9"),
    RafaelDonde = c("A27.RD.1", "A27.RD.2", "A27.RD.3", "A27.RD.4", "A27.RD.5", "A27.RD.6", "A27.RD.7", "A27.RD.8", "A27.RD.9")
  )
  
  bemmNombres<- c("Nacional Monte de Piedad", "Presta Prenda", "First Cash", "Prendamex", "Fundación Rafael Dondé")
  
  maestro<- list(
    ejeX= c("Me hace sentir apoyado","Me hace sentir que cuento con un respaldo","Me permite estar aliviado en los momentos duros","Me hace sentir seguro","Me impulsa a ver otras posibilidades para alcanzar mis metas",
            "Me permite gozar de una relación cercana con él","Me ayuda a cumplir mis objetivos","Me da confianza","Entiendo lo que necesito y me ayuda a obtenerlo"),
    colores=bemmColor,
    cUBMversion=bemmOption,
    datos=list()
  )
  for(i in 1:length(bemmNombres)){
    final<- list(
      name= bemmNombres[i],
      data= NULL
    )
    bemmVariables <- bemmlistado[[i]]
    
    for(t in 1: length(bemmVariables)){
      tablaTemporal<- as.data.frame(table(bemmbase[, bemmVariables[t]]))
      tablaTemporal$pct<- round(tablaTemporal$Freq/sum(tablaTemporal$Freq)*100,0)
      if(bemmOption==1){
        final$data<- c(final$data,tablaTemporal[4,3])
      }else{
        final$data<- c(final$data,tablaTemporal[4,2])
      }
    }  
    maestro$datos[[i]]<- final
  }
  return(toJSON(maestro))
}
# -------------------------------------------------------------------------------------------------------------------------------------------------beneficios emocionales



# 
# # frecuenciaSimplePorCruceMultiple -------------------------------------------------------------------------------------------------------------------------------------------------
# # IMPORTANTE: sólo estamos juntanto UNA pregunta, i.e. al final en la app SÓLO PUEDES FILTRAR POR UNA VARIABLE, i.e. todos los levels del conjunto de variables que paso son iguales
# # input : -Mi base de datos
# #         -Un vector con los nombres de las variables a agrupar y cuyos "levels" de respuesta (en la base) sean exactamente los mismos
# #         -Un string que puede ser 1 para "freq", o 2 para "custom", para saber si los porcentajes se calculan sobre la suma de frecuencias o sobre el total de casos (rows de la base) o sobre un valor dado
# #         -logical, ¿Ordeno los datos de mayor a menor?
# #         -limite, ¿eliminar a partir de qué valor en pct (porcentaje) i.e. 1, elimina todos los row con porcentaje menor a 1
# #         -custom: ¿cual es el valor sobre el que voy a dividir frecuencias, en caso de que se pida un "custom" ?
# #         -colores: un vector de lenght igual al número de opciones de respuesta de mis variables a agrupar, que contenga los colores (que se le van a pasar a HighCharts)
# #
# # en json:
# # creaColumna(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos)
# # agrupaFreq1(base, c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), "custom", TRUE, 2, 2100, c('a'))
# frecuenciaSimplePorCruceMultiple<-function(aSbase, aSvector,aSSumaCual=1, aSsort=TRUE, aSlim=0, aSCustom=2100, aSColores=NULL, ScUBSidG, ScUBSTitulo="", ScUBSSubtitulo="", ScUBSTEjex="", ScUBSTEjey="", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=TRUE, cuales=NA)){
#   # Está cañón: para cada variable que yo saque, me tengo que quedar con sus respuestas... y agruparlas
#   variablesPaCruce<- c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8")
#   aSvector<- c("A15Meses.1RR","A15Meses.2RR","A15Meses.3RR","A15Meses.4RR","A15Meses.5RR","A15Meses.6RR","A15Meses.7RR","A15Meses.8RR")
#   aSbase<- base
#   aSvector2<- c(aSvector, variablesPaCruce)
#   aSvector2 <- match(aSvector2,names(aSbase))
#   sub_baseS<-aSbase%>%select(aSvector2)
#   
#   todasLasRespuestas=list(Si=F, cuales=c(1,15,8,9,17))
#   
#   
#   dameunDFdeParejas(sub_baseS, variablesPaCruce, aSvector, todasLasRespuestas)
#   
#   if(todasLasRespuestas$Si){
#     variablesTotales<- length(levels(sub_baseS[,variablesPaCruce[1]]))
#     levelsTotales<- levels(sub_baseS[,variablesPaCruce[1]])
#   }else
#   {
#     variablesTotales<- length(levels(sub_baseS[,variablesPaCruce[1]])[todasLasRespuestas$cuales])
#     levelsTotales<- levels(sub_baseS[,variablesPaCruce[1]])[todasLasRespuestas$cuales]
#   }
#   finalS<- NULL
#   finalS<- paste(finalS, '["creaContainerMultiple","#',ScUBSidG,'", "", ',variablesTotales,',"',ScUBSidG,'"],',sep="")
#   for(tlvlt in 1: variablesTotales){
#     #Creo una columna, para cada coincidencia de opcion de respuesta en mis variables
#     # nombreVariable <- paste("miTargetvPC",tlvlt, sep="")
#     vectorTrue<- NULL
#     for(tlvltI in 1:length(variablesPaCruce)){
#       subTrue<- sub_baseS[,variablesPaCruce[tlvltI]]==levelsTotales[tlvlt]
#       subTrue[is.na(subTrue)]<- FALSE
#       vectorTrue[subTrue]<- TRUE
#     }
#     vectorTrue[is.na(vectorTrue)]<- FALSE
#     #     sub_baseS[,nombreVariable]<-0
#     #     sub_baseS[vectorTrue,nombreVariable]<-tlvlt
#     #     sub_baseS[,nombreVariable]<- factor(sub_baseS[,nombreVariable], levels = c(1:variablesTotales), labels = levelsTotales)
#     # Creo mis espacios
#     fspc<-tlvlt
#     if(fspc==1 | fspc==2){corrida<- 0 }
#     if(fspc==3 | fspc==4){corrida<- 1 }
#     if(fspc==5 | fspc==6){corrida<- 2 }
#     if(fspc==7 | fspc==8){corrida<- 3 }
#     if(fspc==9 | fspc==10){corrida<- 4 }
#     # Ok, vamos en la fila fspc
#     if(fspc%%2==1){
#       #elemento non, por lo tanto se va a A
#       milugar <- paste(ScUBSidG, corrida, "A",sep="")
#     }else{
#       milugar <- paste(ScUBSidG, corrida, "B",sep="")
#     }
#     # De la respuesta 1, me quedo con el subconjunto de todos los que respondieron respuesta1 en todas las variables de variablesPaCruce
#     sub_baseSRR2<-sub_baseS[vectorTrue,]
#     finalS<- paste(finalS,frecuenciaSimple(sub_baseSRR2, aSvector, aSSumaCual, aSsort, aSlim, aSCustom, aSColores, milugar, cUBSTitulo="", cUBSSubtitulo=levelsTotales[fspc], cUBSTEjex="", cUBSTEjey=""),sep="")
#   }
#   return(finalS)
# }
# # -------------------------------------------------------------------------------------------------------------------------------------------------frecuenciaSimplePorCruce
