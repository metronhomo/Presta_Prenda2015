# creaContainer -------------------------------------------------------------------------------------------------------------------------------------------------
#creaContainer(dondevoy,miClase,miTitulo,miSubtitulo,miIdContenido,miContenido)
creaContainer<- function(dondevoy,miClase,miTitulo,miSubtitulo,miIdContenido,miContenido){
  return(paste('["creaContainer","',dondevoy,'","',miClase,'","',miTitulo,'","',miSubtitulo,'","',miIdContenido,'","',miContenido,'"],', sep=""))
}
# -------------------------------------------------------------------------------------------------------------------------------------------------creaContainer


# creaContainerMultiple -------------------------------------------------------------------------------------------------------------------------------------------------
#creaContainerMultiple(dondevoy,miClase,cuantasCeldas,miIdContenidoOriginal)
creaContainerMultiple<- function(dondevoy,miClase,cuantasCeldas,miIdContenidoOriginal){
  return(paste('["creaContainer","',dondevoy,'","',miClase,'","',cuantasCeldas,'","',miIdContenidoOriginal,'"],', sep=""))
}
# -------------------------------------------------------------------------------------------------------------------------------------------------creaContainerMultiple


# frecuenciaSimple -------------------------------------------------------------------------------------------------------------------------------------------------
# input : -Mi base de datos
#         -Un vector con los nombres de las variables a agrupar y cuyos "levels" de respuesta (en la base) sean exactamente los mismos
#         -Un string que puede ser 1 para "freq", o 2 para "custom", para saber si los porcentajes se calculan sobre la suma de frecuencias o sobre el total de casos (rows de la base) o sobre un valor dado
#         -logical, ¿Ordeno los datos de mayor a menor?
#         -limite, ¿eliminar a partir de qué valor en pct (porcentaje) i.e. 1, elimina todos los row con porcentaje menor a 1
#         -custom: ¿cual es el valor sobre el que voy a dividir frecuencias, en caso de que se pida un "custom" ?
#         -colores: un vector de lenght igual al número de opciones de respuesta de mis variables a agrupar, que contenga los colores (que se le van a pasar a HighCharts)
#
# en json:
# creaColumna(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos)

frecuenciaSimple<-function(aFbase, afvector, afSumaCual=1, afsort=TRUE, aflim=0, afCustom=2100, afColores=NULL, cUBSidG, cUBSTitulo="", cUBSSubtitulo="", cUBSTEjex="", cUBSTEjey="", muestraCustom=T){
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
  miTotalTotal<- sum(tabla[,2])
  if(dim(tabla)[2]==1){
    tabla[,"frecuencias"]<- rep(0, nrow(tabla))
    tabla[,"pct"]<- rep(0, nrow(tabla))
    tabla[,"colores"]<- rep("A", nrow(tabla))
  }else{
    names(tabla)<-c("respuesta","frecuencias")
    if (afSumaCual==1){
      tabla<-tabla%>%mutate(pct=frecuencias/sum(frecuencias))    
    }else if (afSumaCual==2){
      tabla[,"pct"]= tabla$frecuencias / afCustom
    }
    tabla$color<- afColores
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
    dmJDataFrame<- tabla
  }else{
    dmJDataFrame<- tabla
  }
  final<- list()
  previo<- list()
  if(dim(dmJDataFrame)[1]>0){
    for(tba in 1:dim(dmJDataFrame)[1]){
      previo$name<- dmJDataFrame[tba,1]
      previo$casos<- dmJDataFrame[tba,2]
      previo$data<- dmJDataFrame[tba,3]
      previo$color <- dmJDataFrame[tba,4]
      final[[tba]]<- previo
    }
    final<- toJSON(final, asIs=T)
  }else{
    previo$name<- NA
    previo$casos<- NA
    previo$data<- NA
    previo$color <- NA
    final[[1]]<- previo
    final<- toJSON(final, asIs=T)
  }
  
  if(muestraCustom){
    afCustom<-  miTotalTotal
  }
  
  if(length(afColores)>0){
    return(paste('["creaColumna","',cUBSidG,'","',cUBSTitulo,'","',cUBSSubtitulo,'","',cUBSTEjex,'","',cUBSTEjey,'",',final,',',afCustom,'],', sep=""))
  }else{
    return(paste('["creaColumnaAutoColor","',cUBSidG,'","',cUBSTitulo,'","',cUBSSubtitulo,'","',cUBSTEjex,'","',cUBSTEjey,'",',final,',',afCustom,'],', sep=""))
  }
}
# -------------------------------------------------------------------------------------------------------------------------------------------------frecuenciaSimple


# frecuenciaSimplePorCruce -------------------------------------------------------------------------------------------------------------------------------------------------
# input : -Mi base de datos
#         -Un vector con los nombres de las variables a agrupar y cuyos "levels" de respuesta (en la base) sean exactamente los mismos
#         -Un string que puede ser 1 para "freq", o 2 para "custom", para saber si los porcentajes se calculan sobre la suma de frecuencias o sobre el total de casos (rows de la base) o sobre un valor dado
#         -logical, ¿Ordeno los datos de mayor a menor?
#         -limite, ¿eliminar a partir de qué valor en pct (porcentaje) i.e. 1, elimina todos los row con porcentaje menor a 1
#         -custom: ¿cual es el valor sobre el que voy a dividir frecuencias, en caso de que se pida un "custom" ?
#         -colores: un vector de lenght igual al número de opciones de respuesta de mis variables a agrupar, que contenga los colores (que se le van a pasar a HighCharts)
#
# en json:
# creaColumna(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos)

frecuenciaSimplePorCruce<-function(aSbase, aSvector,variable2,aSSumaCual=1, aSsort=TRUE, aSlim=0, aSCustom=2100, aSColores=NULL, ScUBSidG, ScUBSTitulo="", ScUBSSubtitulo="", ScUBSTEjex="", ScUBSTEjey="", variablesPaCruce= c("SEXO","R.Edad", "NSE","Articulos","Zona","A9.CRUCE"), banco=T){
  # Este es un relajo, va a mandar un par de funciones, primero va a pintar su espacio y luego manda a poblar cada uno de sus espacios
  # 2 SEXO, 3 R.Edad, 4 NSE, 5 Articulos, 6 Zona, 7 A9.CRUCE
  variablesPaCruce<- variablesPaCruce[as.numeric(variable2) - 1]
  aSvector2<- c(aSvector, variablesPaCruce)
  aSvector2 <- match(aSvector2,names(aSbase))
  sub_baseS<-aSbase%>%select(aSvector2)
  
  finalS<- NULL
  
  variablesTotales<- length(levels(sub_baseS[,variablesPaCruce]))
  # Creo mis espacios
  finalS<- paste(finalS, '["creaContainerMultiple","#',ScUBSidG,'", "", ',variablesTotales,',"',ScUBSidG,'"],',sep="")
  for(fspc in 1:variablesTotales){
    if(fspc==1 | fspc==2){corrida<- 0 }
    if(fspc==3 | fspc==4){corrida<- 1 }
    if(fspc==5 | fspc==6){corrida<- 2 }
    if(fspc==7 | fspc==8){corrida<- 3 }
    # Ok, vamos en la fila fspc
    if(fspc%%2==1){
      #elemento non, por lo tanto se va a A
      milugar <- paste(ScUBSidG, corrida, "A",sep="")
    }else{
      milugar <- paste(ScUBSidG, corrida, "B",sep="")
    }
    respuestaS<-levels(sub_baseS[,variablesPaCruce])[fspc]
    sub_baseSR<- sub_baseS[sub_baseS[,variablesPaCruce]==respuestaS,]
    
    if(!banco){
      aSCustomFinal<- aSCustom
      aSSumaCual<-1
    }else{
      aSCustomFinal<-nrow(sub_baseSR)
      aSSumaCual<-2
    }
    
    #Correción
    # finalS<- paste(finalS,frecuenciaSimple(sub_baseSR, aSvector, aSSumaCual, aSsort, aSlim, aSCustom, aSColores, milugar, cUBSTitulo="", cUBSSubtitulo=respuestaS, cUBSTEjex="", cUBSTEjey=""),sep="")
    finalS<- paste(finalS,frecuenciaSimple(sub_baseSR, aSvector, aSSumaCual, aSsort, aSlim, aSCustomFinal, aSColores, milugar, cUBSTitulo="", cUBSSubtitulo=respuestaS, cUBSTEjex="", cUBSTEjey="", T),sep="")
  }
  return(finalS)
}
# -------------------------------------------------------------------------------------------------------------------------------------------------frecuenciaSimplePorCruce



# jsonTabla -------------------------------------------------------------------------------------------------------------------------------------------------
# input : -Mi base de datos
#         -Mi variable 1
#         -Mi variable 2
#         -Sobre donde suma 100? 1:fila, 2:columna,3:casos
#         -el id donde se va a pintar la tabla de contingencia
#
# en js: creaTabla(cNTabidObjetivo,cNTabpalabras)

miTablaDeContingencia<- function(mAPSbase, mAPSvariable1, mAPSvariable2, mAPSSumaCual, paJs){
#     mAPSbase<- base
#     mAPSvariable1<-"Articulos"
#     mAPSvariable2<- "SEXO"
#     mAPSSumaCual<- 1
#     paJs<- c("id")
#   
  mitabla<- as.data.frame.matrix(table(mAPSbase[,mAPSvariable1], mAPSbase[,mAPSvariable2]))
  mitabla$Total<- rowSums(mitabla)
  mitabla<- rbind(mitabla, colSums(mitabla))
  row.names(mitabla)[nrow(mitabla)]<- "Total"
  mitabla[,mAPSvariable1]<- row.names(mitabla)
  mitabla<-mitabla[,c(length(mitabla), 1:(length(mitabla)-1)) ]
  mitablaCasos<- mitabla
  mitablaPct<- mitabla
  if(mAPSSumaCual==3){
    miFinal<-toJSON(mitablaCasos, byrow=T, colNames=T, .na="0")
  }else if(mAPSSumaCual==1) {
    mitablaPct[,2:(length(mitabla)-1)]<- round(prop.table(as.matrix(mitabla[,2:(length(mitabla)-1)]), margin=1)*100,0)
    mitablaPct$Total<- 100
    miFinal<-toJSON(mitablaPct, byrow=T, colNames=T, .na="0")
  }else if(mAPSSumaCual==2) {
    mitablaPct<- mitablaPct[1: (nrow(mitablaPct)-1),]
    mitablaPct[,2:length(mitablaPct)]<- round(prop.table(as.matrix(mitablaPct[,2:length(mitablaPct)]), margin=2)*100,0)  
    ttmmmppp<- mitablaPct[1,]
    ttmmmppp[1,]<-100
    row.names(ttmmmppp)<- "Total"
    ttmmmppp[1,1]<- "Total"
    mitablaPct<- rbind(mitablaPct,ttmmmppp )
    miFinal<- toJSON(mitablaPct, byrow=T, colNames=T, .na="0")
  }
return(paste('["creaTabla", "#',paJs,'", ',miFinal,'],', sep=""))
}
# -------------------------------------------------------------------------------------------------------------------------------------------------miTablaDeContingencia



# dameunDFdeParejas-------------------------------------------------------------------------------------------------------------------------------------------------
# Cuando tengo una matriz, tal que, cada opcion de respuesta puede estar en cada variable. e.g. A26, i.e. busco en mis variables1 cada opcion de respuesta y saco su variable2


dameunDFdeParejas<- function(evvbase, evvVar1, evvVar2, evvTodasLasRespuestas=list(Si=T, cuales=NULL)){
   # evvbase<- base
#   evvVar1<- c("A26M.1", "A26M.2", "A26M.3", "A26M.4", "A26M.5", "A26M.6")
#   evvVar2<-c("A26E.1", "A26E.2", "A26E.3", "A26E.4", "A26E.5", "A26E.6")
  
#   evvVar1<- c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8")
#   evvVar2<- c("A15Meses.1RR","A15Meses.2RR","A15Meses.3RR","A15Meses.4RR","A15Meses.5RR","A15Meses.6RR","A15Meses.7RR","A15Meses.8RR")
#   
#    evv1TodasLasRespuestas=list(Si=F, cuales=c(1,15,8,9,17))
  
  if(evvTodasLasRespuestas$Si){
    variablesTotales<- length(levels(evvbase[,evvVar1[1]]))
    levelsTotales<- levels(evvbase[,evvVar1[1]])
  }else
  {
    variablesTotales<- length(levels(evvbase[,evvVar1[1]])[evvTodasLasRespuestas$cuales])
    levelsTotales<- levels(evvbase[,evvVar1[1]])[evvTodasLasRespuestas$cuales]
  }
  
  # Para cada level, para cada var1
  dffinal<- data.frame()
  for(i in 1: length(levelsTotales)){
#   i<-1
    dftemporal<- data.frame()
    dftemporalTT<- data.frame()
    totalCasos<- 0
    for(t in 1: length(evvVar1)){
#   t<-2
#   sumttc
      vectorTrue<- evvbase[,evvVar1[t]] == levelsTotales[i]
      sumttc<-as.data.frame(table(vectorTrue))[2,2]
      if(length(sumttc)>0){
        if(!is.na(sumttc)){
          totalCasos<- totalCasos + sumttc
          if(is.na(totalCasos)){print(t)}
        }
      }
      
      for(w in 1: length(evvVar2)){
        # w<- 1
        if(w==1){
          dftemporal<- as.data.frame(table(evvbase[vectorTrue,evvVar1[t]], evvbase[vectorTrue,evvVar2[w]]))
          dftemporal<- dftemporal[dftemporal[1]==levelsTotales[i],]  
        }else{
          dftemporal2<- as.data.frame(table(evvbase[vectorTrue,evvVar1[t]], evvbase[vectorTrue,evvVar2[w]]))
          dftemporal2<- dftemporal2[dftemporal2[1]==levelsTotales[i],]  
          dftemporal$Freq<- dftemporal$Freq + dftemporal2$Freq
          # dftemporal<- cbind(dftemporal,dftemporal2)
        }        
      }
      
      if(t==1){
        dftemporalTT<- dftemporal
      }else{
        dftemporalTT$Freq<- dftemporalTT$Freq + dftemporal$Freq
      }
    }
    dftemporalTT$totalCasos<- totalCasos
    dffinal<- rbind(dffinal, dftemporalTT)
  }
  return(dffinal)
}
# -------------------------------------------------------------------------------------------------------------------------------------------------dameunDFdeParejas


# frecuenciaSimplePorCruceMultiple -------------------------------------------------------------------------------------------------------------------------------------------------
# IMPORTANTE: sólo estamos juntanto UNA pregunta, i.e. al final en la app SÓLO PUEDES FILTRAR POR UNA VARIABLE, i.e. todos los levels del conjunto de variables que paso son iguales
# input : -Mi base de datos
#         -Un vector con los nombres de las variables a agrupar y cuyos "levels" de respuesta (en la base) sean exactamente los mismos
#         -Un string que puede ser 1 para "freq", o 2 para "custom", para saber si los porcentajes se calculan sobre la suma de frecuencias o sobre el total de casos (rows de la base) o sobre un valor dado
#         -logical, ¿Ordeno los datos de mayor a menor?
#         -limite, ¿eliminar a partir de qué valor en pct (porcentaje) i.e. 1, elimina todos los row con porcentaje menor a 1
#         -custom: ¿cual es el valor sobre el que voy a dividir frecuencias, en caso de que se pida un "custom" ?
#         -colores: un vector de lenght igual al número de opciones de respuesta de mis variables a agrupar, que contenga los colores (que se le van a pasar a HighCharts)
#
# en json:
# creaColumna(cUBSidG,cUBSTitulo,cUBSSubtitulo,cUBSTEjex,cUBSTEjey,cUBSDatos)
# agrupaFreq1(base, c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), "custom", TRUE, 2, 2100, c('a'))
frecuenciaSimplePorCruceMultiple<-function(aSbase, aSvector,aSSumaCual=1, aSsort=TRUE, aSlim=0, aSCustom=2100, aSColores=NULL, ScUBSidG, ScUBSTitulo="", ScUBSSubtitulo="", ScUBSTEjex="", ScUBSTEjey="", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=TRUE, cuales=NA)){
  # Está cañón: para cada variable que yo saque, me tengo que quedar con sus respuestas... y agruparlas
#   variablesPaCruce<- c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8")
#   aSvector<- c("A16.1.1","A16.1.2","A16.1.3","A16.2.1","A16.2.2","A16.2.3","A16.3.1","A16.3.2","A16.3.3","A16.4.1","A16.4.2","A16.4.3","A16.5.1","A16.5.2","A16.5.3","A16.6.1","A16.6.2","A16.6.3","A16.7.1","A16.7.2","A16.7.3","A16.8.1","A16.8.2","A16.8.3")
# #                 variablesPaCruce<- c("A26M.1", "A26M.2", "A26M.3", "A26M.4", "A26M.5", "A26M.6")
# #                 aSvector<-c("A26E.1", "A26E.2", "A26E.3", "A26E.4", "A26E.5", "A26E.6")
# #   
#   aSbase<- base
#   aSSumaCual<-1
#   aSColores<- NULL
#    todasLasRespuestas=list(Si=F, cuales=c(1,15,8,9,17))
  aSvector2<- c(aSvector, variablesPaCruce)
  aSvector2 <- match(aSvector2,names(aSbase))
  sub_baseS<-aSbase%>%select(aSvector2)

  dfFinal<- dameunDFdeParejas(sub_baseS, variablesPaCruce, aSvector, todasLasRespuestas)

  # Módulo especial, para A8, requisito 
  if(ScUBSidG=="graficaSG4"){
    #Estamos en las de A8
    dfFinal[3:13,3]<-0
  }
  
  # Tengo que hacer la gráfica para valor de respuesta
    misLevels<- unique(as.character(dfFinal$Var1))  
    finalS<- NULL
    finalS<- paste(finalS, '["creaContainerMultiple","#',ScUBSidG,'", "", ',length(misLevels),',"',ScUBSidG,'"],',sep="")

  for(tlvltI in 1:length(misLevels)){
    # tlvltI<-1
    miNombre<- misLevels[tlvltI]
    tabla<- dfFinal[dfFinal$Var1==miNombre,2:4]
    casosTabla<- tabla[1,3]
    tabla<- tabla[,1:2]
    names(tabla)<-c("name","casos")
      if (aSSumaCual==1){
        tabla<-tabla%>%mutate(data=casos/sum(casos))    
      }else if (aSSumaCual==2){
        tabla[,"data"]= tabla$casos / casosTabla
      }
      tabla$color<- aSColores
    ### mi tabla está vacía?
    if(sum(tabla[,2])>0){
      
      # y si mis porcentajes son muy bajos? que hago ? no puedo quedarme con una tabla de puros 0
      if(all(round(tabla[,3]*100, 0)==0)){
        tabla[,3]<- tabla[,3]*100
      }else{
        tabla[,3]<- round(tabla[,3]*100, 0)
      }
      
      if(aSsort){
        tabla<-tabla %>% arrange(desc(casos))
      }
      
      if(dim(tabla %>% filter(data >= aSlim))[1] == 0){
        if(dim(tabla %>% filter(data > 0))[1] == 0){
          
        }else{
          tabla<- tabla %>% filter(data > 0)
        }
      }else{
        tabla<-tabla %>% filter(data >= aSlim)
      }
      tabla<- tabla
    }else{
      tabla<- tabla
    }
    
      final<- list()
      previo<- list()
      for(tbb in 1:nrow(tabla)){
        previo$name<- tabla[tbb,1]
        previo$casos<- tabla[tbb,2]
        previo$data<- tabla[tbb,3]
        previo$color<- tabla[tbb,4]
        final[[tbb]]<- previo
      }
      tabla<-   toJSON(final, asIs=T)  
    
    fspc<-tlvltI
    if(fspc==1 | fspc==2){corrida<- 0 }
    if(fspc==3 | fspc==4){corrida<- 1 }
    if(fspc==5 | fspc==6){corrida<- 2 }
    if(fspc==7 | fspc==8){corrida<- 3 }
    if(fspc==9 | fspc==10){corrida<- 4 }
    # Ok, vamos en la fila fspc
    if(fspc%%2==1){
      #elemento non, por lo tanto se va a A
      milugar <- paste(ScUBSidG, corrida, "A",sep="")
    }else{
      milugar <- paste(ScUBSidG, corrida, "B",sep="")
    }
    
    if(length(aSColores)>0){
      finalS<- paste(finalS, '["creaColumna","',milugar,'","',ScUBSTitulo,'","',miNombre,'","',ScUBSTEjex,'","',ScUBSTEjey,'",',tabla,',',casosTabla,'],', sep="")
    }else{
      finalS<- paste(finalS, '["creaColumnaAutoColor","',milugar,'","',ScUBSTitulo,'","',miNombre,'","',ScUBSTEjex,'","',ScUBSTEjey,'",',tabla,',',casosTabla,'],', sep="")
    }
  }
  return(finalS)
}
# -------------------------------------------------------------------------------------------------------------------------------------------------frecuenciaSimplePorCruce



miA26<- function(m26base, m26vector1, m26vector2, m26Listado, m26colores, aSSumaCual, aSCustom, paseCual){
#   m26base<- base
#   m26vector1<-c("A26M.1", "A26M.2", "A26M.3", "A26M.4", "A26M.5", "A26M.6")
#   m26vector2<-c("A26E.1", "A26E.2", "A26E.3", "A26E.4", "A26E.5", "A26E.6")
#   m26Listado<- list(Si=F, cuales=c(1,15,8,9,17))
#   m26colores<- rep("A", 4)
#   aSSumaCual<- 1
#   aSCustom<- 2100

  
  dfFull<- dameunDFdeParejas(m26base, m26vector1, m26vector2, m26Listado)
  m26ejeX<-   unique(as.character(dfFull$Var1))
  respuestas<- unique(as.character(dfFull$Var2))
  
  tablaFinal<- data.frame()
  for(tii in 1:length(m26ejeX)){
    tabla<- dfFull[dfFull$Var1==m26ejeX[tii],2:3]
    names(tabla)<-c("name","casos")
    if (aSSumaCual==1){
      tabla$data<-tabla$casos/sum(tabla$casos)
    }else if (aSSumaCual==2){
      tabla[,"data"]= tabla$casos / aSCustom
    }
    tabla[,3]<- round(tabla[,3]*100, 0)
    tablaFinal<- rbind(tablaFinal, tabla)
  }
  tablaFinal
  ### 
  respuestas
  final<- list()
  previo<- list()
  for(tbb in length(respuestas):1){
    tabla<- tablaFinal[tablaFinal$name==respuestas[tbb],]
    previo$name<- tabla[1,1]
    previo$casos<- tabla[,2]
    previo$data<- tabla[,3]
    final[[length(final)+1]]<- previo
  }
  tabla<-toJSON(final, asIs=T)
  ###
  final26<- paste('["creaColumnaApilada","graficaSH3","¿Cómo evalúa de manera general a ... ?","Para las marcas que consideraría y que usó la última vez",',toJSON(m26ejeX),',"Porcentaje",',tabla,',',toJSON(m26colores),',',paseCual,',],', sep="")
  return(final26)
}

