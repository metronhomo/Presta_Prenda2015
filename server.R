library(shiny)

 base <- readRDS("www/base.rds")
 # base<- readRDS("/Users/JM/Desktop/BasicShiny/00 Gamma1/www/base.rds")

levels(base$Zona)[levels(base$Zona)=="Zona Suereste"] <- "Zona Sureste"
base$A9.CRUCE<- droplevels(base$A9.CRUCE)

############## Cosas que no necesito volver a recalcular

for(i in 1:8){
  nombreAño<-paste("A15A.",i, sep="")
  nombreMes<-paste("A15AM.",i, sep="")
  nombreFinal<- paste("A15Meses.",i, sep="")
  base[,nombreAño][is.na(base[,nombreAño])]<- 0
  base[,nombreMes][is.na(base[,nombreMes])]<- 0
  base[,nombreFinal]<- base[,nombreAño]*12 + base[,nombreMes]
  base[,nombreFinal][base[,nombreFinal]==0]<- NA
}

suciasA15Meses<- c("A15Meses.1","A15Meses.2","A15Meses.3","A15Meses.4","A15Meses.5","A15Meses.6","A15Meses.7","A15Meses.8")
intervalosA15Meses<- list(
  # Primer intervalo cerrado, segundo abierto
  # Asumo nombre del conjunto como el nombre de la etiqueta, y el orden es el level
  "Hasta 1 mes" = c(1,2),
  "2 meses" = c(2,3),
  "3 meses" = c(3,4),
  "4 meses" = c(4,5),
  "5 meses" = c(5,6),
  "6 meses" = c(6,7),
  "7 meses" = c(7,8),
  "8 meses" = c(8,9),
  "9 meses" = c(9,10),
  "10 meses o más" = c(10,100)
)
base<- creameIntervalos(base, suciasA15Meses, intervalosA15Meses)
table(base$A8.1, base$A15Meses.2RR)

# Calculemos el "costo del préstamo"
for(i in 1: 8){
  var1<- paste("A19.",i,sep="")
  var2<- paste("A17.",i,sep="")
  nombre<- paste("variacion",i,sep="")
  base[,nombre]<- ((base[,var1]/base[,var2])-1)*100
  # base[is.na(base[,nombre]),nombre]<-0
}

suciasA19Meses<- c("variacion1","variacion2","variacion3","variacion4","variacion5","variacion6","variacion7","variacion8")
intervalosVariacion<- list(
  # Primer intervalo cerrado, segundo abierto
  # Asumo nombre del conjunto como el nombre de la etiqueta, y el orden es el level
  "De 0% a 5%" = c(0,5),
  "De 5% a 10%" = c(5,10),
  "De 10% a 15%" = c(10,15),
  "De 15% a 20%" = c(15,20),
  "De 20% a 25%" = c(20,25),
  "De 25% a 30%" = c(25,30),
  "De 30% a 35%" = c(30,35),
  "De 35% a 40%" = c(35,40),
  "Más de 40%" = c(40,1200)
)
base<- creameIntervalos(base, suciasA19Meses, intervalosVariacion)

for(i in 1:6){
  variable<- paste("A26E.",i,sep="")
  base[,variable]<- factor(base[,variable], levels = c(1:4), labels = c("Muy malo", "Malo", "Bueno", "Muy bueno"))
}

for(i in 1:8){
  variable<- paste("A22.",i,sep="")
  base[,variable]<- factor(base[,variable], levels = c(0:12), labels = c("Nunca","1 vez", "2 veces", "3 veces","4 veces","5 veces","6 veces","7 veces","8 veces","9 veces","10 veces","11 veces", "12 veces"))
}


# table(base$A22.2)
# 
# base<- readRDS("/Users/JM/Desktop/BasicShiny/00 Gamma1/www/base.rds")

base$A22.1[is.na(base$A22.1)]<-0

target<- c("F4TN.1", "F4TD.1", "F4TC.1","F4CH.1", "F4CC.1", "F4IN.1", "F4SE.1", "F4AF", "F4CH.1.A", "F4CA.1", "F4CE", "F4OTC")
for(i in 1: length(target)){
  nombreVariable <- paste("producto",i, sep="")
  vectorTrue<- is.na(base[,target[i]])
  base[,nombreVariable]<-i
  base[vectorTrue,nombreVariable]<-0
  base[,nombreVariable]<- factor(base[,nombreVariable], levels = c(1:12), labels = c("Tarjeta de nómina", "Tarjeta de débito", "Tarjeta de crédito", "Cuenta de ahorro", "Cuenta de cheques", "Inversiones", "Seguros", "Afore", "Crédito hipotecario", "Crédito automotriz", "Crédito empresarial", "Otro"))
}

target<- c("A13.1", "A13.2", "A13.3","A13.4", "A13.5", "A13.6", "A13.7", "A13.7.A")
for(i in 1: length(target)){
  nombreVariable <- paste("miA13R",i, sep="")
  vectorTrue<- base[,target[i]]=="Si"
  vectorTrue[is.na(vectorTrue)]<- FALSE
  base[,nombreVariable]<-0
  if(i < 8 ){base[vectorTrue,nombreVariable]<-i}else{base[vectorTrue,nombreVariable]<-7}
  base[,nombreVariable]<- factor(base[,nombreVariable], levels = c(1:7), labels = c("Presta Prenda", "Prenda Mex", "First Cash", "Nacional Monte de Piedad", "Prendinero", "Dondé Casa de Empeño", "Otro"))
}

# Medios
target<-c("A5E.TV.1","A5E.RE.1","A5E.CA.1","A5E.ME.1","A5E.PAE.1","A5E.PO.1","A5E.IN.1","A5E.RS.1","A5E.RA.1","A5E.OTR.1","A5E.TV.8","A5E.RE.8",
                                       "A5E.CA.8","A5E.ME.8","A5E.PAE.8","A5E.PO.8","A5E.IN.8","A5E.RS.8","A5E.RA.8","A5E.OTR.8","A5E.TV.9","A5E.RE.9","A5E.CA.9","A5E.ME.9",
                                       "A5E.PAE.9","A5E.PO.9","A5E.IN.9","A5E.RS.9","A5E.RA.9","A5E.OTR.9","A5E.TV.15","A5E.RE.15","A5E.CA.15","A5E.ME.15",
                                       "A5E.PAE.15","A5E.PO.15","A5E.IN.15","A5E.RS.15","A5E.RA.15","A5E.OTR.15","A5E.TV.17","A5E.RE.17","A5E.CA.17","A5E.ME.17","A5E.PAE.17","A5E.PO.17","A5E.IN.17","A5E.RS.17",
                                       "A5E.RA.17","A5E.OTR.17","A6A.TV.1", "A6A.RE.1", "A6A.CA.1", "A6A.ME.1", "A6A.PAE.1","A6A.PO.1", "A6A.IN.1", "A6A.RS.1", "A6A.RA.1", "A6A.TV.8", "A6A.RE.8", "A6A.CA.8", "A6A.ME.8", "A6A.PAE.8","A6A.PO.8", "A6A.IN.8", "A6A.RS.8", "A6A.RA.8",
                  "A6A.TV.9", "A6A.RE.9", "A6A.CA.9", "A6A.ME.9", "A6A.PAE.9","A6A.PO.9", "A6A.IN.9", "A6A.RS.9", "A6A.RA.9","A6A.TV.15","A6A.RE.15","A6A.CA.15","A6A.ME.15","A6A.PAE.15","A6A.PO.15",
                  "A6A.IN.15","A6A.RS.15","A6A.RA.15","A6A.TV.17","A6A.RE.17","A6A.CA.17","A6A.ME.17","A6A.PAE.17","A6A.PO.17","A6A.IN.17","A6A.RS.17","A6A.RA.17")
targetLevels<- levels(base[,target[1]])
for(i in 1: length(targetLevels)){
miLevel<- targetLevels[i]
nombreVariable <- paste("medio",i, sep="")
base[,nombreVariable]<-0
vectorTrue<- rep(FALSE, nrow(base))
  for(t in 1: length(target)){
  targetTempp<- target[t]
  vectorTrue[base[,target[t]]==miLevel]<- TRUE
  }
base[vectorTrue,nombreVariable]<-i
base[,nombreVariable]<- factor(base[,nombreVariable], levels = c(1:10), labels = c("Televisión","Revista","Camiones","Metro","Pantallas Elektra/Banco Azteca","Posters","Internet","Redes Sociales","Radio","Otros"))
}
# Medios

## Necesito crear las columnas para "los otros" de A13
targetVars<- list(
  list("A13.1",1),
  list("A13.2",17),
  list("A13.3",8),
  list("A13.4",15),
  list("A13.5",20),
  list("A13.6",9))
targetLevelsEspecial<- c(1,15,8,9,17,20)
targetLevels<- levels(base$MarcaOtros.1)
misA13<- NULL
for(i in 1:length(targetLevels)){
  if(i %in% targetLevelsEspecial){
    nombreVariable <- paste("miA13N",i, sep="")
    misA13<- c(misA13, nombreVariable)
    for(t in 1: length(targetVars)){
      if(targetVars[[t]][[2]]==i)
      subTargetVar<- targetVars[[t]][[1]]
    }
    vectorTrue<- base[,subTargetVar]=="Si"
    vectorTrue[is.na(vectorTrue)]<- FALSE
    base[,nombreVariable]<-0
    base[vectorTrue,nombreVariable]<-i
    base[,nombreVariable]<- factor(base[,nombreVariable], levels = c(1:length(targetLevels)), labels = targetLevels)
  }else{
    nombreVariable <- paste("miA13N",i, sep="")
    misA13<- c(misA13, nombreVariable)
    quiensoy<- targetLevels[i]
    vectorTrue<- (base[,"A13.7"]=="Si" & base[,"MarcaOtros.1"]==quiensoy) | (base[,"A13.7.A"]=="Si" & base[,"MarcaOtros.2"]==quiensoy)
    vectorTrue[is.na(vectorTrue)]<- FALSE
    base[,nombreVariable]<-0
    base[vectorTrue,nombreVariable]<-i
    base[,nombreVariable]<- factor(base[,nombreVariable], levels = c(1:length(targetLevels)), labels = targetLevels)
  }
}
#########################


suciasA18<- c("A18.1", "A18.2", "A18.3", "A18.4", "A18.5", "A18.6", "A18.7", "A18.8")

intervalosA18<- list(
  # Primer intervalo cerrado, segundo abierto
  # Asumo nombre del conjunto como el nombre de la etiqueta, y el orden es el level
  "De 0% a 3%" = c(0,3),
  "De 3% a 4%" = c(3,4),
  "De 4% a 5%" = c(4,5),
  "De 5% a 6%" = c(5,6),
  "De 6% a 7%" = c(6,7),
  "De 7% a 8%" = c(7,8),
  "De 8% a 9%" = c(8,9),
  "De 9% a 10%" = c(9,10),
  "De 10% a 11%" = c(10,11),
  "Mayor a 11%" = c(11,98),
  "Ns/Nc" = c(98,99)
)

base<- creameIntervalos(base, suciasA18, intervalosA18)

coloresBancos<- c("#7441A5", "#0B3954", "#3C787E", "#F7B32B", "#F2FA88", "#434DB0", "#AF9B46", "#D70230",  "#EF4640", "#7B0828", "#DFD6A7", "#06AED5", "#1F1300", "#DBAD6A", "#5B7830", "#C0DF33", "#6F2735")

clrzTop<- rep("#593A6A", 76)
clrzTop[15]<- "#53131E"
clrzTop[8]<- "#45462A"
clrzTop[1]<- "#F19A3E"
clrzTop[9]<- "#5B8C5A"
clrzTop[17]<- "#98BDEE"

coloresRandom<- c("#6706CA","#1D7B69","#F08F05","#C71922","#5A98E5","#B4DCBA","#6C8D8E","#78EF7E","#5467D8","#9232B2","#866532",
                  "#F9F777","#6FF4F7","#34B9B9","#25C677","#8FEF51","#24B1C2","#922871","#FB5B51","#5761EE","#87E773","#8983B4",
                  "#6469CE","#AC908E","#1D68F4","#68C4F3","#87D0B8","#42CBB9","#95625E","#07C60C","#B647B3","#B3B837","#189D35",
                  "#7CBDF1","#CF8968","#51FA9B","#E4F9D3","#1EBBCF","#03CB1B","#CEB582","#EE3E8D","#96280F","#B2E7DA","#6EE290",
                  "#E6F9C7","#BA5F8E","#D24FA5","#F67DD5","#83376D","#7436DF","#E9A382","#25475F","#C55422","#9B6D2C","#AA0E0E",
                  "#DE7720","#A46FBB","#E814C4","#66FD42","#9ECACD","#8AB02E","#0C27CE","#3A74F3","#86A73E","#ADD67A","#304F16",
                  "#D1309F","#FCA609","#A8F403","#3A0118","#B48F39","#61B914","#901371","#D19D6A","#0941A6")

coloresGradiente<- rev(rainbow_hcl(14))

listadoAbandono<- list(
  A9bMAR.1= list(
    nube = "TA9b.1",
    variables = c("A9.1COD1", "A9.1COD2")
  ),
  
  A9CMAR.2= list(
    nube = "TA9b.2",
    variables = c("A9.2COD1", "A9.2COD2")
  ),
  
  A9DMAR.3= list(
    nube = "TA9b.3",
    variables = c("A9.3COD1", "A9.3COD2")
  ),
  
  A9EMAR.4= list(
    nube = "TA9b.4",
    variables = c("A9.4COD1")
  )
)

listadoRazonesEmpeño<- list(
  A9.CRUCE= list(
    nube = "TA11NUBE",
    variables = c("A11.1COD","A11.2COD","A11.3COD","A11.4COD","A11.5COD")
  )
)

listadoMotivosEmpeño<- list(
  A9.CRUCE= list(
    nube = "TA12NUBE",
    variables = c("A12.1COD","A12.2COD","A12.3COD","A12.4COD","A12.5COD")
  )
)

listadoA30<- list(
  A30= list(
    nube=c("TA30bNUBE"),
    variables=c("A30b.1", "A30b.2")
  )
)

listadoA31<- list(
  A31= list(
    nube=c("A31b.NUBE"),
    variables=c("A31b.1", "A31b.2", "A31b.3", "A31b.4", "A31b.5")
  )
)

sum(table(base$producto1))

# agrupaFreq1(base, c("A31b.1", "A31b.2", "A31b.3", "A31b.4", "A31b.5"), "freq", TRUE, 1, 0, c("A"))
#################################################
library(shiny)

shinyServer(function(input, output, session) {
  # Mis valores de filtros
  filtrosChecklist <- reactive({list(sexo= c(input$formSexo0,input$formSexo1),edad= c(input$formEdad0,input$formEdad1,input$formEdad2,input$formEdad3),nse= c(input$formNSE0,input$formNSE1,input$formNSE2,input$formNSE3),articulos = c(input$formObj0,input$formObj1,input$formObj2,input$formObj3),zona = c(input$formZona0, input$formZona1, input$formZona2, input$formZona3, input$formZona4, input$formZona5),firma = c(input$formFirma0, input$formFirma1, input$formFirma2, input$formFirma3, input$formFirma4, input$formFirma5))})
  # Mi lista para pasar a mi función de filtro
  listaFiltros <- reactive({ list( sexo=list( var="SEXO", valores=c(1, 2), etiquetas=c("Mujer", "Hombre"), actual=filtrosChecklist()$sexo ), edad=list( var="R.Edad", valores=c(1,2,3,4), etiquetas=c("24 - 34", "35 - 44", "45 - 54", "55 - 60"), actual=filtrosChecklist()$edad ), NSE=list( var="NSE", valores=c(1,2,3,4), etiquetas=c("C", "C-", "D+", "D"), actual=filtrosChecklist()$nse ), articulos=list( var="Articulos", valores=c(1,2,3,4), etiquetas=c("Oro", "Relojes o diamantes", "Coches", "Otras mercancias"), actual=filtrosChecklist()$articulos ), zona=list( var="Zona", valores=c(1,2,3,4,5,6), etiquetas=c("Golfo", "AMCM", "Norte", "Centro", "Sureste", "Occidente"), actual=filtrosChecklist()$zona ), firma=list( var="A9.CRUCE", valores=c(1,2,3,4,5,6), etiquetas=c("Presta Prenda", "First Cash", "Fundación Rafael Dondé", "Nacional Monte de Piedad", "Prendamex", "Otros"), actual=filtrosChecklist()$firma ) ) })
  
  # Mi base de datos
  basem<- reactive({basem<- filtro(base, listaFiltros())})
  
  # Mis escuchadores para la sección que esté activa
  seccionActiva<- reactiveValues(valor = NULL)
  observeEvent(input$SBAction, {seccionActiva$valor <- "SB"})
  observeEvent(input$SCAction, {seccionActiva$valor <- "SC"})
  observeEvent(input$SDAction, {seccionActiva$valor <- "SD"})
  observeEvent(input$SEAction, {seccionActiva$valor <- "SE"})
  observeEvent(input$SFAction, {seccionActiva$valor <- "SF"})
  observeEvent(input$SGAction, {seccionActiva$valor <- "SG"})
  observeEvent(input$SHAction, {seccionActiva$valor <- "SH"})
  observeEvent(input$SIAction, {seccionActiva$valor <- "SI"})
  observeEvent(input$SJAction, {seccionActiva$valor <- "SJ"})
  
  # Mis valores gráfica de Venn
  filtrosVenn<- reactive({
    list(
      selectorMarca = input$graficaSE3Select,
      selectorTotal = input$graficaSE3Select2,
      filtros = c(input$graficaSE31, input$graficaSE32, input$graficaSE33, input$graficaSE34, input$graficaSE35, input$graficaSE36, input$graficaSE37, input$graficaSE38, input$graficaSE39, input$graficaSE40)
    )
  })
  # Todo para mi gráfica de venn
  output$graficaSE3 <- renderImage({
    vnnnSelect<- filtrosVenn()$filtros
    vnnnSelect<- c(1:10)[vnnnSelect]
    vnnnNombre<- filtrosVenn()$selectorMarca
    vnnnSumaCual<- filtrosVenn()$selectorTotal
    # Cuál es mi subconjunto?
    if(vnnnNombre=="Total"){
      vnnnVector<- mivnnnVector(99, vnnnSumaCual)
    }
    if(vnnnNombre=="Nacional Monte de Piedad"){
      vnnnVector<- mivnnnVector(15, vnnnSumaCual)
    }
    if(vnnnNombre=="Presta Prenda"){
      vnnnVector<- mivnnnVector(1, vnnnSumaCual)
    }
    if(vnnnNombre=="First Cash"){
      vnnnVector<- mivnnnVector(8, vnnnSumaCual)
    }
    if(vnnnNombre=="Fundación Rafael Dondé"){
      vnnnVector<- mivnnnVector(9, vnnnSumaCual)
    }
    if(vnnnNombre=="Prendamex"){
      vnnnVector<- mivnnnVector(17, vnnnSumaCual)
    }
    # ok.... ¿porqué tengo que filtrar? si al final me quedo con todas las personas que dijeron "Presta Prenda", con quedarme
    # con sus columnas de medios me basta...¿no?
    #     basesub<-filtroVenn(basem(),
    #                         fvnnvariables=c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10",
    #                                         "A4.1","A4.2","A4.3","A4.4","A4.5","A4.6","A4.7","A4.8","A4.9","A4.10"),
    #                         fvnncategorias=vnnnNombre)
    basesub<-basem() %>% select(one_of(vnnnVector))
    outfile = tempfile(fileext='.png')
    png(outfile,width=30,height=18,units="cm",res=100, bg = "transparent")
    graficaVenn(basesub,vnnnSelect)
    dev.off()
    list(src = outfile,
         contentType = 'image/png')
  }, deleteFile = TRUE) 
  
  

  # the good stuff
  miColeccionFunciones<- reactive({
    
    miTotal <- nrow(basem())
    
    mifake<- filtrosVenn()
    
    if (!is.null(seccionActiva$valor)) {
      seccActiv <- seccionActiva$valor
    } else seccActiv <- ""
    
    #    mifake<- filtrosVenn()
    
    micol=''
    if(seccionActiva$valor=="SB"){
      return(
        paste(
          miTablaDeContingencia(basem(), "Articulos", "SEXO", input$graficaSB11Select, "graficaSB11"),
          miTablaDeContingencia(basem(), "Articulos", "R.Edad", input$graficaSB12Select, "graficaSB12"),
          miTablaDeContingencia(basem(), "Articulos", "NSE", input$graficaSB13Select, "graficaSB13"),
          miTablaDeContingencia(basem(), "Articulos", "Zona", input$graficaSB14Select, "graficaSB14"),
          miTablaDeContingencia(basem(), "Articulos", "A9.CRUCE", input$graficaSB15Select, "graficaSB15"),
          "[]",sep="")
      )
    }
    
    if(seccionActiva$valor=="SC"){
      return(
        paste(
          if(input$graficaSC1s1Select==1){
            frecuenciaSimple(basem(), "F3", afSumaCual=2, afsort=F, aflim=0, afCustom=miTotal, afColores=c("#3D8C7D","#F67C09"), "graficaSC1s1", cUBSTitulo="¿Cuenta con algún producto bancario como: tarjeta de crédito, debito/nómina, ahorro, inversión, cuenta de cheques, seguros, afore en algún banco?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(), "F3", afSumaCual=2, afsort=F, aflim=0, afCustom=miTotal, afColores=c("#3D8C7D","#F67C09"), "graficaSC1s1", cUBSTitulo="¿Cuenta con algún producto bancario como: tarjeta de crédito, debito/nómina, ahorro, inversión, cuenta de cheques, seguros, afore en algún banco?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), "F3", input$graficaSC1s1Select,aSSumaCual=2,aSsort=F, aSlim=0, aSCustom=miTotal, aSColores=c("#3D8C7D","#F67C09"), "graficaSC1s1", ScUBSTitulo="¿Cuenta con algún producto bancario como: tarjeta de crédito, debito/nómina, ahorro, inversión, cuenta de cheques, seguros, afore en algún banco?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje",banco=T),sep="")},
          
          if(input$graficaSC1s2Select==1){
            frecuenciaSimple(basem(), c("producto1", "producto2", "producto3","producto4", "producto5", "producto6", "producto7", "producto8", "producto9", "producto10", "producto11", "producto12"), afSumaCual=2, afsort=T, aflim=0, afCustom=sum(table(basem()$F3)[1]), afColores=coloresRandom[1:12], "graficaSC1s2", cUBSTitulo="¿Cuenta con algún producto bancario como: tarjeta de crédito, debito/nómina, ahorro, inversión, cuenta de cheques, seguros, afore en algún banco?", cUBSSubtitulo="Total de personas por producto bancario", cUBSTEjex="Productos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(), c("producto1", "producto2", "producto3","producto4", "producto5", "producto6", "producto7", "producto8", "producto9", "producto10", "producto11", "producto12"), afSumaCual=2, afsort=T, aflim=0, afCustom=sum(table(basem()$F3)[1]), afColores=coloresRandom[1:12], "graficaSC1s2", cUBSTitulo="¿Cuenta con algún producto bancario como: tarjeta de crédito, debito/nómina, ahorro, inversión, cuenta de cheques, seguros, afore en algún banco?", cUBSSubtitulo="Total de personas por producto bancario", cUBSTEjex="Productos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("producto1", "producto2", "producto3","producto4", "producto5", "producto6", "producto7", "producto8", "producto9", "producto10", "producto11", "producto12"), input$graficaSC1s2Select,aSSumaCual=2,aSsort=T, aSlim=0, aSCustom=sum(table(base$F3)[1]), aSColores=coloresRandom[1:12], "graficaSC1s2", ScUBSTitulo="¿Cuenta con algún producto bancario como: tarjeta de crédito, debito/nómina, ahorro, inversión, cuenta de cheques, seguros, afore en algún banco?", ScUBSSubtitulo="Total de personas por producto bancario", ScUBSTEjex="Productos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC2Select==1){
            frecuenciaSimple(basem(),c("F4TN.1","F4TN.2","F4TN.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto1)), afColores=coloresBancos, "graficaSC2", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Tarjeta de nómina</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4TN.1","F4TN.2","F4TN.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto1)), afColores=coloresBancos, "graficaSC2", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Tarjeta de nómina</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4TN.1","F4TN.2","F4TN.3"), input$graficaSC2Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto1)), aSColores=coloresBancos, "graficaSC2", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Tarjeta de nómina</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC3Select==1){
            frecuenciaSimple(basem(),c("F4TD.1", "F4TD.2", "F4TD.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto2)), afColores=coloresBancos, "graficaSC3", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Tarjeta de débito</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4TD.1", "F4TD.2", "F4TD.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto2)), afColores=coloresBancos, "graficaSC3", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Tarjeta de débito</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4TD.1", "F4TD.2", "F4TD.3"), input$graficaSC3Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto2)), aSColores=coloresBancos, "graficaSC3", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Tarjeta de débito</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC4Select==1){
            frecuenciaSimple(basem(),c("F4TC.1", "F4TC.2", "F4TC.3", "F4TC.4", "F4TC.5"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto3)), afColores=coloresBancos, "graficaSC4", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Tarjeta de crédito</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4TC.1", "F4TC.2", "F4TC.3", "F4TC.4", "F4TC.5"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto3)), afColores=coloresBancos, "graficaSC4", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Tarjeta de crédito</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4TC.1", "F4TC.2", "F4TC.3", "F4TC.4", "F4TC.5"), input$graficaSC4Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto3)), aSColores=coloresBancos, "graficaSC4", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Tarjeta de crédito</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC5Select==1){
            frecuenciaSimple(basem(),c("F4CH.1", "F4CH.2", "F4CH.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto4)), afColores=coloresBancos, "graficaSC5", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Cuenta de ahorro</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4CH.1", "F4CH.2", "F4CH.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto4)), afColores=coloresBancos, "graficaSC5", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Cuenta de ahorro</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4CH.1", "F4CH.2", "F4CH.3"), input$graficaSC5Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto4)), aSColores=coloresBancos, "graficaSC5", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Cuenta de ahorro</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC6Select==1){
            frecuenciaSimple(basem(),c("F4CC.1", "F4CC.2", "F4CC.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto5)), afColores=coloresBancos, "graficaSC6", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Cuenta de cheques</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4CC.1", "F4CC.2", "F4CC.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto5)), afColores=coloresBancos, "graficaSC6", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Cuenta de cheques</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4CC.1", "F4CC.2", "F4CC.3"), input$graficaSC6Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto5)), aSColores=coloresBancos, "graficaSC6", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Cuenta de cheques</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC7Select==1){
            frecuenciaSimple(basem(),c("F4SE.1", "F4SE.2", "F4SE.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto7)), afColores=coloresBancos, "graficaSC7", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Seguros</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4SE.1", "F4SE.2", "F4SE.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto7)), afColores=coloresBancos, "graficaSC7", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Seguros</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4SE.1", "F4SE.2", "F4SE.3"), input$graficaSC7Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto7)), aSColores=coloresBancos, "graficaSC7", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Seguros</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC8Select==1){
            frecuenciaSimple(basem(),c("F4AF"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto8)), afColores=coloresBancos, "graficaSC8", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Afore</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4AF"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto8)), afColores=coloresBancos, "graficaSC8", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Afore</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4AF"), input$graficaSC8Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto8)), aSColores=coloresBancos, "graficaSC8", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Afore</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC9Select==1){
            frecuenciaSimple(basem(),c("F4CH.1.A", "F4CH.2.A", "F4CH.3.A"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto9)), afColores=coloresBancos, "graficaSC9", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Crédito Hipotercario</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4CH.1.A", "F4CH.2.A", "F4CH.3.A"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto9)), afColores=coloresBancos, "graficaSC9", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Crédito Hipotercario</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4CH.1.A", "F4CH.2.A", "F4CH.3.A"), input$graficaSC9Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto9)), aSColores=coloresBancos, "graficaSC9", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Crédito Hipotecario</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC10Select==1){
            frecuenciaSimple(basem(),c("F4CA.1", "F4CA.2"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto10)), afColores=coloresBancos, "graficaSC10", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Crédito Automotriz</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4CA.1", "F4CA.2"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto10)), afColores=coloresBancos, "graficaSC10", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Crédito Automotriz</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4CA.1", "F4CA.2"), input$graficaSC10Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto10)), aSColores=coloresBancos, "graficaSC10", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Crédito Automotriz</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC11Select==1){
            frecuenciaSimple(basem(),c("F4CE"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto11)), afColores=coloresBancos, "graficaSC11", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Crédito Empresarial</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4CE"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto11)), afColores=coloresBancos, "graficaSC11", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Crédito Empresarial</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4CE"), input$graficaSC11Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto11)), aSColores=coloresBancos, "graficaSC11", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Crédito Empresarial</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          
          if(input$graficaSC12Select==1){
            frecuenciaSimple(basem(),c("F4OTC"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto12)), afColores=coloresBancos, "graficaSC12", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Otro préstamo personal</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F)
          }else{paste(
            frecuenciaSimple(basem(),c("F4OTC"), afSumaCual=2, afsort=T, aflim=1, afCustom=sum(table(basem()$producto12)), afColores=coloresBancos, "graficaSC12", cUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", cUBSSubtitulo="<b>Otro préstamo personal</b>", cUBSTEjex="Bancos", cUBSTEjey="Porcentaje", F),
            frecuenciaSimplePorCruce(basem(), c("F4OTC"), input$graficaSC12Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=sum(table(basem()$producto12)), aSColores=coloresBancos, "graficaSC12", ScUBSTitulo="¿De cuál o cuáles productos bancarios tiene y con qué banco?", ScUBSSubtitulo="<b>Otro crédito personal</b>", ScUBSTEjex="Bancos", ScUBSTEjey="Porcentaje", banco=F),sep="")},
          #fin de SC
          "[]",sep="")
      )
    }
    
    if(seccionActiva$valor=="SD"){
      return(
        paste(
          if(input$graficaSD1Select==1){
            frecuenciaSimple(basem(), c("A1.Top"), afSumaCual=1, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD1", cUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?", cUBSSubtitulo="Top of mind", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A1.Top"), afSumaCual=1, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD1", cUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?", cUBSSubtitulo="Top of mind", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A1.Top"), input$graficaSD1Select,aSSumaCual=1,aSsort=T, aSlim=2, aSCustom=miTotal, aSColores=clrzTop, "graficaSD1", ScUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?", ScUBSSubtitulo="Top of mind", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSD1ASelect==1){
            frecuenciaSimple(basem(), c("A1.Top","A1.1","A1.2","A1.3","A1.4","A1.5","A1.6","A1.7","A1.8","A1.9","A1.10"), afSumaCual=2, afsort=T, aflim=3, afCustom=miTotal, afColores=clrzTop, "graficaSD1A", cUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?", cUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A1.Top","A1.1","A1.2","A1.3","A1.4","A1.5","A1.6","A1.7","A1.8","A1.9","A1.10"), afSumaCual=2, afsort=T, aflim=3, afCustom=miTotal, afColores=clrzTop, "graficaSD1A", cUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?", cUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A1.Top","A1.1","A1.2","A1.3","A1.4","A1.5","A1.6","A1.7","A1.8","A1.9","A1.10"), input$graficaSD1ASelect,aSSumaCual=2,aSsort=T, aSlim=3, aSCustom=miTotal, aSColores=clrzTop, "graficaSD1A", ScUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?", ScUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSD2Select==1){
            frecuenciaSimple(basem(), c("A1.Top","A1.1","A1.2","A1.3","A1.4","A1.5","A1.6","A1.7","A1.8","A1.9","A1.10", "A3.1","A3.2","A3.3","A3.4","A3.5","A3.6","A3.7","A3.8","A3.9","A3.10"), afSumaCual=2, afsort=T, aflim=5, afCustom=miTotal, afColores=clrzTop, "graficaSD2", cUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?, ¿Algún otro?, ¿Otro más?; De estos lugares para empeñar, ¿Cuáles conoce o ha oido nombrar?", cUBSSubtitulo="Top of mind + Respuestas espontáneas + Respuestas con ayuda", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A1.Top","A1.1","A1.2","A1.3","A1.4","A1.5","A1.6","A1.7","A1.8","A1.9","A1.10", "A3.1","A3.2","A3.3","A3.4","A3.5","A3.6","A3.7","A3.8","A3.9","A3.10"), afSumaCual=2, afsort=T, aflim=5, afCustom=miTotal, afColores=clrzTop, "graficaSD2", cUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?, ¿Algún otro?, ¿Otro más?; De estos lugares para empeñar, ¿Cuáles conoce o ha oido nombrar?", cUBSSubtitulo="Top of mind + Respuestas espontáneas + Respuestas con ayuda", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A1.Top","A1.1","A1.2","A1.3","A1.4","A1.5","A1.6","A1.7","A1.8","A1.9","A1.10", "A3.1","A3.2","A3.3","A3.4","A3.5","A3.6","A3.7","A3.8","A3.9","A3.10"), input$graficaSD2Select,aSSumaCual=2,aSsort=T, aSlim=5, aSCustom=miTotal, aSColores=clrzTop, "graficaSD2", ScUBSTitulo="Cuando piensa en empeñar un artículo para obtener un préstamo prendario ¿Qué lugares le vienen a la mente?, ¿Algún otro?, ¿Otro más?; De estos lugares para empeñar, ¿Cuáles conoce o ha oido nombrar?", ScUBSSubtitulo="Top of mind + Respuestas espontáneas + Respuestas con ayuda", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSD3Select==1){
            frecuenciaSimple(basem(), c("A7.1","A7.2","A7.3","A7.4","A7.5","A7.6","A7.7","A7.8","A7.9","A7.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD3", cUBSTitulo="¿En cuál o cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo?", cUBSSubtitulo="(No importa hace cuanto tiempo lo hizo)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A7.1","A7.2","A7.3","A7.4","A7.5","A7.6","A7.7","A7.8","A7.9","A7.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD3", cUBSTitulo="¿En cuál o cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo?", cUBSSubtitulo="(No importa hace cuanto tiempo lo hizo)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A7.1","A7.2","A7.3","A7.4","A7.5","A7.6","A7.7","A7.8","A7.9","A7.10"), input$graficaSD3Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=clrzTop, "graficaSD3", ScUBSTitulo="¿En cuál o cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo?", ScUBSSubtitulo="(No importa hace cuanto tiempo lo hizo)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSD4Select==1){
            frecuenciaSimple(basem(), c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD4", cUBSTitulo="¿De cuál o de cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo en los últimos 12 meses, sin contemplar refrendos?", cUBSSubtitulo="Utilizó en los últimos 12 meses", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD4", cUBSTitulo="¿De cuál o de cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo en los últimos 12 meses, sin contemplar refrendos?", cUBSSubtitulo="Utilizó en los últimos 12 meses", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), input$graficaSD4Select,aSSumaCual=2,aSsort=T, aSlim=2, aSCustom=miTotal, aSColores=clrzTop, "graficaSD4", ScUBSTitulo="¿De cuál o de cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo en los últimos 12 meses, sin contemplar refrendos?", ScUBSSubtitulo="Utilizó en los últimos 12 meses", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSD5Select==1){
            frecuenciaSimple(basem(), c("A9"), afSumaCual=1, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD5", cUBSTitulo="¿En qué lugar de préstamos prendarios empeñó algún artículo la última vez?", cUBSSubtitulo="Utilizó en su último empeño", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A9"), afSumaCual=1, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSD5", cUBSTitulo="¿En qué lugar de préstamos prendarios empeñó algún artículo la última vez?", cUBSSubtitulo="Utilizó en su último empeño", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A9"), input$graficaSD5Select,aSSumaCual=2,aSsort=T, aSlim=2, aSCustom=miTotal, aSColores=clrzTop, "graficaSD5", ScUBSTitulo="¿En qué lugar de préstamos prendarios empeñó algún artículo la última vez?", ScUBSSubtitulo="Utilizó en su último empeño", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSDB11Select==1){
            frecuenciaSimple(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSDB1", cUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", cUBSSubtitulo="Lugar que dejó de utilizar", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSDB1", cUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", cUBSSubtitulo="Lugar que dejó de utilizar", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), input$graficaSDB11Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=clrzTop, "graficaSDB1", ScUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", ScUBSSubtitulo="Lugar que dejó de utilizar", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSDB11Select==1){
            frecuenciaSimple(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSDB1", cUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", cUBSSubtitulo="Lugar que dejó de utilizar", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSDB1", cUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", cUBSSubtitulo="Lugar que dejó de utilizar", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), input$graficaSDB11Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=clrzTop, "graficaSDB1", ScUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", ScUBSSubtitulo="Lugar que dejó de utilizar", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSDB11Select==1){
            frecuenciaSimple(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSDB1", cUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", cUBSSubtitulo="Lugar que dejó de utilizar", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSDB1", cUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", cUBSSubtitulo="Lugar que dejó de utilizar", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A9bMAR.1", "A9CMAR.2","A9DMAR.3", "A9EMAR.4"), input$graficaSDB11Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=clrzTop, "graficaSDB1", ScUBSTitulo="¿Por qué dejó de utilizar a ...  para obtener sus préstamos prendarios?", ScUBSSubtitulo="Lugar que dejó de utilizar", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep=)},
          
          if(input$graficaSDB12Select==1){
            paste('["creaTabla", "#graficaSDB2", ',damemiJsonDFTable(agrupaFreq1(basem(), c("A9.1COD1", "A9.1COD2", "A9.2COD1", "A9.2COD2","A9.3COD1", "A9.3COD2", "A9.4COD1"), "custom", TRUE, 1, miTotal, c("A"))),'],',sep="")
          }
          else{paste('["creaTabla", "#graficaSDB2", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), input$graficaSDB12Select , "personas", listadoAbandono, agrffOrden=T, agrfflimite=0)[[2]]),'],',sep="")},
          
          "[]",sep="")
      )
    }
    
    if(seccionActiva$valor=="SE"){
      return(
        paste(
          if(input$graficaSE1Select==1){
            frecuenciaSimple(basem(), c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSE1", cUBSTitulo="¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?", cUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSE1", cUBSTitulo="¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?", cUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10"), input$graficaSE1Select,aSSumaCual=2,aSsort=T, aSlim=2, aSCustom=miTotal, aSColores=clrzTop, "graficaSE1", ScUBSTitulo="¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?", ScUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          if(input$graficaSE2VSelect==1){
            frecuenciaSimple(basem(), c("medio1", "medio2", "medio3", "medio4", "medio5", "medio6", "medio7", "medio8", "medio9", "medio10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=NULL, "graficaSE2V", cUBSTitulo="<h3><small>¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3><h3><small>De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3>", cUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("medio1", "medio2", "medio3", "medio4", "medio5", "medio6", "medio7", "medio8", "medio9", "medio10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=NULL, "graficaSE2V", cUBSTitulo="<h3><small>¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3><h3><small>De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3>", cUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("medio1", "medio2", "medio3", "medio4", "medio5", "medio6", "medio7", "medio8", "medio9", "medio10"), input$graficaSE2VSelect,aSSumaCual=2,aSsort=T, aSlim=2, aSCustom=miTotal, aSColores=NULL, "graficaSE2V", ScUBSTitulo="<h3><small>¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3><h3><small>De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?</small></h3>", ScUBSSubtitulo="Top of mind + Todas las respuestas espontáneas", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          if(input$graficaSE2Select==1){
            frecuenciaSimple(basem(), c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10", "A4.1","A4.2",  "A4.3",  "A4.4",  "A4.5" , "A4.6"  ,"A4.7" , "A4.8"  ,"A4.9" , "A4.10"), afSumaCual=2, afsort=T, aflim=3, afCustom=miTotal, afColores=clrzTop, "graficaSE2", cUBSTitulo="De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?", cUBSSubtitulo="Top of mind + Respuestas espontáneas + Respuestas con ayuda", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10", "A4.1","A4.2",  "A4.3",  "A4.4",  "A4.5" , "A4.6"  ,"A4.7" , "A4.8"  ,"A4.9" , "A4.10"), afSumaCual=2, afsort=T, aflim=3, afCustom=miTotal, afColores=clrzTop, "graficaSE2", cUBSTitulo="De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?", cUBSSubtitulo="Top of mind + Respuestas espontáneas + Respuestas con ayuda", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A2.1","A2.2","A2.3","A2.4","A2.5","A2.6","A2.7","A2.8","A2.9","A2.10", "A4.1","A4.2",  "A4.3",  "A4.4",  "A4.5" , "A4.6"  ,"A4.7" , "A4.8"  ,"A4.9" , "A4.10"), input$graficaSE2Select,aSSumaCual=2,aSsort=T, aSlim=3, aSCustom=miTotal, aSColores=clrzTop, "graficaSE2", ScUBSTitulo="De estos lugares para empeñar artículos que le voy a mostrar ¿De cuál o cuáles lugares recuerda haber VISTO O ESCUCHADO publicidad/ anuncios sobre el servicio de préstamos prendarios?", ScUBSSubtitulo="Top of mind + Respuestas espontáneas + Respuestas con ayuda", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          "[]",sep="")
      )
    }
    
    if(seccionActiva$valor=="SF"){
      return(
        paste(
          if(input$graficaSF1Select==1){
            frecuenciaSimple(basem(), c("A10cod1","A10cod2","A10cod3"), afSumaCual=1, afsort=T, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSF1", cUBSTitulo="¿Para qué ocupa el dinero que le prestan por su empeño?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A10cod1","A10cod2","A10cod3"), afSumaCual=1, afsort=T, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSF1", cUBSTitulo="¿Para qué ocupa el dinero que le prestan por su empeño?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A10cod1","A10cod2","A10cod3"), input$graficaSF1Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=NULL, "graficaSF1", ScUBSTitulo="¿Para qué ocupa el dinero que le prestan por su empeño?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          if(input$graficaSF2Select==1){
            frecuenciaSimple(basem(), c("A9"), afSumaCual=1, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSF2", cUBSTitulo="¿En qué lugar de préstamos prendarios empeñó algún artículo la última vez?", cUBSSubtitulo="Utilizó en su último empeño", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A9"), afSumaCual=1, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSF2", cUBSTitulo="¿En qué lugar de préstamos prendarios empeñó algún artículo la última vez?", cUBSSubtitulo="Utilizó en su último empeño", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A9"), input$graficaSF2Select,aSSumaCual=2,aSsort=T, aSlim=2, aSCustom=miTotal, aSColores=clrzTop, "graficaSF2", ScUBSTitulo="¿En qué lugar de préstamos prendarios empeñó algún artículo la última vez?", ScUBSSubtitulo="Utilizó en su último empeño", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          if(input$graficaSF3Select==1){
            paste('["creaTabla", "#graficaSF3", ',damemiJsonDFTable(agrupaFreq1(basem(), c("A11.1COD","A11.2COD","A11.3COD","A11.4COD","A11.5COD"), "custom", TRUE, 1, miTotal, c("A"))),'],',sep="")
          }
          else{paste('["creaTabla", "#graficaSF3", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), input$graficaSF3Select , "personas", listadoRazonesEmpeño, agrffOrden=T, agrfflimite=1)[[2]]),'],',sep="")},
          
          if(input$graficaSF4Select==1){
            paste('["creaTabla", "#graficaSF4", ',damemiJsonDFTable(agrupaFreq1(basem(), c("A12.1COD","A12.2COD","A12.3COD","A12.4COD","A12.5COD"), "custom", TRUE, 1, miTotal, c("A"))),'],',sep="")
          }
          else{paste('["creaTabla", "#graficaSF4", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), input$graficaSF4Select , "personas", listadoMotivosEmpeño, agrffOrden=T, agrfflimite=1)[[2]]),'],',sep="")}
        )
      )
    }
    
    if(seccionActiva$valor=="SG"){
      return(
        paste(
          if(input$graficaSG1Select==1){
            frecuenciaSimple(basem(), misA13, afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSG1", cUBSTitulo="¿Tiene un préstamo prendario que esté pagando actualmente con ...", cUBSSubtitulo="Total de empeños actuales", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), misA13, afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=clrzTop, "graficaSG1", cUBSTitulo="¿Tiene un préstamo prendario que esté pagando actualmente con ...", cUBSSubtitulo="Total de empeños actuales", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), misA13, input$graficaSG1Select,aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=clrzTop, "graficaSG1", ScUBSTitulo="¿Tiene un préstamo prendario que esté pagando actualmente con ...", ScUBSSubtitulo="Total de empeños actuales", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          frecuenciaSimple(basem(), c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), afSumaCual=2, afsort=T, aflim=2, afCustom=miTotal, afColores=clrzTop, "graficaSG2", cUBSTitulo="¿De cuál o de cuáles de estos lugares de préstamos prendarios ha empeñado algún artículo en los últimos 12 meses, sin contemplar refrendos?", cUBSSubtitulo="Utilizó en los últimos 12 meses", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
          
          if(input$graficaSG3Select==1){
            frecuenciaSimple(basem(), c("A15Meses.1RR","A15Meses.2RR","A15Meses.3RR","A15Meses.4RR","A15Meses.5RR","A15Meses.6RR","A15Meses.7RR","A15Meses.8RR"), afSumaCual=2, afsort=F, aflim=0, afCustom=miTotal, afColores=NULL, "graficaSG3", cUBSTitulo="¿Hace cuánto tiempo obtuvo su préstamo prendario?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A15Meses.1RR","A15Meses.2RR","A15Meses.3RR","A15Meses.4RR","A15Meses.5RR","A15Meses.6RR","A15Meses.7RR","A15Meses.8RR"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG3", cUBSTitulo="¿Hace cuánto tiempo obtuvo su préstamo prendario?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A15Meses.1RR","A15Meses.2RR","A15Meses.3RR","A15Meses.4RR","A15Meses.5RR","A15Meses.6RR","A15Meses.7RR","A15Meses.8RR"),aSSumaCual=2,aSsort=F, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG3", ScUBSTitulo="¿Hace cuánto tiempo obtuvo su préstamo prendario?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG4Select==1){
            frecuenciaSimple(basem(), c("A16.1.1","A16.1.2","A16.1.3","A16.2.1","A16.2.2","A16.2.3","A16.3.1","A16.3.2","A16.3.3","A16.4.1","A16.4.2","A16.4.3","A16.5.1","A16.5.2","A16.5.3","A16.6.1","A16.6.2","A16.6.3","A16.7.1","A16.7.2","A16.7.3","A16.8.1","A16.8.2","A16.8.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=coloresRandom[1:13], "graficaSG4", cUBSTitulo="Para este préstamo prendario, ¿Qué artículo empeñó?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A16.1.1","A16.1.2","A16.1.3","A16.2.1","A16.2.2","A16.2.3","A16.3.1","A16.3.2","A16.3.3","A16.4.1","A16.4.2","A16.4.3","A16.5.1","A16.5.2","A16.5.3","A16.6.1","A16.6.2","A16.6.3","A16.7.1","A16.7.2","A16.7.3","A16.8.1","A16.8.2","A16.8.3"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=coloresRandom[1:13], "graficaSG4", cUBSTitulo="Para este préstamo prendario, ¿Qué artículo empeñó?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A16.1.1","A16.1.2","A16.1.3","A16.2.1","A16.2.2","A16.2.3","A16.3.1","A16.3.2","A16.3.3","A16.4.1","A16.4.2","A16.4.3","A16.5.1","A16.5.2","A16.5.3","A16.6.1","A16.6.2","A16.6.3","A16.7.1","A16.7.2","A16.7.3","A16.8.1","A16.8.2","A16.8.3"),aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=coloresRandom[1:13], ScUBSidG="graficaSG4", ScUBSTitulo="Para este préstamo prendario, ¿Qué artículo empeñó?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG5Select==1){
            frecuenciaSimple(basem(), c("A17RR.1","A17RR.2","A17RR.3","A17RR.4","A17RR.5","A17RR.6","A17RR.7","A17RR.8"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG5", cUBSTitulo="Para este préstamo prendario, ¿Cuánto dinero le dieron por su empeño?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A17RR.1","A17RR.2","A17RR.3","A17RR.4","A17RR.5","A17RR.6","A17RR.7","A17RR.8"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG5", cUBSTitulo="Para este préstamo prendario, ¿Cuánto dinero le dieron por su empeño?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A17RR.1","A17RR.2","A17RR.3","A17RR.4","A17RR.5","A17RR.6","A17RR.7","A17RR.8"),aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG5", ScUBSTitulo="Para este préstamo prendario, ¿Cuánto dinero le dieron por su empeño?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          #           if(input$graficaSG6Select==1){
          #             frecuenciaSimple(basem(), c("A19RR.1","A19RR.2","A19RR.3","A19RR.4","A19RR.5","A19RR.6","A19RR.7","A19RR.8"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=coloresGradiente[1:7], "graficaSG6", cUBSTitulo="Para este préstamo prendario, ¿Cuál fue el monto total que terminó pagando?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          #           }else{paste(
          #             frecuenciaSimple(basem(), c("A19RR.1","A19RR.2","A19RR.3","A19RR.4","A19RR.5","A19RR.6","A19RR.7","A19RR.8"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=coloresGradiente[1:7], "graficaSG6", cUBSTitulo="Para este préstamo prendario, ¿Cuál fue el monto total que terminó pagando?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
          #             frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A19RR.1","A19RR.2","A19RR.3","A19RR.4","A19RR.5","A19RR.6","A19RR.7","A19RR.8"),aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=coloresGradiente[1:7], ScUBSidG="graficaSG6", ScUBSTitulo="Para este préstamo prendario, ¿Cuál fue el monto total que terminó pagando?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG7Select==1){
            frecuenciaSimple(basem(), c("A18.1RR","A18.2RR","A18.3RR","A18.4RR","A18.5RR","A18.6RR","A18.7RR","A18.8RR"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG7", cUBSTitulo="Para este préstamo prendario, ¿Qué tasa de interés tiene?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A18.1RR","A18.2RR","A18.3RR","A18.4RR","A18.5RR","A18.6RR","A18.7RR","A18.8RR"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG7", cUBSTitulo="Para este préstamo prendario, ¿Qué tasa de interés tiene?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A18.1RR","A18.2RR","A18.3RR","A18.4RR","A18.5RR","A18.6RR","A18.7RR","A18.8RR"),aSSumaCual=2,aSsort=F, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG7", ScUBSTitulo="Para este préstamo prendario, ¿Qué tasa de interés tiene?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG8Select==1){
            frecuenciaSimple(basem(), c("variacion1RR","variacion2RR","variacion3RR","variacion4RR","variacion5RR","variacion6RR","variacion7RR","variacion8RR"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG8", cUBSTitulo="Costo del préstamo: Incremento porcentual del monto recibido respecto al monto pagado", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("variacion1RR","variacion2RR","variacion3RR","variacion4RR","variacion5RR","variacion6RR","variacion7RR","variacion8RR"), afSumaCual=2, afsort=F, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG8", cUBSTitulo="Costo del préstamo: Incremento porcentual del monto recibido respecto al monto pagado", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("variacion1RR","variacion2RR","variacion3RR","variacion4RR","variacion5RR","variacion6RR","variacion7RR","variacion8RR"),aSSumaCual=2,aSsort=F, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG8", ScUBSTitulo="Costo del préstamo: Incremento porcentual del monto recibido respecto al monto pagado", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG9Select==1){
            frecuenciaSimple(basem(), c("A20.1","A20.2","A20.3","A20.4","A20.5","A20.6","A20.7","A20.8"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG9", cUBSTitulo="Para este préstamo prendario, ¿Con qué frecuencia acostumbraba pagar este préstamo?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A20.1","A20.2","A20.3","A20.4","A20.5","A20.6","A20.7","A20.8"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG9", cUBSTitulo="Para este préstamo prendario, ¿Con qué frecuencia acostumbraba pagar este préstamo?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A20.1","A20.2","A20.3","A20.4","A20.5","A20.6","A20.7","A20.8"),aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG9", ScUBSTitulo="Para este préstamo prendario, ¿Con qué frecuencia acostumbraba pagar este préstamo?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG10Select==1){
            frecuenciaSimple(basem(), c("A21.1", "A21.2", "A21.3", "A21.4", "A21.5", "A21.6", "A21.7", "A21.8"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG10", cUBSTitulo="¿Cómo acostumbraba pagar este préstamo prendario?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A21.1", "A21.2", "A21.3", "A21.4", "A21.5", "A21.6", "A21.7", "A21.8"), afSumaCual=2, afsort=T, aflim=1, afCustom=miTotal, afColores=NULL, "graficaSG10", cUBSTitulo="¿Cómo acostumbraba pagar este préstamo prendario?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A21.1", "A21.2", "A21.3", "A21.4", "A21.5", "A21.6", "A21.7", "A21.8"),aSSumaCual=2,aSsort=T, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG10", ScUBSTitulo="¿Cómo acostumbraba pagar este préstamo prendario?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")},
          
          if(input$graficaSG11Select==1){
            frecuenciaSimple(basem(), c("A22.1", "A22.2", "A22.3", "A22.4", "A22.5", "A22.6", "A22.7", "A22.8"), afSumaCual=2, afsort=F, aflim=0, afCustom=miTotal, afColores=NULL, "graficaSG11", cUBSTitulo="¿Cuántas veces ha refrendado?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A22.1", "A22.2", "A22.3", "A22.4", "A22.5", "A22.6", "A22.7", "A22.8"), afSumaCual=2, afsort=F, aflim=0, afCustom=miTotal, afColores=NULL, "graficaSG11", cUBSTitulo="¿Cuántas veces ha refrendado?", cUBSSubtitulo="(Préstamo en los últimos 12 meses)", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruceMultiple(aSbase=basem(), aSvector=c("A22.1", "A22.2", "A22.3", "A22.4", "A22.5", "A22.6", "A22.7", "A22.8"),aSSumaCual=2,aSsort=F, aSlim=1, aSCustom=miTotal, aSColores=NULL, ScUBSidG="graficaSG11", ScUBSTitulo="¿Cuántas veces ha refrendado?", ScUBSSubtitulo="(Préstamo en los últimos 12 meses)", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje", variablesPaCruce= c("A8.1","A8.2","A8.3","A8.4","A8.5","A8.6","A8.7","A8.8","A8.9","A8.10"), todasLasRespuestas=list(Si=F, cuales=targetLevelsEspecial)),sep="")}
          
        )
      )
    }
    
    if(seccionActiva$valor=="SH"){
      return(
        paste(
          if(input$graficaSH1Select==1){
            frecuenciaSimple(basem(), c("A23"), afSumaCual=1, afsort=T, aflim=0, afCustom=miTotal, afColores=c("#3D8C7D","#F67C09"), "graficaSH1", cUBSTitulo="Cuando le interesa solicitar un préstamo prendario, ¿qué frase lo describiría mejor?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A23"), afSumaCual=1, afsort=T, aflim=0, afCustom=miTotal, afColores=c("#3D8C7D","#F67C09"), "graficaSH1", cUBSTitulo="Cuando le interesa solicitar un préstamo prendario, ¿qué frase lo describiría mejor?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A23"), input$graficaSH1Select,aSSumaCual=2,aSsort=T, aSlim=0, aSCustom=miTotal, aSColores=c("#3D8C7D","#F67C09"), "graficaSH1", ScUBSTitulo="Cuando le interesa solicitar un préstamo prendario, ¿qué frase lo describiría mejor?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          paste('["creaTabla", "#graficaSH2", ',miTablaA24(basem(), input$graficaSH2Select),'],',sep=""),
          
          miA26(basem(), c("A26M.1", "A26M.2", "A26M.3", "A26M.4", "A26M.5", "A26M.6"), c("A26E.1", "A26E.2", "A26E.3", "A26E.4", "A26E.5", "A26E.6"), list(Si=F, cuales=c(1,15,8,9,17)), coloresGradiente[c(6,8,11,12)], 1, 2100, input$graficaSH3Select),
          
          paste('["creaABeneficios", "#graficaSH4","¿Qué tan de acuerdo estaría en que...?","(% de respuesta de \'Totalmente de acuerdo\' y \'Acuerdo\')",',beneficiosEmocionales(basem(), input$graficaSH4Select),'],',sep=""),
          
          paste('["creaABeneficios", "#graficaSH5","¿Qué tan de acuerdo estaría en que...?","(% de respuesta de \'Totalmente de acuerdo\')",',beneficiosEmocionales2(basem(), input$graficaSH4Select),'],',sep=""),
          "[]",sep="")
      )
    }
    
    if(seccionActiva$valor=="SI"){
      return(
        paste(
          paste('["creaTabla", "#graficaSI1", ',damemiJsonDFTable(agrupaFreq1(basem(), c("A28.1COD", "A28.2COD", "A28.3COD"), "custom", TRUE, 1, miTotal, c("A"))),'],',sep=""),       
          
#           if(input$graficaSI2Select==1){
#             frecuenciaSimple(basem(), c("A29"), afSumaCual=1, afsort=T, aflim=0, afCustom=miTotal, afColores=coloresRandom[12:23], "graficaSI2", cUBSTitulo="¿Cuándo menciono 'Presta Prenda' a qué/quién la asocia?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
#           }else{paste(
#             frecuenciaSimple(basem(), c("A29"), afSumaCual=1, afsort=T, aflim=0, afCustom=miTotal, afColores=coloresRandom[12:23], "graficaSI2", cUBSTitulo="¿Cuándo menciono 'Presta Prenda' a qué/quién la asocia?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
#             frecuenciaSimplePorCruce(basem(), c("A29"), input$graficaSI2Select,aSSumaCual=2,aSsort=T, aSlim=0, aSCustom=miTotal, aSColores=coloresRandom[12:23], "graficaSI2", ScUBSTitulo="¿Cuándo menciono 'Presta Prenda' a qué/quién la asocia?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          if(input$graficaSI31Select==1){
            frecuenciaSimple(basem(), c("A30"), afSumaCual=1, afsort=F, aflim=0, afCustom=miTotal, afColores=coloresGradiente[1:5], "graficaSI31", cUBSTitulo="Si yo le dijera que Banco Azteca está relacionado con Presta Prenda, ¿qué tanto diría que el Banco le aporta a la imagen de Presta Prenda?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A30"), afSumaCual=1, afsort=F, aflim=0, afCustom=miTotal, afColores=coloresGradiente[1:5], "graficaSI31", cUBSTitulo="Si yo le dijera que Banco Azteca está relacionado con Presta Prenda, ¿qué tanto diría que el Banco le aporta a la imagen de Presta Prenda?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A30"), input$graficaSI31Select,aSSumaCual=2,aSsort=F, aSlim=0, aSCustom=miTotal, aSColores=coloresGradiente[1:5], "graficaSI31", ScUBSTitulo="Si yo le dijera que Banco Azteca está relacionado con Presta Prenda, ¿qué tanto diría que el Banco le aporta a la imagen de Presta Prenda?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          paste('["creaTabla", "#graficaSI32", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), c("Le aporta positivamente","Le aporta muy positivamente") , "personas", listadoA30,agrffOrden=T, agrfflimite=1)[[2]]),'],',sep=""),       
          paste('["creaTabla", "#graficaSI33", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), c("Le afecta negativamente","Le afecta muy negativamente") , "personas", listadoA30,agrffOrden=T, agrfflimite=1)[[2]]),'],',sep=""),       
          
          if(input$graficaSI41Select==1){
            frecuenciaSimple(basem(), c("A31"), afSumaCual=1, afsort=F, aflim=0, afCustom=miTotal, afColores=coloresGradiente[1:5], "graficaSI41", cUBSTitulo="Si yo le dijera que Elektra está relacionado con Presta Prenda, ¿qué tanto diría que Elektra le aporta a la imagen de Presta Prenda?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A31"), afSumaCual=1, afsort=F, aflim=0, afCustom=miTotal, afColores=coloresGradiente[1:5], "graficaSI41", cUBSTitulo="Si yo le dijera que Elektra está relacionado con Presta Prenda, ¿qué tanto diría que Elektra le aporta a la imagen de Presta Prenda?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A31"), input$graficaSI41Select,aSSumaCual=2,aSsort=F, aSlim=0, aSCustom=miTotal, aSColores=coloresGradiente[1:5], "graficaSI41", ScUBSTitulo="Si yo le dijera que Elektra está relacionado con Presta Prenda, ¿qué tanto diría que Elektra le aporta a la imagen de Presta Prenda?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          paste('["creaTabla", "#graficaSI42", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), c("Le aporta positivamente","Le aporta muy positivamente") , "personas", listadoA31,agrffOrden=T, agrfflimite=1)[[2]]),'],',sep=""),       
          paste('["creaTabla", "#graficaSI43", ',damemiJsonDFTable(agrupaFreqFiltro(basem(), c("Le afecta negativamente","Le afecta muy negativamente") , "personas", listadoA31,agrffOrden=T, agrfflimite=1)[[2]]),'],',sep=""),       
          
          if(input$graficaSI5Select==1){
            frecuenciaSimple(basem(), c("A32"), afSumaCual=1, afsort=T, aflim=0, afCustom=miTotal, afColores=coloresRandom[12:15], "graficaSI5", cUBSTitulo="¿Cuál de estas dos tiendas Elektra o Banco Azteca le aporta más a Presta Prenda?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje")
          }else{paste(
            frecuenciaSimple(basem(), c("A32"), afSumaCual=1, afsort=T, aflim=0, afCustom=miTotal, afColores=coloresRandom[12:15], "graficaSI5", cUBSTitulo="¿Cuál de estas dos tiendas Elektra o Banco Azteca le aporta más a Presta Prenda?", cUBSSubtitulo="", cUBSTEjex="Respuestas", cUBSTEjey="Porcentaje"),
            frecuenciaSimplePorCruce(basem(), c("A32"), input$graficaSI5Select,aSSumaCual=2,aSsort=T, aSlim=0, aSCustom=miTotal, aSColores=coloresRandom[12:15], "graficaSI5", ScUBSTitulo="¿Cuál de estas dos tiendas Elektra o Banco Azteca le aporta más a Presta Prenda?", ScUBSSubtitulo="", ScUBSTEjex="Respuestas", ScUBSTEjey="Porcentaje"),sep="")},
          
          paste('["creaTabla", "#graficaSI6", ',damemiJsonDFTable(agrupaFreq1(basem(), c("A33.1", "A33.2"), "custom", TRUE, 1, miTotal, c("A"))),'],',sep=""),       
          
          "[]",sep="")
      )
    }
    
    
  })
  # end of the good stuff


  # Nada especial, mis motores de ejecución
  output$masterRDeposit <- renderUI({
    tags$script(HTML(
      'var sith = [',
      miColeccionFunciones(),
      '];',
      '$("#graficaSE3").children("img").addClass("img-responsive img-rounded center-block image");',
      'runMasterr(sith);'
    ))
  })
  output$alertDeposit <- renderUI({
    misAlertas<-''
    for(fftr in 1: length(filtrosChecklist())){
      listillaTemporal <- filtrosChecklist()[[fftr]]
      if(all(!listillaTemporal)){
        misAlertas<- paste(misAlertas,
                           '$.notify({icon: "glyphicon glyphicon-warning-sign",',
                           'message: "Porfavor selecciona al menos un filtro para <b>',names(filtrosChecklist())[fftr],'</b>. Por el momento no se está filtrando por <b>',names(filtrosChecklist())[fftr],'</b>"',
                           '},{',
                           'type: "danger",',
                           'timer: 1000',
                           '});',
                           sep=""
        )
      }
    }
    midim<- nrow(basem())
    tags$script(HTML(
      misAlertas,
      'if(',midim,'<101){',
      '$.notify({',
      'icon: "glyphicon glyphicon-warning-sign",',
      'message: "<b>El tamaño de muestra es muy pequeño para obtener resultados representativos, por favor modifica algún filtro</b>"',
      '},{',
      'type: "danger",',
      'timer: 1000',
      '});',
      ';$("#muestraWarningId").show()}else{$("#muestraWarningId").hide()}',
      '$("#casosId").html("<span class=\'glyphicon glyphicon-th\' aria-hidden=\'true\'></span> Total de casos: ',midim,'")',
      ''
    ))
  })
})