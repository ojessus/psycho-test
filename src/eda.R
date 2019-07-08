rm(list = ls())
load.project()
############################################################################################
# Función que recibe como parametro el año y regresa tres dataframes correspondientes      #
# a los calculos para los datos de este ciclo utilizando el modelo de Rash que en resumen  #
# da una estimación de la dificultad de los items en una prueba, la habilidad de las       #
# personas para rersponder estos items correctamente y un parámetro de discriminación      #
# para los items. Es un hecho conocido que estas estimaciones por lo general estan         #
# todas en el intervalo [-5,5] es por eso que algunos casos los resultados pueden          #
# parecer poco intutivos por eso hacemos  un "normalización" que mapea estos datos         #
# al intervalo [0,100] en el calculo de rankigs. Los dataframes que regresa son:           #
#                                                                                          #
# DaDf: Es un datafrmae con los campos dificultad y discrimicación que contiene            #
#       la estimacion de estos parámetros para el modelo de Rash y se calculan             #
#       utilizando la libreia TAM que incluye funciones para estimar los paámetros         #
#       del modelo de Rasch en partricular se utiliza TAM::tam.mml.2pl que recibe como     #
#       parámetro principal un dataframe donde los identificadores de los items son        #
#       los headers y los datos son 0 si el item se contesto incorrectamente, 1            #
#       si el item se contesto correctamente y NA si no se contesto y regresa un objeto    #
#       con los parámetros habilidad y discriminación junto con mas información, para      #
#       una descripcion completa se puede consultar:                                       #
#       https://www.rdocumentation.org/packages/TAM/versions/2.13-15/topics/tam.mml        #
#                                                                                          #
# habilidadDf:Es un Dataframe con los campos campusName, idUser_int, habilidad             #
#             y scaledHability, la habilidad se calcula utlizando la función               #
#             TAM::tam.wle que recibe como parámetros el objeto que devuelve               #
#             TAM::tam.mml.2pl. y el campo scaledHability es una "normalización"           #
#             entre 0 y 100 del parámetro habilidad. Para una descripción mas detalla      #
#             se puede consultar:                                                          #
#             https://www.rdocumentation.org/packages/TAM/versions/2.13-15/topics/tam.wle  #
#                                                                                          #
# rankingCampus: Es un dataframe con los campos campusName que contiene los nombres de     #
#                los campus y ranking que contiene el ranking de cada campus, este         #
#                rankink se calcula como la habilidad promedio en una escala de 0 a 100    #
############################################################################################
############################# Inicia Funcion raschModel ####################################
raschModel<-function(ciclo,model){
  #Costruimos un dataframe con los datos requeridos para TAM::tam.mml.2pl
  itemDf <- tidy.test.data %>% filter(year==ciclo) %>% spread(key=idQuestion,value=isCorrect)
  resp <- itemDf %>% select(-c("year", "campusName","idUser_int"))
  #utilizaremos TAM::tam.mml.2pl por que es el unico que estima las pendientes, es decir, el parámetro
  #de discriminacón
  if(model=='rasch'){
    rashM <- TAM::tam(resp=resp,verbose=FALSE)
  }else{
    rashM <- TAM::tam.mml.2pl(resp=resp,irtmodel="2PL",verbose=FALSE)
  }
  #Construimos el dataframe con los parametros dificultad y discriminacion para los items de 2018
  # rash2018$item$AXsi_.Cat1 => dificultad para cada pregunta
  # rash2018$item$B.Cat1.Dim1 => discriminación para cada pregunta
  DaDDf <- rashM$item %>% select(AXsi_.Cat1, B.Cat1.Dim1)
  colnames(DaDDf)<-c('dificultad','discriminacion')
  #En DaDDf esta el dataframe con los parámetros dificultad y discriminación por pregunta

  #utilizaremos TAM::tam.wle para calcular la habilidad de cada persona
  rashH <-TAM::tam.wle(rashM,progress=FALSE)

  #almacenamos las habilidades en un dataframe
  habilidad <- as.data.frame(rashH$theta)
  habilidadDf <- data.frame(itemDf$campusName,itemDf$idUser_int,habilidad)
  colnames(habilidadDf)<-c("campusName","idUser_int","habilidad")

  #Scalamos el score de las habilidades entre 0 y 100
  minh = min(habilidadDf$habilidad)
  maxh = max(habilidadDf$habilidad)
  habilidadDf$scaledHability<-with(habilidadDf,((habilidad-minh)/(maxh-minh))*100)

  #Calculamos el ranking entre 0 y 100 por escuela
  rankingCampus<-aggregate(scaledHability~campusName,habilidadDf,mean)
  colnames(rankingCampus)<-c("campusName","ranking")
  vareturn <- list("DaDDf"=DaDDf,"habilidadDf"=habilidadDf,"rankingCampus"=rankingCampus)
  return(vareturn)
}
################################Termina Función raschModel #######################################


