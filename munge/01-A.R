#Preprocesamiento de datos.
#hacemos esto
#verificamos la existencias de la carpeta data/raw
if (dir.exists("data/raw")){
  #Lista de los archivos con extension .csv
  filelist = list.files(path="data/raw",pattern="*.csv")
  for (name in filelist){
    namestr = gsub("_",".",tools::file_path_sans_ext(name))
    namevar = paste("tidy",namestr,sep=".")
    namefile = name #paste(paste("tidy",tools::file_path_sans_ext(name),sep="_"),"csv",sep='.')

    #si no se tiene tidyverse podemos usar fileEncoding = "UTF-8-BOM"
    #assign(namevar,as.data.frame(read.csv(paste("data/raw/",name,sep=""),fileEncoding = "UTF-8-BOM")))

    #como ya agregamos tidyverse a global.dcf podemos usar read_csv que no tiene problemas
    #con los byte order mark (BOM)
    temp = read_csv(paste("data/raw/",name,sep=""))

    #verificamos que en las respuestas solo aparezcan: 0,1,NA
    testnona <- temp %>% filter(complete.cases(.))
    tests<-prod((testnona$isCorrect==1) + (testnona$isCorrect==0))
    if(tests==1){
      #Salvamos los datos limpios en un dataframe
      assign(namevar,read_csv(paste("data/raw/",name,sep="")))
      cat("Load clean data from file",name, "in",namevar,"\n")
      #En el directorio data/tidy guasrdamos los datos procesados
      if (dir.exists("data/tidy")){
        write_csv(temp,paste(path="data/tidy",namefile,sep='/'))
        cat("Save clean data from file",name, "in","data/tidy",namefile)
      }else{
        write_csv(temp,path=paste("data/tidy",namefile,sep='/'))
        cat("Save clean data from file",name, "in","data/tidy",namefile)
      }
    }else{
      cat("Data from file",name,"contains invalid values")
    }
  }
}
