library(forecast)
library(plotly)

shinyServer(function(input, output){
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot <- renderPlotly({
    if(input$provincia == "Guayas"){
      prov=data.frame(table(fallecidos$ï..fecha,fallecidos$prov_fall))
      prov=prov[prov$Var2=='guayas',]
      
      
      enero1=2191
      marz12=2261
      
    }else
      if(input$provincia == "Pichincha"){
        prov=data.frame(table(fallecidos$ï..fecha,fallecidos$prov_fall))
        prov=prov[prov$Var2=='pichincha',]
        enero1=2191
        marz12=2261  
        
      }else
        if(input$provincia == "Manabi"){
          prov=data.frame(table(fallecidos$ï..fecha,fallecidos$prov_fall))
          prov=prov[prov$Var2=='manabi',]
          enero1=2191
          marz12=2261
          
        }else
          if(input$provincia == "Ecuador"){
            prov=data.frame(table(fallecidos$ï..fecha))
            enero1=2191
            marz12=2261
            
          }
    
    #fijar los datos hasta el 12 de marzo de 2020
    prov.corte=prov[1:marz12,]
    
    #numero de dias para predecir
    dias=nrow(prov)-nrow(prov.corte)
    
    #Convertir los datos de 2020 a time series
    provincia.ts<-ts(prov.corte$Freq, start = c(2014,1), frequency = 365)
    
    #ajustar modelo de provincias
    robusto.provincia=forecast(provincia.ts,robust = T)
    
    #Ajuste del 1 de enero al 12 de marzo 2020
    ajustados.provincia=robusto.provincia$fitted
    ajustados.provincia=ajustados.provincia[enero1:marz12]
    
    #prediccion provincia
    prediccion.provincia=forecast(provincia.ts,h=dias,robust = T)
    prediccion.provincia=as.numeric(prediccion.provincia$mean)
    
    #plot de resultados
    ajuste=rep(0,nrow(prov)-enero1+1)
    
    resultados=data.frame(prov$Var1[enero1:nrow(prov)],ajuste)
    resultados$ajuste[1:71]=round(prov$Freq[enero1:marz12]-ajustados.provincia)
    resultados$ajuste[72:nrow(resultados)]=round(prov$Freq[(marz12+1):nrow(prov)]-prediccion.provincia)
    
    
    names(resultados)[1] <- "fecha"
    names(resultados)[2] <- "exceso"
    resultados$fecha=as.Date(resultados$fecha)
    
    p <- ggplot(resultados, aes(x=fecha, y=exceso)) +
      geom_line() + 
      xlab("Dias")+ geom_hline(yintercept=0, color = "black", size=0.75)+
      labs(x="Fecha",y="Exceso de fallecidos",title=paste("Exceso de muertes de",input$provincia,sep = " "))+
      theme_minimal()
    p
    
  })
  
  output$distPlot1 <- renderPlotly({
    if(input$canton == "Guayaquil"){
      can=data.frame(table(fallecidos$ï..fecha,fallecidos$cant_fall))
      can=can[can$Var2=='guayaquil',]
      enero1=2191
      marz12=2261
      
    }else
      if(input$canton == "Quito"){
        can=data.frame(table(fallecidos$ï..fecha,fallecidos$cant_fall))
        can=can[can$Var2=='quito',]
        can[2261,]
        enero1=2191
        marz12=2261 
        
      }else
        if(input$canton == "Cuenca"){
          can=data.frame(table(fallecidos$ï..fecha,fallecidos$cant_fall))
          can=can[can$Var2=='cuenca',]
          enero1=2191
          marz12=2261
          
        }
    
    #fijar los datos hasta el 12 de marzo de 2020
    can.corte=can[1:marz12,]
    
    #numero de dias para predecir
    dias1=nrow(can)-nrow(can.corte)
    
    #Convertir los datos de 2020 a time series
    canton.ts<-ts(can.corte$Freq, start = c(2014,1), frequency = 365)
    
    #ajustar modelo de cantones
    robusto.canton=forecast(canton.ts,robust = T)
    
    #Ajuste del 1 de enero al 12 de marzo 2020
    ajustados.canton=robusto.canton$fitted
    ajustados.canton=ajustados.canton[enero1:marz12]
    
    #prediccion cantones
    prediccion.canton=forecast(canton.ts,h=dias1,robust = T)
    prediccion.canton=as.numeric(prediccion.canton$mean)
    
    #plot de resultados
    ajuste1=rep(0,nrow(can)-enero1+1)
    
    result=data.frame(can$Var1[enero1:nrow(can)],ajuste1)
    result$ajuste1[1:71]=round(can$Freq[enero1:marz12]-ajustados.canton)
    result$ajuste1[72:nrow(result)]=round(can$Freq[(marz12+1):nrow(can)]-prediccion.canton)
    
    names(result)[1] <- "fecha"
    names(result)[2] <- "exceso"
    result$fecha=as.Date(result$fecha)
    
    p1 <- ggplot(result, aes(x=fecha, y=exceso)) +
      geom_line() + 
      xlab("Dias")+ geom_hline(yintercept=0, color = "black", size=0.75)+
      labs(x="Fecha",y="Exceso de fallecidos",title=paste("Exceso de muertes de",input$canton,sep = " "))+
      theme_minimal()
    p1
    
    
  })
  
})
