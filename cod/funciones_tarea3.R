library(dplyr)

alerta.movil <- function(df, ventana, k.alerta){
  
  df.limpio <- df %>% 
    summarise(
      n.tx = n(),
      .by = fecha
              )
  
  t <- nrow(df.limpio)
  
  desv <- rep(NA, t)
  media <- rep(NA, t)
  alerta <- rep(NA, t)
  
  for (n in (ventana+1):t) {
    
    ventana.datos <- df.limpio$n.tx[(n-ventana):(n-1)]
    
    desv[n] <- sd(ventana.datos)
    media[n] <- mean(ventana.datos)
    
    alerta[n] <- df.limpio$n.tx[n] > (media[n] + k.alerta * desv[n])
    
  }
  
  resultados <- data_frame(
    fecha = df.limpio$fecha,
    n.tx = df.limpio$n.tx,
    media = media,
    desv = desv,
    alerta = alerta
  )
  return(resultados)
  
}

