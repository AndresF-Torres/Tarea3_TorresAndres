library(dplyr)
library(lubridate)

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





# ej2


benford.mad <- function(x){
  x <- x[x > 0 & !is.na(x)]
  
  d <- floor(x/10^floor(log10(x)))
  
  Bd <- log10(1+1/(1:9))
  
  Pd <- tabulate(d, nbins = 9) / length(d)
  
  mean(abs(Pd - Bd))
}

benford.canton <- function(df, canal1, cantones){
 
  
  solo.cantones <- cantones %>% 
    select(-provincia)
  
    df %>%
      filter(canal == canal1, cod_canton %in% solo.cantones$cod_canton) %>%
      summarise(
        n = sum(!is.na(monto_crc)),
        mad = benford.mad(monto_crc),
        .by = cod_canton
      ) %>% mutate(
        clase = case_when(
          mad < 0.006 ~ "conformidad cercana",
          mad < 0.012 ~ "conformidad aceptable",
          mad < 0.015 ~ "conformidad marginal",
          .default = "no conformidad"
        )
      ) %>% 
      left_join(
        solo.cantones,
        "cod_canton"
      ) %>% 
      select(cod_canton, canton, n, mad, clase)
  }




# ej3


valor.acumulado <- function(df, tasa.anual, canal1) {
 
  df.filtrado <- df %>%
    filter(canal == canal1) %>%
    mutate(
      fecha = as.Date(fecha),
      mes = month(fecha)
    ) %>%
    group_by(mes) %>%
    summarise(monto = sum(monto_crc, na.rm = TRUE)) %>%
    ungroup()
  

  # Calcular la tasa efectiva mensual equivalente 
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  
  
  
  # Calcular el factor de capitalización y el valor parcial por mes
  df.mensual <- df.filtrado %>%
    mutate(
      factor = (1 + tasa.mensual)^(12 - mes),
      valor.parcial = monto * factor
    )
  
  # Calcular el Valor Acumulado (VA) y los intereses totales
  va <- sum(df.mensual$valor.parcial)
  suma.montos <- sum(df.mensual$monto)
  interes <- va - suma.montos
  
  
  tabla <- df.mensual %>% 
    select(mes, monto, factor)
  
  return(list(
    tabla = tabla,
    tasa.mensual = tasa.mensual,
    va = va,
    interes = interes
  ))
}









