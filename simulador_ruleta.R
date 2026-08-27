#tapete#
dibujar_tapete <- function() {
  
  rojos <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  
  par(mar = c(1,1,3,1))
  plot(0, 0, type = "n",
       xlim = c(0, 16), ylim = c(-2.5, 6.5),
       axes = FALSE, xlab = "", ylab = "",
       asp = 0.9,
       main = "Casino del TFG")
  
  verde  <- "#0A4F2E"
  rojo   <- "#B03A2E"
  negro  <- "#1B1B1B"
  dorado <- "#D4AF37"
  borde  <- "#F2F2F2"
  
  # Fondo general
  rect(0, -2.5, 16, 6.5, col = verde, border = dorado, lwd = 4)
  
  # CERO
  rect(0.5, 2, 1.5, 5, col = verde, border = borde, lwd = 1.5)
  text(1, 3.5, "0", col = "white", cex = 1.4, font = 2)
  
  # NÚMEROS
  n <- 1
  for (x in 1:12) {
    for (y in 3:1) {
      color <- if (n %in% rojos) rojo else negro
      rect(x+1, y+1, x+2, y+2,
           col = color, border = borde, lwd = 1)
      text(x+1.5, y+1.5, n,
           col = "white", cex = 0.9, font = 2)
      n <- n + 1
    }
  }
  
  # DOCENAS
  rect(2, 0.5, 6, 1.5, col = verde, border = borde, lwd = 1.2)
  text(4, 1, "1st 12", col = "white", font = 2)
  
  rect(6, 0.5, 10, 1.5, col = verde, border = borde, lwd = 1.2)
  text(8, 1, "2nd 12", col = "white", font = 2)
  
  rect(10, 0.5, 14, 1.5, col = verde, border = borde, lwd = 1.2)
  text(12, 1, "3rd 12", col = "white", font = 2)
  
  # 1-18 y 19-36
  rect(2, -0.5, 6, 0.5, col = verde, border = borde, lwd = 1.2)
  text(4, 0, "1 to 18", col = "white", font = 2, cex = 0.8)
  
  rect(10, -0.5, 14, 0.5, col = verde, border = borde, lwd = 1.2)
  text(12, 0, "19 to 36", col = "white", font = 2, cex = 0.8)
  
  # ROJO / NEGRO con rombos
  # ROJO
  rect(6, -0.5, 8, 0.5, col = verde, border = borde, lwd = 1.2)
  
  polygon(c(6.5,7,7.5,7,6.5),
          c(0,0.5,0, -0.5,0),
          col = rojo, border = NA)
  
  # NEGRO
  rect(8, -0.5, 10, 0.5, col = verde, border = borde, lwd = 1.2)
  
  polygon(c(8.5,9,9.5,9,8.5),
          c(0,0.5,0,-0.5,0),
          col = negro, border = NA)
  
  
  # PAR / IMPAR
  rect(2, -1.5, 6, -0.5, col = verde, border = borde, lwd = 1.2)
  text(4, -1, "EVEN", col = "white", font = 2)
  
  rect(10, -1.5, 14, -0.5, col = verde, border = borde, lwd = 1.2)
  text(12, -1, "ODD", col = "white", font = 2)
  
  # FILAS (2 to 1)
  rect(14, 4, 15.5, 5, col = verde, border = borde, lwd = 1.2)
  text(14.75, 4.5, "2 to 1", col = "white", font = 2, cex = 0.8)
  
  rect(14, 3, 15.5, 4, col = verde, border = borde, lwd = 1.2)
  text(14.75, 3.5, "2 to 1", col = "white", font = 2, cex = 0.8)
  
  rect(14, 2, 15.5, 3, col = verde, border = borde, lwd = 1.2)
  text(14.75, 2.5, "2 to 1", col = "white", font = 2, cex = 0.8)
}

dibujar_tapete()


##########################
# FUNCION DIBUJAR FICHAS #
##########################

dibujar_ficha <- function(x,y,valor){
  
  points(x,y,
         pch=21,
         bg=COLORES_FICHAS[as.character(valor)],
         cex=2)
  
  text(x,y,valor,col="white",cex=0.7)
  
}

########################
#    DIBUJO BOLITA      #
########################
dibujar_ruleta_bolita <- function(numero){
  
  orden <- c(0,32,15,19,4,21,2,25,17,34,
             6,27,13,36,11,30,8,23,10,
             5,24,16,33,1,20,14,31,9,
             22,18,29,7,28,12,35,3,26)
  
  rojos <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  
  n <- length(orden)
  angulos <- seq(0,2*pi,length.out=n+1)
  
  idx <- which(orden==numero)
  ang_final <- mean(c(angulos[idx],angulos[idx+1]))
  
  angulo <- 0
  
  for(i in 1:150){
    
    plot(0,0,
         type="n",
         xlim=c(-1.3,1.3),
         ylim=c(-1.3,1.3),
         asp=1,
         axes=FALSE,
         main="Girando la ruleta")
    
    for(j in 1:n){
      
      num <- orden[j]
      
      col <- if(num==0) "#0A4F2E"
      else if(num %in% rojos) "#B03A2E"
      else "#1B1B1B"
      
      polygon(c(0,cos(angulos[j]),cos(angulos[j+1])),
              c(0,sin(angulos[j]),sin(angulos[j+1])),
              col=col,
              border="white")
      
      ang <- mean(c(angulos[j],angulos[j+1]))
      
      text(0.85*cos(ang),
           0.85*sin(ang),
           num,
           col="white",
           cex=0.7)
      
    }
    
    points(1.05*cos(angulo),
           1.05*sin(angulo),
           pch=21,
           bg="white",
           cex=2)
    
    angulo <- angulo + 0.3
    
    Sys.sleep(0.02)
    
  }
  
  plot(0,0,
       type="n",
       xlim=c(-1.3,1.3),
       ylim=c(-1.3,1.3),
       asp=1,
       axes=FALSE,
       main=paste("Resultado:",numero))
  
  for(j in 1:n){
    
    num <- orden[j]
    
    col <- if(num==0) "#0A4F2E"
    else if(num %in% rojos) "#B03A2E"
    else "#1B1B1B"
    
    polygon(c(0,cos(angulos[j]),cos(angulos[j+1])),
            c(0,sin(angulos[j]),sin(angulos[j+1])),
            col=col,
            border="white")
    
    ang <- mean(c(angulos[j],angulos[j+1]))
    
    text(0.85*cos(ang),
         0.85*sin(ang),
         num,
         col="white",
         cex=0.7)
    
  }
  
  points(1.05*cos(ang_final),
         1.05*sin(ang_final),
         pch=21,
         bg="white",
         cex=2.5)
  
}
#############################################
# POSICIONES DE LOS NUMEROS EN EL TAPETE    #
#############################################

posiciones <- data.frame()

n <- 1

for(x in 1:12){
  for(y in 3:1){
    
    posiciones <- rbind(
      posiciones,
      data.frame(
        numero = n,
        x = x + 1.5,
        y = y + 1.5
      )
    )
    
    n <- n + 1
    
  }
}

##############################################################




#juego normal#

NUMEROS <- 0:36

ROJOS <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
NEGROS <- setdiff(1:36, ROJOS)





#############################################
# FICHAS DE CASINO                          #
#############################################

FICHAS <- c(5,10,25,50,100,500,1000)

COLORES_FICHAS <- c(
  "5"="red",
  "10"="blue",
  "25"="green",
  "50"="brown",
  "100"="black",
  "500"="purple",
  "1000"="yellow"
)

#############################################
# COLUMNAS Y DOCENAS                        #
#############################################

COLUMNAS <- list(
  c(1,4,7,10,13,16,19,22,25,28,31,34),
  c(2,5,8,11,14,17,20,23,26,29,32,35),
  c(3,6,9,12,15,18,21,24,27,30,33,36)
)

DOCENAS <- list(
  1:12,
  13:24,
  25:36
)

#######################################################################

TIPOS_APUESTA <- c(
  "numero", "rojo", "negro",
  "par", "impar",
  "bajo", "alto",
  "docena", "columna", "multiple"
)


girar_ruleta <- function() {
  sample(NUMEROS, 1)
}



validar_apuesta <- function(tipo, valor, apuesta, saldo) {
  
  if (!tipo %in% TIPOS_APUESTA)
    stop("Tipo de apuesta no válido")
  
  if (tipo == "numero") {
    if (is.na(valor) || valor < 0 || valor > 36)
      stop("Número fuera de rango")
  }
  
  if (apuesta <= 0 || apuesta > saldo)
    stop("Cantidad apostada no válida")
}


evaluar_apuesta <- function(tipo, valor, resultado) {
  
  if (resultado == 0 && tipo != "numero") return(FALSE)
  
  switch(tipo,
         
         "numero" = resultado == valor,
         
         "rojo"   = resultado %in% ROJOS,
         "negro"  = resultado %in% NEGROS,
         
         "par"    = resultado %% 2 == 0,
         "impar"  = resultado %% 2 == 1,
         
         "bajo"   = resultado >= 1 & resultado <= 18,
         "alto"   = resultado >= 19 & resultado <= 36,
         
         "docena" = resultado %in% DOCENAS[[valor]],
         
         "columna" = resultado %in% COLUMNAS[[valor]]
  )
}


calcular_premio <- function(tipo, apuesta) {
  # Pleno: paga 35 a 1
  if (tipo == "numero")                    return(apuesta * 35)
  # Docenas y columnas: pagan 2 a 1
  if (tipo %in% c("docena", "columna"))    return(apuesta * 2)
  # Suertes sencillas (rojo/negro/par/impar/alto/bajo): pagan 1 a 1
  return(apuesta)
}


jugar_ruleta <- function(saldo_inicial = 1000) {
  
  saldo <- saldo_inicial
  historial <- data.frame()
  
  cat("\n🎰 SIMULADOR DE RULETA EUROPEA 🎰\n")
  
  repeat {
    
    apuestas <- list()
    
    dibujar_tapete()
    
    cat("\nSaldo actual:", saldo, "€\n")
    
    repeat{
      
      cat("\nTipos: numero | rojo | negro | par | impar | bajo | alto | docena | columna | multiple\n")
      
      tipo <- tolower(readline("Tipo de apuesta (o 'girar'): "))
      
      if(tipo=="girar") break
      
      ficha <- as.numeric(readline("Ficha (5,10,25,50,100,500,1000): "))
      
      if(!ficha %in% FICHAS){
        
        cat("❌ ficha no válida\n")
        next
        
      }
      
      if(ficha > saldo){
        
        cat("❌ saldo insuficiente\n")
        next
        
      }
      
      if(tipo=="numero"){
        
        valor <- as.numeric(readline("Número: "))
        numeros <- valor
        
      }
      
      else if(tipo=="rojo") numeros <- ROJOS
      else if(tipo=="negro") numeros <- NEGROS
      else if(tipo=="par") numeros <- seq(2,36,2)
      else if(tipo=="impar") numeros <- seq(1,35,2)
      else if(tipo=="bajo") numeros <- 1:18
      else if(tipo=="alto") numeros <- 19:36
      
      else if(tipo=="docena"){
        
        d <- as.numeric(readline("Docena (1,2,3): "))
        numeros <- DOCENAS[[d]]
        
      }
      
      else if(tipo=="columna"){
        
        num_col <- as.numeric(readline("Columna (1,2,3): "))
        numeros <- COLUMNAS[[num_col]]
        
      }
      
      else if(tipo=="multiple"){
        
        txt <- readline("Números separados por coma: ")
        numeros <- as.numeric(strsplit(txt,",")[[1]])
        
      }
      
      else{
        
        cat("❌ tipo no válido\n")
        next
        
      }
      
      saldo <- saldo - ficha
      
      apuestas[[length(apuestas)+1]] <- list(
        tipo=tipo,
        numeros=numeros,
        ficha=ficha
      )
      
      # dibujar fichas
      
      if(tipo=="numero"){
        
        p <- posiciones[posiciones$numero==valor,]
        dibujar_ficha(p$x,p$y,ficha)
        
      }
      
      if(tipo=="multiple"){
        
        p <- posiciones[posiciones$numero %in% numeros,]
        dibujar_ficha(mean(p$x),mean(p$y),ficha)
        
      }
      
      if(tipo=="rojo") dibujar_ficha(7,0,ficha)
      if(tipo=="negro") dibujar_ficha(9,0,ficha)
      
      if(tipo=="par") dibujar_ficha(4,-1,ficha)
      if(tipo=="impar") dibujar_ficha(12,-1,ficha)
      
      if(tipo=="docena"){
        
        if(d==1) dibujar_ficha(4,1,ficha)
        if(d==2) dibujar_ficha(8,1,ficha)
        if(d==3) dibujar_ficha(12,1,ficha)
        
      }
      
      if(tipo=="columna"){
        
        if(num_col==1) dibujar_ficha(14.75,4.5,ficha)
        if(num_col==2) dibujar_ficha(14.75,3.5,ficha)
        if(num_col==3) dibujar_ficha(14.75,2.5,ficha)
        
      }
      
    }
    
    resultado <- girar_ruleta()
    
    dibujar_ruleta_bolita(resultado)
    
    ganancia <- 0
    
    for(a in apuestas){
      
      if(resultado %in% a$numeros){
        
        n <- length(a$numeros)
        
        if(n==1) pago <- 35
        else if(n==2) pago <- 17
        else if(n==3) pago <- 11
        else if(n==4) pago <- 8
        else if(n==6) pago <- 5
        else if(n==12) pago <- 2
        else if(n==18) pago <- 1
        else pago <- 35/n
        
        premio <- a$ficha * pago
        
        saldo <- saldo + a$ficha + premio
        
        ganancia <- ganancia + premio
        
      }
      
    }
    
    cat("\n==============================\n")
    cat("🎲 RESULTADO DE LA RULETA:", resultado,"\n")
    cat("==============================\n")
    
    for(a in apuestas){
      
      gana <- resultado %in% a$numeros
      
      if(gana){
        
        n <- length(a$numeros)
        
        if(n==1) pago <- 35
        else if(n==2) pago <- 17
        else if(n==3) pago <- 11
        else if(n==4) pago <- 8
        else if(n==6) pago <- 5
        else if(n==12) pago <- 2
        else if(n==18) pago <- 1
        else pago <- 35/n
        
        premio <- a$ficha * pago
        
        cat("✅ Apuesta ganadora:", a$tipo,
            "| Ficha:", a$ficha,
            "| Premio:", premio, "\n")
        
      } else {
        
        cat("❌ Apuesta perdida:", a$tipo,
            "| Ficha:", a$ficha,"\n")
        
      }
    }
    
    cat("\n💰 Ganancia total:",ganancia,"\n")
    cat("💳 Saldo actual:",saldo,"\n\n")
    
    if(saldo<=0){
      
      cat("\n💀 Sin saldo\n")
      break
      
    }
    
    seguir <- readline("¿Seguir jugando? (s/n): ")
    
    if(seguir!="s") break
    
  }
  
}
jugar_ruleta()













##############
#MONTECARLO  #
##############
simular_partida <- function(
    n_tiradas = 100,
    saldo_inicial = 1000,
    tipo = "rojo",
    valor = NA,
    apuesta = 10
) {
  
  saldo <- saldo_inicial
  
  for (i in 1:n_tiradas) {
    
    if (saldo < apuesta) {
      return(0)  # ruina
    }
    resultado <- girar_ruleta()
    gana <- evaluar_apuesta(tipo, valor, resultado)
    
    saldo <- saldo - apuesta
    
    if (gana) {
      premio <- calcular_premio(tipo, apuesta)
      saldo <- saldo + apuesta + premio
    }
  }
  
  return(saldo)
}

#Función Monte Carlo (N simulaciones)
monte_carlo_ruleta <- function(
    n_simulaciones,
    n_tiradas = 100,
    saldo_inicial = 1000,
    tipo = "rojo",
    valor = NA,
    apuesta = 10
) {
  
  saldos_finales <- replicate(
    n_simulaciones,
    simular_partida(
      n_tiradas,
      saldo_inicial,
      tipo,
      valor,
      apuesta
    )
  )
  
  data.frame(
    simulaciones = n_simulaciones,
    saldo_medio = mean(saldos_finales),
    ganancia_media = mean(saldos_finales - saldo_inicial),
    prob_ruina = mean(saldos_finales <= 0)
  )
}

#Ejecutar simulaciones (1.000 / 10.000 / 20.000)
set.seed(123)
mc_100  <- monte_carlo_ruleta(100)
mc_1000  <- monte_carlo_ruleta(1000)
mc_10000 <- monte_carlo_ruleta(10000)
mc_20000 <- monte_carlo_ruleta(20000)

resultados <- rbind(mc_100, mc_1000, mc_10000, mc_20000)
print(resultados)

#Distribución del saldo final (10.000 simulaciones)
saldos_10000 <- replicate(
  10000,
  simular_partida(
    n_tiradas = 100,
    saldo_inicial = 1000,
    tipo = "rojo",
    apuesta = 10
  )
)

hist(
  saldos_10000,
  breaks = 50,
  col = "darkred",
  main = "Distribución del saldo final (10.000 simulaciones)",
  xlab = "Saldo final"
)


########################
# MONTE CARLO DOCENAS #
#######################

mc_docena1 <- monte_carlo_ruleta(1000, tipo="docena", valor=1)
mc_docena2 <- monte_carlo_ruleta(10000, tipo="docena", valor=2)
mc_docena3 <- monte_carlo_ruleta(20000, tipo="docena", valor=3)

docenas <- rbind(
  docena1 = mc_docena1,
  docena2 = mc_docena2,
  docena3 = mc_docena3
)

print(docenas)

# Distribución del saldo final en apuestas a Docenas (10.000 simulaciones)
# Recordamos que aquí la probabilidad de éxito es del 32.43% pero el pago es 2:1
saldos_docenas_10000 <- replicate(
  10000,
  simular_partida(
    n_tiradas = 100,
    saldo_inicial = 1000,
    tipo = "docena",
    valor = 1,      # Apostamos a la primera docena
    apuesta = 10
  )
)

# Generación del Histograma
hist(
  saldos_docenas_10000,
  breaks = 50,
  col = "darkred", 
  main = "Distribución del saldo final: Apuestas a Docenas",
  xlab = "Saldo final (Euros)",
  ylab = "Frecuencia"
)

# Añadimos una línea vertical en el saldo inicial para ver visualmente las pérdidas
abline(v = 1000, col = "red", lwd = 2, lty = 2)

#############################
### MONTE CARLO COLUMNAS ###
#############################
mc_columna1 <- monte_carlo_ruleta(1000, tipo="columna", valor=1)
mc_columna2 <- monte_carlo_ruleta(10000, tipo="columna", valor=2)
mc_columna3 <- monte_carlo_ruleta(20000, tipo="columna", valor=3)

columnas <- rbind(
  columna1 = mc_columna1,
  columna2 = mc_columna2,
  columna3 = mc_columna3
)

print(columnas)

#############################
### MONTE CARLO NUMERO ###
#############################

mc_numero17 <- monte_carlo_ruleta(
  10000,
  tipo="numero",
  valor=17
)

print(mc_numero17)


# 1. Simulación de 10.000 partidas apostando al número 17
# En cada partida se realizan 100 tiradas de 10€ cada una
set.seed(123) # Para que el resultado sea replicable
saldos_pleno_10000 <- replicate(
  10000,
  simular_partida(
    n_tiradas = 100,
    saldo_inicial = 1000,
    tipo = "numero",
    valor = 17,
    apuesta = 10
  )
)

# 2. Generación del Histograma de Plenos
hist(
  saldos_pleno_10000,
  breaks = 50,              
  col = "darkred",        
  main = "Distribución del Saldo Final (Apuesta al Número 17)",
  xlab = "Saldo final (Euros)",
  ylab = "Frecuencia de jugadores",
  xlim = c(0, 4000)         # Ampliamos el eje X para ver los casos de éxito
)





##########################################################
#### GENERACIÓN DE ESCENARIOS Y ANÁLISIS AVANZADO
##########################################################

# Definimos parámetros comunes
n_sims <- 10000
esperanza_teorica_total <- 1000 + (100 * (-(1/37) * 10)) # 972.97

# A. CREAMOS LAS VARIABLES
cat("Simulando escenario de ROJO...\n")
saldos_rojo <- replicate(n_sims, simular_partida(n_tiradas = 100, tipo = "rojo", apuesta = 10))

cat("Simulando escenario de DOCENA...\n")
saldos_docena <- replicate(n_sims, simular_partida(n_tiradas = 100, tipo = "docena", valor = 1, apuesta = 10))

cat("Simulando escenario de NÚMERO PLENO (17)...\n")
saldos_numero <- replicate(n_sims, simular_partida(n_tiradas = 100, tipo = "numero", valor = 17, apuesta = 10))


# B. FUNCIÓN DE ANÁLISIS
realizar_analisis_avanzado <- function(saldos_vector, nombre_escenario) {
  
  n_obs <- length(saldos_vector)
  media_muestral <- mean(saldos_vector)
  desv_tipica <- sd(saldos_vector)
  error_estandar <- desv_tipica / sqrt(n_obs)
  
  # Intervalo de Confianza 95%
  ic_inf <- media_muestral - 1.96 * error_estandar
  ic_sup <- media_muestral + 1.96 * error_estandar
  
  # Métricas de precisión
  sesgo <- media_muestral - esperanza_teorica_total
  mse <- mean((saldos_vector - esperanza_teorica_total)^2)
  
  # Impresión de resultados
  cat("\n================================================\n")
  cat("  RESULTADOS:", nombre_escenario, "\n")
  cat("================================================\n")
  cat("Media saldo final:     ", round(media_muestral, 3), "€\n")
  cat("Desviación típica:     ", round(desv_tipica, 3), "\n")
  cat("Error estándar:        ", round(error_estandar, 4), "\n")
  cat("IC 95% Saldo Medio:    [", round(ic_inf, 2), ",", round(ic_sup, 2), "]\n")
  cat("------------------------------------------------\n")
  cat("Sesgo (Bias):          ", round(sesgo, 4), "\n")
  cat("MSE (Error Cuadrático):", round(mse, 2), "\n")
}

# C. EJECUTAR EL ANÁLISIS
realizar_analisis_avanzado(saldos_rojo, "APUESTAS SENCILLAS (ROJO)")
realizar_analisis_avanzado(saldos_docena, "APUESTAS MÚLTIPLES (DOCENA)")
realizar_analisis_avanzado(saldos_numero, "APUESTA DE PLENO (NÚMERO 17)")



##########################
# CONVERGENCIA MONTE CARLO
##########################

# Cálculo de media acumulada para Rojo
n_sims <- length(saldos_rojo)
media_acum_rojo <- cumsum(saldos_rojo) / (1:n_sims)
esperanza_teorica <- 1000 + (100 * (-(1/37) * 10))

# Generación del gráfico
plot(media_acum_rojo, 
     type = "l", 
     col = "red", 
     lwd = 2,
     main = "Convergencia de la Media: Apuestas 1 a 1", 
     xlab = "Número de Simulaciones", 
     ylab = "Saldo Medio Acumulado (€)",
     ylim = c(min(media_acum_rojo)-10, 1010)) # Ajuste de escala para ver el 1000

# Línea de esperanza teórica (Rojo) y saldo inicial (Negro punteado)
abline(h = esperanza_teorica, col = "darkgrey", lwd = 2, lty = 2)
abline(h = 1000, col = "black", lwd = 1, lty = 3)

legend("topright", legend=c("Media Muestral", "Esperanza Teórica"), 
       col=c("red", "darkgrey"), lty=c(1, 2), lwd=2)






# Cálculo de media acumulada para Docena
n_sims <- length(saldos_docena)
media_acum_docena <- cumsum(saldos_docena) / (1:n_sims)

# Generación del gráfico
plot(media_acum_docena, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     main = "Convergencia de la Media: Apuestas 1 a 2", 
     xlab = "Número de Simulaciones", 
     ylab = "Saldo Medio Acumulado (€)",
     ylim = c(min(media_acum_docena)-10, 1010))

# Líneas de referencia
abline(h = esperanza_teorica, col = "darkgrey", lwd = 2, lty = 2)
abline(h = 1000, col = "black", lwd = 1, lty = 3)

legend("topright", legend=c("Media Muestral", "Esperanza Teórica"), 
       col=c("blue", "darkgrey"), lty=c(1, 2), lwd=2)




# Cálculo de media acumulada para Número 17
n_sims <- length(saldos_numero)
media_acum_num <- cumsum(saldos_numero) / (1:n_sims)

# Generación del gráfico
plot(media_acum_num, 
     type = "l", 
     col = "darkgreen", 
     lwd = 2,
     main = "Convergencia de la Media: 35 a 1", 
     xlab = "Número de Simulaciones", 
     ylab = "Saldo Medio Acumulado (€)",
     ylim = c(min(media_acum_num)-50, max(media_acum_num)+50))

# Líneas de referencia
abline(h = esperanza_teorica, col = "darkgrey", lwd = 2, lty = 2)
abline(h = 1000, col = "black", lwd = 1, lty = 3)

legend("topright", legend=c("Media Muestral", "Esperanza Teórica"), 
       col=c("darkgreen", "darkgrey"), lty=c(1, 2), lwd=2)









##############################
#HISTOGRAMA CON NORMAL TEORICA
##############################

hist(saldos_10000,  #saldos finales de las 10.000 simulaciones.
     probability = TRUE,
     breaks = 50,
     col = "darkred",
     main = "Distribucion saldo final + Normal aproximada",
     xlab = "Saldo final")

curve(dnorm(x, mean = mean(saldos_10000), sd = sd(saldos_10000)),
      col = "blue",
      lwd = 2,
      add = TRUE)



###################################
# Análisis de frecuencia de números#
###################################

frecuencias <- table(sample(0:36, 100000, replace = TRUE))
barplot(frecuencias, col = ifelse(names(frecuencias) == "0", "green", 
                                  ifelse(names(frecuencias) %in% ROJOS, "red", "black")),
        main = "Distribución de Frecuencias: 100.000 Giros",
        xlab = "Número", ylab = "Frecuencia")




######################################################
# Cálculo de probabilidad de ruina variando la apuesta
####################################################
ruina_10  <- monte_carlo_ruleta(10000, apuesta = 10)  
ruina_50  <- monte_carlo_ruleta(10000, apuesta = 50)
ruina_100 <- monte_carlo_ruleta(10000, apuesta = 100)

# Resultados para la tabla
print(ruina_10$prob_ruina)
print(ruina_50$prob_ruina)
print(ruina_100$prob_ruina)




##########################################
#ANÁLISIS DE SENSIBILIDAD: APUESTA vs RUINA#
##########################################

# Definimos diferentes niveles de apuesta para testear
niveles_apuesta <- c(10, 20, 50, 100, 150, 200, 500)
resultados_sensibilidad <- data.frame(Apuesta = numeric(), 
                                      Prob_Ruina = numeric(), 
                                      Media_Final = numeric())

# Ejecutamos el bucle de simulación
for(ap in niveles_apuesta) {
  # Simulamos 5.000 partidas para cada nivel (para ir más rápido)
  sim_temp <- monte_carlo_ruleta(5000, apuesta = ap)
  
  # Guardamos los datos
  resultados_sensibilidad <- rbind(resultados_sensibilidad, 
                                   data.frame(Apuesta = ap, 
                                              Prob_Ruina = sim_temp$prob_ruina, 
                                              Media_Final = sim_temp$saldo_medio))
}

# Ver tabla de resultados en consola
print(resultados_sensibilidad)

# Gráfico de Sensibilidad: ¿Cómo sube la ruina al subir la apuesta?
par(mar = c(5, 5, 4, 2))

plot(resultados_sensibilidad$Apuesta, 
     resultados_sensibilidad$Prob_Ruina * 100,
     type = "b", col = "darkblue", pch = 19, lwd = 2,
     main = "Análisis de Sensibilidad: Riesgo de Ruina",
     xlab = "Tamaño de la Apuesta (€)", 
     ylab = "Probabilidad de Ruina (%)",
     xlim = c(-20, 560),
     ylim = c(-5, 105),
     axes = FALSE)

box()

axis(1, at = c(10, 20, 50, 100, 150, 200, 500),
     labels = c("10€", "20€", "50€", "100€", "150€", "200€", "500€"),
     cex.axis = 0.75, las = 2)

axis(2, at = seq(0, 100, by = 10),
     labels = paste0(seq(0, 100, by = 10), "%"),
     las = 1)

grid()


# Etiquetas encima de cada punto
text(resultados_sensibilidad$Apuesta,
     resultados_sensibilidad$Prob_Ruina * 100,
     labels = paste0(round(resultados_sensibilidad$Prob_Ruina * 100, 1), "%"),
     pos = 4, cex = 0.85, col = "darkblue", font = 2)



##########################
#### SIMULACION MARTINGALA
##########################

simular_martingala <- function(
    n_tiradas      = 100,
    saldo_inicial  = 1000,
    apuesta_base   = 10,
    limite_mesa    = 500
) {
  saldo   <- saldo_inicial
  apuesta <- apuesta_base
  
  for (i in 1:n_tiradas) {
    if (saldo < apuesta) return(0)          
    
    resultado <- girar_ruleta()
    gana      <- evaluar_apuesta("rojo", NA, resultado)
    
    if (gana) {
      saldo   <- saldo - apuesta + apuesta + apuesta  # recupera ficha + gana igual
      apuesta <- apuesta_base                          # reinicia la apuesta
    } else {
      saldo   <- saldo - apuesta
      apuesta <- min(apuesta * 2, limite_mesa)         # dobla, respetando límite de mesa
    }
  }
  return(saldo)
}

# Comparativa: estrategia fija vs Martingala (10.000 simulaciones)
set.seed(123)
saldos_martingala <- replicate(10000, simular_martingala())

cat("\n=== MARTINGALA vs APUESTA FIJA (Rojo, 10.000 sims) ===\n")
cat("Media Martingala:    ", round(mean(saldos_martingala), 2), "€\n")
cat("Media Apuesta Fija:  ", round(mean(saldos_rojo), 2),       "€\n")
cat("Prob. Ruina Martingala:", round(mean(saldos_martingala <= 0), 4), "\n")
cat("Prob. Ruina Fija:      ", round(mean(saldos_rojo <= 0), 4),       "\n")

# Histograma comparativo
par(mfrow = c(1, 2))
hist(saldos_rojo,       breaks=40, col="darkred",  main="Apuesta Fija (Rojo)",  xlab="Saldo final", xlim=c(0,1400))
hist(saldos_martingala, breaks=40, col="darkblue", main="Martingala",           xlab="Saldo final", xlim=c(0,1400))
par(mfrow = c(1, 1))




##########################
#### TEST DE NORMALIDAD (KS)
##########################

# Verifica formalmente que los saldos finales siguen una distribución normal
ks_rojo <- ks.test(
  saldos_rojo,
  "pnorm",
  mean = mean(saldos_rojo),
  sd   = sd(saldos_rojo)
)

cat("\n=== TEST KOLMOGOROV-SMIRNOV (Apuesta Rojo) ===\n")
cat("Estadístico D:", round(ks_rojo$statistic, 4), "\n")
cat("p-valor:      ", round(ks_rojo$p.value, 4),   "\n")
cat("Conclusión: si p > 0.05, no se rechaza la normalidad\n")





################################
#HISTOGRAMA MARTINGALA%
################################

par(mfrow = c(1, 2))

hist(saldos_rojo,
     breaks = 40, col = "darkred",
     main = "Apuesta Fija (Rojo)",
     xlab = "Saldo final (€)", ylab = "Frecuencia")

hist(saldos_martingala[saldos_martingala > 0],  # excluimos ruinas para ver la distribución
     breaks = 40, col = "darkblue",
     main = "Martingala (supervivientes)",
     xlab = "Saldo final (€)", ylab = "Frecuencia")

par(mfrow = c(1, 1))

# Y aparte, un barplot que muestre ruina vs supervivencia
barplot(c(Ruina = mean(saldos_martingala <= 0),
          Supervivencia = mean(saldos_martingala > 0)),
        col = c("red", "darkblue"),
        main = "Martingala: Resultado final (10.000 sims)",
        ylab = "Proporción de jugadores",
        ylim = c(0, 1))

