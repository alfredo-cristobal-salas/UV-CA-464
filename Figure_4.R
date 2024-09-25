#-------------------------------------Carga de Librerias-------------------------------------#
librerias <-  c("mco", "neuralnet", "microbenchmark", "extrafont", "XML")

instalar_libreria <- function(libreria){
  if (!requireNamespace(libreria, quietly = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
  }
}

sapply(librerias, instalar_libreria)

lapply(librerias, library, character.only = TRUE)

#Evitar Notacion Cientifica
options(scipen = 999)

#Cantidad de decimales
options(digits = 16)

#Obtener Reproducibilidad
set.seed(245)

#################################################
#
#          Cargar Datos
#
#################################################
setwd("/Users/DANNO/Downloads/CuerpoAcademico_UV-464/Tatiana")
datos_originales <- read.csv("Experimental_data.csv", sep=' ' , header=TRUE, )
soluciones <<- cbind("","","","","")
colnames(soluciones) <- c("posicion", "capas", "arq", "R2", "t")
posicion <<- 1

#################################################
#
#          Normalización de datos 
#
#################################################      
maximo      <- apply(datos_originales, 2, max)
minimo      <- apply(datos_originales, 2, min)
rango <-  maximo - minimo
escalamiento <- scale(datos_originales, center = minimo, scale = rango)
datos <- as.data.frame(escalamiento)

#################################################
#
#          Tomar muestra
#
#################################################
n <- nrow(datos)
total_entrenamiento <- floor(n*0.7)   #70%
total_prueba <- n-total_entrenamiento #30%

indices  <- sample(1:n, total_entrenamiento, replace=FALSE)
datos_entrenamiento <- datos[indices,]
datos_prueba <- datos[-indices,]

#################################################
#
#          Lista para almacenar pesos de los modelos
#
#################################################
lista_pesos <- list()

#################################################
#
#          Entrenamiento de la red neuronal
#
#################################################
entrena_nn <- function(arquitectura, datos_entrenamiento){
  encabezados <- as.formula("m ~ g+T+L")
  nn <- neuralnet(encabezados,
                  data = datos_entrenamiento,
                  hidden = arquitectura,
                  threshold = 0.005,
                  err.fct = "sse", 
                  rep = 1,
                  act.fct = "logistic", 
                  learningrate = 0.005, 
                  startweights = NULL, 
                  stepmax = 500000
  )
  
  return(nn)
}

#################################################
#
#          Guardar pesos de modelos generados
#
#################################################
guardar_pesos <- function(nn, posicion){
  lista_pesos[[posicion]] <<- nn$weights
}

#################################################
#
#          Prueba de la red neuronal
#
#################################################
prueba_nn_tiempo <- function(nn, datos_prueba){
  datos_entrada_prueba <- subset(datos_prueba, select = -m)
  execute <- microbenchmark(resultados <- predict(nn, datos_prueba))
  execution_time <- round(((mean(execute$time))*0.001),3)
  
  return (execution_time)
}

#################################################
#
#          Prueba de la red neuronal
#
#################################################
prueba_nn_resultados <- function(nn, datos_prueba){
  datos_entrada_prueba <- subset(datos_prueba, select = -m)
  resultados <- predict(nn, datos_prueba)
  
  return (resultados)
}

#################################################
#
#          genera_arquitectura
#
#################################################
genera_arquitectura <- function(vector){
  corte <- floor(runif(1, min=1, max=40))
  restante <- 40-corte
  temp <- rep(0, restante)
  temp2 <- (vector)[1:corte]
  temp3 <- c(temp2,temp)
  i <- 1
  pos <- 1
  cromosoma <- c()
  while(i < 11){
    temp4 <- temp3[pos+3] + 2*temp3[pos+2] + 4*temp3[pos+1] + 8*temp3[pos]
    temp5 <- floor(temp4)
    if(temp5 > 0){
      if(temp5 < 10){
        cromosoma[i] <- temp5
      } else {
        cromosoma[i] <- 10
      }
    } else {
      break;
    }
    
    i <- i + 1
    pos <- pos + 4
  }
  cromosoma[1] <- floor(runif(1, min=1, max=10))
  return (cromosoma)
}

#################################################
#
#          Resultados
#
#################################################
genera_resultados <- function(nn, datos_originales, datos_prueba, resultados, execution_time){
  temp1 <- max(datos_originales$m) - min(datos_originales$m)
  temp2 <- min(datos_originales$m)
  resultados_sin_escalar <- resultados * temp1 + temp2
  datos_sin_escalar <- datos_prueba$m * temp1 + temp2
  SS_res_2 <- sum((datos_sin_escalar - resultados_sin_escalar)^2)
  SS_tot_2 <- sum((datos_sin_escalar - mean(datos_sin_escalar))^2)
  R2_2 <- 1 - (SS_res_2 / SS_tot_2)
  datos_finales_sin_escala <- data.frame(datos_sin_escalar, resultados_sin_escalar, R2_2)
  pasos <- nn$result.matrix[3]
  error <- nn$result.matrix[1]
  inverso <- 1 - R2_2
  tabla_2 <- c(inverso, execution_time)
  return (tabla_2)
}

#################################################
#
#          fitness
#
#################################################
fitness <- function(vector){
  vector2 <- c()
  for (i in 1:40){
    vector2[i] <- round(vector[i],0)
  }
  cromosoma <- genera_arquitectura(vector)
  if(sum(cromosoma) == 0){
    cromosoma[1] <- floor(runif(1, min=1, max=20))
  }
  
  nn <- entrena_nn(cromosoma, datos_entrenamiento)
  guardar_pesos(nn, posicion)
  tiempo <- prueba_nn_tiempo(nn, datos_prueba)
  resultados <- prueba_nn_resultados(nn, datos_prueba)
  y <- numeric(2)
  y <- genera_resultados(nn, datos_originales, datos_prueba, resultados, tiempo)
  
  arq <- toString(cromosoma)
  capas <- length(cromosoma)
  R2 <- y[1]
  t <- y[2]
  nuevo <- cbind(posicion, capas, arq, R2, t)
  soluciones <<- rbind(soluciones, nuevo)
  posicion <<- posicion + 1
  return (y)
}

#################################################
#
#          principal
#
#################################################

r1 <- nsga2(fn=fitness,
            idim=40,
            odim=2,
            lower.bounds=rep(0, 40),
            upper.bounds=rep(1, 40),
            popsize=1000,
            generations=10,
            cprob=0.3, cdist=7,
            mprob=0.4, mdist=4)

#Cargar Fuentes
loadfonts(quiet = TRUE)
par(family = "Arial")

#Modificar Limites de Gráfico, Tipo de Fuente, Estilo de Fuente
plot(r1, xlab = expression(bold(mu == 1 - R^2)), 
     ylab = expression(bold(tau == eval(phi) ~ (mss))), 
     family = "Arial", 
     font.lab = 2)
legend("topright", inset = c(-0.005, -0.01),
       legend = c("Solutions", "Optimal Solutions"),
       col = c("blue", "red"),
       pch = c(16, 4),
       bty = "n",
       cex = 1)

soluciones <- soluciones[-1, ]

#Identificar soluciones optimas
indices_pareto <- which(r1$pareto.optimal)
r1_optimas <- r1$value[indices_pareto, ]
indices_soluciones <- which(soluciones[,4] %in% r1_optimas[,1])
soluciones_optimas <- soluciones[indices_soluciones,]

#Encontrar arquitectura
indice_arquitectura <- as.numeric(soluciones_optimas[which(soluciones_optimas[,4] == min((soluciones_optimas[,4]))), 1])

#Exportar pesos arquitectura
exportar_pesos_XML <- function(indice_arquitectura) {
  
  pesos <- lista_pesos[[indice_arquitectura]]
  
  # Convertir a un nodo XML
  matriz_a_xml <- function(matriz, nombre) {
    xml <- newXMLNode("Matrix", attrs = c(name = nombre))
    for (i in 1:nrow(matriz)) {
      fila <- newXMLNode("Row", attrs = c(index = i), parent = xml)
      for (j in 1:ncol(matriz)) {
        newXMLNode("Cell", matriz[i, j], attrs = c(index = j), parent = fila)
      }
    }
    xml
  }
  
  # Crear nodo raíz
  root <- newXMLNode("Pesos")
  
  for (i in seq_along(pesos)) {
    nombre_lista <- paste0("Layer", i)
    lista <- pesos[[i]]
    lista_xml <- newXMLNode("Layer", attrs = c(index = i), parent = root)
    for (j in seq_along(lista)) {
      nombre_sublista <- paste0("List", j)
      sublista <- lista[[j]]
      sublista_xml <- matriz_a_xml(sublista, nombre_sublista)
      addChildren(lista_xml, sublista_xml)
    }
  }
  
  # Guardar el XML en un archivo
  saveXML(root, file = "Optimal_model_weights.xml")
}

#Exportar metricas NN
exportar_metricas_csv <- function() {
  
  metricas <- data.frame(
    posicion = as.numeric(soluciones[indice_arquitectura, 1]),
    capas = as.numeric(soluciones[indice_arquitectura, 2]),
    arq = soluciones[indice_arquitectura,3],
    R2 = as.numeric(soluciones[indice_arquitectura, 4]),
    t = as.numeric(soluciones[indice_arquitectura, 5])
  )
  
  rownames(metricas) <- NULL
  write.csv(metricas, file = "Optimal_model_data.csv")
}

#Generar archivos 
exportar_pesos_XML(indice_arquitectura)
exportar_metricas_csv()
write.csv(soluciones_optimas, file = "Additional_models.csv")
write.csv(soluciones, file = "Evaluated_models.csv")
write.csv(cbind(r1$value[,1], r1$value[,2], r1$pareto.optimal), file="Models_space.csv")

print("termine")