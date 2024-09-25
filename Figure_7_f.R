
#-------------------------------------Carga de Librerias-------------------------------------#
librerias <-  c("neuralnet", "XML")

instalar_libreria <- function(libreria){
  if (!requireNamespace(libreria, quietly = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
  }
}

sapply(librerias, instalar_libreria)

lapply(librerias, library, character.only = TRUE)

# Evitar Notacion Cientifica
options(scipen = 999)

#################################################
#
#          Cargar Datos
#
#################################################
setwd("/Users/DANNO/Downloads/CuerpoAcademico_UV-464/Tatiana")
datos_originales <- read.csv("Experimental_data.csv", sep=' ' , header=TRUE)

#################################################
#
#          Normalizacion de datos 
#
#################################################      
maximo <- apply(datos_originales, 2, max)
minimo <- apply(datos_originales, 2, min)
rango <- maximo - minimo
escalamiento <- scale(datos_originales, center = minimo, scale = rango)
datos <- as.data.frame(escalamiento)

#################################################
#
#          Tomar muestra
#
#################################################
n <- nrow(datos)
total_entrenamiento <- floor(n*0.7)   #70%

indices <- sample(1:n, total_entrenamiento, replace=FALSE)
datos_entrenamiento <- datos[indices,]

# Función para importar pesos desde XML
importar_pesos <- function() {
  
  # Leer XML
  xml_pesos <- xmlParse("Optimal_model_weights.xml")
  
  # Convertir nodo XML a matriz
  xml_a_matriz <- function(nodo) {
    filas <- getNodeSet(nodo, ".//Row")
    matriz <- do.call(rbind, lapply(filas, function(fila) {
      celdas <- getNodeSet(fila, ".//Cell")
      as.numeric(sapply(celdas, xmlValue))
    }))
    return(matriz)
  }
  
  # Convertir nodo XML a sublista
  xml_a_sublista <- function(nodo) {
    sublistas <- getNodeSet(nodo, ".//Matrix")
    lapply(sublistas, xml_a_matriz)
  }
  
  # Obtener nodos de capas
  nodos_capas <- getNodeSet(xml_pesos, "//Layer")
  pesos <- lapply(nodos_capas, function(capa_nodo) {
    xml_a_sublista(capa_nodo)
  })
  
  return(pesos)
}

# Importar los pesos
pesos_importados <- importar_pesos()

#Importar metricas nn
metricas <- read.csv("Optimal_model_data.csv")

#################################################
#
#          Definir la estructura de la red neuronal
#
#################################################
encabezados <- as.formula("m ~ g+T+L")
architecture <- as.numeric(strsplit(metricas$arq, ", ")[[1]])

# Crear la red neuronal sin entrenar
nn <- neuralnet(encabezados,
                data = datos_entrenamiento,
                hidden = architecture,
                threshold = 0.005,
                err.fct = "sse", 
                rep = 1,
                act.fct = "logistic", 
                learningrate = 0.001, 
                startweights = pesos_importados, 
                stepmax = 500000)

# Asignar los pesos importados a la red neuronal
nn$weights <- pesos_importados

#################################################
#
#          Generación de curva de degradación
#
#################################################
g_con_escala <- as.vector(t(datos[127:133,1]))
T_con_escala <- as.vector(t(datos[127:133,2]))
L_con_escala <- as.vector(t(datos[127:133,3]))
m_real_con_escala <- 1 - as.vector(t(datos[127:133,4]))
datos_curva <- data.frame(g=g_con_escala, T=T_con_escala, L=L_con_escala, m=m_real_con_escala)
resultado_curva <- predict(nn, datos_curva)
m_generada_con_escala <- 1 - resultado_curva


temp1 <- max(datos_originales$m) - min(datos_originales$m)
temp2 <- min(datos_originales$m)
temp <- m_real_con_escala * temp1 + temp2
m_real_sin_escala <- as.data.frame(temp)
temp <- m_generada_con_escala * temp1 + temp2
m_generada_sin_escala <- as.data.frame(temp)

tiempo_de_captura <- as.vector(t(datos_originales[1:7,2]))

grafica_curvas <- cbind(t=tiempo_de_captura, real=7*(m_real_sin_escala/100), generada=7*(m_generada_sin_escala/100))
#Neural Network Model (NNM)
colnames(grafica_curvas) = c("t", "Experimental", "NNM")
print(grafica_curvas)
