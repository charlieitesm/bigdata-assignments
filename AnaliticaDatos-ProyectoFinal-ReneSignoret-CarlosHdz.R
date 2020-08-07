################################
#
# Instituto Tecnologico de Estudios Superiores de Monterrey
# MCC
#
# Analitica de Datos
#
# Proyecto Final
#
# Rene Signoret - A
# Carlos E. Hernandez - A01181616
#
# Agosto 2020
################################


getwd()

# Modificar esto a la carpeta donde se encuentran los datasets
dataset.folder.rene = "/home/renesignoret/Documents/bigdata-assignments/ProfessorFiles"
dataset.folder.carlos = "/Users/carlosh/Desktop/CHARLIE/ITESM/CODE/bigdata-assignments/ProfessorFiles"
dataset.folder <- dataset.folder.carlos

setwd(dataset.folder)


# --- 1. Cargar Datasets y codificacion - truncar la informacion para usar solo 2 semestres


# Este sera el repositorio del perfil de todas las caracteristicas de los alumnos
load("perfilAlumnos.R")
#class(perfil.alumnos) # Dataframe
#dim(perfil.alumnos) # 1000 rows, 7 columns
#head(perfil.alumnos)

datos.alumnos.integrados <- perfil.alumnos

# Cambiar a factores las columnas de valores discretos/categorigos
datos.alumnos.integrados$genero <- as.factor(datos.alumnos.integrados$genero)
datos.alumnos.integrados$evaluacion.socioeconomica <- as.factor(datos.alumnos.integrados$evalucion.socioeconomica)
datos.alumnos.integrados$evalucion.socioeconomica <- NULL # Arreglar typo en nombre columna

load("AsistenciasTotales.R")
#class(asistencias.totales)  # List
#length(asistencias.totales) # 1000 matrices of 32,54
#head(asistencias.totales)
#asistencias.totales[[1]]
#dim(asistencias.totales[[1]])

# Obtener todos los rows de las matrices de los 1000 alumnos, pero solo 12 
# materias (2 anios)
# Codificamos las asistencias en un solo numero por alumno usando la suma de sus 
# codigos de asistencia
#  Los que atendieron menos clases o tuvieron retardo, tendran menores puntajes
#  2 A tiempo, 1 Retraso, 0 falto a clases
asistencias.totales.filtered <- lapply(asistencias.totales, function(m){
        sum(m[,1:12])
    })

# Add the column to the dataframe
datos.alumnos.integrados$asistencias.totales <- unlist(asistencias.totales.filtered)
head(datos.alumnos.integrados)



load("ResultadosExamenes.R")
#class(resultados.examenes.totales) # List
#length(resultados.examenes.totales) # 1000 matrices of 2, 54
#class(resultados.examenes.totales[[1]])
#dim(resultados.examenes.totales[[1]])
#resultados.examenes.totales[[1]]

# Filtrar a solo 12 materias (2 anios) y codificarlas al promedio de todas las materias
resultados.examenes.filtrados <- lapply(resultados.examenes.totales, function(m){
    mean(m[,1:12])
})
datos.alumnos.integrados$resultados.examenes <- unlist(resultados.examenes.filtrados)
head(datos.alumnos.integrados)



load("ResultadoTrabajos.R")
#class(resultados.trabajos.totales) # List
#length(resultados.trabajos.totales) # 1000 matrices of 4, 54
#class(resultados.trabajos.totales[[1]])
#dim(resultados.trabajos.totales[[1]])
#resultados.trabajos.totales[[1]]

# Filtrar a solo 12 materias (2 anios) y codificarlas al promedio de todas las materias
resultados.trabajos.filtrados <- lapply(resultados.trabajos.totales, function(m){
    mean(m[,1:12])
})
#head(resultados.trabajos.filtrados)
datos.alumnos.integrados$resultados.trabajos <- unlist(resultados.trabajos.filtrados)
head(datos.alumnos.integrados)



load("UsoBiblioteca.R")
#class(uso.biblioteca.totales) # List
#length(uso.biblioteca.totales) # 1000 matrices of 1, 54
#class(uso.biblioteca.totales[[1]])
#dim(uso.biblioteca.totales[[1]])
#uso.biblioteca.totales[[1]]

# Filtrar a 12 materias y codificarlas con el promedio redondeado de visitas
uso.biblioteca.filtrados <- lapply(uso.biblioteca.totales, function(m){
    round(mean(m[,1:12]))
})
#head(uso.biblioteca.filtrados)
datos.alumnos.integrados$uso.biblioteca <- unlist(uso.biblioteca.filtrados)
head(datos.alumnos.integrados)



load("UsoPlataforma.R")
#class(uso.plataforma.totales) # List
#length(uso.plataforma.totales) # 1000 matrices of 1, 54
#dim(uso.plataforma.totales[[1]])
#uso.plataforma.totales[[1]]

# Filtrar a 12 materias y codificar con el promedio redondeado de uso
uso.plataforma.filtrado <- lapply(uso.plataforma.totales, function(m){
    round(mean(m[,1:12]))
})
#head(uso.plataforma.filtrado)
datos.alumnos.integrados$uso.plataforma <- unlist(uso.plataforma.filtrado)
head(datos.alumnos.integrados)
summary(datos.alumnos.integrados)


load("ApartadoDeLibros.R")
#class(separacion.libros.totales) # List of 1000 matrices of 1,54
#dim(separacion.libros.totales[[1]])
#head(separacion.libros.totales)

# Filtrar a 12 materias y codificar con la suma del numero de prestamos
apartado.libros.filtrado <- lapply(separacion.libros.totales, function(m){
    sum(m[,1:12])
})
#head(apartado.libros.filtrado)
datos.alumnos.integrados$apartado.libros <- unlist(apartado.libros.filtrado)
head(datos.alumnos.integrados)



load("Becas.R")
#class(distribucion.becas) # Vector of 1000 0-1 values
#length(distribucion.becas)
#head(distribucion.becas)
datos.alumnos.integrados$becado <- as.factor(distribucion.becas)
head(datos.alumnos.integrados)



load("HistorialPagos.R")
# 2 A tiempo, 1 retraso, 0 no pago
#class(registro.pagos)
#length(registro.pagos) # List of 1000 matrices of 4, 9 (payments,semesters)
#dim(registro.pagos[[1]])
#head(registro.pagos)

# Filtrar a 2 columnas que corresponden a los semestres y hacer una suma simple,
# los mas problematicos tendran un puntaje menor que los cumplidos
registro.pagos.filtrados <- lapply(registro.pagos, function(m){
    sum(m[,1:2])
})
#head(registro.pagos.filtrados)
datos.alumnos.integrados$historial.pagos <- unlist(registro.pagos.filtrados)
head(datos.alumnos.integrados)


load("CambioCarrera.R")
#class(cambio.carrera) # Vector of 1000 0-1 values
#length(cambio.carrera)
#head(cambio.carrera)
datos.alumnos.integrados$cambio.carrera <- as.factor(cambio.carrera)
head(datos.alumnos.integrados)



# --- 2. Feature engineering - Escoger que features se usaran
summary(datos.alumnos.integrados)

# Salvamos los resultados completos antes de remover columnas
save(datos.alumnos.integrados, file="datos.alumnos.integrados.noscaling.R")




# --- 3. Dataframes para alumnos

# Salvamos los resultados
save(datos.alumnos.integrados, file="datos.alumnos.integrados.noscaling.R")

# --- 4. Train/test split 90%/10%



#Usemos esta semilla para mantener datos constantes
set.seed(1234)
ind <-
  sample(
    x = c(1, 2),
    size = nrow(datos.alumnos.integrados),
    replace = TRUE,
    prob = c(.9, .1)
  )

training.set <- datos.alumnos.integrados[ind == 1, ]
summary(training.set)
test.set <- datos.alumnos.integrados[ind == 2 , ]
summary(test.set)

# --- 5. Generacion de labels usando K-Means
test <- training.set
training.set <- subset(training.set, select = -c(apartado.libros, uso.biblioteca, uso.plataforma))

wss <- vector()
for (i in 1:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(training.set, 
                       centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Numero de clusters",
     ylab="Error Standard")

# Consideran el plot, el numero de centros indicado parece ser 4.
centers <- 4

set.seed(1234)
kmeans.training <- kmeans(x = training.set, centers = centers)

# asistencias maximas 768
# grupo 3 y 4 parecen tener mejores resultados
# determinamos que 1 -> riesgo alto
# 2 -> riesgo medio
# 3 -> riesgo minimo
# 4 -> riesgo nulo
head(clusters.4)
data.enriched <- training.set
data.enriched$cluster <- kmeans.training$cluster
data.enriched$classification <- kmeans.training$cluster 
head(data.enriched)
data.enriched[data.enriched$cluster==4,]$classification <- "riesgo.nulo"
data.enriched[data.enriched$cluster==3 ,]$classification <- "riesgo.minimo"
data.enriched[data.enriched$cluster==2,]$classification <- "riesgo.medio"
data.enriched[data.enriched$cluster==1,]$classification <- "riesgo.alto"

library(rpart)
library(party)
names(data.enriched)

plot(data.enriched[,c("asistencias.totales", "cluster") ], 
     col = data.enriched$cluster)
# 4 azul oscuro
# 3 verde
# 2 rojo
# 1 negro
plot(data.enriched[,c("asistencias.totales", "resultados.trabajos") ], 
     col = data.enriched$cluster)

plot(data.enriched[,c("asistencias.totales", "resultados.examenes") ], 
     col = data.enriched$cluster)

plot(data.enriched[,c("admision.numeros", "resultados.examenes") ], 
     col = data.enriched$cluster)

data.enriched$desertor <- ifelse(data.enriched$cluster == 1, 1, 0)




# --- 2. Feature scaling en preparacion para Red neuronal
summary(datos.alumnos.integrados)


escalar.data.frame <- function(df) {
    # Esta funcion revisara si el dataframe contiene las columnas a escalar y
    #  si las tiene, procedera a escalarlas. Esto nos permite reutilizar la
    #  la escalacion para diferentes dataframes
    
    escalar.a.valor.max <- function(columna) {
        # Regresa el valor de las columnas escalados al valor maximo encontrado
        columna / max(columna)
    }
    
    columnas.en.df <- colnames(df)
    
    # La calificacion maxima en los examenes de admision es de 80, usemos eso para
    #  escalar las columnas
    if("admision.letras" %in% columnas.en.df) {
        df$admision.letras <- df$admision.letras / 80
    }
    if("admision.numeros" %in% columnas.en.df) {
        df$admision.numeros <- df$admision.numeros / 80
    }
    
    # Promedio prepa dividido entre 100
    if("promedio.preparatoria" %in% columnas.en.df) {
        df$promedio.preparatoria <- df$promedio.preparatoria / 100
    }
    
    # Maximo valor de asistencias para los 2 semestres es 2 (asistio a todas) * 12 * 32 = 768
    if("asistencias.totales" %in% columnas.en.df) {
        df$asistencias.totales <- df$asistencias.totales / (2 * 12 * 32)
    }
    
    # Maximo valor para historial.pagos 2 * 4 * 2 (pago a tiempo, 4 pagos por semestre, 2 semestres)
    if("historial.pagos" %in% columnas.en.df) {
        df$historial.pagos <- df$historial.pagos / 16
    }
    
    # Este campo es un factor, puede que no querramos escalarlo
    # Evaluacion socioeconomica, los menos privilegiados tienen 4
    #if("evaluacion.socioeconomica" %in% columnas.en.df) {
    #    df$evaluacion.socioeconomica <- df / 4
    #}
    
    # Para las siguientes, tomamos el maximo valor encontrado para poder escalarlo
    if("edad.ingreso" %in% columnas.en.df) {
        df$edad.ingreso <- escalar.a.valor.max(df$edad.ingreso)
    }
    if("nota.conducta" %in% columnas.en.df) {
        df$nota.conducta <- escalar.a.valor.max(df$nota.conducta)
    }
    if("resultados.examenes" %in% columnas.en.df) {
        df$resultados.examenes <- escalar.a.valor.max(df$resultados.examenes)
    }
    if("resultados.trabajos" %in% columnas.en.df) {
        df$resultados.trabajos <- escalar.a.valor.max(df$resultados.trabajos)
    }
    if("uso.biblioteca" %in% columnas.en.df) {
        df$uso.biblioteca <- escalar.a.valor.max(df$uso.biblioteca)
    }
    if("uso.plataforma" %in% columnas.en.df) {
        df$uso.plataforma <- escalar.a.valor.max(df$uso.plataforma)
    }
    if("apartado.libros" %in% columnas.en.df) {
        df$apartado.libros <- escalar.a.valor.max(df$apartado.libros)
    }
    return(df)
}

# --- 7. Entrenamiento de red neuronal



# --- 8. Predicciones para el set de test



# 9. Algoritmo genetico para accion remedial

install.packages("genalg")
library(genalg)
alumnos.riesgo <- data.frame
alumnos.riesgo <- data.enriched[data.enriched$desertor == 1,]
colnames(alumnos.riesgo)
presupuesto <- 10000
acciones.correctivas <- data.frame(
  accion = c("apoyo.economico","beca.media", "beca.alta","asesoria", "mentoria", "intervencion", "clases.remotas", "vale.transporte" ),
  costo = c(20, 50, 100, 20, 20, 25, 5, 10),
  nivel.ayuda = c(5, 10, 30, 5, 5, 10, 3, 4)
)
colnames(datos.alumnos.integrados)
summary(datos.alumnos.integrados)
nrow(alumnos.riesgo)
alumnos.riesgo[2,"classification"]
fitness.generic <- function (x) {
  #costo <- x %*% acciones.correctivas$costo * NROW(alumnos.riesgo)
  costo <- 0
  ayuda <- 0
 
  
  for ( i in 1: NROW(alumnos.riesgo)) {
    #el accion index nos permite acceder a cada genoma por cada alumno, asi podemos evaluar las acciones
    #segun el alumno
    accion.index <- ((i-1) * nrow(acciones.correctivas))
    for (j in 1: NROW(acciones.correctivas)) {
      if (x[j + accion.index] == 1) {
        costo <- acciones.correctivas$costo[j] + costo
        ayuda <- acciones.correctivas$nivel.ayuda[j] + ayuda
      }
    }
    #Evaluacion economica
    if (alumnos.riesgo[i,"evaluacion.socioeconomica"] == 4 ) {
      if (alumnos.riesgo[i,"becado"] == 1 && (x[accion.index + 3] == 1 ))  {
        # El alumno ya tenia beca, darle otra probablemente no es la solucion
        ayuda <- ayuda-100
      } else {
        if (alumnos.riesgo[i,"historial.pagos"] <= 14) {
          if (x[accion.index +3] ==1) {
            ayuda <- ayuda+50
          }
        } else {
          if (x[accion.index + 1] == 1 ) {
            ayuda <- ayuda+50
          }
        }
      }
    }
    
    # Asesorias y mentorias
    if (alumnos.riesgo[i,"resultados.examenes"] <= 13 && x[accion.index + 5] == 1) {
      ayuda <- ayuda +50
    }
    if (alumnos.riesgo[i,"resultados.trabajos"] <= 13 && x[accion.index + 4] ==1) {
      ayuda <- ayuda +50
    }
    
    # Faltas pero buenos resultados en examenes o trabajos
    if (alumnos.riesgo[i, "asistencias.totales"] < 650 && (alumnos.riesgo[i,"resultados.examenes"]>14 
                                                        || alumnos.riesgo[i,"resultados.trabajos"] > 14)) {
      #si el alumno es aplicado, pero no asiste seguido, probablemente tiene razones por las cuales no asistir
      # y clases remotas le permitirian continuar con sus estudios.
      if (x[accion.index + 7] == 1) {
        ayuda <- ayuda + 90
      }
      # Muchas veces el transporte tambien influye.
      if (x[accion.index + 8] == 1) {
        ayuda <- ayuda + 30
      }
    }
    
  }
  # Si se excede el presupuesto, pues no es una buena opcion
  if (costo > presupuesto) {
    return (1000)
  } 
  return (-ayuda)
  
}

?rbga.bin
ga.tree <- rbga.bin(size = (nrow(acciones.correctivas) * nrow(alumnos.riesgo)), popSize = nrow(alumnos.riesgo), iters=10,
                    mutationChance = 0.1, elitism = 4,  
                    evalFunc = fitness.generic,
                    verbose = T)

#Obtenemos el mejor cromosoma. 
best <- ga.tree$population[ga.tree$evaluations == min(ga.tree$best),]

sum(best)

#Ponemos las acciones preventivas en una columna nueva
alumnos.riesgo["accion"] <- vector()
for (i in 1: NROW(alumnos.riesgo)) {
  accion.index <- ((i-1) * nrow(acciones.correctivas))
  accion <- " "
  for (j in 1: NROW(acciones.correctivas)) {
    if (best[j + accion.index] == 1) {
      accion <- paste(acciones.correctivas$accion[j], accion, sep = " + ")
    }
  }
  alumnos.riesgo[i, "accion"] <- accion
}
alumnos.riesgo$accion
acciones = "beca.alta + vale.transporte"

