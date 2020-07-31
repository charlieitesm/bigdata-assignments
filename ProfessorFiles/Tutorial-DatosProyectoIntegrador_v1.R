# Datos proyecto integrador

getwd()
setwd("C:/Users/Charlie/OneDrive\ -\ Instituto\ Tecnologico\ y\ de\ Estudios\ Superiores\ de\ Monterrey/BigData/FinalProject/ProfessorFiles")

# Son 9 semestres de 6 materias cada uno.

# 1.- Asistencias Totales
load("AsistenciasTotales.R")
asistencias.totales
asistencias.totales[[150]]
asistencias.totales[1]
asistencias.totales[[1]]
class(asistencias.totales)
class(asistencias.totales[1])
class(asistencias.totales[[1]])
class(asistencias.totales)
length(asistencias.totales)
asistencias.totales[[1]][1,1]
asistencias.totales[[1]][32,54]
class(asistencias.totales[[1]])
dim(asistencias.totales[[1]])

class(asistencias.totales[1])

asistencias.totales[[1]][1:10,1:10]


# 2 asistencia, 1 retardo y 0 falta.
asistencias.totales[[1]][32,]

asistencias.totales.2.semestres <- asistencias.totales[,1:12]

# Generar promedios asistencia por alumno

total.asistencias <- 32 * 6 * 2

dim(asistencias.totales[[1]])

promedio.asistencia.mtx <-
  matrix(,nrow=0,ncol=9)
for (i in 1:1000) {
  datos.asistencia <- asistencias.totales[[i]]
  pagos.vct <- vector()
  total.asistencias <- 32 * 6 * 2
  pagos.vct[1] <- sum(datos.asistencia[,1:6]) / total.asistencias
  pagos.vct[2] <- sum(datos.asistencia[,7:12]) / total.asistencias
  pagos.vct[3] <- sum(datos.asistencia[,13:18]) / total.asistencias
  pagos.vct[4] <- sum(datos.asistencia[,19:24]) / total.asistencias
  pagos.vct[5] <- sum(datos.asistencia[,25:30]) / total.asistencias
  pagos.vct[6] <- sum(datos.asistencia[,31:36]) / total.asistencias
  pagos.vct[7] <- sum(datos.asistencia[,37:42]) / total.asistencias
  pagos.vct[8] <- sum(datos.asistencia[,43:48]) / total.asistencias
  pagos.vct[9] <- sum(datos.asistencia[,49:54]) / total.asistencias

  promedio.asistencia.mtx <- rbind(promedio.asistencia.mtx,
                                   pagos.vct)

}

promedio.asistencia.mtx

promedio.dos.semestres <- promedio.asistencia.mtx[,1:2]
dim(promedio.dos.semestres)
head(promedio.dos.semestres)

promedio.asistencias.final <- rowMeans(promedio.dos.semestres)
promedio.asistencias.final
length(promedio.asistencias.final)

names(promedio.asistencias.final) <- 1:1000
promedio.asistencias.final[500]


# 2 Se sube a spark tal como está.
load("perfilAlumnos.R")
head(perfil.alumnos,1)
class(perfil.alumnos)
str(perfil.alumnos)
perfil.alumnos$genero <- factor(perfil.alumnos$genero)
perfil.alumnos$evalucion.socioeconomica <-
  factor(perfil.alumnos$evalucion.socioeconomica)
summary(perfil.alumnos)
perfil.alumnos$edad.ingreso <-
  factor(perfil.alumnos$edad.ingreso)


# integrar información
datos.integrados <- cbind(perfil.alumnos, promedio.asistencias.final)
str(datos.integrados)
datos.integrados[1,]
# 3 1000 matrices de 2 x 54, calificación entre 1 y 20
load("ResultadosExamenes.R")
resultados.examenes.totales
length(resultados.examenes.totales)
resultados.examenes.totales[[1000]]

promedio.examenes.mtx <-
  matrix(,nrow=0,ncol=1)
for (i in 1:1000) {
  datos.examens <- resultados.examenes.totales[[i]]
  resultados.vct <- vector()
  resultados.vct[1] <- mean(datos.examens[,1:12])
  promedio.examenes.mtx <- rbind(promedio.examenes.mtx,
                                 resultados.vct)

}
class(promedio.examenes.mtx)
head(promedio.examenes.mtx)
nrow(promedio.examenes.mtx)
rownames(promedio.examenes.mtx) <- 1:1000

datos.integrados <- cbind(datos.integrados, promedio.examenes.mtx)
head(datos.integrados)
# 4 1000 matrices de 4 x 54, son 4 trabajos por clase, entre 1 y 20
load("ResultadoTrabajos.R")
resultados.trabajos.totales
resultados.trabajos.totales[[500]]



# 5 Redondear. Uso f�?sico y virtual. vector.
load("UsoBiblioteca.R")
uso.biblioteca.totales
length(uso.biblioteca.totales)
uso.biblioteca.totales[[1]]

# 6 Redondear, vector.
load("UsoPlataforma.R")
uso.plataforma.totales
uso.plataforma.totales[[1]]

# 7
load("ApartadoDeLibros.R")
separacion.libros.totales[[1]]

# 8 vector binario, 1 tiene beca
load("Becas.R")
distribucion.becas
distribucion.becas[[1]]
distribucion.becas
sum(distribucion.becas)

# 9  2 en tiempo, 1 retraso, 0
load("HistorialPagos.R")
registro.pagos
length(registro.pagos)
registro.pagos[[500]]

mean(c(2,2,2,2,2,2,2,1))

promedio.pagos.mtx <-
  matrix(,nrow=0,ncol=1)
for (i in 1:1000) {
  datos.pagos <- registro.pagos[[i]]
  pagos.vct <- vector()
  pagos.vct[1] <- mean(datos.pagos[,1:2])
  promedio.pagos.mtx <- rbind(promedio.pagos.mtx,
                              pagos.vct)
}
class(promedio.pagos.mtx)
promedio.pagos.mtx
rownames(promedio.pagos.mtx) <- 1:1000

promedio.pagos.mtx[promedio.pagos.mtx < 1.875] <- 0

# 10 evaluación general del profesor
load("EvaluacionProfesorMateria.R")
length(encuesta.profesor.materia)
encuesta.profesor.materia

# 11 vector, 1 indica que cambió
load("CambioCarrera.R")
cambio.carrera

save(datos.integrados, file="datos.integrados.R")
getwd()
load("datos.integrados.R")
datos.integrados
head(datos.integrados)

set.seed(1234)

ind <- sample(x=c(0,1),size=nrow(datos.integrados),
              replace=TRUE,prob = c(0.9,0.1))
ind

alumnos.nuevos <- datos.integrados[ind==1,]
alumnos.actuales <- datos.integrados[ind==0,]

set.seed(1234)

ind <- sample(x=c(0,1),size=nrow(alumnos.actuales),
              replace=TRUE,prob = c(0.7,0.3))
ind

alumnos.training <- alumnos.actuales[ind==0,]
alumnos.test <- alumnos.actuales[ind==1,]

str(alumnos.training)





