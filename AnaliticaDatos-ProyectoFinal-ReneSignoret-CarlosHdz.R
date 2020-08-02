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
dataset.folder = "C:/Users/Charlie/OneDrive\ -\ Instituto\ Tecnologico\ y\ de\ Estudios\ Superiores\ de\ Monterrey/BigData/FinalProject/ProfessorFiles"

setwd(dataset.folder)


# --- 1. Cargar Datasets y codificacion - truncar la informacion para usar solo 2 semestres


# Este sera el repositorio del perfil de todas las caracteristicas de los alumnos
load("perfilAlumnos.R")
#class(perfil.alumnos) # Dataframe
#dim(perfil.alumnos) # 1000 rows, 7 columns
#head(perfil.alumnos)

datos.alumnos.integrados <- perfil.alumnos



load("AsistenciasTotales.R")
#class(asistencias.totales)  # List
#length(asistencias.totales) # 1000 matrices of 32,54
#head(asistencias.totales)
#asistencias.totales[[1]]
#dim(asistencias.totales[[1]])

# Obtener todos los rows de las matrices de los 1000 alumnos, pero solo 12 materias (2 anios)
# Codificamos las asistencias en un solo numero por alumno usando la suma de sus codigos de asistencia
#  Los que atendieron menos clases o tuvieron retardo, tendran menores puntajes
#  2 A tiempo, 1 Retraso, 0 falto a clases
asistencias.totales.filtered <- lapply(asistencias.totales, function(m){
        sum(m[,1:12])
    })

# Add the column to the dataframe
datos.alumnos.integrados$asistencias.totales <- asistencias.totales.filtered
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
datos.alumnos.integrados$resultados.examenes <- resultados.examenes.filtrados
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
datos.alumnos.integrados$resultados.trabajos <- resultados.trabajos.filtrados
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
datos.alumnos.integrados$uso.biblioteca <- uso.biblioteca.filtrados
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
datos.alumnos.integrados$uso.plataforma <- uso.plataforma.filtrado
head(datos.alumnos.integrados)



load("ApartadoDeLibros.R")
#class(separacion.libros.totales) # List of 1000 matrices of 1,54
#dim(separacion.libros.totales[[1]])
#head(separacion.libros.totales)

# Filtrar a 12 materias y codificar con la suma del numero de prestamos
apartado.libros.filtrado <- lapply(separacion.libros.totales, function(m){
    sum(m[,1:12])
})
#head(apartado.libros.filtrado)
datos.alumnos.integrados$apartado.libros <- apartado.libros.filtrado
head(datos.alumnos.integrados)



load("Becas.R")
#class(distribucion.becas) # Vector of 1000 0-1 values
#length(distribucion.becas)
#head(distribucion.becas)
datos.alumnos.integrados$becado <- distribucion.becas
head(datos.alumnos.integrados)



load("HistorialPagos.R")
# 2 A tiempo, 1 retraso, 0 no pago
#class(registro.pagos)
#length(registro.pagos) # List of 1000 matrices of 4, 9 (payments,semesters)
#dim(registro.pagos[[1]])
#head(registro.pagos)

# Filtrar a 2 columnas que corresponden a los semestres y hacer una suma simple, los mas
#  problematicos tendran un puntaje menor que los cumplidos
registro.pagos.filtrados <- lapply(registro.pagos, function(m){
    sum(m[,1:2])
})
#head(registro.pagos.filtrados)
datos.alumnos.integrados$historial.pagos <- registro.pagos.filtrados
head(datos.alumnos.integrados)


load("CambioCarrera.R")
#class(cambio.carrera) # Vector of 1000 0-1 values
#length(cambio.carrera)
#head(cambio.carrera)
datos.alumnos.integrados$cambio.carrera <- cambio.carrera
head(datos.alumnos.integrados)


# --- 2. Feature engineering - Escoger que features se usaran



# --- 3. Feature scaling



# --- 4. Dataframes para alumnos


# Salvamos los resultados
save(datos.alumnos.integrados, file="datos.alumnos.integrados.R")

# --- 5. Train/test split 90%/10%



# --- 6. Generacion de labels usando K-Means



# --- 7. Entrenamiento de red neuronal



# --- 8. Predicciones para el set de test



# 9. Algoritmo genetico para accion remedial


