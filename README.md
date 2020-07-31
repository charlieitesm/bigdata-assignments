# ProyectoFinal - Desercion Escolar


## Instrucciones

El objetivo del proyecto es identificar alumnos en riesgo de deserción. 

### Metodología
 
1. Integración de datos Los datos están limpios. El mismo identificador de alumno es utilizado en los diferentes archivos, es necesario integrar adecuadamente los datos en un solo dataframe. Separar 100 alumnos para correr la red, estos alumnos no entran al kmeans ni entrenan la red.  
2. Clustering (usan 900 alumnos) Es necesario identificar a los alumnos que están en riesgo a través de encontrar el cluster problemático. plot el codo. Digamos que encuentran 5 clusters, y hay un cluster con los más bajos indicadores (cluster.centers) con 200 alumnos. Crean una nueva columna deserción y ponen el valor de 1 para estos 200 y para los otros 700 con 0. 
3. Red neuronal Se debe crear un dataset de entrenamiento creando una nueva columna en el dataset llamada "deserción" que sea 1 si el alumno pertenece al cluster problemático. Este será el set de entrenamiento para la red neuronal. Se debe particionar la información. Entrenan con 700 y prueban con 200, diferentes topologías. 
4. Corran la red para los 100 alumnos que separaron. Identifiquen a los desertores. Digamos que fueron 20.  
5. Algoritmo genético Se deben establecer medidas de remediación para la deserción tales como becas, vales de transporte, asesorías, mentorías y establecer un costo y un beneficio. El resultado el algoritmo genético son las series de medidas a aplicar para mitigar la deserción. 

### Ideas de listas:
Mecanismos:
#### Presupuesto: 10k dolares
 1. dar beca estudiantil, 500 dolares. 
 2. vales de transporte, 100 dolares.
 3. mentoria, 200 dolares. 
 4. consultoria psicologica, 400 dolares. 
 5. boleto a evento integracion, 50 dolares.
 6. Asesoria individual, 250 dolares
 7. Cursos remediales, 2500 dlls
 8. Visita a empresa, 50 dlls
 9. Examen extempor??neo, 100 dlls
 10. Platica motivacional, 50 dlls 
 11. Viaje recreativo, 10 dlls

Para este caso, tenemos 20 alumnos y 11 medidas. Esto representa un cromosoma binario de tamaño 20 x 11 = 220

10000000000 0...
00000000000 100...
11111111111 0...

---

## Plan de accion

1. Decidir qué features usaremos y cómo los codificaremos
   * Si bien no necesitamos hacer feature scaling para K-Means, sí lo necesitaremos para las redes neuronales, una idea es hacer lo que hacíamos con Falcon: determinar cuál es el valor mayor para el feature y dividir todos los valores entre este máximo, así nos quedara todo entre 0-1
2. En el script, leer todos los datasets
3. Truncar la información en los datasets pertinentes para traerse sólo la información para 2 semestres, i.e. 12 materias
4. Aplicar las estrategias de codificación a todos los features que lo necesiten.
   * El objetivo es tener un solo valor para cada uno de los features, en este punto ya debimos haber combinado de alguna manera los datos de todos los semestres en un sólo valor numérico.
5. Aplicar las estrategias de escalamiento a todos los features que lo necesiten
6. Crear un dataframe de todos para cada uno de los 1000 alumnos usando los features ya escalados y combinados, en este punto debemos tener un registro para cada alumno.
7. Hacer el split de train/test, 90%/10%.
   * Tenemos que ver una manera de asegurarse que el set de entrenamiento no este sesgado hacia hombres, debe haber un numero balanceado de los dos generos.
8. Generar las etiquetas con K-Means
    * Usar grafica del codo para ver los clusters y ver cuales son los clusters que comparten features que son los menos deseables, estos son los que seran los que desertaron 
    * El dataset de prueba no debe ser pasado por K-Means
    * 1=Deserto, 0=No deserto
9. Entrenar una red neuronal con los datos etiquetados para 900 alumnos
10. Con el entrenamiento de la red, predecir las etiquetas para los 100 alumnos restantes e identificar a los desertores
11. Disenar estrategias para que sean probadas por el algoritmo genetico

---

## Features

Definir la lista de features, cómo los codificaremos y de qué manera los escalaremos:
1. TBD