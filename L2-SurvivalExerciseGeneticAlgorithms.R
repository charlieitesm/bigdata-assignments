######################################
#
# ITESM
# MCC
# Big Data
#
#
# Rene Signoret - A00354797
# Carlos Hernandez - A01181616
#
######################################



# Import genalg
library(genalg)

# Definition of items

items <- data.frame(
  item=c("encendedor","casa_camp","navaja","linterna",
         "manta","sleep_bag","brujula", "agua.5.litro",
         "atun.kilo", "cuchillo", "cerillos", "bat.extra"
         ,"pedernal", "jabon", "cepillo", "papel.higie",
         "barritas.avena","kit.pesca", "cazuela", "botiquin",
         "carne.seca","cerveza", "mezcal", "hielos",
         "bloqueador_solar","camisa_larga","sombrero","gps",
         "paneles_solares", "mapa",
         "lapicero",
         "lentes_sol",
         "libreta",
         "hacha",
         "pala",
         "mapa_constelaciones",
         "baston",
         "cubeta"),

  survivalpoints = c(90,95,85,70,
                     50,60,80,100,
                     80,85,70,50,
                     80,40,40,10,
                     40,5,55,99,
                     99,5,5,2,
                     5,80,50,90,
                     50,99,
                     1,
                     60,
                     15,
                     20,
                     60,
                     80,
                     50,
                     80),

  weight = c(.01, 5,.1,.5,
             2,1,.01, 5,
             1,.1,.01,.5,
             1,.1,.01,.1,
             .5,2,.5,.5,
             1,1,1,1,
             .5,.2,.3,1,
             2,.3,
             0.1,
             0.1,
             0.1,
             3,
             5,
             0.2,
             2,
             0.1)
)


# Fitness function, less is better
fitness.generic <- function(x, weight.limit = 20) {

  # Dot product

  items.weight <- x %*% items$weight
  items.s.p <- x %*% items$survivalpoints

  if (items.weight > weight.limit)
  {
    # Penalize if gone overweight
    return(1000)
  }
  else
  {
    return (-items.s.p)
  }
}

ga.before.improvements <- rbga.bin(size = 38,
                     popSize = 200,
                     mutationChance = .01,
                     elitism = 4,
                     iters = 200,
                     evalFunc = fitness.generic,
                     verbose = F)

best.before.improvements <- ga.before.improvements$population[ga.before.improvements$evaluations == min(ga.before.improvements$best),][1,]
best.items.before <- items$item[best.before.improvements == 1]
#best.items.before
#length(best.items)

items.weight.before <- best.before.improvements %*% items$weight
#items.weight

survivalpoints.before <- best.before.improvements %*% items$survivalpoints
#survivalpoints

print("*************** BEFORE IMPROVEMENTS ****************")
print(paste0("Best items before improvements for weight 20: ", length(best.items.before), collapse = " "))
print(best.items.before)
print(paste0("Weight of items: ", items.weight.before, collapse = " "))
print(paste0("Survival points: ", survivalpoints.before, collapse = " "))


# Fitness function improved
fitness.improved <- function(x, weight.limit = 20) {

  # Dot product
  items.weight <- x %*% items$weight
  items.s.p <- x %*% items$survivalpoints

  if (items.weight > weight.limit)
  {
    return(1000)
  }
  else
  {
    partial.result <- items.s.p

    if(x[4] == 1 && x[12] == 0) {
      partial.result <- partial.result - 70
    }

    if(x[4] == 0 && x[12] == 1) {
      partial.result <- partial.result - 200
    }
    if (x[31] == 0 && x[33] == 1 ) {
      partial.result <- partial.result -14
    }
    if (x[31] == 1 && x[33] == 1 ) {
      partial.result <- partial.result  + 30
    }
    if (x[9] ==1 && (x[10] == 1  || x[3] ==1)) {
      partial.result <- partial.result +20
    }

    # Transform to negative to make it less
    return(-partial.result)
  }
}

ga.after.improvements <- rbga.bin(size = 38,
                                 popSize = 200,
                                 mutationChance = .01,
                                 elitism = 4,
                                 iters = 200,
                                 evalFunc = fitness.improved,
                                 verbose = F)

best.after.improvements <- ga.after.improvements$population[ga.after.improvements$evaluations == min(ga.after.improvements$best),][1,]
best.items.after <- items$item[best == 1]

items.weight.after <- best.after.improvements %*% items$weight

survivalpoints.after <- best.after %*% items$survivalpoints

print("*************** After IMPROVEMENTS ****************")
print(paste0("Best items after improvements for weight 20: ", length(best.items.after), collapse=" "))
print(best.items.after)
print(paste0("Weight of items:", items.weight.after, collapse = " "))
print(paste0("Survival points:", survivalpoints.after, collapse = " "))
