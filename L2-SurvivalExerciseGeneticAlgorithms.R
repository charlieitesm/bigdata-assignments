# Import genalg
library(genalg)

# supervivencia

weight.limit <- 20
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
fitness.generic <- function(x) {
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

fitness.generic(c(1,rep(0,37)))

ga.three <- rbga.bin(size = 38,
                     popSize = 200,
                     mutationChance = .01,
                     elitism = 4,
                     iters = 200,
                     evalFunc = fitness.generic,
                     verbose = T)

best <- ga.three$population[ga.three$evaluations == min(ga.three$best),][1,]
best.items <- items$item[best == 1]
best.items
length(best.items)

items.weight <- best %*% items$weight
items.weight

survivalpoints <- best %*% items$survivalpoints
survivalpoints



# Fitness function improved
fitness.generic.2 <- function(x) {
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
    
    # Transform to negative to make it less
    return(-partial.result)
  }
}

ga.three <- rbga.bin(size = 38,
                     popSize = 200,
                     mutationChance = .01,
                     elitism = 4,
                     iters = 200,
                     evalFunc = fitness.generic,
                     verbose = T)

best <- ga.three$population[ga.three$evaluations == min(ga.three$best),][1,]
best.items <- items$item[best == 1]
best.items
length(best.items)

items.weight <- best %*% items$weight
items.weight

survivalpoints <- best %*% items$survivalpoints
survivalpoints
