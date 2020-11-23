##La primera funcion establece los parametros para el calculo dela matriz inversa y trata de almacenar los datos en la cache para ser usados luego
##La segunda brinda la inversa si los valores almacenados no osn iguales a NULL, de lo contrario la calcula

## Esta funcion lo que hace es establecer los parametros para la matriz x
## Tambien establece variables como "inversa" la inversa de la matriz como nulos 

makeCacheMatrix <- function(x = matrix()) {
inversa <- NULL #Configura la variable inversa como nula dejando espacio para el futuro valor de la matriz inversa/ set "inversa" as NULL making a placeholder for the future value of the inverse matrix
establecer <- function(y) {  #Crea una funcion/Creates a function  
      x<<-y                  #que da el valor de x a un vector "y"/that set the values of x to a vector "y"
      inversa <<- NULL       #y vuelve a establecer inversa como nula/ and resets again "inversa"
}
obtener <- function() x #retorna la matriz en x/ returns matrix in x
establecerinversa <- function(solve) inversa <<- solve #establece "solve"(la inversa de la variable "inversa"....(lo se mucho redundo)) a solve/ set the inverse "inversa" to solve
obtenerinversa <- function() inversa #retorna la inversa/returns "inversa"
list(establecer=establecer,obtener=obtener,establecerinversa=establecerinversa,obtenerinversa=obtenerinversa) #simplemente devuelve una lista de las funciones de makeCacheMatrix/ Just returns a list with the functions on makeCacheMatrix
#Y ahi queda...
}


## Esta funcion, brinda la inversa si esta almacenada en la cache, sino la calcula y almacena su valor en cache para ser usado cuando se necesite...
## This function gives the inverse if it's stored in cache, if not the function calculates it and stores the value in cache to be used when needed...

cacheSolve <- function(x, ...) {
      inversa <- x$obtenerinversa()  #Obtiene la inversa almacenada en la cache/get the "inversa" values stored in cache
      if(!is.null(inversa)){  #si el valor de "inversa" no es NULL entonces, tira un mensaje y retorna la inversa/if "inversa" isn`t NULL then show the message and print the inverse stored in cache
            message("Obteniendo datos almacenados en la cache, se paciente, solo tomara un momento o talvez dos/ Getting Cache stored data, be patient, it will take just a moment or maybe two...")
            return(inversa)
      }
      ##Si "inversa" es NULL.../If "inversa" is null...
      matriz <- x$obtener() #asignar a la variable "matriz" los valores en obtener/asigns "matriz" the value of "obtener"
      inversa <- solve(matriz,...) #"inversa" va a ser igual a la inversa de "matriz".../"inversa" is equal to the inverse of "matriz"
      x$establecerinversa(inversa) #Establece "inversa" a "establecerinversa" y asi almacenarla en cache/stores "inversa" to cache via "establecerinversa"
       
      
      inversa #Y retorna el valor de la inversa.../and returns the value of the inverse of the matrix...
      #Y ahi finaliza...
}
