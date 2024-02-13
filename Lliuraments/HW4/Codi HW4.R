# INTRODUCTION TO FINANCIAL ENGINEERING (HW4)

# Miguel Angel Aguilo Gonzalez, 1699413 
# Judit de Paz Ramírez, 1570590
# Laia Mòdol Rodríguez, 1565282 
# Elena Rubio Zabala, 1699049 
# Guillem Tutusaus Alcaraz, 1533701 


rm(list=ls())


## Ejercicio 1

# Vamos a programar un arbol algoritmico binomial para calcular opciones de precios

### Apartado (a)

# Creamos una función que nos devuelva un árbol binomial
build_stock_tree = function(S, u, v, N) { 
  tree=matrix(0,nrow=N+1,ncol=N+1) # Inicializamos la matriz del árbol con ceros
  for(i in 1:(N+1)){ # Llenamos la matriz del árbol con los precios de la acción en cada nodo
    for(j in 1:i){
      tree[i,j] = S*u^(j-1)*v^(i-j)
    }
  }
  return(tree)
}

# Comprobamos que nuestro árbol sea correcto usando el ejemplo dado en el enunciado
build_stock_tree(S=10, u=1.1, v=0.9, N=2)


### Apartado (b)

# Creamos una función que nos devuelva la probabilidad neutral al riesgo
# Sabemos que S(1+r*dt)=q*u*S+(1-q)*v*S y por lo tanto q=(1+r*dt-v)/(u-v) así pues nos queda la siguiente función
q_prob=function(r, u, v, dt){
  q=(1+r*dt-v)/(u-v)
  return(q)
}

# De nuevo comprobamos que nuestra función nos devuelva el mismo resultado que el ejemplo de los enunciados
q_prob(0.1, 1.1, 0.9, 1/256)


### Apartado (c)

# Escribimos una función que calcule el valor de una opción de forma recursiva, para eso definiremos primero
# las funciones call y put

# Definimos una función call con s subyacente y strike K
call=function(K,s){
  (s-K)*(s-K>0)
}

# Definimos una función pull con las mismas variables
pull=function(K,s){
  (K-s)*(K-s>0)
}

value_binomial_option=function(tree, u, v, r, dt, K, type){
  q=q_prob(r, u, v, dt)
  option_tree=matrix(0, nrow=nrow(tree), ncol=ncol(tree)) # creamos un árbol vacío para rellenar con la opción 
  # de forma recursiva
  
  # diferenciamos si tendremos un call o un put
  if(type=="call"){
    for(i in 1:nrow(tree)){
      option_tree[nrow(tree),i]=call(K, tree[nrow(tree), i])
    }
  }
  
  if(type=="put"){
    for(i in 1:nrow(tree)){
      option_tree[nrow(tree),i] = put(K,tree[nrow(tree),i])
    }
  }
  
  for(i in (nrow(tree)-1):1){
    for(j in i:1){
      option_tree[i,j] = (option_tree[i+1,j+1]*q+option_tree[i+1,j]*(1-q))/(1+r*dt)
    }
  }
  
  return(option_tree)
}

# Comprobamos que la función que acabamos de escribir nos proporcione unos resultados iguales al ejemplo
tree=build_stock_tree(10, 1.01, 0.99, 5)
value_binomial_option(tree, 1.01, 0.99, 0.1, 1/256, 10, type="call")


### Apartado (d) 

# Juntamos ahora todas las funciones para construir una función que nos proporcione el precio de la opción
binomial_option=function(type,u,v,dt,r,K,S,N){
  tree=build_stock_tree(S,u,v,N)
  option_tree=value_binomial_option(tree,u,v,r,dt,K,type)
  price=option_tree[1,1]
  return(price)
}

# De nuevo, comprobamos que nos de el mismo resultado que el ejemplo
binomial_option(type="call",u=1.01, v=0.99, dt=1/256, r=0.1, K=10, S=10, N=5)



## Ejercicio 2

# Usaremos ahora la función que acabamos de crear para calcular el precio de una opción CALL con 
# S=100
# dt=1/12 ya que son 12 meses
# r=10%
# y la acción puede subir o bajar 1% cada mes

binomial_option(type="call",u=1.01,v=0.99,dt=1/12,r=0.1,K=100,S=100,N=12)
