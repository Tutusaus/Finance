
#Exercici 1
#a)

build_stock_tree = function(S, u, v, N){
  tree = matrix(0,N+1,N+1)
  tree[1,1]=S
  for(i in 2:(N+1)){
   for(j in 1:i){
     tree[i,j] = S*v^(i-j)*u^(j-1)
    }
  }
  return(tree)
}
build_stock_tree(10,1.1,0.9,2)

#b)
q_prob = function(r, u, v, dt){
  return((1+r*dt-v)/(u-v))
}
q_prob(0.1,1.1,0.9,1/256)

#c)
value_binomial_option = function(tree, u, v, r, dt, K, type){
  q = q_prob(r, u, v, dt)
  n = nrow(tree)
  option_tree = matrix(0, n, n)
  
  for(j in n:1){
    if(type == "call"){
      option_tree[n,j] = pmax(tree[n,j]-K,0)
    }else{
      option_tree[n,j] = pmax(K-tree[n,j],0)
    }
  }
  
  for(i in (n-1):1){
    for(j in i:1){
      option_tree[i,j] = (option_tree[(i+1),(j+1)]*q+option_tree[(i+1),(j)]*(1-q))/(1+r*dt)
    }
  }
  return(option_tree)
}

S=10
N=5
u = 1.01 
v = 0.99 
r = 0.1 
dt = 1/256
K=10
tree = build_stock_tree(S, u, v, N)
value_binomial_option(tree, u, v, r, dt, K, "call")

#d)
binomial_option = function(type, u, v, dt, r, K, S, N){
  tree = build_stock_tree(S, u, v, N)
  V = value_binomial_option(tree, u, v, r, dt, K, type)
  price = V[1,1]
  return(price)
}
binomial_option("call", u, v, dt, r, K, S, N)


#Exercici 2
S=100
K=S
dt=1/12
r=0.1
u=1.1
v=0.9
N=12
binomial_option("call", u, v, dt, r, K, S, N)
