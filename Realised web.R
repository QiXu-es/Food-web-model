library(igraph)

predation_matrix=read.table("feasible.csv",header = TRUE)
predators = colnames(predation_matrix)
preys = rownames(predation_matrix)
M=nrow(predation_matrix)

#Set the mixed exponential–power law
e=exp(0.6*log(M))
P = function(r){
  return(exp(-r/e))
}

#Sampling from the pool of potential interactions
web = data.frame(Predator = character(), Prey = character(), stringsAsFactors = FALSE)
check=0
while (check==0) {
  for (predator in predators) {
    available_preys = preys[predation_matrix[, predator] == 1]
    max_in_degree = length(available_preys)
    in_degree = sample(1:max_in_degree, 1, prob = sapply(1:max_in_degree, P))
    selected_preys = sample(available_preys, in_degree)
    for (prey in selected_preys) {
      web = rbind(web, data.frame(Predator = predator, Prey = prey, stringsAsFactors = FALSE))
    }
  }
  #Check if there are species that only feed on themselves
  graph=graph_from_data_frame(web)
  graph=simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
  isolated_nodes = which(degree(graph) == 0, arr.ind = TRUE)
  if (length(isolated_nodes) == 0) {
    check=1
  }
  else {
    check=0
  }
}
print(web)

#Analysis the realised web
library(foodweb)
library(omnivor)

shortTL=function(graph){
  y=0
  x=degree(graph,,mode="out",loops=FALSE)
  m=distances(graph,,,mode="in")
  i=1
  while(i<nb_node(graph)+1){
    m[i,i]=Inf
    i=i+1
  }
  i=1
  while(i<nb_node(graph)+1) {
    y[i]=nb_node(graph)
    j=1
    if(x[i]==0){
      y[i]=1
      i=i+1
    }
    else{
      while(j<nb_node(graph)+1){
        if(x[j]==0){
          if(m[j,i]!=Inf&m[j,i]<y[i])
            y[i]=m[j,i]
        }
        j=j+1
      }
      y[i]=y[i]+1
      i=i+1
    }
  }
  return(y)
}

avgTL=function(graph) {
  return(sum((prey_avg_tl(graph)[,2]+shortTL(graph))/2)/nb_node(graph))
}

graph=graph_from_data_frame(web)
wtc=cluster_walktrap(graph)
nb_node(graph)
nb_edge(graph)
modularity(graph, membership(wtc))
graph=simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
avgTL(graph)+1