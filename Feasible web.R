con= 8#Number of predators
res= 25#Number of all animals
prey=matrix(0, res,con)#Initializing the predation matrix
web=as.matrix(read.table("info.csv",header = TRUE))#Input ecological information

#Check the predation rules
for (i in 1:con) {
  for (j in 1:res) {
    #For scavenger–carnivore
    if (web[i,1]==4) {
      if (web[j,1]==1) {#Check prey's diet
        if (web[j,2]<web[i,2]+1+web[i,4]) {#Check body size
          prey[j,i]=1
          if (web[i,3]+web[j,3]==7){#Check habitat
            prey[j,i]=0
          }
        }
      }
      if (web[j,1]>1) {#Check prey's diet
        if (web[j,2]<web[i,2]+web[i,4]) {#Check body size
          prey[j,i]=1
          if (web[i,3]+web[j,3]==7){#Check habitat
            prey[j,i]=0
          }
        }
      }
    }
    #For carnivore
    if (web[i,1]==3) {
      if (web[j,1]==1) {#Check prey's diet
        if (web[j,2]<web[i,2]+2+web[i,4]) {#Check body size
          prey[j,i]=1
          if (web[i,3]+web[j,3]==7){#Check habitat
            prey[j,i]=0
          }
        }
      }
      if (web[j,1]>1) {#Check prey's diet
        if (web[j,2]<web[i,2]+1+web[i,4]) {#Check body size
          prey[j,i]=1
          if (web[i,3]+web[j,3]==7){#Check habitat
              prey[j,i]=0
            }
          }
        }
    }
    j=j+1
  }
  i=i+1
}
#Output the predation matrix
write.csv(prey, file = "prey.csv", row.names=FALSE)

#Analysis the feasible web
library(foodweb)
library(igraph)
library(omnivor)

#Read the web
predation_matrix=read.table("feasible.csv", row.names = 1, stringsAsFactors = FALSE)
result <- data.frame(consumer = character(), resource = character())
for (i in 1:nrow(predation_matrix)) {
  prey <- rownames(predation_matrix)[i]  # Get the prey's name
  for (j in 1:ncol(predation_matrix)) {  # Check predators
    if (predation_matrix[i, j] == 1) {
      predator <- colnames(predation_matrix)[j]  # Get the predator's name
      result <- rbind(result, data.frame(consumer = predator, resource = prey))
    }
  }
}

#Calculate the mean trophic level
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

#Network analysis
graph=graph_from_data_frame(result)
wtc=cluster_walktrap(graph)
nb_node(graph)
nb_edge(graph)
modularity(graph, membership(wtc))
graph=simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
avgTL(graph)+1