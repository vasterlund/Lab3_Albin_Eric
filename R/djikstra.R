wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))


init_node <- 1
Q <- matrix(0, ncol=length(unique(wiki_graph[,1]))+1, nrow=length(unique(wiki_graph[,1])))
colnames(Q) <- c("Nodes", paste(1:length(unique(wiki_graph[,1]))))
rownames(Q) <- c(paste(1:length(unique(wiki_graph[,1]))))
Q[,1] <- unique(wiki_graph[,1])


listan <- list()

for(m in 1:length(unique(wiki_graph[,1]))){
  init_node <- m
  visited <- c()
  range_node <- c()
  Q <- matrix(0, ncol=length(unique(wiki_graph[,1]))+1, nrow=length(unique(wiki_graph[,1])))
  colnames(Q) <- c("Nodes", paste(1:length(unique(wiki_graph[,1]))))
  rownames(Q) <- c(paste(1:length(unique(wiki_graph[,1]))))
  Q[,1] <- unique(wiki_graph[,1])
  
  for(i in 1:length(wiki_graph[wiki_graph[,1] == init_node  ,3])){ 
    
    #Slänger in de första längderna från init_node till de närmsata nodesen i data frame, Q
    visited[i] <- wiki_graph[wiki_graph[,1] == init_node  ,2][i]
    
    Q[Q[,1] == wiki_graph[i,2],2] <- wiki_graph[wiki_graph[,1] == init_node,3][i]
    
    
    #kollar längderna innom varje nod och plussar på den föregående noden
    range_node[ i] <- Q[Q[,1] == init_node,2] + Q[Q[,1] == visited[i],2]
    
    
    
  }
  visited_exra <- visited
  range_node <- sort(range_node)
  #nästa interation vill jag ha det näst minsta
  
  k=2
  for(k in 0:length(visited_exra)){
    if(k==0){
      mini <- min(range_node)
    } else {
      mini <- min(range_node[-c(1:k)])
    }
    
    # Kollar längderna till andra node ifrån den första
    range_plus <- c()
    checking <- wiki_graph[wiki_graph[,1] == Q[Q[,2] == mini,1], ]
    for(j in 1:length(checking[,1])){
      if(length(checking[,1]) == 0 ){
        break
      }

      range_plus[j] <- mini + checking[j,3]
      Q[checking[j,2], colnames(Q)==checking[1,1]] <- range_plus[j]
    }

  }
  
  
  
  listan[[m]] <- Q
  
}




for(i in 1:length(wiki_graph[wiki_graph[,1] == init_node  ,])){ #lägg till k iställer för 1
  
  #Slänger in de första längderna från init_node till de närmsata nodesen i data frame, Q
  visited[i] <- wiki_graph[wiki_graph[,1] == init_node  ,2][i]
  
  Q[Q[,1] == wiki_graph[i,2],2] <- wiki_graph[wiki_graph[,1] == init_node,3][i]
  

  #kollar längderna innom varje nod och plussar på den föregående noden
  range_node[ i] <- Q[Q[,1] == init_node,2] + Q[Q[,1] == visited[i],2]

  

}
visited_exra <- visited
range_node <- sort(range_node)
#nästa interation vill jag ha det näst minsta


for(k in 0:length(visited_exra)){
  if(k==0){
    mini <- min(range_node)
  } else {
    mini <- min(range_node[-c(1:k)])
  }
  
  # Kollar längderna till andra node ifrån den första
  range_plus <- c()
  checking <- wiki_graph[wiki_graph[,1] == Q[Q[,2] == mini,1], ]
  for(j in 1:length(checking[,1])){
    if(length(checking[,1]) == 0 ){
      break
      }
    # if(checking[j,2] %in% visited | checking[j,2]==init_node){
    #   next
    # }
    visited[length(visited)+1] <- checking[j,2]
    range_node[length(range_node)+1] <- mini + checking[j,3]
    range_plus[j] <- mini + checking[j,3]
    Q[checking[j,2], colnames(Q)==checking[1,1]] <- range_plus[j]
  }
  
}





init_node <- 2






m=5
for(m in 1:length(Q[,1])){
  if(sum(Q[m,2:length(Q[,1])])==0){
    range_plus <- c()
    checking <- wiki_graph[wiki_graph[,1]==m,]
    for(j in 1:length(checking[,1])){
      if(length(checking[,1]) == 0 ){
        break
      }

      range_plus[j] <- mini + checking[j,3]
      Q[checking[j,2], colnames(Q)==checking[1,1]] <- range_plus[j]
    }
  }
  
}


wiki_graph[]






  