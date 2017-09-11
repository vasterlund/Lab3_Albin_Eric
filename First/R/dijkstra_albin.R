#'@title Dijkstra algorithm
#'
#'@description The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph. 
#'@param graph shall be a data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#'@param init_node shall be a numeric scalar that exist in the graph  
#'@export
#'@return Return the shortest path to everyother node from the starting node as a vector. 
#'@examples
#'wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'
#'dijkstra(wiki_graph,1)
#'dijkstra(wiki_graph,3)
#'@references You can check how the algorithm work at it's page at wikepedia.
#'\url{}
dijkstra<-function(graph,init_node){
  
  if(class(graph)!="data.frame"){
    stop()
  }
  if(FALSE%in%(colnames(graph)%in%c("v1","v2","w"))){
    stop()
  }
  if(FALSE%in%(c("v1","v2","w")%in%colnames(graph))){
    stop()
  }
  if(class(init_node)!="numeric"){
    stop()
  }
  
  if(class(init_node)!="numeric"){
    stop()
  }
  
  
  varden<-sort(unique(graph[,1]))
  langd<-varden[length(varden)]
  
  if((init_node%in%varden)==FALSE){
    stop()
  }
  
  datamatrial<-matrix(nrow=langd,ncol=langd,data=Inf)
  colnames(datamatrial)<-varden
  rownames(datamatrial)<-rep("?",langd)
  rownames(datamatrial)[1]<-init_node
  datamatrial[1,init_node]<-0
  
  fardiga<-init_node
  for(i in (1:nrow(graph))[(graph[,1]==init_node)]){
    datamatrial[1,graph[i,2]]<-graph[i,3]
  }
  
  
  for(k in 2:langd){
    
    
    rownames(datamatrial)[k]<-names(which.min(datamatrial[(k-1),][( names(datamatrial[(k-1),])%in%fardiga)==FALSE]))
    #rownames(datamatrial)[k]<-varden[datamatrial[(k-1),]==min(datamatrial[(k-1),][( names(datamatrial[(k-1),])%in%fardiga)==FALSE])][1]
    fardiga[k]<-as.numeric(rownames(datamatrial)[k])
    
    
    
    datamatrial_for<-graph[(graph[,1]==rownames(datamatrial)[k]),]
    datamatrial_for<-datamatrial_for[(datamatrial_for[,2]%in%fardiga)==FALSE,]
     
    for(i in 1:length(fardiga)){
      datamatrial[k,colnames(datamatrial)==fardiga[i]]<-datamatrial[(k-1),colnames(datamatrial)==fardiga[i]]
    }
    
    plus<-datamatrial[(k-1),fardiga[length(fardiga)]]
    j<-0
    for(i in as.numeric(rownames(datamatrial_for))){
      j<-j+1
      senaste_varde<-datamatrial[(k-1),colnames(datamatrial)==datamatrial_for[j,2]]
      nytt_varde<-graph[i,3]+plus
      
      if(senaste_varde>nytt_varde){
        datamatrial[k,colnames(datamatrial)==datamatrial_for[j,2]]<-nytt_varde
      }
      else if(senaste_varde<nytt_varde){
        datamatrial[k,colnames(datamatrial)==datamatrial_for[j,2]]<-senaste_varde
      }
    }
    datamatrial[k,datamatrial[k,]==Inf]<-datamatrial[(k-1),datamatrial[k,]==Inf]
  }
  svar<-as.numeric(datamatrial[langd,])
  return(svar)
}