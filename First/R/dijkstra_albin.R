library(first)

euclidean(100,10)


wiki_graph<-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
rad<-1

dijkstra<-function(wiki_graph,rad){
  
  varden<-sort(unique(wiki_graph[,1]))
  langd<-varden[length(varden)]
  
  datamatrial<-matrix(nrow=langd,ncol=langd,data=Inf)
  colnames(datamatrial)<-varden
  rownames(datamatrial)<-rep("?",langd)
  rownames(datamatrial)[1]<-rad
  datamatrial[1,rad]<-0
  
  fardiga<-rad
  for(i in (1:nrow(wiki_graph))[(wiki_graph[,1]==rad)]){
    datamatrial[1,wiki_graph[i,2]]<-wiki_graph[i,3]
  }
  
  
  
  k=3
  for(k in 2:langd){
    
    
    rownames(datamatrial)[k]<-varden[datamatrial[(k-1),]==min(datamatrial[(k-1),][( names(datamatrial[(k-1),])%in%fardiga)==FALSE])]
    
    
    
    
    
   
    
    
    fardiga[k]<-as.numeric(rownames(datamatrial)[k])

  
  
    datamatrial_for<-wiki_graph[(wiki_graph[,(k-1)]==rownames(datamatrial)[k]),]
    datamatrial_for<-datamatrial_for[(datamatrial_for[,2]%in%fardiga)==FALSE,]
    
    for(i in 1:length(fardiga)){
      datamatrial[k,colnames(datamatrial)==fardiga[i]]<-datamatrial[(k-1),colnames(datamatrial)==fardiga[i]]
    }
    
    
    plus<-datamatrial[(k-1),fardiga[length(fardiga)]]
    j<-0
    for(i in as.numeric(rownames(datamatrial_for))){
      j<-j+1
      senaste_varde<-datamatrial[colnames(datamatrial)==datamatrial_for[j,k],(k-1)]
      nytt_varde<-wiki_graph[i,3]+plus
      
      if(senaste_varde>nytt_varde){
        datamatrial[k,colnames(datamatrial)==datamatrial_for[j,2]]<-nytt_varde
      }
    }
    datamatrial[k,datamatrial[2,]==Inf]<-datamatrial[(k-1),datamatrial[2,]==Inf]
    
  }
    
    
  
 
  
}





