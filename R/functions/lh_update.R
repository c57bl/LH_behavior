# convert med output files into table, and write tables to db
# med out put should be arranged into a folder with name(batch_testnumber) 
# including file(two mice,mice id in left, mice id in right) 
# or (1 mice, mice id)

lh_update<-function(complete=T,plot.by.test=T){
  # build in functions----
  extract_table<-function(given_filename,batch){
    curr.data<-readLines(paste0("data/",batch,"/",given_filename))
    
    box.numer<-str_count(curr.data,"Box") %>% sum() 
    
    if (str_detect(given_filename,pattern="-")){
      if (box.numer!=2) stop(paste0(given_filename,": incorrect boxnumber"))
      # check box rank and file name
      box.rank<-(str_detect(curr.data,pattern = "Box: 2") %>% which())-
        (str_detect(curr.data,pattern = "Box: 1") %>% which())
      box.name<-paste0(batch,"_",str_split(given_filename,pattern = "-") %>% .[[1]])
     
      # extract table
      start.index<-str_detect(curr.data,pattern = "D:") %>% which()
      end.index<-str_detect(curr.data,pattern = "S:") %>% which()
      box.table.1<-curr.data[(start.index[1]+1):(end.index[1]-1)]
      box.table.2<-curr.data[(start.index[2]+1):(end.index[2]-1)]
      # name table
      if (box.rank>0) {
        writeLines(box.table.1,paste0("output/tables/",batch,"/",box.name[1],'.txt'))
        writeLines(box.table.2,paste0("output/tables/",batch,"/",box.name[2],'.txt'))
        } else {
          writeLines(box.table.2,paste0("output/tables/",batch,"/",box.name[1],'.txt'))
          writeLines(box.table.1,paste0("output/tables/",batch,"/",box.name[2],'.txt'))}
    } else {
      # extract table
      box.name<-paste0(batch,"_",given_filename)
      start.index<-str_detect(curr.data,pattern = "D:") %>% which()
      end.index<-str_detect(curr.data,pattern = "S:") %>% which()
      box.table<-curr.data[(start.index+1):(end.index-1)]
      writeLines(box.table,paste0("output/tables/",batch,"/",box.name,'.txt'))
    }
  }
  extract_behavior<-function(given_table){
    trail.number<-1:dim(given_table)[1]
    avoid<-given_table$V3
    avoid_latency<-given_table$V4
    escape<-given_table$V5
    escape_latency<-given_table$V6
    action<-c()
    latency<-c()
    for (i in 1:length(avoid)){
      if (avoid[i]==1 & escape[i]==0) 
      {action[i]=3 
      latency[i]=avoid_latency[i]}
      if (avoid[i]==0 & escape[i]==1)
      {action[i]=2 
      latency[i]=escape_latency[i]}
      if (avoid[i]==0 & escape[i]==0)
      {action[i]=1 
      latency[i]=15}
    }
    behavior.table<-data.frame(trail.number,avoid,avoid_latency,escape,escape_latency,latency,action)
    return(behavior.table)
  }
  # output table, mice by mice----
  
  walk(dir("data"),function(given_batch){
   if ((given_batch %in% dir("output/tables"))==F){
     dir.create(paste0("output/tables/",given_batch))
   }
   mice.list<-list.files(paste0("data/",given_batch)) 
   walk(mice.list,extract_table,given_batch)
  }) 
  
  # creat mice list----
  
  table.list<-map(dir("output/tables",full.names = T),dir,full.names=T) %>% unlist
  mice.table<-map(table.list,read.table)
  names(mice.table)<-map(dir("output/tables",full.names = T),dir) %>% unlist

  # construct dataframes----
  is.complete<-(do.call(rbind,map(mice.table,dim))[,1]==30)  
  behavior.table<-map(mice.table,extract_behavior)
  lh_behavior.obj<-list(mice.table=mice.table,behavior.table=behavior.table,is.complete=is.complete)
  
  # write rds----
  curr.version<-list.files("db/",pattern = ".RDS") %>% length()
  saveRDS(lh_behavior.obj,paste0("db/lh_behavior_",as.character(curr.version),".RDS"))
  message(paste0("save db/lh_behavior_",as.character(curr.version),".RDS"))
  return(lh_behavior.obj)
} 