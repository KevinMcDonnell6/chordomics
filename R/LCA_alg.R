

LCA_alg <- function(df){
  # check if any matched taxa
  if(nrow(df)==0){return(rep("",8))}
  # Given df a dataframe of possible lineages as rows
  # Make sure they are characters (not factors)
  df[,] <- vapply(df[,], as.character,rep("",nrow(df)))
  
  # Create vector of lineage
  LCAdf <- vapply(df, FUN = function(x){
    un <- unique(x)
    return(ifelse(length(un)==1, # Is there a unique taxon at this rank? 
                  un, # If unqique (only 1) return it
                  "")) # If not unique return blank
  },FUN.VALUE = "")
  
  return(LCAdf)
}


assign_taxa <- function(df,shinylogs){
  t1 <- Sys.time()
  
  df <- as.data.frame(df)
  # Add taxonomy columns to dataframe
  cols <- colnames(alltaxa[-1])
  for(rank in  cols){
    df[,rank] <- ""
  }
  
  len <- nrow(df)
  # progress bar 
  pb <- txtProgressBar(min = 0,max = len,style = 3)
  
  # Loop through each read
  for( i in 1:len){
    
    # update progress bar
    setTxtProgressBar(pb,i)
    shinyjs::html(id= "progress",paste0(shinylogs,"\n",format(round((i/len)*100,digits = 2),nsmall = 2),"%"),add = F)
    #Create vector of tax_ids
    ids <- unique(as.integer(unlist(strsplit(df[i,"taxids_by_seq"],";"))))
    
    # Merge tax_ids and taxonomy into single dataframe
    # Select lineages for possible ids
    tempdf <- alltaxa[alltaxa$tax_id %in% ids,]
    
    # Get rid of ids not present in the dataset (containing NAs)
    tempdf <- tempdf[complete.cases(tempdf),]
    
    # Compute LCA
    # returns vector of lineage
    lca <- LCA_alg(tempdf[,-1])
    
    # append taxa to original df
    df[i,4:11] <- lca
  }#end loop
  t2<- Sys.time()
  print(difftime(t2,t1))
  
  return(df)
}
