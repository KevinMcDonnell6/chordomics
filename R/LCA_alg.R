

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
                  "Unknown")) # If not unique return Unknown
  },FUN.VALUE = "")

  return(LCAdf)
}


assign_taxa <- function(df,shinylogs=NULL){
  t1 <- Sys.time()
  # for saving on printing time
  t3 <- t1
  df <- as.data.frame(df)
  # Add taxonomy columns to dataframe
  cols <- colnames(ncbiLineages::alltaxa[-1])
  for(rank in  cols){
    df[,rank] <- ""
  }

  len <- nrow(df)
  # progress bar
  pb <- txtProgressBar(min = 0,max = len,style = 3)

  # Loop through each read
  for( i in 1:len){

    # update progress bar every 10 seconds
    if (Sys.time() - t3 > 10){
      setTxtProgressBar(pb,i)
      if(!is.null(shinylogs)){
        shinyjs::html(id= "progressMGRAST",paste0(shinylogs,"\n",format(round((i/len)*100,digits = 2),nsmall = 2),"%"),add = F)

      }
      t3 <- Sys.time()
    }
    #Create vector of tax_ids
    ids <- unique(as.integer(unlist(strsplit(df[i,"taxids_by_seq"],";"))))

    if(any(is.na(ids))){next}


    # Merge tax_ids and taxonomy into single dataframe
    # Select lineages for possible ids
    tempdf <- ncbiLineages::alltaxa[ncbiLineages::alltaxa$tax_id %in% ids,]

    # Get rid of ids not present in the dataset (containing NAs)
    tempdf <- tempdf[complete.cases(tempdf),]

    # Compute LCA
    # returns vector of lineage
    lca <- LCA_alg(tempdf[,-1])

    # append taxa to original df
    df[i, 4:11] <- lca[1:8]
  }#end loop
  t2<- Sys.time()
  print(difftime(t2,t1))

  return(df)
}
