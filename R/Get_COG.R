
Get_COG <- function(df,UniprotColumn,shinylogs=NULL){
  len <- nrow(df)
 
  df_cog <- data.frame(Uniprot = df[,UniprotColumn],
                       AllCOGs = "",
                       UniqueCOGs = "",
                       stringsAsFactors = F)
  
  
  for(i in 1:len){
    # setTxtProgressBar(pb, i)
    # i<-1
    if(!is.null(shinylogs)){
      shinyjs::html("progress", paste0(shinylogs, format(round((i/len)*100,digits = 2),nsmall = 2),"%"),add = F)
    }
    
    UniprotIDs <- stringr::str_trim(unlist(strsplit(df_cog[i,"Uniprot"],",")))
    for(j in UniprotIDs)
    
      {
        # print(j)
        # LCA <- df$Lowest.Common.Ancestor[i]
        # Uni <- j
        url  <- "https://www.uniprot.org"
        columns <- paste("id","entry%20name","database(EGGNOG)",sep=",")
        
        path <- paste("uniprot/?query=",j,"&sort=score&columns=",columns,"&limit=1&format=tab",sep = "")
        
        
        # send request
        uni.raw.result <- httr::GET(url = url, path = path)
        # uni.raw.result
        
        if (uni.raw.result$status_code == 400){
          df_cog[i,AllCOGs] <- paste(df_cog[i,"AllCOGs"],"")
        }
        
        else{
          # convert returned data to charachters
          uni.this.raw.content <- rawToChar(uni.raw.result$content)
          # uni.this.raw.content
          # convert data to dataframe
          res <- read.delim(textConnection(uni.this.raw.content),sep="\t",stringsAsFactors = F)
          # res
          #df_cog[i,"COG"] <- paste(df_cog[i,"COG"],unlist(strsplit(as.character(res$Cross.reference..EGGNOG.),";"))[2])
          # print(paste(j,res$Cross.reference..EGGNOG.))
          if(stringr::str_detect(res$Cross.reference..EGGNOG.,"COG\\d{4}") & !is.na(res$Cross.reference..EGGNOG.)){
            df_cog[i,"AllCOGs"] <- ifelse(df_cog[i,"AllCOGs"] == "",
                                      paste(unlist(stringr::str_extract_all(res$Cross.reference..EGGNOG.,"COG\\d{4}")),collapse = " "),
                                      paste(df_cog[i,"AllCOGs"],paste(unlist(stringr::str_extract_all(res$Cross.reference..EGGNOG.,"COG\\d{4}")),collapse = " "))
            )
            
          
            }
          }
       
    }
    
    df_cog[i,"UniqueCOGs"] <- ifelse(df_cog$AllCOGs[i]=="",
                                     "",
                                     paste(unique(unlist(strsplit(df_cog$AllCOGs[i]," "))),collapse = " ")
                                    )
  }
    
 
  
  
  
  return(cbind(df,df_cog))
  
}


