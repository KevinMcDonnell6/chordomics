
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
        url="https://rest.uniprot.org/"
        path=paste0("uniprotkb/", j, "?sort=score&limit=1&fields=accession,id,xref_eggnog")
        uni.raw.result <- httr::GET(url = url,path=path, httr::add_headers("Accept"="text/plain; format=tsv"))

        # uni.raw.result
        if (uni.raw.result$status_code == 400){
          print(uni.raw.result)
          df_cog[i,"AllCOGs"] <- paste(df_cog[i,"AllCOGs"],"")
        } else{
          # convert returned data to charachters
          uni.this.raw.content <- rawToChar(uni.raw.result$content)
          # uni.this.raw.content
          # convert data to dataframe
          res <- read.delim(textConnection(uni.this.raw.content),sep="\t",stringsAsFactors = F)
          # res
          if(stringr::str_detect(res$eggNOG,"COG\\d{4}") & !is.na(res$eggNOG)){
            df_cog[i,"AllCOGs"] <- ifelse(df_cog[i,"AllCOGs"] == "",
                                      paste(unlist(stringr::str_extract_all(res$eggNOG,"COG\\d{4}")),collapse = " "),
                                      paste(df_cog[i,"AllCOGs"],paste(unlist(stringr::str_extract_all(res$eggNOG,"COG\\d{4}")),collapse = " "))
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


