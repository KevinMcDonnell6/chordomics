

COG_names <- function(df,UniqueCOGs = "UniqueCOGs"){
  
  df <- as.data.frame(df)
  
  len <- nrow(df)
  df$COG_Name <- ""
  df$GroupLetters <- ""
  df$COG_Categorys <- ""
  
  for(i in 1:len){
    # print(df$Group)
    
    if(!is.null(df[i,UniqueCOGs]) & !is.na(df[i,UniqueCOGs]) & as.character(df[i,UniqueCOGs]) != "" & any(stringr::str_detect(categories$COG,as.character(df[i,UniqueCOGs])))){
      # print(i)
      res <- categories[stringr::str_detect(categories$COG,as.character(df[,UniqueCOGs][i])),]
      df$COG_Name[i] <- res$name
      df$GroupLetters[i] <- res$func
      # print(df$GroupLetter[i])
      GL <- unlist(strsplit(df$GroupLetters[i],""))
      for(letter in GL){
        
        res <- funGroups[stringr::str_detect(funGroups$Code,letter),]
        # print(res$Name)
        df$COG_Categorys[i] <- ifelse(df$COG_Categorys[i]=="",
                                       res$Name,
                                       paste(df$COG_Categorys[i],res$Name,sep = ";")
                                       )
        } # close loop through letters
      
      } # close if statement
    
    } # close for loop through reads
  

  df$COG_Category <- df$COG_Categorys
  
  
 return(df) 
}
