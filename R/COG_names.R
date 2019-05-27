COG_names <- function(df,UniqueCOGs = "UniqueCOGs"){
  # see https://stackoverflow.com/questions/9521009/
  # loads the categories object into env; otherwise, tests wont work
  data(categories, envir=environment())
  data(funGroups, envir=environment())
  df <- as.data.frame(df)

  # ensure that only one listed per row
  if (any(grepl(" ",df$UniqueCOGs ))){
    warning("No more than a single COG should be provided per row! If multiple CDS on a sequence, split into multiple rows. For now, we will select the first... ")
    # select the first one only
    for(i in 1: nrow(df)){
      df[i, UniqueCOGs] <- strsplit(df[i, UniqueCOGs], " ")[[1]][1]
    }
  }

  df <- dplyr::left_join(df, categories, by=c(UniqueCOGs = "COG"))
  df <- dplyr::rename(df, COG_Category = "func", COG_Name = "name")
  # for(i in 1:len){
  #   # print(df$Group)
  #
  #   if(!is.null(df[i,UniqueCOGs]) & !is.na(df[i,UniqueCOGs]) & as.character(df[i,UniqueCOGs]) != "" & any(stringr::str_detect(categories$COG,as.character(df[i,UniqueCOGs])))){
  #     # print(i)
  #     res <- categories[stringr::str_detect(categories$COG,as.character(df[,UniqueCOGs][i])),]
  #     df$COG_Name[i] <- res$name
  #     df$GroupLetters[i] <- res$func
  #     # print(df$GroupLetter[i])
  #     GL <- unlist(strsplit(df$GroupLetters[i],""))
  #     for(letter in GL){
  #
  #       res <- funGroups[stringr::str_detect(funGroups$Code,letter),]
  #       # print(res$Name)
  #       df$COG_Categorys[i] <- ifelse(df$COG_Categorys[i]=="",
  #                                      res$Name,
  #                                      paste(df$COG_Categorys[i],res$Name,sep = ";")
  #                                      )
  #       } # close loop through letters
  #
  #     } # close if statement
  #
  #   } # close for loop through reads
  #
  #
  # df$COG_Category <- df$COG_Categorys


 return(df)
}
