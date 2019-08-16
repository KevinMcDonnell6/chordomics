#' @importFrom magrittr "%>%"
ChordShinyAppServer <- function(input, output, session) {
    options(shiny.maxRequestSize=10000*1024^2)

    url <- shiny::a("GitHub.",href="https://github.com/KevinMcDonnell6/chordomics")
    output$github <- renderUI({shiny::tagList("For more information visit us on ",url)})


    processMPA <- function(path, outputdir){
    logging <- ("")

    # read in raw MPA file
    logging <- paste0(logging, "Read in Data")
    shinyjs::html("progress", logging,add = F)

    Data <- read.csv(path,stringsAsFactors = F)

    withCallingHandlers({
      logging <- paste0(logging, "\nGetting data from UniProt\n")
      shinyjs::html("progress", logging,add = F)
      Data <- Get_COG(Data,"Proteins",logging)

      logging <- paste0(logging, "Complete")
      logging <- paste0(logging, "\nAdd extra rows if needed")
      shinyjs::html("progress", logging,add = F)
      Data <- tidyr::separate_rows(Data,"UniqueCOGs",sep = " ")


      logging <- paste0(logging, "\nAdd labels to COG ids")
      shinyjs::html("progress", logging,add = F)
      Data <- COG_names(Data,"UniqueCOGs")
    },
    error = function(e){
      shiny::showNotification(ui = paste("Possible network error, please try again"),duration = 5)
      print("try again network error")
      print(e)})

    return(Data)


  }



  ########################### Prepare Data ######################################

  processedData <- shiny::reactiveVal()

  shiny::observeEvent(input$preparedata,{

    # create directory if it doesn't exist
    DATA_DIR <- file.path(path.expand("~"),"chordomics")
    if(!dir.exists(DATA_DIR)){
      dir.create(DATA_DIR)
    }

    # check mpa file or mg-rast id
    if(!is.null(input$rawMPAfile)){

      #run funtion process MPA
      processData <- processMPA(input$rawMPAfile$datapath)
      processedData(processData)
      shinyjs::html("progress", "\nFile ready to download!",add = T)
      # shinyjs::html("progress", paste0("\nResults in ",DATA_DIR), add = T)

      New_Name <- gsub(pattern = "(.*)(\\..*)",replacement = "\\1",x=input$rawMPAfile$name)#"(.*?)")
      new_file_name <- paste0(New_Name,"_clean.csv")
      # write.csv(processedData,file.path(DATA_DIR,paste0(New_Name,"_clean.csv")))
      output$thedownloadbutton <- shiny::renderUI({
        shiny::downloadButton('downloadData', 'Download')
      })

      # shinyjs::html("progress","\nDone",add = T)
      shinyjs::html("progress","\nClick the 'Download' button on the side panel.",add = T)

    }
      # else if(input$MGMid != ""){
    #
    #   # run process MG-rast
    #   tryCatch(
    #     {
    #
    #   logging <- ("")
    #   processData <-  processMGRAST(input$MGMid,DATA_DIR, output, e=environment())
    #
    #   # assign taxa / LCA
    #   logging <- paste0(logging,"\nAssigning LCA...")
    #   shinyjs::html("progress",logging)
    #   processData <- assign_taxa(processData,logging)
    #   colnames(processData) <- stringr::str_to_title(colnames(processData))
    #   processData <- processData %>%
    #     dplyr::rename("UniqueCOGs"=Cogs_by_seq) %>%
    #     tidyr::separate_rows("UniqueCOGs",sep=";")
    #
    #   #colnames(processData)[colnames(processData)=="Cogs_by_seq"]<-"COG"
    #
    #
    #   # COG Names
    #   shinyjs::html("progress","\nAdding names",add = T)
    #   processData <- COG_names(processData,"COG")
    #   processedData(processData)
    #   # save data
    #   shinyjs::html("progress","\nFile ready to download!",add = T)
    #   New_Name <- input$MGMid
    #   new_file_name <- paste0(New_Name,"_clean.csv")
    #   output$thedownloadbuttonMGRAST <- shiny::renderUI({
    #     shiny::downloadButton('downloadData', 'Download')
    #   })
    #   #write.csv(processData,file.path(DATA_DIR,paste0(New_Name,"_clean.csv")))
    #   # shinyjs::html("progress","\nDone",add = T)
    #   shinyjs::html("progress","\nClick the 'Download' button on the side panel.",add = T)
    #
    #     },
    #   error = function(e){
    #     shiny::showNotification(ui = paste("Possible network error, please try again"),duration = 5)
    #     print("try again network error")
    #     print(e)})
    #
    # }
    else{
      #prompt user to enter something
      shiny::showNotification(ui = paste("Upload an MPA csv or select an MG-Rast dataset"),duration = 5)
    }
  })


  processedDataMGRAST <- shiny::reactiveVal()
  shiny::observeEvent(input$preparedataMGRAST,{

    # create directory if it doesn't exist
    DATA_DIR <- file.path(path.expand("~"),"chordomics")
    if(!dir.exists(DATA_DIR)){
      dir.create(DATA_DIR)
    }

    # check mpa file or mg-rast id
    # if(!is.null(input$rawMPAfile)){
    #
    #   #run funtion process MPA
    #   processData <- processMPA(input$rawMPAfile$datapath)
    #   processedData(processData)
    #   shinyjs::html("progress", "\nFile ready to download!",add = T)
    #   # shinyjs::html("progress", paste0("\nResults in ",DATA_DIR), add = T)
    #
    #   New_Name <- gsub(pattern = "(.*)(\\..*)",replacement = "\\1",x=input$rawMPAfile$name)#"(.*?)")
    #   new_file_name <- paste0(New_Name,"_clean.csv")
    #   # write.csv(processedData,file.path(DATA_DIR,paste0(New_Name,"_clean.csv")))
    #   output$thedownloadbutton <- shiny::renderUI({
    #     shiny::downloadButton('downloadData', 'Download')
    #   })
    #
    #   # shinyjs::html("progress","\nDone",add = T)
    #   shinyjs::html("progress","\nClick the 'Download' button on the side panel.",add = T)
    #
    # }else
      if(input$MGMid != ""){

      # run process MG-rast
      tryCatch(
        {

          logging <- ("")
          processData <-  processMGRAST(ID = input$MGMid,
                                        TMP_DIR = DATA_DIR,
                                        e=environment())

          # assign taxa / LCA
          logging <- paste0(logging,"\nAssigning LCA...")
          shinyjs::html("progressMGRAST",logging)
          processData <- assign_taxa(processData,logging)
          colnames(processData) <- stringr::str_to_title(colnames(processData))
          processData <- processData %>%
            dplyr::rename("UniqueCOGs"=Cogs_by_seq) %>%
            tidyr::separate_rows("UniqueCOGs",sep=";")

          #colnames(processData)[colnames(processData)=="Cogs_by_seq"]<-"COG"


          # COG Names
          shinyjs::html("progressMGRAST","\nAdding names",add = T)
          processData <- COG_names(processData,"COG")
          processedDataMGRAST(processData)
          # save data
          shinyjs::html("progressMGRAST","\nFile ready to download!",add = T)
          New_Name <- input$MGMid
          new_file_name <- paste0(New_Name,"_clean.csv")
          output$thedownloadbuttonMGRAST <- shiny::renderUI({
            shiny::downloadButton('downloadDataMGRAST', 'Download')
          })
          #write.csv(processData,file.path(DATA_DIR,paste0(New_Name,"_clean.csv")))
          # shinyjs::html("progressMGRAST","\nDone",add = T)
          shinyjs::html("progressMGRAST","\nClick the 'Download' button on the side panel.",add = T)

        },
        error = function(e){
          shiny::showNotification(ui = paste("Possible network error, please try again"),duration = 5)
          print("try again network error")
          print(e)})

    }else if(!is.null(input$MGRASTcogfile) & !is.null(input$MGRASTtaxfile) & !is.null(input$MGRASTinfofile)){
      # run process MG-rast
      tryCatch(
        {

          logging <- ("")
          processData <-  processMGRAST(privateCOGfile = input$MGRASTcogfile$datapath,
                                        privateRefSeqfile = input$MGRASTtaxfile$datapath,
                                        privateInfofile = input$MGRASTinfofile$datapath,
                                        TMP_DIR = DATA_DIR,
                                        e=environment())

          # assign taxa / LCA
          logging <- paste0(logging,"\nAssigning LCA...")
          shinyjs::html("progressMGRAST",logging)
          processData <- assign_taxa(processData,logging)
          colnames(processData) <- stringr::str_to_title(colnames(processData))
          processData <- processData %>%
            dplyr::rename("UniqueCOGs"=Cogs_by_seq) %>%
            tidyr::separate_rows("UniqueCOGs",sep=";")

          #colnames(processData)[colnames(processData)=="Cogs_by_seq"]<-"COG"


          # COG Names
          shinyjs::html("progressMGRAST","\nAdding names",add = T)
          processData <- COG_names(processData,"COG")
          processedDataMGRAST(processData)
          # save data
          shinyjs::html("progressMGRAST","\nFile ready to download!",add = T)
          New_Name <- input$MGMid
          new_file_name <- paste0(New_Name,"_clean.csv")
          output$thedownloadbuttonMGRAST <- shiny::renderUI({
            shiny::downloadButton('downloadDataMGRAST', 'Download')
          })
          #write.csv(processData,file.path(DATA_DIR,paste0(New_Name,"_clean.csv")))
          # shinyjs::html("progressMGRAST","\nDone",add = T)
          shinyjs::html("progressMGRAST","\nClick the 'Download' button on the side panel.",add = T)

        },
        error = function(e){
          shiny::showNotification(ui = paste("Possible network error, please try again"),duration = 5)
          print("try again network error")
          print(e)})


    }else{
      #prompt user to enter something
      shiny::showNotification(ui = paste("Upload an MPA csv or select an MG-Rast dataset"),duration = 5)
    }
  })

  # Reactice to hold example datasets
  exampleData <- shiny::reactiveVal()
  observeEvent(input$example,{
    # Data <- list()
    # Data[["df1"]] <- Day1
    # Data[["df2"]] <- Day3
    # Data[["df3"]] <- Day7

    exampleData(c(
      system.file("extdata", "Day1.csv", package = "chordomics"),
      system.file("extdata", "Day3.csv", package = "chordomics"),
      system.file("extdata", "Day7.csv", package = "chordomics")
    ))
  })

  output$exampleTable <- shiny::renderTable({read.csv(system.file("extdata", "Day1.csv", package = "chordomics"),
                                                       nrows = 10)[3:10,]},
                                            width = "75%",striped = T)

  # Reset example data if data loaded in
  # taken from https://github.com/rstudio/shiny-examples/blob/master/039-download-file/server.R
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- shiny::downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
        if(!is.null(input$rawMPAfile)){
            new_file_name <- gsub(
                pattern = "(.*)(\\..*)",
                replacement = "\\1_clean.csv",
                x=input$rawMPAfile$name)#"(.*?)")
        } else {
            new_file_name = paste0(input$MGMid,"_clean.csv")
        }
        new_file_name
	  },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(processedData(), file, sep = ",",
        row.names = FALSE)
    },
    contentType = "text/csv"
  )


  output$downloadDataMGRAST <- shiny::downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      if(!is.null(input$MGRASTcogfile) & !is.null(input$MGRASTtaxfile) & !is.null(input$MGRASTinfofile)){
        new_file_name <- paste0(unlist(strsplit(processedDataMGRAST()[1,1],"\\|"))[1],"_clean.csv")
      # }
      # if(!is.null(input$rawfile)){
      #   new_file_name <- gsub(
      #     pattern = "(.*)(\\..*)",
      #     replacement = "\\1_clean.csv",
      #     x=input$rawMPAfile$name)#"(.*?)")
      } else if(!is.null(input$MGMid)) {
        new_file_name = paste0(input$MGMid,"_clean.csv")
      }
      new_file_name
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(processedDataMGRAST(), file, sep = ",",
                  row.names = FALSE)
    },
    contentType = "text/csv"
  )

  # Reset example data if data loaded in
  observeEvent(input$files,{
    exampleData(NULL)
  })



  #################### Get file names #######################
  # Reactive to store name of files
  file_name <- shiny::reactive({
    inFile <- input$files
    numberOfFiles <- length(input$files$datapath)
    names_ <- character()

    if(!is.null(inFile)){
      for(i in numberOfFiles){
        #df_<- read.csv(input$files$datapath[i])
        names_ <- c(names_,stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))

      }
    }

    if(!is.null(exampleData())){
      names_ <- c("Day1","Day2","Day3")
    }
    return(names_)
  })


  taxonomicRanksList <- c("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species")
  functionList <- c("COG_Category","COG_Name")


  ######################### Dataset upload #########################

  # Reactive to store the Data as a list
  Data <- shiny::reactive({
    shiny::req({!is.null(input$files) | !is.null(exampleData())})
    if (!is.null(exampleData())){
      files <- exampleData()
    } else{
      files <- c()
      for (f in input$files$datapath){
        files <- c(files, f)
      }
    }
    Data <- list(All=data.frame(stringsAsFactors = F))
    Datadf <- NA
    numberOfFiles <- length(files)


    # Loop through the datasets, loading successive ones into a list
    for (i in 1:numberOfFiles) {
      name_ <- paste0("df", i)
      tmpdf <- read.csv(files[i])
      tmpdf <-  tmpdf %>%
        dplyr::select(intersect(colnames(tmpdf),
                                c(taxonomicRanksList, functionList)))
      tmpdf$choromics_dataset <- i
      for (thiscol in functionList) {
        if (thiscol %in% colnames(tmpdf)) {
          # Convert NULLs to "NO COG"
          tmpdf[, thiscol] <- ifelse(is.null(tmpdf[, thiscol]) |
                                       is.na(tmpdf[, thiscol]) |
                                       tmpdf[, thiscol] == "",
                                     "No COG",
                                     tmpdf[, thiscol])
        }
      }
      for (thiscol in taxonomicRanksList) {
        if (thiscol %in% colnames(tmpdf)) {
          # Convert NULLs to "NO COG"
          tmpdf[, thiscol] <- ifelse(is.null(tmpdf[, thiscol]) |
                                       is.na(tmpdf[, thiscol]) |
                                       tmpdf[, thiscol] == "",
                                     "No taxonomy",
                                     tmpdf[, thiscol])
        }
      }
      if (i == 1) {
        Datadf <- tmpdf
      } else {
        tryCatch({
          Datadf <- rbind(Datadf, tmpdf)
        }, error = function(e) {
          shiny::showNotification(ui = paste("Datasets of Different structure"),
                                  duration = NULL)
        })
      }
    } # end files loop
    Data[["All"]] <- Datadf
    for (i in 1:numberOfFiles){
      Data[[i + 1]] <- Datadf %>% dplyr::filter(choromics_dataset == i)

    }
    if(!any(functionList %in% colnames(Data[["All"]]))){
      shiny::showNotification(ui = paste("Dataset(s) missing required columns!"),
                              duration = NULL)
    }
    return(Data)

  })



  ####################### Individual colours for functions ####################################################


  All_fns <- shiny::reactive(names(sort(table(c(Data()[["All"]]$COG_Category,"Other")),decreasing = T)))

  # Colour holder
  Function_colour_list <- shiny::reactive({
    num_fns <- length(All_fns())
    c(rep(rev(grDevices::rainbow(10)),num_fns %/% 10),rev(grDevices::rainbow(num_fns %%10)) )
  })

  ###################### Reactive Function and Phylogeny getters#############

  # Reactive to hold all taxonomic ranks in the dataset
  taxa_ranks <- shiny::reactive({
    col_names <- colnames(Data()[["All"]])
    Ranks=c("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species")
    return(intersect(col_names,Ranks))
  })

  # Reactive to hold function levels
  functionSelection <- shiny::reactive({
    col_names <- colnames(Data()[[1]])
    names_ <- c("COG_Category","COG_Name")
    return(intersect(names_,col_names))
  })


  ################ Reactives for subsetting Tax and function ###########################


  # Assigning file names to an output
  output$myFileNames <- shiny::renderText({ file_name() })

  # Create reactive value Group
  # This will hold the selected group the user chooses to look at
  Group <- shiny::reactiveVal(NULL)
  Grouptaxa <- shiny::reactiveVal(NULL)
  previoustaxa <- shiny::reactiveVal(NULL)
  previousrank <- shiny::reactiveVal(NULL)
  previousrankholder <- shiny::reactiveVal(NULL)
  previousFun <- shiny::reactiveVal(NULL)

  # When Group is selected, assign name to Group (groupSelection comes from JS)
  shiny::observeEvent(input$groupSelection,{
    Group(input$groupSelection)
  })

  # When Group is selected, assign name to Group (grouptaxaSelection comes from JS)
  shiny::observeEvent(input$grouptaxaSelection,{
    Grouptaxa(input$grouptaxaSelection)
    # shiny::updateCheckboxInput(session = session,inputId = "noTax",value = F)
    if(is.null(Grouptaxa()) || Grouptaxa()!= "Other Taxa"){
      previousrank(taxa_ranks()[input$tbl_rows_selected])
      previoustaxa(Grouptaxa())
    }
  })

  shiny::observeEvent(input$tbl_rows_selected,{
    # shiny::updateCheckboxInput(session = session,inputId = "noTax",value = F)
  })

  # Initialse empty place holder for "Others" category
  others <- shiny::reactiveVal(character(0))

  # Initialse empty place holder for "Others" category
  othertaxa <- shiny::reactiveVal(character(0))

  # If group seleccted show their name
  output$SelectedGroupName <- shiny::renderText({
    shiny::req(Group())
    return(paste("<b>Selected Function:</b> ",Group()))})

  # If group selected show their name
  output$SelectedGrouptaxaName <- shiny::renderText({
    shiny::req(previoustaxa())
    return(paste("<b>Selected Taxa:</b> ",previousrank(),"-",previoustaxa()))})




  Total_entries <- shiny::reactiveVal()
  # Show total entries shown
  output$Total <- shiny::renderText({
    shiny::req(Total_entries())
    # if(!is.null(Total_entries)){

      return(paste("<b>Total count:</b> ",Total_entries()))
    # }
    })


  ############# Selection Tables for plot ####################

  # Dataset selection table, default is All
  output$tbl2 = DT::renderDT(
    DT::datatable(data.frame(Data=c("All",file_name())),
                  selection = list(mode="single", selected=1),
                  options = list(sDom  = '<"top">rt<"bottom">i',
                                 lengthChange = FALSE)
    )

  )


  # Taxonomic rank selection table
  output$tbl = DT::renderDT(
    DT::datatable(data.frame("Taxonomy"=taxa_ranks()),#c("Kingdom","Phylum","Class","Order","Family","Genus","Species")),
                  selection = list(mode="single", selected=1),
                  options = list(sDom  = '<"top">rt<"bottom">i',
                                 lengthChange = FALSE)
    )

  )

  # Function level selection table
  output$tbl3 = DT::renderDT(
    DT::datatable(data.frame("Function"=functionSelection()),#c("Group Function","Predicted Function")),
                  selection = list(mode="single", selected=1),
                  options = list(sDom  = '<"top">rt<"bottom">i',
                                 lengthChange = FALSE)
    )

  )


  ################ Reset button ######################
  shiny::observeEvent(input$reset,{
    Group(NULL)
    Grouptaxa(NULL)
    previoustaxa(NULL)
    previousrank(taxa_ranks()[input$tbl_rows_selected])
    previousrankholder(NULL)
    previousFun(NULL)

    })



 ######################## Create Plot ############################

  # reactive containing code to create plot
  Cplot <- shiny::reactive({
    numberOfFiles <- length(Data())-1#length(input$files$datapath)

    # selected dataset
    d<- input$tbl2_rows_selected
    shiny::req(d)
    # Assign selected datasets
    table1 <- as.data.frame(Data()[[d]], stringsAsFactors = F)

    # level of selected taxonomic rank
    s<- input$tbl_rows_selected

    # level of function resolution
    f <- input$tbl3_rows_selected
    shiny::req(f)

    if(input$noTax){table1 <- table1[table1[,taxa_ranks()[s]]!="No taxonomy",]}
    if(input$unknown){table1 <- table1[table1[,taxa_ranks()[s]]!="Unknown",]}
    if(input$noCOG){table1 <- table1[table1[,functionSelection()[f]]!="No COG",]}


    if (!taxa_ranks()[s] %in% colnames(table1)){
      stop(paste0("Misformatted data: '", s, "' not found in header of input data."))
    }
    # Update function and phylogeny selection
    table1[,taxa_ranks()[s]] <- as.factor(stringr::str_trim(as.character(table1[,taxa_ranks()[s]])))

    # If a taxaonomic group was selected subset the data to that group
    if(!is.null(previoustaxa())){
      table1 <- table1[table1[,previousrank()] %in% previoustaxa(),]

    }

    # Show selected function level
    table1$Predicted.Function <- stringr::str_trim(as.character(table1[,functionSelection()[f]]))

    # If "other" functions are selected update the dataset
    if(!is.null(Group()) && Group() %in% c("Other")){

      # colours update
      if(!is.null(previousFun())){
        table1 <- table1[table1[,functionSelection()[f]]%in%previousFun() & table1[,"COG_Name"] %in% others(),]
        table1$Predicted.Function <- stringr::str_trim(as.character(table1[,"COG_Name"]))

      }else{
        table1 <- table1[table1[,functionSelection()[f]] %in% others(),]

      }
    }

    # If one of the functional groups is selected update to higher resolution
    else if(!is.null(Group()) &&  Group() %in% unique(table1$COG_Category)){

      table1 <- table1[table1[,functionSelection()[f]]==input$groupSelection,]
      table1$Predicted.Function <- as.factor(stringr::str_trim(as.character(table1$COG_Name)))
      previousFun(Group())

    }

    else { Group(NULL)
      previousFun(NULL)}


    ##################### Taxonomy if Statements ################

    # if GroupTaxa is selected and the selection is "other taxa" show only the "other taxa"
    if(!is.null(Grouptaxa()) && Grouptaxa() %in% c("Other Taxa") && all(othertaxa() %in% unique(table1[,taxa_ranks()[s]]))){

      table1 <- table1[table1[,taxa_ranks()[s]] %in% othertaxa(),]
    }

    # If Grouptaxa selected show only that group
    else if(!is.null(Grouptaxa()) &&  Grouptaxa() %in% unique(table1[,taxa_ranks()[s]])){


      table1 <- table1[table1[,taxa_ranks()[s]]==input$grouptaxaSelection,]

    }else{ Grouptaxa(NULL)}


    tryCatch({
      # extract functions and taxonomy from dataset
      chord_table <- data.frame(functionCol=table1[,"Predicted.Function"],taxonomy=table1[,taxa_ranks()[s]])#Lowest.Common.Ancestor

      # encode NA's as factors
      chord_table$functionCol <- addNA(chord_table$functionCol)
      levels(chord_table$functionCol)[is.na(levels(chord_table$functionCol))]<- "N/A"
      chord_table[is.na(chord_table$functionCol),"functionCol"] <- "N/A"

      # remove NA's from analysis
      chord_table<- chord_table[chord_table$functionCol!=""&chord_table$functionCol!="N/A",]

      # ensure functions are factors
      chord_table <- data.frame("functionCol" = as.factor(as.character(chord_table$functionCol)),
                                 "taxonomy"=as.factor(as.character(chord_table$taxonomy)))
    },error= function(e){
      shiny::showNotification(ui = paste("Error in selected data please reset"),duration = 5)
    })

    # assign Total_entries
    Total_entries(nrow(chord_table))

    # summarise the tibble
    mat_list<- chord_table %>% dplyr::group_by(taxonomy,functionCol) %>% dplyr::summarise(n=dplyr::n())

    # Set those below threshold to "Other"
    sum_function <- mat_list %>% dplyr::group_by(functionCol) %>% dplyr::summarise(N=sum(n))
    sum_taxonomy <- mat_list %>% dplyr::group_by(taxonomy) %>% dplyr::summarise(N=sum(n))

    threshold <- 0.02
    funcThreshold <- ifelse(!is.null(Group()) && Group()=="Other",0,threshold)
    taxThreshold <- ifelse(!is.null(Grouptaxa()) && Grouptaxa()=="Other Taxa",0,threshold)

    Total_entries <- sum(mat_list$n)

    # Change entriess to characters
    mat_list$functionCol<- as.character(mat_list$functionCol)



    ##################### Threshold on Groups ###################################

    # Create place holder for "other" functions
    others_holder <- character(0)

    # Check if functions account for less than 2% of data
    # If yes assign them to "other" and store their name
    for(i in 1:length(sum_function$functionCol)){
      if(sum_function$N[i]/Total_entries < funcThreshold){
        others_holder <- c(others_holder,as.character(sum_function$functionCol[i]))
        mat_list$functionCol[mat_list$functionCol==sum_function$functionCol[i]]<- "Other"
      }
    }

    # If group is selcted update others
    if(is.null(Group()) || Group()!="Other"){
      others(others_holder)

    }

    #
    mat_list$taxonomy<- as.character(mat_list$taxonomy)

    # Create place holder for "other taxa"
    othertaxa_holder <- character(0)


    for(i in 1:length(sum_taxonomy$taxonomy)){

      if(sum_taxonomy$N[i]/Total_entries < taxThreshold){
        othertaxa_holder <- c(othertaxa_holder,as.character(sum_taxonomy$taxonomy[i]))
        mat_list$taxonomy[mat_list$taxonomy==sum_taxonomy$taxonomy[i]]<- "Other Taxa"
      }
    }

    # If group is selcted update others
    if(is.null(Grouptaxa()) || Grouptaxa()!="Other Taxa"){
      othertaxa(othertaxa_holder)

    }


    mat_list$functionCol<- as.factor(mat_list$functionCol)
    mat_list$taxonomy<- as.factor(mat_list$taxonomy)
    mat_list <- mat_list %>% dplyr::group_by(taxonomy,functionCol) %>% dplyr::summarise(n=sum(n))


    ################# Multiple datasets ##############################


    if(d==1 & numberOfFiles>1){

      #selected rank
      taxa <- taxa_ranks()[s]

      all_df_sums <- list()

      for(i in 2:length(Data())){
        name <- paste("df",i-1,"sum",sep = "")

        # Create holder for data
        Data.holder <- Data()[[i]]
        Data.holder[,taxa_ranks()[s]] <- (stringr::str_trim(as.character(Data()[[i]][,taxa_ranks()[s]])))


        if(input$noTax){Data.holder <- Data.holder[Data.holder[,taxa]!="No taxonomy",]}
        if(input$unknown){Data.holder <- Data.holder[Data.holder[,taxa]!="Unknown",]}
        if(input$noCOG){Data.holder <- Data.holder[Data.holder[,functionSelection()[f]]!="No COG",]}

        # if taxa was selected subset the data
        if(!is.null(previoustaxa())){
          Data.holder <- Data.holder[Data.holder[,previousrank()]%in%previoustaxa(),]

        }

        # Show selected function level
        Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))

        # If "Other" functions are selected show them
        if(!is.null(Group()) && Group() %in% c("Other")){

          # colours update
          if(!is.null(previousFun())){
            Data.holder <- Data.holder[Data.holder[,functionSelection()[f]]%in%previousFun() & Data.holder[,"COG_Name"] %in% others(),]
            Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,"COG_Name"]))

          }else{
            Data.holder <- Data.holder[Data.holder[,functionSelection()[f]] %in% others(),]
            Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))

          }
        }


        # If a functional group is selcted show it at a higher resolution
        else if(!is.null(Group()) &&  Group() %in% unique(table1$COG_Category)){

          Data.holder <- Data.holder[Data.holder[,functionSelection()[f]]==input$groupSelection,]
          Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$COG_Name))

        }

        ############# Taxonomy Selection ##################
        # If Other taxa slected show them
        if(!is.null(Grouptaxa()) && Grouptaxa() %in% c("Other Taxa") && all(othertaxa() %in% unique(table1[,taxa_ranks()[s]]))){

          Data.holder <- Data.holder[Data.holder[,taxa_ranks()[s]] %in% othertaxa(),]

          # Show selcted functions as above
          if(!is.null(Group()) &&  Group() %in% unique(table1$COG_Category)){
            Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$COG_Name))

          }else{
            Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))
          }
        }


        # If taxon selected subset the data
        else if(!is.null(Grouptaxa()) &&  Grouptaxa() %in% unique(table1[,taxa_ranks()[s]])){


          Data.holder <- Data.holder[Data.holder[,taxa_ranks()[s]]==input$grouptaxaSelection,]
          if(!is.null(Group()) &&  Group() %in% unique(table1$COG_Category)){
            Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$COG_Name))

          }else{
            # colours update
            if(!is.null(previousFun())){

              Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,"COG_Name"]))

            }else{
              Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))

            }
            }
        }


        # Use data.holder and COG_Name.holder to be consistent with earlier preprocessiing
        # Create temporary data frame
        # Then group by function and summarise
        temp <- data.frame(taxa=Data.holder[,taxa],#taxa=stringr::str_trim(as.character(table1[,taxa])),#stringr::str_trim(as.character(Data()[[i]][,taxa])),
                           Predicted.Function = Predicted.Function.holder,#Predicted.Function.holder,#stringr::str_trim(as.character(table1[,functionSelection()[f]])),#stringr::str_trim(as.character(Data()[[i]][,functionSelection()[f]])),
                           stringsAsFactors = F) %>%
          dplyr::group_by(taxa, Predicted.Function) %>% dplyr::summarise(N = dplyr::n())
        temp$N <- as.numeric(temp$N)
        colnames(temp)[3] <- paste(file_name()[i-1])
        assign(name, temp)
        all_df_sums[[i-1]] <- get(name)
      }

      # Join all  grouped datasets from above
      all_df_sums_join <- suppressMessages(Reduce(dplyr::full_join,all_df_sums))


      # Replace NA's
      all_df_sums_join <- all_df_sums_join %>% replace(is.na(.), 0)

      # Sum the function taxa combinations over all datasets
      all_df_sums_join$SUM <- rowSums(all_df_sums_join[,c(3:(length(Data())+1))])

      # arrange the data first by taxa then function
      all_df_sums_join <- dplyr::arrange(all_df_sums_join,taxa,Predicted.Function)


      # Create individual summary tables for taxonomy and function
      Sum_fun <- all_df_sums_join %>% dplyr::group_by(Predicted.Function) %>% dplyr::summarise(N=sum(SUM))
      Sum_taxa <- all_df_sums_join %>% dplyr::group_by(taxa) %>% dplyr::summarise(N=sum(SUM))

      # getthe total number of entries
      Total_entries <- sum(all_df_sums_join$SUM)

      # Reset all_df_sums_join
      all_df_sums_join <- suppressMessages(Reduce(dplyr::full_join,all_df_sums))
      all_df_sums_join <- all_df_sums_join %>% replace(is.na(.), 0)

      # For given threshold replace entries with "Other"
      for(i in 1:length(Sum_fun$Predicted.Function)){

        if(Sum_fun$N[i]/Total_entries < funcThreshold){
          all_df_sums_join$Predicted.Function[all_df_sums_join$Predicted.Function == Sum_fun$Predicted.Function[i]] <- "Other"
        }
      }

      # For given threshold replace entries with "Other"
      for(i in 1:length(Sum_taxa$taxa)){

        if(Sum_taxa$N[i]/Total_entries < taxThreshold){
          all_df_sums_join$taxa[all_df_sums_join$taxa==Sum_taxa$taxa[i]]<- "Other Taxa"
        }
      }

      # Re-summarise after re-labelling
      data_cols <- colnames(all_df_sums_join)[3: (2+numberOfFiles)]
      df_all <- all_df_sums_join
      df_all$SUM <- rowSums(df_all[, data_cols])
      df_all <- df_all %>% dplyr::group_by(taxa,Predicted.Function) %>%
        dplyr::summarise_at(data_cols, sum)#+1 for SUM

      df_group_fun <- df_all %>% dplyr::group_by(Predicted.Function) %>%
        dplyr::summarise_at(data_cols, sum)
      df_group_fun$N <- rowSums(df_group_fun[data_cols])
      df_group_fun <- dplyr::arrange(df_group_fun,dplyr::desc(N))

      df_group_tax <- df_all %>% dplyr::group_by(taxa) %>%
        dplyr::summarise_at(data_cols,sum)
      df_group_tax$N <- rowSums(df_group_tax[data_cols])
      df_group_tax <- dplyr::arrange(df_group_tax,dplyr::desc(N))

      Group_sum <- jsonlite::toJSON(as.list(df_group_fun))

      df_all <- df_all %>% dplyr::arrange(
        match(taxa,df_group_tax$taxa),
        match(Predicted.Function,df_group_fun$Predicted.Function))



      # Convert df_all to list
      # needed for converion to json
      l<-list()
      for(i in 1:nrow(df_all)){
        for(j in 1:numberOfFiles){
          if(j==1){
            l[[i]]<- as.numeric(df_all[i,j+2])
          }

          else{
            l[[i]][j]<- as.numeric(df_all[i,j+2])
          }

        }

      }


      # convert to json
      exportJson <- jsonlite::toJSON(l)

    }

    # If the first dataset isnt selected or if there isnt more than one dataset dont export
    if(d!=1 | numberOfFiles==1){exportJson<-NULL
    Group_sum <- NULL}

    # Group and summarise mat_list
    mat_list_groupFun <- mat_list %>% dplyr::group_by(functionCol) %>% dplyr::summarise(N=sum(n)) %>% dplyr::arrange(dplyr::desc(N))

    mat_list_groupTaxa <- mat_list %>% dplyr::group_by(taxonomy) %>% dplyr::summarise(N=sum(n)) %>% dplyr::arrange(dplyr::desc(N))

    mat_list <- mat_list %>% dplyr::arrange(match(taxonomy,mat_list_groupTaxa$taxonomy),match(functionCol,mat_list_groupFun$functionCol))


    # Change in order depending if multiple datasets
    if(d==1 & numberOfFiles>1){
      x <- unique(df_group_fun$Predicted.Function)

      y<- unique(df_group_tax$taxa)


    }else{
      x <- unique(mat_list$functionCol)

      y<- unique(mat_list$taxonomy)
    }

    # create zero matrix of the dimensions of the functions and taxa
    m_1 <- matrix(0,nrow = length(y),ncol=length(x),dimnames = list(y,x))


    # convert the summary table back to a dataframe
    df<- as.data.frame(mat_list)

    # add the size of the links to the zero matrix
    for( i in 1:(nrow(df))){

      m_1[toString(df[i,1]),toString(df[i,2])]<-df[i,3]
    }

    # colours update


    All_taxons <- names(sort(table(c(as.character(Data()[[1]][,taxa_ranks()[s]]),"Other Taxa")),decreasing = T))

    num_taxa <- length(All_taxons)
    Taxa_colour_list <- c(rep(rev(grDevices::rainbow(10)),num_taxa %/% 10),rev(grDevices::rainbow(num_taxa %%10)) )

    # colours update
    if(input$tbl3_rows_selected == 1 && (is.null(Group()) || Group()=="Other") && is.null(previousFun())){
      colour_list2 <- substr( Function_colour_list()[match(x,All_fns())],0,7)

    }else{
      colour_list2 <- substr(grDevices::rainbow(ncol(m_1)),0,7)
    }

    # if(input$tbl3_rows_selected == 1 && (is.null(Grouptaxa()) || Grouptaxa()=="Other Taxa") && is.null(previoustaxa())){
    colour_list1 <- substr( Taxa_colour_list[match(y,All_taxons)],0,7)
    #




    # create the chord diagram
    return(
      chorddiag::chorddiag(m_1,type = "bipartite",
                           groupColors = c(colour_list1,colour_list2),#substr(grDevices::rainbow(nrow(m_1)+ncol(m_1)),0,7),
                           groupnamePadding = 20,
                           groupnameFontsize = input$fontsize,
                           # categoryNames = T,
                           categorynamePadding = 200,
                           ticklabelFontsize = 9,
                           tickInterval = max(1,sum(mat_list$n)%/%100),
                           margin = 400-input$margin,
                           reactor = exportJson,
                           grouptotals = Group_sum,
                           firstfunindex = length(y))
    )
  })


  output$ChordPlot <- chorddiag::renderChorddiag({Cplot()})
#}

IntroPlot <- shiny::reactive({


  # Assign selected datasets
  table1 <- as.data.frame(read.csv(system.file("extdata", "Day1.csv", package = "chordomics")),
                          stringsAsFactors = F)

  # level of selected taxonomic rank
  s<- "Phylum"#input$tbl_rows_selected

  # level of function resolution
  f <- "COG_Category"#input$tbl3_rows_selected
  shiny::req(f)


  # Update function and phylogeny selection
  table1[,s] <- as.factor(stringr::str_trim(as.character(table1[,s])))


  # Show selected function level
  table1$Predicted.Function <- stringr::str_trim(as.character(table1[,f]))

    # extract functions and taxonomy from dataset
    chord_table <- data.frame(functionCol=table1[,"Predicted.Function"],taxonomy=table1[,s])#Lowest.Common.Ancestor

    # encode NA's as factors
    chord_table$functionCol <- addNA(chord_table$functionCol)
    levels(chord_table$functionCol)[is.na(levels(chord_table$functionCol))]<- "N/A"
    chord_table[is.na(chord_table$functionCol),"functionCol"] <- "N/A"

    # remove NA's from analysis
    chord_table<- chord_table[chord_table$functionCol!=""&chord_table$functionCol!="N/A",]

    # ensure functions are factors
    chord_table <- data.frame("functionCol" = as.factor(as.character(chord_table$functionCol)),
                              "taxonomy"=as.factor(as.character(chord_table$taxonomy)))



  # summarise the tibble
  mat_list<- chord_table %>% dplyr::group_by(taxonomy,functionCol) %>% dplyr::summarise(n=dplyr::n())

  # Set those below threshold to "Other"
  sum_function <- mat_list %>% dplyr::group_by(functionCol) %>% dplyr::summarise(N=sum(n))
  sum_taxonomy <- mat_list %>% dplyr::group_by(taxonomy) %>% dplyr::summarise(N=sum(n))

  threshold <- 0.02
  funcThreshold <- ifelse(!is.null(Group()) && Group()=="Other",0,threshold)
  taxThreshold <- ifelse(!is.null(Grouptaxa()) && Grouptaxa()=="Other Taxa",0,threshold)

  Total_entries <- sum(mat_list$n)

  # Change entriess to characters
  mat_list$functionCol<- as.character(mat_list$functionCol)



  ##################### Threshold on Groups ###################################

  # Create place holder for "other" functions
  others_holder <- character(0)

  # Check if functions account for less than 2% of data
  # If yes assign them to "other" and store their name
  for(i in 1:length(sum_function$functionCol)){
    if(sum_function$N[i]/Total_entries < funcThreshold){
      others_holder <- c(others_holder,as.character(sum_function$functionCol[i]))
      mat_list$functionCol[mat_list$functionCol==sum_function$functionCol[i]]<- "Other"
    }
  }



  #
  mat_list$taxonomy<- as.character(mat_list$taxonomy)

  # Create place holder for "other taxa"
  othertaxa_holder <- character(0)


  for(i in 1:length(sum_taxonomy$taxonomy)){

    if(sum_taxonomy$N[i]/Total_entries < taxThreshold){
      othertaxa_holder <- c(othertaxa_holder,as.character(sum_taxonomy$taxonomy[i]))
      mat_list$taxonomy[mat_list$taxonomy==sum_taxonomy$taxonomy[i]]<- "Other Taxa"
    }
  }




  mat_list$functionCol<- as.factor(mat_list$functionCol)
  mat_list$taxonomy<- as.factor(mat_list$taxonomy)
  mat_list <- mat_list %>% dplyr::group_by(taxonomy,functionCol) %>% dplyr::summarise(n=sum(n))

  # Group and summarise mat_list
  mat_list_groupFun <- mat_list %>% dplyr::group_by(functionCol) %>% dplyr::summarise(N=sum(n)) %>% dplyr::arrange(dplyr::desc(N))

  mat_list_groupTaxa <- mat_list %>% dplyr::group_by(taxonomy) %>% dplyr::summarise(N=sum(n)) %>% dplyr::arrange(dplyr::desc(N))

  mat_list <- mat_list %>% dplyr::arrange(match(taxonomy,mat_list_groupTaxa$taxonomy),match(functionCol,mat_list_groupFun$functionCol))

  x <- unique(mat_list$functionCol)

  y<- unique(mat_list$taxonomy)


  # create zero matrix of the dimensions of the functions and taxa
  m_1 <- matrix(0,nrow = length(y),ncol=length(x),dimnames = list(y,x))


  # convert the summary table back to a dataframe
  df<- as.data.frame(mat_list)

  # add the size of the links to the zero matrix
  for( i in 1:(nrow(df))){

    m_1[toString(df[i,1]),toString(df[i,2])]<-df[i,3]
  }

  # create the chord diagram
  return(
    chorddiag::chorddiag(m_1,type = "bipartite",
                         groupColors = substr(grDevices::rainbow(nrow(m_1)+ncol(m_1)),0,7),
                         groupnamePadding = 20,
                         groupnameFontsize = input$fontsize,
                         # categoryNames = T,
                         categorynamePadding = 200,
                         ticklabelFontsize = 9,
                         tickInterval = max(1,sum(mat_list$n)%/%100),
                         margin = 200,
                         reactor = NULL,
                         grouptotals = NULL,
                         firstfunindex = length(y),
                         clickGroupAction = "static")
  )
})

output$StaticPlot <- chorddiag::renderChorddiag({IntroPlot()})

}
