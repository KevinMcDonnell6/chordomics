ChordShinyAppUI <- shiny::fluidPage( shinyjs::useShinyjs(),
  shiny::tags$head(shiny::tags$style(".rightAlign{float:right;}
                                     .shiny-notification{position:fixed;
                                     top: calc(50%);;
                                     left: calc(50%);;
                                     }
                                     hr {border-top: 1px solid #808080;}")),
  # Application title
  # titlePanel("CircosPro"),
  shiny::tabsetPanel(

    shiny::tabPanel(
      "1. File Upload",

      shiny::fluidRow( shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput("files",
                           label="Upload CSVs here",
                           multiple = TRUE)
        ),

        shiny::mainPanel(
          shiny::h2("Welcome to Chordomics!"),
          shiny::p("Upload your processed datasets using the panel on the left."),
          shiny::p("Ensure the file types are csv format and contain headings
                   of taxonomic rank (Superkingdom, Kingdom, Phylum, Class, Order, Family, Genus and/or Species) and function (COG_Category and/or COG_Name)"),
          shiny::uiOutput("github")
        )
      )
      )
    ),
    shiny::tabPanel("2. Chord Plot",

                    # Sidebar with a slider input for zoom and level of taxa
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        width = 3,

                        shiny::actionButton("example","Load Example Data"),
                        shiny::sliderInput("margin", "Zoom",  min = 0, max = 400, value = 200),
                        shiny::sliderInput("fontsize", "Font size",  min = 6, max = 40, value = 10),
                        DT::DTOutput("tbl2"),
                        DT::DTOutput("tbl"),
                        DT::DTOutput("tbl3")
                      ),

                      # Show a plot of the Circos
                      shiny::mainPanel(
                        shiny::tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        # mainPanel( textOutput("myFileNames")),
                        #downloadButton('foo',class = "rightAlign"),
                        # uiOutput("CPlot"),
                        #verbatimTextOutput("summary"),
                        shiny::actionButton('dloadImage', "Download",  class = "rightAlign", onclick =paste0(
                          "javascript: (function () { var e = document.createElement('script'); ",
                          "e.setAttribute('src', '",
                          #"svg-crowbar.js",
                          "https://combinatronics.com/KevinMcDonnell6/chordomics/master/inst/www/svg-crowbar.js",
                          "'); e.setAttribute('class', 'svg-crowbar'); document.body.appendChild(e); })();")),
                        shiny::actionButton('reset',"Reset",class = "rightAlign"),
                        # uiOutput("CPlot"),
                        shiny::htmlOutput("SelectedGroupName"),
                        shiny::htmlOutput("SelectedGrouptaxaName"),
                        chorddiag::chorddiagOutput("ChordPlot", width="100%", height='1000px')#,

                      )
                    )
    ),
    shiny::tabPanel("3. Metaproteomics Data Processing",
                    shiny::fluidRow( shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::fileInput("rawMPAfile",
                                         label="Upload MPA CSVs here",
                                         multiple = F),
                        # shiny::textInput("MGMid","MG-RAST Identification",placeholder = "mgm4491407.3"),
                        shiny::actionButton("preparedata", "Prepare Data"),
                        shiny::uiOutput("thedownloadbutton")
                      ),

                      shiny::mainPanel(shiny::verbatimTextOutput("Status"),
                                       shiny::h2("Metaproteomics Data Preprocessing"),
                                       shiny::br(),
                                       shiny::p("Before data can be explored using chordomics it must be presented in the correct format.
                                                Use this tool to get both functional (COG) and phylogenetic information as well as format your data."),
                                       shiny::p("Please upload an MetaProteomeAnalyzer(MPA) dataset to the upper section of the
                                                panel on the left"),
                                       shiny::p("Once the dataset is entered press Prepare Data"),
                                       shiny::br(),
                                       "Progress:",
                                       shiny::tags$pre(id = "progress")#,
                                       #shiny::HTML("<p style='color:lightgrey'>try mgm4762935.3</p>")

                      )

                    ))),
    shiny::tabPanel("4. MG-RAST Data Processing",
                    shiny::fluidRow( shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        # shiny::fileInput("rawMPAfile",
                        #                  label="Upload MPA CSVs here",
                        #                  multiple = F),
                        shiny::h2("Public Repository"),
                        shiny::textInput("MGMid","MG-RAST Identification",placeholder = "mgm4491407.3"),
                        shiny::br(),
                        shiny::hr(),
                        shiny::h2("Private Repository"),
                        shiny::fileInput("MGRASTcogfile",
                                         label="Upload MG-RAST COG file here",
                                         multiple = F),
                        shiny::fileInput("MGRASTtaxfile",
                                         label="Upload MG-RAST RefSeq file here",
                                         multiple = F),
                        shiny::hr(),
                        shiny::actionButton("preparedataMGRAST", "Prepare Data"),
                        shiny::uiOutput("thedownloadbuttonMGRAST")
                      ),

                      shiny::mainPanel(shiny::h2("Metagenomics/Metatranscriptomics Data Preprocessing"),
                                       shiny::br(),
                                       shiny::p("Before data can be explored using chordomics it must be presented in the correct format.
                                                Use this tool to get both functional (COG) and phylogenetic information as well as format your data."),
                                       shiny::p("Please enter an MG-RAST ID to the upper section of the
                                                panel on the left or upload the COG functional file and RefSeq phylogeny file from your private repository to the lower section"),
                                       shiny::p("Once the ID is entered/datasets are uploaded press Prepare Data"),
                                       shiny::br(),
                                       "Progress:",
                                       shiny::tags$pre(id = "progressMGRAST")#,
                                       #shiny::HTML("<p style='color:lightgrey'>try mgm4762935.3</p>")

                      )

                    )))
  )
)
