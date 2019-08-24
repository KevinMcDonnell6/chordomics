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

    shiny::tabPanel("Chordomics",

                    shiny::fluidRow(
                      shiny::column(5,
                      shiny::h2("Welcome to Chordomics!"),
                      shiny::p("Chordomics is a tool for visualising the link between
                               taxonomy and function in meta-omics data. We also provide a
                               pipeline for correct file formatting."),
                      shiny::uiOutput("github"),
                      shiny::br(),
                      shiny::p("Please navigate to the required tab:"),

                      shiny::h3("1. Metagenomics/Metatranscriptomics Data Processing"),
                      shiny::p("Here users can join their MG-RAST organism and ontology files.
                               The processed files can then be uploaded into Tab 3 for
                               analysis."),
                      shiny::h3("2. Metaproteomics Data Processing"),
                      shiny::p("Here the user can annotate their MetaProteomeAnalyzer
                               datasets with COG ID's using the UniProt API. The processed
                                files can then be uploaded into Tab 3 for analysis."),
                      shiny::h3("3. Processed File Upload"),
                      shiny::p("Here users can upload their processed files from Tabs 1 and 2 or a manually formatted file for analysis and visualisation on Tab 4."),
                      shiny::p("Ensure the file types are csv format and contain headings
                   of taxonomic rank (Superkingdom, Kingdom, Phylum, Class, Order, Family, Genus and/or Species) and function (COG_Category and/or COG_Name)."),
                      shiny::h3("4. Chord Plot"),
                      shiny::p("Interact with your uploaded data here or select 'Load Example Data' to see an example.")),
                      shiny::column(7,
                      # shiny::img(#src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/exampleChordPlot.svg",
                      #   src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/Screenshot.png",
                      #            width="75%"),
                      chorddiag::chorddiagOutput("StaticPlot",width = "100%",height = "800px")
                      )


                    )),

    shiny::tabPanel("1. Metagenomics/Metatranscriptomics Data Processing",
                    shiny::fluidRow( shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        # shiny::fileInput("rawMPAfile",
                        #                  label="Upload MPA CSVs here",
                        #                  multiple = F),
                        shiny::h2("Automatic"),
                        shiny::textInput("MGMid","MG-RAST Identification",placeholder = "mgm4491407.3"),
                        shiny::br(),
                        shiny::hr(),
                        shiny::h2("Manual"),
                        shiny::fileInput("MGRASTcogfile",
                                         label="Upload MG-RAST COG file here",
                                         multiple = F),
                        shiny::fileInput("MGRASTtaxfile",
                                         label="Upload MG-RAST RefSeq or other organism file here",
                                         multiple = F),
                        shiny::fileInput("MGRASTinfofile",
                                         label="Upload MG-RAST original input file (fasta) here",
                                         multiple = F),
                        shiny::hr(),
                        shiny::actionButton("preparedataMGRAST", "Prepare Data"),
                        shiny::uiOutput("thedownloadbuttonMGRAST")
                      ),

                      shiny::mainPanel(shiny::h2("Metagenomics/Metatranscriptomics Data Processing"),
                                       shiny::br(),
                                       shiny::p("Before data can be explored using chordomics it must be presented in the correct format.
                                                Use this utility to merge both functional (COG) and phylogenetic information from MG-RAST as well as format your data for the next step (Tab3)."),
                                       shiny::p("Please enter an MG-RAST ID to the upper section of the
                                                panel on the left OR upload the COG functional file, RefSeq or other phylogeny file and original
                                                input fasta file from your repository to the lower section."),
                                       shiny::p("Note, large files will take a long time to download using the Automatic pipeline."),
                                       # shiny::p("Private projects cannot be accessed automatically, please download manually the relevant files from MG-RAST."),
                                       shiny::uiOutput("github2"),
                                       shiny::p("Once the ID is entered/datasets are uploaded press Prepare Data."),
                                       shiny::p("A download button will appear once the file is ready."),
                                       shiny::br(),
                                       "Progress:",
                                       shiny::tags$pre(id = "progressMGRAST")#,
                                       #shiny::HTML("<p style='color:lightgrey'>try mgm4762935.3</p>")

                                       ))#,
                      # shiny::column(8,
                      # shiny::h3("MG-RAST Walkthrough"),
                      # shiny::p("In your repository click the 'Download' button"),
                      # shiny::img(src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/MG-RASTdownload.png",
                      #            width="100%"),
                      # shiny::hr(),
                      # shiny::br(),
                      # shiny::br(),
                      # shiny::br(),
                      # shiny::p("Under 'Annotation Downloads' download the COG ontology file and RefSeq or other organism file"),
                      # shiny::img(src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/MG-RASTfiles.png",
                      #             width="100%"),
                      # shiny::br(),
                      # shiny::br(),
                      # shiny::hr(),
                      # shiny::br(),
                      # shiny::br(),
                      # shiny::p("Download the input fasta file"),
                      # shiny::img(src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/MG-RASTinput.png",
                      #            width="100%"))#75%
                      )),
    shiny::tabPanel("2. Metaproteomics Data Processing",
                    shiny::fluidRow( shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::fileInput("rawMPAfile",
                                         label="Upload MPA CSVs here",
                                         multiple = F),
                        shiny::hr(),
                        # shiny::textInput("MGMid","MG-RAST Identification",placeholder = "mgm4491407.3"),
                        shiny::actionButton("preparedata", "Prepare Data"),
                        shiny::uiOutput("thedownloadbutton")
                      ),

                      shiny::mainPanel(shiny::verbatimTextOutput("Status"),
                                       shiny::h2("Metaproteomics Data Processing"),
                                       shiny::br(),
                                       shiny::p("Before data can be explored using chordomics it must be presented in the correct format.
                                                Use this utility to get functional (COG) information using the UniProt API as well as format your data for the next step (Tab 3)."),
                                       # shiny::p("Please upload a MetaProteomeAnalyzer(MPA) dataset to the panel on the left."),
                                       shiny::uiOutput("github3"),
                                       shiny::p("Once the dataset is entered press Prepare Data."),
                                       shiny::p("A download button will appear once the file is ready."),
                                       shiny::br(),
                                       "Progress:",
                                       shiny::tags$pre(id = "progress")#,
                                       #shiny::HTML("<p style='color:lightgrey'>try mgm4762935.3</p>")

                                       ))#,
                      # shiny::column(10,
                      # shiny::h3("MPA Walkthrough"),
                      # shiny::p("Click on the 'Export' tab"),
                      # shiny::img(src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/MPA_export_csv.png",
                      #            width="75%"),
                      # shiny::br(),
                      # shiny::br(),
                      # shiny::br(),
                      # shiny::p("Export Metaproteins"),
                      # shiny::img(src="https://raw.githubusercontent.com/KevinMcDonnell6/chordomics/master/Walkthroughs/MPA_export_metaproteins.png",
                      #            width="75%"))

                      )),

    shiny::tabPanel(
      "3. Processed File Upload",

      shiny::fluidRow( shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput("files",
                           label="Upload CSVs here",
                           multiple = TRUE)
        ),

        shiny::mainPanel(
          shiny::h2("Processed File Upload"),
          shiny::p("Upload your processed datasets using the panel on the left."),
          shiny::p("Datasets can be the output of the previous two processing tabs (1 and 2) or data formatted in the same way."),
          shiny::p("Ensure the file types are csv format and contain headings
                   of taxonomic rank (Superkingdom, Kingdom, Phylum, Class, Order, Family, Genus and/or Species)
                   and function (COG_Category and/or COG_Name). All other headings are ignored."),
          shiny::p("See example table below.")
          # shiny::uiOutput("github")

        )
      )
      ),
      shiny::br(),
      shiny::h3("Example data format"),
      shiny::div(shiny::tableOutput("exampleTable"),style="font-size:80%")
    ),
    shiny::tabPanel("4. Chord Plot",

                    # Sidebar with a slider input for zoom and level of taxa
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        width = 3,

                        shiny::actionButton("example","Load Example Data"),
                        shiny::checkboxInput("noTax", "exclude No taxonomy",value = F),
                        shiny::checkboxInput("unknown", "exclude Unknown taxonomy",value = F),
                        shiny::checkboxInput("noCOG", "exclude No COG",value = F),

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
                        shiny::htmlOutput("Total"),
                        shiny::htmlOutput("SelectedGroupName"),
                        shiny::htmlOutput("SelectedGrouptaxaName"),
                        chorddiag::chorddiagOutput("ChordPlot", width="100%", height='1000px')#,

                      )
                    )
    )

  )
)


