library(shiny)
library(shinyBS)

shinyUI(fluidPage(
#    title("Song Extraction"),

    navbarPage(title = "Song Extraction",

               #################
               ## SETUP
               #################
               
               tabPanel("Setup",
                        h2("Setup"),
                        bsButton("setup_save1", "Save Defaults", style = "primary"),
                        hr(),
                        h3("Location of Wave Files"),
                        p("Individual vocalizations should be WAV files stored in folders according to the ID of the bird they were recorded from. E.g., ", strong("BIRD_ID/001.wav")),
                        p("Where BIRD_ID is a folder named by a unique identifier for each bird and 001.wav represents the first song for that bird"),
                        br(),
                        fluidRow(
                            column(6, strong("Current folder (edit to change):"),
                                   uiOutput("UI_setup_songs_location")),
                            column(6, strong("Current folder contains the following subfolders (SAVE DEFAULTS to update; showing a max of 10):"),
                                   verbatimTextOutput('setup_folders'))),
                        br(),
                        hr(),
                        h3("Labelling of Wave Files"),
                        fluidRow(
                            column(6, strong("Bird ID Pattern"), br(), "This is a regular expression to pick out the ID from the folders. You can match patterns on either end of the ID, just make sure the ID part of the pattern is surrounded by: ()"),
                            column(6, strong("Vocalization ID (n) Pattern"), br(), "This picks out the vocalization ID (n). You can match patterns on either end of the 'n', just make sure the 'n' part of the pattern is surrounded by: ()")),
                        fluidRow(
                            column(6, uiOutput("UI_setup_pattern_ID")),
                            column(6, uiOutput("UI_setup_pattern_n"))),
                        fluidRow(
                            column(6, strong("Example (SAVE DEFAULTS to update; showing first 3 folders):"), tableOutput('setup_ID_eg')),
                            column(6, strong("Example (SAVE DEFAULTS to update; showing first 3 .wav files from the first folder:)"), tableOutput('setup_n_eg'))),

                        
                        hr(),
                        h3("Extraction Defaults"),
                        fluidRow(
                            column(4,
                                   uiOutput("UI_setup_notes"),
                                   uiOutput("UI_setup_wl")),
                            column(4,
                                   uiOutput("UI_setup_collevels_min"),
                                   uiOutput("UI_setup_collevels_bin")),
                            column(4,
                                   uiOutput("UI_setup_save_plot"))),
                        br()
                        ##hr(),
                        ##p("Choose a different folder?"),
                        ##shinyDirButton("dir",label = "Select folder", title = "Where are your song folders?")
                        ),

               #################
               ## STATUS
               #################
               tabPanel("Status",
                        h2("Progress"),
                        br(),
                        dataTableOutput('progress'),
                        br(),
                        strong("Currently active vocalization:"),
                        textOutput("current_song"),
                        br(),
                        fluidRow(
                            column(2,
                                   bsButton("progress_update", "Update Status?", style = "primary")),
                            column(5, strong(span("CAUTION:", style = "color:red"), "This make take a while")),
                            column(5, textOutput("status_error"))),
                        br(),
                        p("Updating the status is only relevant if a) you have changed the directory in Setup, b) you have changed the song files on disk, or c) some of this information (ID, counts, etc.) is incorrect"),
                        p("You do NOT have to update the status if all you have been doing is extracting time/frequency data from your vocalizations"),
                        br(),
                        h2("Song lists"),
                        p("These are the Wav files for each ID that will be/have been extracted."),
                        uiOutput("progress_select_list"),
                        hr(), 
                        dataTableOutput("data_song_list")
                        ),

               #################
               ## DATA
               #################
               tabPanel("Data",
                        h2("Data"),
                        br(),
                        uiOutput("select_data"),
                        hr(), 
                        dataTableOutput("data_time")
                        ),
               #################
               ## Time Extraction
               #################
               tabPanel("Extract",
                        sidebarLayout(
                            sidebarPanel(
                                h2("Extract"),
                                #textOutput("wave"),
                                h4("Spectrogram Properties"),
                                uiOutput("UI_time_wl"),
                                uiOutput("UI_time_collevels_min"),
                                uiOutput("UI_time_collevels_bin"),
                                sliderInput("time_flim", 
                                            label = "Frequency Limits",
                                            min = 0, max = 15, value = c(1, 5), step = 0.1),
                                uiOutput("UI_time_tlim"),
                                sliderInput("time_fraction",
                                            label = "Fraction",
                                            min = 0, max = 100, value = 90, step = 5)#,
                                #sliderInput("time_skip",
                               #            label = "For max freq, skip how many seconds at start (-'ve) and at end (+'ve)?",
                                #            min = -0.1, max = 0.1, value = c(0,0), step = 0.005)
                                #h4("Aesthetic Properties"),
                                #radioButtons("pointline",
                                #             label = "Use points or lines?",
                                #             choices = list("Points","Lines"),
                                #             selected = "Points"),
                                #sliderInput("pointsize",
                                #            label = "Point/Line size",
                                #            min = 0, max = 6, value = 3, step = 0.5)
                                ),
                            mainPanel(
                                textOutput("hovertext"),
                                                 
                                plotOutput("song", hover = "hover", brush = brushOpts(id = "click", opacity = 0.4, delay = 500, delayType = "debounce", resetOnNew = TRUE)),
                                p(div(strong("Calculations Legend:")),
                                  div(span("Green Lines", style = "font-weight: bold; color:green"),
                                      ": Dominant Frequency (frequency at max amplitude from meanspec())"),
                                  div(span("Red Lines", style = "font-weight: bold; color:red"),
                                      ": Median Frequency from probability contours (Acoustat)"),
                                  div(span("Box", style = "font-weight: bold"),
                                      ": Start/End times (P1/P2), Min/Max (P1/P2) frequencies based on Acoustat probability contours")),
                                  #div(span("Blue Lines", style = "font-weight: bold; color:blue"),
                                  #": Maximum Frequency from sequential power spectra slices"),
                                  #div(strong("Black Lines"),": Time of Maximum frequency measured from power spectra slices")),
                                #p(span("Thick black lines (or dots, if short) show the times over which sequential power spectral slices were skipped (only used for the calculation of the '"),span("Maximum Frequency", style = "font-weight: bold; color:blue"), span(" and '"),strong("Time of Maximum Frequency"),span("' from sequential power spectra to allow skipping over sections which might catch harmonics above or below)")),
                                p("Window Lengths are 512 for dfreq, 256 for contour temporal and 1024 for contour frequency (Acoustat probability contours)."),
                                hr(),
                                h2("Times collected"),
                                tableOutput("data"),
                                br(),
                                fluidRow(bsButton("calc", "Calculate Measures", style = "default"),
                                         actionButton("reset", "Reset times"),
                                         bsButton("nogood", "Song No Good", style = "danger"),
                                         actionButton("pause", "Pause")),
                                fluidRow(uiOutput("UI_time_save")),
                                br(),
                                h4(strong(div(textOutput("time_error"), style = "color:green"))),
                                plotOutput("acoustat_plot")
                                )
                            )
                        )
               )
    ))
