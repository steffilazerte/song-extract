# server.R

library(tuneR)
library(seewave)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("functions.R")

shinyServer(
    function(input, output, session) {
        ###############
        ## Defaults
        ###############
        ## First look for defaults
        if(file.exists("./Data/defaults.txt")) {
            if(file.info("./Data/defaults.txt")$size > 0) {
                defaults <- read.table("./Data/defaults.txt", header = T)
            } else defaults <- NULL
        } else{ 
            defaults <- NULL
        }
        
        ## Load progress to date
        if(file.exists("./Data/progress.txt")) {
            p <- read.csv("./Data/progress.txt")
        } else {
            p <- NULL
        }
        
        ## Load song list
        if(file.exists("./Data/song_list.txt")) {
            sl <- read.csv("./Data/song_list.txt")
        } else {
            sl <- NULL
        }

        ## Setup file structure
        if(!dir.exists("./Data/")) dir.create("./Data/")
        if(!dir.exists("./Data/Times/")) dir.create("./Data/Times/")
        if(!dir.exists("./Data/Images/")) dir.create("./Data/Images/")
        
        ###############
        ### General
        ###############

        ## reactive data
        data <- reactiveValues(song_list = sl,
                               progress = p,
                               times = NULL,
                               write = NULL,
                               status_error = "",
                               a.null = data.frame(type = "song",
                                                   time.P1 = NA,
                                                   time.P2 = NA,
                                                   freq.P1 = NA,
                                                   freq.P2 = NA,
                                                   freq.M = NA,
                                                   freq.dom.spec = NA),
                               a = data.frame(type = "song",
                                              time.P1 = NA,
                                              time.P2 = NA,
                                              freq.P1 = NA,
                                              freq.P2 = NA,
                                              freq.M = NA,
                                              freq.dom.spec = NA),
                               measures = NULL)      
        
        ## Delay some things until we're ready to go
        session$onFlushed(function() {
            data$starting <- FALSE
        })
        
        ## Reactive ID of the currently active song
        current <- reactive({
            if(!is.null(data$song_list)){
                if(nrow(data$song_list[data$song_list$time.done == FALSE,]) > 0) {
                    ## Sort so not done is at the top but descending
                    data$song_list <- data$song_list[order(data$song_list$time.done, data$song_list$ID),]
                    data$song_list[data$song_list$time.done==FALSE,][1,]
                } else {
                    NULL
                }
            } else {
                NULL
            }
        })

        current.song <- reactive({
            if(!is.null(current())) as.character(current()$file)
        })
        current.ID <- reactive({
            if(!is.null(current())) current()$ID
        })
        current.n <- reactive({
            if(!is.null(current())) current()$n
        })

        current.IDs <- reactive({
            if(!is.null(data$song_list)) {
                unique(as.character(data$song_list$ID))
            } else NULL
        })
        
        ###############
        ### Setup
        ###############
        if(is.null(defaults)){
            data$defaults <- list(
                songs_location = "/home/steffi/Data.big/Testing",
                pattern_ID = "([BCMC]{2}[ABC]{1}[0-9]{2})",
                pattern_n = "([0-9]{3}).wav",
                notes = c(1,3),
                collevels_min = -35,
                collevels_bin = 3,
                wl = 512,
                save_plot = TRUE
                )
                
        } else {
            ## Grab the defaults from the file, remove any non-unique values (based on some defaults having min/max)
            data$defaults <- lapply(as.list(defaults), "unique")
            isolate(data$defaults[grep("pattern|songs_location", names(data$defaults))] <- lapply(data$defaults[grep("pattern|songs_location", names(data$defaults))], "as.character"))
            ## Make sure there are always two notes (otherwise don't get a proper slider)
            isolate(if(length(data$defaults$notes) < 2) data$defaults$notes[2] <- data$defaults$notes[1])
        }

        ## Get the defaults and create the UIs
        output$UI_setup_songs_location <- renderUI({
            textInput("default_songs_location", label = "", value = data$defaults$songs_location)
        })
        output$UI_setup_pattern_ID <- renderUI({
            textInput("default_pattern_ID", label = "", value = data$defaults$pattern_ID)
        })
        output$UI_setup_pattern_n <- renderUI({
            textInput("default_pattern_n", label = "", value = data$defaults$pattern_n)
        })
        output$UI_setup_notes <- renderUI({
            sliderInput("default_notes", label = strong("Assign the min and max number of notes you reasonably expect to see"), min = 1, max = 10, value = data$defaults$notes, step = 1)
        })
        output$UI_setup_collevels_min <- renderUI({
            sliderInput("default_collevels_min",
                        label = "Default minimum dB to show",
                        min = -100, max = 0, step = 1, value = data$defaults$collevels_min)
        })
        output$UI_setup_collevels_bin <- renderUI({
            sliderInput("default_collevels_bin",
                        label = "Default # amplitude bins per colour",
                        min = 1, max = 20, step = 1, value = data$defaults$collevels_bin)
        })
        output$UI_setup_wl <- renderUI({
            radioButtons("default_wl",
                         label = "Default window Length",
                         choices = list(128, 256, 512, 1024, 2048),
                         selected = data$defaults$wl)
        })
        output$UI_setup_save_plot<- renderUI({
            radioButtons("default_save_plot",
                         label = "Should a PNG of each plot (with min/max times, freq etc. be saved?",
                         choices = list("TRUE","FALSE"),
                         selected = data$defaults$save_plot)
        })
        
        ## Save and Set Defaults when buttom pressed
        observeEvent(input$setup_save1, {
                         ## Grab all the setup input and save to the defaults list (d)
                         temp <- reactiveValuesToList(input)
                         temp <- temp[grep("default_",names(temp))]
                         names(temp) <- gsub("default_(.+)", "\\1", names(temp))
                         data$defaults <- temp
                         write.table(as.data.frame(do.call(cbind, data$defaults)), "./Data/defaults.txt", row.names = F, sep = "\t")
                     })

        ## Render values for showing:
        output$setup_folders <- renderPrint({
            if(!is.null(data$sl)) {
                sl$file[1:10]
            } else list.files(data$defaults$songs_location)[1:10]
        })

        ## Render examples for showing:
        output$setup_ID_eg <- renderTable({
            data.frame(Original = list.dirs(data$defaults$songs_location)[2:4], 
                       ID = gsub(paste0(".*",data$defaults$pattern_ID,".*"), "\\1", list.dirs(data$defaults$songs_location)[2:4]))
            data.frame(Original = list.dirs(data$defaults$songs_location)[2:4], 
                       ID = gsub(paste0(".*",data$defaults$pattern_ID,".*"), "\\1", list.dirs(data$defaults$songs_location)[2:4]))
        }, include.rownames = F)
        
        output$setup_n_eg <- renderTable({
            data.frame(Original = list.files(list.dirs(data$defaults$songs_location)[2])[1:3], 
                       n = gsub(paste0(".*",data$defaults$pattern_n,".*"), "\\1", list.files(list.dirs(data$defaults$songs_location)[2])[1:3]))
        }, include.rownames = F)
        
        ###############
        ### Status
        ###############
        #volumes <- c(wd = "/home/steffi/Data.big")
        #shinyDirChoose(input, 'dir', session = session, roots = volumes)

        output$status_error <- renderText({
            data$status_error
        })
        
        observeEvent(input$progress_update, {
            ## Songs total
            l.s <- list.files(data$defaults$songs_location, pattern = data$defaults$pattern_n, recursive = TRUE, include.dirs = TRUE, full.names = T)
            
            ## Progress
            sl <- data.frame(file = l.s,
                             ID = gsub(paste0(".*",data$defaults$pattern_ID,".*"), "\\1", l.s),
                             n = as.integer(gsub(paste0(".*",data$defaults$pattern_n,".*"), "\\1", l.s)))
            sl <- sl[!is.na(sl$n) & !is.na(sl$ID),]

            p <- sl %>%
              group_by(ID) %>%
              summarise(total = length(file))
            
            times <- unique(do.call("rbind", lapply(list.files("./Data/Times/", pattern = "*.csv$", full.names = T), read.csv))[,c("ID","n")])

            if(!is.null(times)){
                times$time.done <- TRUE
                sl <- merge(sl, times, by = c("ID","n"), all.x = T)
                sl$time.done[is.na(sl$time.done)] <- FALSE
                
                ## Summarize progress
                times <- times %>%
                  group_by(ID) %>%
                  summarize(time.n = length(n))
                
                p <- merge(p, times, by = c("ID"), all.x = T)
                p$time.n[is.na(p$time.n)] <- 0
            } else {
                sl$time.done <- FALSE
                p$time.n <- 0
            }
            
            p$time.done <- p$time.n == p$total
            
            ## Save to disk
            write.csv(p, "./Data/progress.txt", row.names = F)
            write.csv(sl, "./Data/song_list.txt", row.names = F)
            
            ## Save to reactive variables
            data$song_list <- sl
            data$progress <- p
        })
       
        ## Load list of songs from directory
        output$progress <- renderDataTable({
            if(!is.null(data$progress)){
                temp <- data$progress
                temp$time.n <- as.integer(temp$time.n)
                names(temp) <- c("ID","Total # Vocs","# Extracted", "Individual Complete?")
                temp
            }
        }, options = list(pageLength = 10))

        output$current_song <- renderText({
            if(!is.null(current())) {
                current.song()
            } else {
                "No vocalization active (No vocalizations in cue)"
            }
        })

        output$progress_select_list<- renderUI({
            if(!is.null(current.IDs())){
                selectInput("songlistID", 
                            label = h4("Select ID"),
                            choices = current.IDs(), selected = current.IDs()[1])
            }
        })

        ## Load current time data table
        output$data_song_list <- renderDataTable({
            if(!is.null(data$song_list) & !is.null(input$songlistID)){
                validate(need(!is.null(input$songlistID) & nrow(data$song_list[data$song_list$ID == input$songlistID,]) > 0, "No data for this Individual"))

                data$song_list[data$song_list$ID == input$songlistID,]
            }
        }, options = list(pageLength = 10, columnDefs = list(list(width = "30em", targets = 0))))
                                            
                  

        ###############
        ## Data Tables
        ###############
        output$select_data <- renderUI({
            temp <- as.list(unique(as.character(data$song_list$ID)))
            names(temp) <- unique(as.character(data$song_list$ID))
            selectInput("dataID", 
                        label = h4("Select ID (default is active ID)"),
                        choices = temp, selected = current.ID())
        })

        ## Load current time data table
        output$data_time <- renderDataTable({
            validate(need(!is.null(input$dataID) & file.exists(paste0("./Data/Times/",input$dataID,"_times.csv")), "No data for this Individual"))
            read.csv(paste0("./Data/Times/",input$dataID,"_times.csv"))
        }, options = list(pageLength = 50))
                                            
                    
        ###############
        ## Time extraction
        ###############

        ## Prep measures for saving, whether or not we have them
        measures <- reactive({
            temp <- data$a %>%
              gather(key = "measure", value = "value", -type) %>%
              spread(key = "measure", value = "value")
            
            names(temp) <- gsub("_", ".", names(temp))
            temp <- cbind(ID = current.ID(), 
                          n = current.n(), 
                          temp, 
                          fraction = input$time_fraction, 
                          extracted = Sys.time())
        })
        
        ## Load wave
        wave <- reactive({
          validate(need(file.exists(current.song()), paste0("Song file ", current.song(), " couldn't be found, consider using the 'Update Status' button on the 'Status' tab.")))
            readWave(current.song())
        })
        

        ## Save clicks as time data        
        observeEvent(input$click, {
                         data$times <- rbind(data$times, data.frame(xmin = input$click$xmin, xmax = input$click$xmax, ymin = input$click$ymin, ymax = input$click$ymax))
                         data$times <- data$times[order(data$times$xmin),]

                         res <- as.integer(input$time_wl) / wave()@samp.rate
                         data$times$xmin[data$times$xmin < res] <- res
                         data$times$xmax[data$times$xmax >= length(wave())/wave()@samp.rate] <- length(wave())/wave()@samp.rate - res*2
                     })

        ## Change button depending on times


        ## Disable the buttons if no song present
        buttonDisabled <- observe({
            min.n <- data$defaults$notes[1]
            max.n <- data$defaults$notes[2]
            if(is.null(current())) {
                updateButton(session, "calc", disabled = TRUE, style = "default")
                updateButton(session, "reset", disabled = TRUE)
                updateButton(session, "nogood", disabled = TRUE)
            } else if (length(data$times$xmin) >= min.n & length(data$times$xmin) <= max.n) {
                updateButton(session, "calc", disabled = FALSE, style = "success")
                updateButton(session, "reset", disabled = FALSE)
                updateButton(session, "nogood", disabled = FALSE)
            } else {
                updateButton(session, "calc", disabled = TRUE, style = "default")
                updateButton(session, "reset", disabled = FALSE)
                updateButton(session, "nogood", disabled = FALSE)
            }
        })
                                       
        ## Reset times if reset button clicked
        observeEvent(input$reset, {
            data$times <- NULL
            data$a <- data$a.null
        })


        ## Calculate Times if Calc button pressed
        observeEvent(input$calc, {
            ck <- check_times(times = data$times, notes = data$defaults$notes)
            if(ck != TRUE)  {
                output$time_error <- renderText(ck)
                data$times <- NULL
            } else {
                output$time_error <- renderText("")

                withProgress(message = "Calculating...", expr = {
                  prog_inc <- 1 / nrow(data$times)
                  ## Acoustat
                  data$a <- data$a.null
                  res <- as.integer(input$time_wl) / wave()@samp.rate
                  for(i in 1:nrow(data$times)){
                    temp <- data$times[i,]
                    #browser()
                    setProgress(value = prog_inc * (i-1), detail = paste0("Note ", i, " of ", nrow(data$times), ": Acoustat"))
                    acous <- c(
                      acoustat(wave(), wl = 256, fraction = input$time_fraction, tlim=c(temp$xmin,temp$xmax), flim = c(temp$ymin,temp$ymax), plot = F)[c("time.P1", "time.P2")], 
                      acoustat(wave(), wl = 1024, fraction = input$time_fraction, tlim=c(temp$xmin,temp$xmax), flim = c(temp$ymin,temp$ymax), plot = F)[c("freq.P1", "freq.M", "freq.P2")])
                    temp <- data.frame(note = i, 
                                       acous)
                    temp[, c("time.P1", "time.P2")] <-  temp[, c("time.P1", "time.P2")] + data$times[i, 'xmin']
                    temp <- temp[, names(temp) %in% names(data$a.null)]
                    if(i == 1) data$a <- temp else data$a <- rbind(data$a, temp)
                  }
                  
                  ## Dom freqs
                  for(i in 1:nrow(data$times)){
                    setProgress(value = prog_inc * (i-1), detail = paste0("Note ", i, " of ", nrow(data$times), ": Dom Freq"))
                    temp <- data$times[i,]
                    wave2 <- cutw(wave(), from = temp$xmin, to = temp$xmax, output = "Wave") %>%
                      fir(wave = ., wl = 512, from = temp$ymin*1000, to = temp$ymax*1000, output = "Wave")
                    
                    temp.spec <- as.data.frame(meanspec(wave2,
                                                        wl = 512,
                                                        fftw = FALSE,
                                                        plot = FALSE,
                                                        norm = FALSE)) %>%
                      dplyr::filter(y == max(y, na.rm = TRUE)) %>%
                      rename(freq = x, amp = y)
                    data$a$freq.dom.spec[i] = temp.spec$freq
                  }
                  
                  ## Identify notes
                  data$a <- data$a[order(data$a$time.P1),]
                  data$a$type <- paste0("note", 1:nrow(data$a))
                  
                }) #End of withProgress
            }
        })
                
        ## Write null data if button "nogood" pressed, change the value of data$a AND SAVE
        observeEvent(input$nogood, {
            data$a <- data$a.null
            data$write <- measures()
        })
        
        ## Write data for saving when SAVE Button pressed
        observeEvent(input$save, {
          output$time_error <- renderText("")
          data$write <- measures()              
        })

        ## When data saved for writing, write it!
        observeEvent(data$write, {
            if(data$defaults$save_plot == TRUE & data$write$type[1] != "song"){
                if(!dir.exists(paste0("./Data/Images/",current.ID()))) dir.create(paste0("./Data/Images/",current.ID()))
                png(paste0("./Data/Images/",current.ID(),"/",current.ID(),"_",current.n()), width = 600, height = 480)
                spectro(wave(), wl = as.integer(input$time_wl), flim = c(input$time_flim[1], input$time_flim[2]), tlim = c(input$time_tlim[1], input$time_tlim[2]), scale = F, collevels = seq(input$time_collevels_min, 0, by = input$time_collevels_bin), fftw = FALSE)
                title(paste0(current.ID()," - ", current.n()))
                rect(xright = data$a$time.P1, xleft = data$a$time.P2, ybottom = data$a$freq.P1, ytop = data$a$freq.P2, col = "#0000FF30")
                segments(x0 = data$a$time.P1, x1 = data$a$time.P2, y0 = data$a$freq.M, y1 = data$a$freq.M, col = "red", lwd = 3)
                segments(x0 = data$a$time.P1, x1 = data$a$time.P2, y0 = data$a$freq.dom.spec, y1 = data$a$freq.dom.spec, col = "green", lwd = 3)
                dev.off()
            }
            
            append <- file.exists(paste0("./Data/Times/",current.ID(),"_times.csv"))
            write.table(x = data$write,
                        append = append,
                        col.names = !append,
                        file = paste0("./Data/Times/",current.ID(),"_times.csv"),
                        row.names = FALSE,
                        sep = ",", quote = FALSE)
            
            data$times <- NULL
            data$a <- data$a.null
            data$write <- NULL
            data$progress <- update.progress(data$progress, type = "time")
            data$song_list[data$song_list$ID == current.ID() & data$song_list$n == current.n(),'time.done'] <- TRUE
            write.csv(data$progress, "./Data/progress.txt", row.names = F)
            write.csv(data$song_list, "./Data/song_list.txt", row.names = F)
        })

        
        ## Time limits slider based on song length
        output$UI_time_tlim <- renderUI({
            if(!is.null(current())){
                t <- c(0,length(wave())/wave()@samp.rate)
            } else if(!is.null(input$time_tlim)){
                t <- c(input$time_tlim[1], input$time_tlim[2])
            } else {
                t <- c(0,2.5)
            }

            if(!is.null(current())){
                t.max <- floor(length(wave())/wave()@samp.rate/0.001)*0.001
            } else {
                t.max <- 2.5
            }
            
            sliderInput("time_tlim", 
                        label = "Time Limits",
                        min = 0, max = t.max, value = t, step = 0.05)
        })

        ## Spectrogram UI Settings
        output$UI_time_collevels_min <- renderUI({
            sliderInput("time_collevels_min",
                        label = "Minimum dB to show",
                        min = -100, max = 0, step = 1, value = data$defaults$collevels_min)
        })
        output$UI_time_collevels_bin <- renderUI({
            sliderInput("time_collevels_bin",
                        label = "# Amplitude bins per colour",
                        min = 1, max = 20, step = 1, value = data$defaults$collevels_bin)
        })
        output$UI_time_wl <- renderUI({
            radioButtons("time_wl",
                         label = "Window Length",
                         choices = list(128, 256, 512, 1024, 2048),
                         selected = data$defaults$wl)
        })

        
        ## If data$a exists, allow saving
        output$UI_time_save <- renderUI({
            if(data$a$type[1]!="song") bsButton("save", "Save data", style = "default")
        })

        
        ## Spectrogram
        output$song <- renderPlot({
            if(!is.null(current()) & !is.null(input$time_wl)){
                if(input$time_tlim[2] <= length(wave())/wave()@samp.rate){
                    spectro(wave(), wl = as.integer(input$time_wl), flim = c(input$time_flim[1], input$time_flim[2]), tlim = c(input$time_tlim[1], input$time_tlim[2]), scale = F, collevels = seq(input$time_collevels_min, 0, by = input$time_collevels_bin), fftw = FALSE)
                    title(current.song())
                    if(data$a$type[1]!="song") {
                        rect(xright = data$a$time.P1, xleft = data$a$time.P2, ybottom = data$a$freq.P1, ytop = data$a$freq.P2, col = "#0000FF30")
                        segments(x0 = data$a$time.P1, x1 = data$a$time.P2, y0 = data$a$freq.M, y1 = data$a$freq.M, col = "red", lwd = 3)
                        segments(x0 = data$a$time.P1, x1 = data$a$time.P2, y0 = data$a$freq.dom.spec, y1 = data$a$freq.dom.spec, col = "green", lwd = 3)
                        #segments(x0 = data$a$time.P1, x1 = data$a$time.P2, y0 = data$a$freq.max.spec, y1 = data$a$freq.max.spec, col = "blue", lwd = 3)
                        #segments(x0 = data$a$time.at.max, x1 = data$a$time.at.max, y0 = data$a$freq.P1, y1 = data$a$freq.P2, col = "black", lwd = 3)
                        #segments(x0 = data$times$xmin, x1 = data$times$xmin+abs(input$time_skip[1]), y0 = data$times$ymin, y1 = data$times$ymin, col = "black", lwd = 7)
                        #segments(x0 = data$times$xmax, x1 = data$times$xmax-abs(input$time_skip[2]), y0 = data$times$ymin, y1 = data$times$ymin, col = "black", lwd = 7)
                    }
                } else {
                    plot(0:10, 0:10, type = "n", axes = F, xlab = "", ylab = "")
                    text(x = 5, y = 5, labels = "No vocalization active", cex = 3)
                }
            } else {
                plot(0:10, 0:10, type = "n", axes = F, xlab = "", ylab = "")
                text(x = 5, y = 5, labels = "No vocalization active", cex = 3)
            }
        })

        ## Get current coordinates
        output$hovertext <- renderText({
            if(is.null(input$hover)) "( , )" else paste0("(",round(input$hover$x,2),", ",round(input$hover$y,2),")")
        })

        ## Populate data frame with clicks
        output$data <- renderText({if(is.null(data$times)) "No times selected" else round(data$times$xmin, 3)})

        observeEvent(input$pause, {
          browser()
        })
    }
)