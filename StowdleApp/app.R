#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(formattable)
library(shinythemes)
library(dplyr)
library(RCurl)
library(rclipboard)
library(shinyalert)
library(shinyjs)
library(jsonlite)
library(purrr)
library(shinyStore)
library(bslib)
library(shinyWidgets)


# Data
data <- read.csv("data/StowRoads.csv")
data <- dplyr::arrange(data, Name)

# Define UI (was theme = shinytheme("cosmo"))
ui <- fluidPage(theme = bs_theme(font_scale = 1.0, 
                                bootswatch = "sketchy", bg = "#F5F5F5", 
                                fg = "#000"),

    # Application title
    
    titlePanel(tagList(img(src = "apple.png", height = 35, width = 35),"Stowdle"
      ),
    windowTitle = "Stowdle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 9, # was 9
            uiOutput("StreetName"),
            shiny::actionButton("help", "?", 
              style = "position: absolute; top:-35px; right: 15px; 
                      width: 25px;
                      height: 25px;
                      border-radius: 15px;
                      border: 1px solid black;
                      color: black;
                      background-color: white;
                      text-align: center;
                      font-size: 16px;
                      padding-top: 5px;
                      padding-right: 4px;
                      line-height: 15px;
                      padding-left: 5px;
                      padding-bottom: -10px"
              )
        ),
        
        

        # Show a table of Guesses
        mainPanel(width = 8,
           formattable::formattableOutput("table"),
           actionButton("Guess", "Guess"),
           uiOutput("Copy"),
           htmlOutput("answer"),
           htmlOutput("text"),
           rclipboardSetup(),
           useShinyjs(),
           initStore("store", "AppStore-Stowdle")
        )
    )
)

# Define server logic required to interpret guess
server <- function(input, output, session) {
    
    # Null copy button to start
    output$Copy <- renderUI({NULL})
    
    # Help button modal
    observeEvent(input$help, {
      showModal(modalDialog(
        title = "How to Play",
        HTML("<ul>
  <li>Guess the correct Stow, MA street name in 6 tries.</li>
  <li>Type your own guess or select from the dropdown.</li>
  <li>After each guess the following will appear:
  <ul>
  <li><strong>Distance:</strong> The distance in miles from the <strong>midpoint</strong> of the guessed  street to the <strong>midpoint</strong> of the correct street.</li>
  <li><strong>Direction:</strong> An arrow specifying the direction of the correct street relative to the guessed street.</li>
  <li><strong>Percent:</strong> How close the guessed street is to the correct street. <FONT COLOR=\"RED\">Red</FONT> indicates a close guess.</li>
  </ul>
  <li>After you have completed the Stowdle, press the copy button to share your score with friends! </li>
</ul>"),
        easyClose = TRUE
      ))
    })
    
    
    # Reactive Value for Street Name selection
    StreetName_reactive <- reactive({
        input$StreetName
    })
    
    # First, Define the Correct Answer for the Day
    set.seed(as.numeric(as.Date(substr(Sys.time(), 1, 10)), origin = "1970-01-01") - 25)
    answer <- sample(data$Name, 1)
    long <- data[data$Name == answer, "Longitude"]
    lat <- data[data$Name == answer, "Latitude"]
    
    
    # Add reactive values for guesses and finished
    finished <- reactiveVal(FALSE)
    all_guesses <- reactiveVal(list())
    copy_output <- reactiveVal(list())
    
    # Now check to see if there is a reload from the same day
    if (!(is.null(isolate(input$store)$StowdleLastDate))){
      # Get the list from local browser storage
      user_lastdate <- isolate(input$store)$StowdleLastDate

      if (user_lastdate[1] ==
          as.numeric(as.Date(substr(Sys.time(), 1, 10)), origin = "1970-01-01")){
        progress <- isolate(input$store)$StowdleProgress
        output_df <- do.call(rbind.data.frame, progress)
        rownames(output_df) <- seq_len(nrow(output_df))
        output_df$Percent <- percent(output_df$Percent)
        # Repopulate table of guesses
        ## Function for percent
        customRange = c(0, 1.001) # custom min / max values
        colors      = csscolor(gradient(as.numeric(c(customRange,
                                                     output_df$Percent)), "#FFFEE9", "#980101"))
        colors      = colors[-(1:2)] ## remove colors for min/max

        fmt    = formatter("span",
                           style = function(x){
                             style(display            = "block",
                                   padding            = "0 4px",
                                   `border-radius`    = "4px",
                                   `background-color` = ifelse(output_df$Percent == percent(1),"#007500",colors)
                             )})

        output$table <- renderFormattable({formattable(output_df,
                                                       list(`Percent` = fmt))})
      }
      # test_output_df <<- output_df
      # And now repopulate guesses list. Need to read from cookie and reconvert
      # to a list
      # Get the list from stored data
      guess_list <- isolate(input$store)$StowdleGuesses
      guess_list <- map(guess_list, 
                        function(x) { x$percent <- percent(x$percent); x })
      all_guesses(guess_list)


      # And repeat for text_list
        # Get the list from cookies
      text_list <- isolate(input$store)$StowdleText
      copy_output(text_list)

      # And if the user was already finished
      if (user_lastdate[2] == TRUE){
        finished(TRUE) #set finished to true
        fraction <- paste0(length(text_list),"/6")
        fraction <- ifelse(guess_list[[length(guess_list)]][1] == answer, fraction,
                           "X/6")

        textoutput <- paste(paste("Stowdle",
                                  as.numeric(Sys.Date()) - 19143,
                                  fraction, "<br/>"),
                            paste(lapply(text_list, paste,
                                         collapse = ""),
                                  collapse = "<br/>"), sep = "")
        textoutput_test <- "test"

        textoutput_tocopy <- paste(paste("Stowdle",
                                         as.numeric(Sys.Date()) - 19143,
                                         fraction, "\n"),
                                   paste(lapply(text_list, paste,
                                                collapse = ""),
                                         collapse = "\n"),
                                   "\nhttps://yangjasp.shinyapps.io/StowdleApp/",
                                   sep = "")

        output$text<-renderText({
          textoutput
        })
        if(!(guess_list[[length(guess_list)]][1]  == answer)){
          output$answer<-renderText({
           paste("Answer:", answer , collapse = "<br/>")
         })

        }

        output$Copy <- renderUI({
          rclipButton(
            inputId = "clipbtn",
            label = "Copy to Clipboard",
            clipText = textoutput_tocopy,
            icon = icon("clipboard")
          )
        })

      }
      }

    
    # Add choices for street name
    output$StreetName <- renderUI({
        selectInput(inputId = "StreetName",
                    label = "Street Name",
                    choices = data$Name)
    })
    
    # Function to evaluate guess, generate info
    eval_guess <- function(guess, correct_answer){
        if(guess == correct_answer){
            win <- TRUE
            distance <- "0 miles"
            direction <- "Here"
            percent <- percent(1)
    } else{
        win <- FALSE
        long <- data[data$Name == answer, "Longitude"]
        lat <- data[data$Name == answer, "Latitude"]
        guessed_lat <- data[data$Name == guess, "Latitude"]
        guessed_long <- data[data$Name == guess, "Longitude"]
        
        # Conversion according to USGS
        # One Degree Latitude ~ 69 miles
        # One degree Longitude (at 42 degrees north) ~ 51 miles
        distanceNS <- (lat - guessed_lat)*69
        distanceEW <- (long - guessed_long)*51
        distance <- sqrt(distanceNS^2 + distanceEW^2)
        angle <- dplyr::case_when(distanceNS >= 0 & distanceEW >= 0 ~
                               atan(distanceNS/distanceEW)*(180/pi),
                           distanceNS >= 0 & distanceEW < 0 ~
                               90 + atan(-1*distanceEW/distanceNS)*(180/pi),
                           distanceNS < 0 & distanceEW < 0 ~
                               180 + atan(distanceNS/distanceEW)*(180/pi),
                           distanceNS < 0 & distanceEW >= 0 ~
                               360 + atan(distanceNS/distanceEW)*(180/pi))
        
        percent <- (5.5 - distance)*1/5.5
        percent <- ifelse(percent < 0, 0, percent)
        #percent <- paste0(sprintf("%.0f", percent), "%")
        percent <- percent(percent)
        distance <- paste(sprintf("%.2f", distance), "miles")
        direction <- dplyr::case_when(angle <= 15 | angle > 345 ~ "east",
                               angle > 15 & angle <= 75 ~ "northeast",
                               angle > 75 & angle <= 105 ~ "north",
                               angle > 105 & angle <= 165 ~ "northwest",
                               angle > 165 & angle <= 195 ~ "west",
                               angle > 195 & angle <= 255 ~ "southwest",
                               angle > 255 & angle <= 285 ~ "south",
                               angle > 285 & angle <= 345  ~ "southeast")
    
        
    }
    return(list(
        guess = guess,
        distance = distance,
        direction = direction,
        percent = percent,
        win = win
    ))}
    
    # Function to update copied output
    update_copy <- function(x){
        output <- case_when(x == percent(1) ~ 
                                rep("🟩", times = 5),
                            x > percent(.9) ~
                                c(rep("🟩", times = 4), "🟨"),
                            x > percent(.8) ~
                                c(rep("🟩", times = 4), "⬜"),
                            x > percent(.7) ~
                                c(rep("🟩", times = 3), "🟨",
                                  "⬜"),
                            x > percent(.6) ~
                                c(rep("🟩", times = 3), "⬜",
                                  "⬜"),
                            x > percent(.5) ~
                                c(rep("🟩", times = 2), "🟨",
                                  rep("⬜", times = 2)),
                            x > percent(.4) ~
                                c(rep("🟩", times = 2), 
                                  rep("⬜", times = 3)),
                            x > percent(.3) ~
                                c(rep("🟩", times = 1), "🟨",
                                  rep("⬜", times = 3)),
                            x > percent(.2) ~
                                c(rep("🟩", times = 1),
                                  rep("⬜", times = 4)),
                            x > percent(.1) ~
                                c("🟨",
                                  rep("⬜", times = 4)),
                            x > percent(0) ~
                                  rep("⬜", times = 5))
        return(output)
                            
    }
    
    # When "Guess" is pressed, calculate the values
   
    observeEvent(input$Guess, {
        if(length(all_guesses()) < 6 & finished() == FALSE){
        finished(ifelse(StreetName_reactive() == answer |
                            length(all_guesses()) == 5, TRUE, FALSE))
        all_guesses_new <- all_guesses()
        guess_evaluation <- eval_guess(guess = StreetName_reactive(), answer)
        all_guesses_new[[length(all_guesses_new) + 1]] <- guess_evaluation
        all_guesses(all_guesses_new)
        
        # Update copy_output
        copy_output_new <- copy_output()
        copy_evaluation <- update_copy(guess_evaluation$percent)
        copy_output_new[[length(copy_output_new) + 1]] <- copy_evaluation
        copy_output(copy_output_new)
        
        output_df <- dplyr::bind_rows(all_guesses()) %>%
            dplyr::mutate(direction = case_when(
                direction == "east" ~ "&#x27A1;",
                direction == "northeast" ~ "&#x2197;",
                direction == "north" ~  "&#x2B06;",
                direction == "northwest" ~ "&#x2196;",
                direction == "west" ~  "&#x2B05;",
                direction == "southwest" ~  "&#x2199;",
                direction == "south" ~ "&#x2B07;",
                direction == "southeast" ~  "&#x2198;",
                direction == "Here" ~ "&#x1F7E9;"
            ))
        names(output_df) <- c("Guess", "Distance", "Direction", "Percent",
                              "Win")
        output_df <- output_df[,1:4]
        
        ## Function for percent
        customRange = c(0, 1.001) # custom min / max values
        colors      = csscolor(gradient(as.numeric(c(customRange,
                                                     output_df$Percent)), "#FFFEE9", "#980101"))
        colors      = colors[-(1:2)] ## remove colors for min/max
        
        fmt    = formatter("span", 
                           style = function(x){
                               style(display            = "block",
                                     padding            = "0 4px",
                                     `border-radius`    = "4px",
                                     `background-color` = ifelse(output_df$Percent == percent(1),"#007500",colors)
                               )})
        
        output$table <- renderFormattable({formattable(output_df,
                                                       list(`Percent` = fmt))})
        
        if (finished() == TRUE){
          
          fraction <- paste0(length(copy_output()),"/6")
          fraction <- ifelse(StreetName_reactive() == answer, fraction,
                             "X/6")
          
          textoutput <- paste(paste("Stowdle", 
                                    as.numeric(Sys.Date()) - 19143, 
                                    fraction, "<br/>"), 
                              paste(lapply(copy_output(), paste,
                                           collapse = ""), 
                                    collapse = "<br/>"), sep = "")
          
          textoutput_tocopy <- paste(paste("Stowdle", 
                                           as.numeric(Sys.Date()) - 19143, 
                                           fraction, "\n"), 
                                     paste(lapply(copy_output(), paste,
                                                  collapse = ""), 
                                           collapse = "\n"), 
                                     "\nhttps://yangjasp.shinyapps.io/StowdleApp/",
                                     sep = "")
          
          output$text<-renderText({
            textoutput
          })
          if(!(StreetName_reactive() == answer)){
            output$answer<-renderText({
              paste("Answer:", answer , collapse = "<br/>")
            })
            
          }
          
          output$Copy <- renderUI({
            output$Copy <- renderUI({
              rclipButton(
                inputId = "clipbtn",
                label = "Copy to Clipboard",
                clipText = textoutput_tocopy, 
                icon = icon("clipboard")
              )
              
            })
          })
        }
        
        }
        
      
      # Now add code to store guesses in a df and reload upon re-opening app
      # Add a row to output_df specifying the day of last save
      progress <- output_df
      user_progress_json <- toJSON(progress)
      guess_list <- all_guesses()
      guess_list_json <- toJSON(guess_list)
      text_list <- copy_output()
      text_list_json <- toJSON(text_list)

      user_lastdate <- c(as.numeric(as.Date(substr(Sys.time(), 1, 10)),
                                  origin = "1970-01-01"), finished())
      user_lastdate_json <- toJSON(user_lastdate)

      # Store the json files 
      updateStore(session, "StowdleProgress", user_progress_json)
      updateStore(session, "StowdleGuesses", guess_list_json)
      updateStore(session, "StowdleText", text_list_json)
      updateStore(session, "StowdleLastDate", user_lastdate_json)
    })
            
    observeEvent(input$clipbtn, {
      # Code to execute when button is pressed
        shinyjs::runjs(
          "document.getElementById('clipbtn').style.backgroundColor = 'gray';
           document.getElementById('clipbtn').innerHTML = 'Copied!';"
         )
          shinyjs::html("copy_message", "Text copied to clipboard!")
          shinyjs::show("copy_message")
          Sys.sleep(1)
          shinyjs::hide("copy_message")
          shinyjs::runjs(
            "document.getElementById('clipbtn').style.backgroundColor = 'black';
            document.getElementById('clipbtn').innerHTML = 'Copy to Clipboard';"
          )
        })

}


# Run the application 
shinyApp(ui = ui, server = server)
