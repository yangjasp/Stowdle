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
library(shinyCopy2clipboard)


# Data
#data <- read.csv("StowdleApp/data/StowRoads.csv")
#data <- read.csv("/Users/jasper/Desktop/Stowdle/Stowdle/StowdleApp/data/StowRoads.csv")
#x <- getURL("https://github.com/yangjasp/Stowdle/blob/main/StowdleApp/data/StowRoads.csv")
#data <- read.csv(text = x)
data <- read.csv("data/StowRoads.csv")
data <- dplyr::arrange(data, Name)

# Define UI 
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Stowdle"),


    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 9,
            uiOutput("StreetName")
        ),

        # Show a table of Guesses
        mainPanel(width = 8,
           #DT::dataTableOutput("previousguesses"),
           formattable::formattableOutput("table"),
           actionButton("Guess", "Guess"),
           #actionButton("Copy", "Copy to Clipboard"),
           #use_copy(),
           #CopyButton("copybtn",
                      #label = "Copy to Clipboard",
                      #icon = icon("clipboard"),
                      #text = "No text found"),
           uiOutput("Copy"),
           htmlOutput("answer"),
           htmlOutput("text"),
           rclipboardSetup(),
           #textInput("text1","yes")
        )
    )
)

# Define server logic required to interpret guess
server <- function(input, output, session) {
    
    # Null copy button to start
    output$Copy <- renderUI({NULL})
    
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
        
        
        # function to turn list into df
       # make_df <- function(list){
            
        #}
        
        #output$previousguesses <- DT::renderDataTable({
        #    output_df <- dplyr::bind_rows(all_guesses())
        #    names(output_df) <- c("Guess", "Distance", "Direction","Win")
        #    output_df <- output_df[,1:3]
        #    output_df
        #})
        
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
        }
        
        if (finished() == TRUE){
        
            # Workaround for execution within RStudio version < 1.2
            #if (interactive()){
            #    observeEvent(input$clipbtn, clipr::write_clip(input$text))
            #}
            
            fraction <- paste0(length(copy_output()),"/6")
            fraction <- ifelse(StreetName_reactive() == answer, fraction,
                               "X/6")
            
            textoutput <- paste(paste("Stowdle", 
                                      as.numeric(Sys.Date()) - 19143, 
                                      fraction, "<br/>"), 
                                paste(lapply(copy_output(), paste,
                                             collapse = ""), 
                                      collapse = "<br/>"), sep = "")
            #textoutput_test <- "test"
            
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
            
          
            
            # Rclipbutton attempt
            
        
            
            #if (interactive()){
            #    observeEvent(input$clipbtn, clipr::write_clip(textoutput))
            #}  
            
            # Other clip button attempt
            
            #observe({
              
            #  req(input$copybtn)
            #  CopyButtonUpdate(session,
            #                   id = "copybtn",
            #                   label = "Copy to Clipboard",
            #                   icon = icon("clipboard"),
            #                   text = textoutput
            #  )
            #})
            
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
        
       # if (finished() == FALSE){
            
       # }
      
      
      
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
