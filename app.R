library(shiny)
library(ggplot2)
library(shinyjs)
library(keras)
library(imager)
library(reshape)
library(base64enc)
library(stringr)
library(purrr)
library(shinydashboard)
library(dplyr)



### Import model ###
model <- load_model_hdf5("model.hdf5")

### Create Background Grid ###

background_grid <- ggplot(tibble(a = 1, b = 1), aes(x = a, y = b)) +
    coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

jscode <- "shinyjs.init = function() {

var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
backgroundColor: 'rgba(255, 255, 255, 0)',
penColor: 'rgb(0, 0, 0)',
maxWidth: 12,
minWidth: 9
});

var saveButton = document.getElementById('save');
var clearButton = document.getElementById('clear');

saveButton.addEventListener('click', function () {
Shiny.onInputChange('image_input', signaturePad.toDataURL('png'));
});

clearButton.addEventListener('click', function () {
signaturePad.clear();
});

}"

server <- function(input, output, session){
    
    ### sourcing ###
    
    source("src/interpret_results.R")
    source("src/plot_image_matrix.R")
    source("src/preprocess_image.R")
    
    ### Generate Background Grid ###
    
    output$plot1 <- renderPlot({background_grid})
    
    ### Set reactive values ###
    
    new_file <- reactiveValues(number = NULL, path = NULL, name = NULL, state = NULL)
    image <- reactiveValues(matrix = array(0, dim = c(1, 28, 28, 1)),
                            preprocessed = plot_image_matrix(array(0, dim = c(1, 28, 28, 1))))
    predictions <- reactiveValues(results = rep(0, 10), df = interpret_results(0) %>% arrange(digit))
    
    ### Compute everything following drawing ###
    observe({
        
        req(input$image_input)
        
        new_file$number <- list.files("holdout/images") %>% 
            map2_dbl(.x = str_locate_all(., "_.*\\."),
                     .y = .,
                     .f = ~ as.numeric(substr(.y, .x[,"start"] + 1, .x[,"end"] - 1))) %>% 
            max + 1
    
        new_file$path <- paste0("holdout/images/img_", new_file$number, ".png")
        
        enc <- input$image_input %>% 
            gsub(pattern = "data:image/png;base64,", replacement = "", x = .)
        outconn <- file(new_file$path,"wb")
        base64decode(what=enc, output=outconn)
        close(outconn)

        image$matrix <- image_matrix(new_file$path)
        
        predictions$result <- model$predict_on_batch(image$matrix)
        
        predictions$df <- interpret_results(predictions$result)
        
        image$preprocessed <- plot_image_matrix(image$matrix)
        
        updateRadioButtons(session, "label", selected = which.max(predictions$result) - 1)
        
        disable("save")
        disable("clear")
        enable("submit")
        
        new_file$state <- "unlabelled"
    })
    
    ### Return Result and transformation ###
    
    output$prediction <- renderTable(predictions$df, digits = 4, striped = TRUE)
    
    output$preprocessed <- renderPlot(image$preprocessed, width = 280, height = 280)
    
    observeEvent(input$submit, {
        
        old_labels <- readRDS("holdout/labels/labels.RDS")
        new_file$name <- paste0("img_", new_file$number, ".png")
        
        if(!identical(integer(0), pos <- which(new_file$name == old_labels$file_name)))
        {
            old_labels[pos, "label"] <- as.integer(input$label)
        } else {
            old_labels <- rbind(old_labels, data.frame(file_name = new_file$name, label = as.integer(input$label)))
        }
        
        saveRDS(old_labels, "holdout/labels/labels.RDS")
        
        js$init()
        
        enable("save")
        enable("clear")
        disable("submit")
        
        new_file$state <- "labelled"
    })
    
    session$onSessionEnded(function() {isolate(if(new_file$state == "unlabelled") file.remove(new_file$path))})

}

ui <- dashboardPage(
    dashboardHeader(title = "Digit Recognition App"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(width = 4,
                title = "Draw your favorite digit",
                
                includeCSS("CSS_file.css"),
                tags$head(tags$script(src = "signature_pad.js")),

                shinyjs::useShinyjs(),
                shinyjs::extendShinyjs(text = jscode),

                div(class="wrapper",
                    style="background-color:white",
                    plotOutput("plot1", width = 280, height = 280),
                    HTML("<canvas id='signature-pad' class='signature-pad' width=280 height=280></canvas>")
                ),
                actionButton("save", "Save"),
                actionButton("clear", "Clear")
            ),
            box(width = 3,
                solidHeader = TRUE,
                title = "Prediction",
                tableOutput("prediction")
            ),
            box(width = 4,
                solidHeader = TRUE,
                title = "Model Input",
                plotOutput("preprocessed")
            )
        ),
        radioButtons(inputId = "label", choices = 0:9, selected = 0, inline = TRUE, label = "What was your digit?"),
        actionButton("submit", "Submit", disabled = TRUE)
    )
)

shinyApp(ui = ui, server = server)

