library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(keras)
library(imager)
library(reshape)
library(base64enc)
library(stringr)
library(purrr)

jsinit <- "shinyjs.init = function() {

var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
backgroundColor: 'rgba(255, 255, 255, 0)',
penColor: 'rgb(0, 0, 0)',
maxWidth: 15,
minWidth: 13
});

var saveButton = document.getElementById('save');
var cancelButton = document.getElementById('clear');

saveButton.addEventListener('click', function (event) {
var gh = signaturePad.toDataURL('png');

var link  = document.createElement('a');
link.href = gh;
link.download = 'image.png';
Shiny.onInputChange('image_input', gh);
//link.click();
});

cancelButton.addEventListener('click', function (event) {
signaturePad.clear();
});

}"

server <- function(input, output, session){
    
    ### sourcing ###
    
    source("src/interpret_results.R")
    source("src/plot_image_matrix.R")
    source("src/image_matrix.R")
    
    ### Generate Background Grid ###
    
    output$plot1 <- renderPlot({
        
        df <- tibble(a = 1, b = 1)
        
        ggplot(df, aes(x = a, y = b)) +
            coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        
    })
    
    ### Determine new file name ###
    
    new_file <- list(number = list.files("holdout/images") %>% 
                         map2_dbl(.x = str_locate_all(., "_.*\\."),
                                  .y = .,
                                  .f = ~ as.numeric(substr(.y, .x[,"start"] + 1, .x[,"end"] - 1))) %>% 
                         max + 1)
    
    ### Import model ###
    model <- load_model_hdf5("model.hdf5")
    
    ### Input digit ###
    input_digit <- reactive({
        
        req(input$image_input)
        
        new_file_path <- paste0("holdout/images/img_", new_file$number, ".png")
        
        enc <- input$image_input %>% 
            gsub(pattern = "data:image/png;base64,", replacement = "", x = .)
        outconn <- file(new_file_path,"wb")
        base64decode(what=enc, output=outconn)
        close(outconn)

        image_matrix(new_file_path)
    })
    
    observeEvent(input$submit, {
        
        old_labels <- readRDS("holdout/labels/labels.RDS")
        new_file_name <- paste0("img_", new_file$number, ".png")
        
        if(!identical(integer(0), pos <- which(new_file_name == old_labels$file_name)))
        {
            old_labels[pos, "label"] <- input$label
        } else {
            old_labels <- rbind(old_labels, data.frame(file_name = new_file_name, label = input$label))
        }
        
        saveRDS(old_labels, "holdout/labels/labels.RDS")
        

    })
    
    ### Return Result and transformation ###
    
    output$prediction <- renderTable({
        
        req(input_digit())
        
        input_digit() %>% 
            model$predict_on_batch() %>% 
            interpret_results()
    }, digits = 4)
    
    
    output$resized_digit <- renderPlot({
        req(input_digit())
        
        input_digit() %>% 
            plot_image_matrix()
    }, width = 280, height = 280)

}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            includeCSS("CSS_file.css"),
            tags$head(tags$script(src = "signature_pad.js")),
            
            shinyjs::useShinyjs(),
            shinyjs::extendShinyjs(text = jsinit),

            h1("Draw your favorite digit"),
            div(class="wrapper",
                plotOutput("plot1", width = 280, height = 280),
                HTML("<canvas id='signature-pad' class='signature-pad' width=280 height=280></canvas>"),
                HTML("<div>
             <button id='save'>Save</button>
             <button id='clear'>Clear</button>
             </div>")
            )
        ),
        
        mainPanel(
            tableOutput("prediction"),
            plotOutput("resized_digit"),
            radioButtons(inputId = "label", choices = 0:9, selected = 0, inline = TRUE, label = "What was your digit?"),
            actionButton("submit", "Submit")
        )
    )
)

shinyApp(ui = ui, server = server)

