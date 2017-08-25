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

jscode <- "shinyjs.init = function() {

var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
backgroundColor: 'rgba(255, 255, 255, 0)',
penColor: 'rgb(0, 0, 0)',
maxWidth: 20,
minWidth: 10
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
    
    ### Import model and digit ###
    
    model <- load_model_hdf5("model.hdf5")
    
    input_digit <- reactive({
        
        req(input$image_input)
        
        new_file_number <- list.files("images") %>% 
            map2_dbl(str_locate_all(., "_.*\\."),
                     .,
                     ~ as.numeric(substr(.y, .x[,"start"] + 1, .x[,"end"] - 1))) %>% 
            max + 1
        
        new_file_path <- paste0("images/img_", new_file_number, ".png")
        
        
        enc <- input$image_input %>% 
            gsub(pattern = "data:image/png;base64,", replacement = "", x = .)
        outconn <- file(new_file_path,"wb")
        base64decode(what=enc, output=outconn)
        close(outconn)

        load.image(file = new_file_path) %>%
            resize(size_x = 28, size_y = 28, interpolation_type = 2) %>%
            "["(,,,4) %>%
            t()
    })
            
    
    ### Return Result and transformation ###
    
    output$prediction <- renderTable({
        
        req(input_digit())
        
        input_digit() %>% 
            array(dim = c(1, 784)) %>% 
            model$predict_on_batch() %>% 
            interpret_results()
    }, digits = 4)
    
    
    output$resized_digit <- renderPlot({
        req(input_digit())
        
        input_digit() %>% 
            "["(28:1,) %>% 
            melt() %>% 
            ggplot(aes(x = X2, y = X1, fill = value)) + 
            geom_tile() +
            scale_fill_gradient(low = "white", high = "black") +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  legend.position = "none")
    }, width = 280, height = 280)

}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            includeCSS("CSS_file.css"),
            tags$head(tags$script(src = "signature_pad.js")),
            
            shinyjs::useShinyjs(),
            shinyjs::extendShinyjs(text = jscode),
            
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
            plotOutput("resized_digit")
        )
    )
)

shinyApp(ui = ui, server = server)