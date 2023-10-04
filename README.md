#name: Shanay Wadhwani
#R Version: 3.6.3 Rstudio Version: 1.3.959
#librarys used are shiny, datasets, DT, shinyalert, ggplot2










library(shiny)
library(datasets)
library(DT)
library(shinyalert)
library(ggplot2)
ui <- shinyUI(fluidPage(
    titlePanel("EDA Data Viz"),
    tabsetPanel(
        tabPanel("Data Upload, View, and Summary",
                 titlePanel("Uploading Files"),
                 actionButton("accept data upload", "ACCEPT DATA UPLOAD", style="simple", size="sm", color = "warning"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv',
                                            'text/comma-separated-values,text/plain',
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ',')
                         
                     ),
                     mainPanel(
                         DTOutput('contents'),
                         HTML(
                             paste(
                                 h1('\n'),'<br/>'
                             )
                         ),
                         verbatimTextOutput('structure'),
                         verbatimTextOutput('summary'),
                         verbatimTextOutput('class')
                     )
                 )
        ),
        useShinyalert(force = TRUE),
        
        tabPanel("Visualization",
                 pageWithSidebar(
                     headerPanel(''),
                     sidebarPanel(
                         
                         radioButtons('distribution', 'Distribution',
                                      c(
                                          'Univariate Distribution'='univariate',
                                          'Bivariate Distribution'="bivariate"),
                                      '"'),
                         
                         conditionalPanel(
                             condition = "input.distribution == 'bivariate'",
                             selectInput('bivariate', 'Y Variable', "", selected = "")
                         ),
                         selectInput('univariate', 'X Variable', "", selected = "")
                         
                        
                     ),
                     mainPanel(
                         plotOutput("densityplot"),
                         conditionalPanel(
                             condition = "input.distribution == 'univariate'",
                             plotOutput("barplot")
                         ),
                         conditionalPanel(
                             condition = "input.distribution == 'bivariate'",
                             plotOutput("MyPlot")
                         )
                         
                         
                     )
                 )
        )
        
    )
)
)

server <- shinyServer(function(input, output, session) {
    #
    data <- reactive({
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        
        updateSelectInput(session, inputId = 'univariate', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'bivariate', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderDT({
        data()
    })
    
    output$summary <- renderPrint({
        summary(data())
    })
    
    output$structure <- renderPrint({
        str(data())
    })
    
    output$class <- renderPrint({
        sapply(data(), class)
    })
    
    output$MyPlot <- renderPlot({
        
        req(data(), input$univariate, input$bivariate)
        ggplot(data = data(), mapping = aes_string(input$univariate, input$bivariate)) +
            geom_point() +
            theme_light()+
            ggtitle("Bivariate Plot")
        
    })#FF69B4
    
    output$densityplot <- renderPlot({
        req(data(), input$univariate)
        ggplot(data = data(), mapping = aes_string(input$univariate)) + geom_density(fill="#FF69B4",
                                                                                     color="#F0F8FF",
                                                                                     alpha=0.8)+
            theme_light()+
            ggtitle("Univariate Plot")
    })
    
    output$barplot <- renderPlot({
        req(data(), input$univariate)
        ggplot(data = data(), mapping = aes_string(input$univariate)) + geom_bar(fill="navy",
                                                                                 color="red",
                                                                                 alpha=0.8)+
            theme_light()+
            ggtitle("Univariate Plot")
    })
    
    l <- reactiveValues()
    observeEvent(input$`accept data upload`, {
        
        showModal(modalDialog(
            tags$h2('Please accept the data upload'),
            
            footer=tagList(
                modalButton('submit')
            )
        ))
    })
})

shinyApp(ui, server)









