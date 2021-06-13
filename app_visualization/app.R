# Interactive Visualization
# Amanda Oliveira

midterm <- read.csv("midterm-results.csv")
quiz <- read.csv("quiz-categories.csv")

#install.packages("shiny")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("markdown")
#install.packages("RColorBrewer")
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("DT")
#install.packages("rsconnect")

library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(DT)
library(rsconnect)


# I. User Interface ____________________________________________________

# Page 1 - Introduction -------------------------------------------------
intro_panel <- tabPanel(
    "Introduction",
    titlePanel("Interactive Visualization"),
    fluidRow(column(12,
                    includeHTML("Intro.html")
                    #includeMarkdown("Intro.Rmd")
                    )
             )
    )

## Page 2 - Visualize Datasets ------------------------------------------

side_content <- sidebarPanel(
    selectInput("dataset", "Dataset",
                list("midterm", "quiz"),
                selected = "midterm")
    )

data_content <- mainPanel( 
    tableOutput("table")
    #DT::dataTableOutput("table")
    )         

data_panel <- vis_panel <- tabPanel(
    "Data",
    titlePanel("Visualize Datasets"),
    p(""),
    sidebarLayout(
        side_content, data_content
        )
    )

## Page 3 - Visualization ----------------------------------------------

sidebar_content <- sidebarPanel(
    selectInput("q", "Midterm Question:", 
                # choices=colnames(midterm[,3:32]) #gives varnames as options
                choices = paste0("Q", 1:30) # more elegant! 
                ),
    radioButtons("type", "",
                     choices = list("Mean Correct" = "_c", "Time to Answer" = "_time"), 
                     selected = "_c"),
    helpText("Select Test Item and Variable to Plot")
    
    )

main_content <- mainPanel( # Plots
    fluidRow(
    plotOutput("QuestionPlot", height = 450, width = 500), 
    wordcloud2Output("WordCloud", height=450, width=500)
    )
    )

vis_panel <- tabPanel(
    "Visualization",
    titlePanel("By Attendance Type"),
    p(""),
    sidebarLayout(
        sidebar_content,  main_content
        )
    )


## User Interface ------------------------------------------------------
ui <- navbarPage(
    "Vis - Final Project",
    intro_panel,
    data_panel,
    vis_panel
    )

# II - Server __________________________________________________________

server <- function(input, output) {
    
    dat <- reactive({
        x = paste0(input$q, input$type)
        data <- midterm %>% group_by(audit) %>% summarise_all(mean)
        data <- data[, c("audit", x)]
        colnames(data)[2] <- "sum"
        data <- as.data.frame(data)
    })
    
    dat2 <- reactive({
        x = paste0(input$q, "_c")
        d <- quiz  
        d <- as.data.frame(t(d))
        d <- d %>% add_rownames(var="skill")
        colnames(d) <- d[1,]
        d <- d[, c("Question", x)]
        colnames(d)[2] <- "x"
        d <- d %>% filter(x==1) %>% select(Question)
        d$freq =1
        d <- as.data.frame(d)
    })
    
    dat3=reactive({
        get(input$dataset)
    }) 
    
    # Barplot
    output$QuestionPlot <- renderPlot({
        if (input$type == "_c")  {   
        ggplot(dat(), aes(x=as.factor(audit), y=sum*100, fill=as.factor(audit))) +
            geom_bar(stat="identity", alpha=.6, width=.4) +
            labs(title="Correct Answers by Attendance Type", y="mean correct (%)", x="") +
            scale_fill_discrete(name = "Attendance", labels = c("Regular", "Audit")) +
            scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "") +
            coord_flip() +
            theme_bw()
        }
        else if (input$type == "_time")  {
            ggplot(dat(), aes(x=as.factor(audit), y=sum, fill=as.factor(audit))) +
                geom_bar(stat="identity", alpha=.6, width=.4) +
                labs(title="Average Time to Answer (sec)", y="mean time in seconds", x="") +
                scale_fill_discrete(name = "Attendance", labels = c("Regular", "Audit")) +
                scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "") +
                theme_bw()
        }
    })
    
    # Word Cloud
    output$WordCloud <- renderWordcloud2({ 
        wordcloud2(dat2(), size=.2)
    })
    
    # Dataset
    output$table<-renderTable(dat3())
    
}

# III. APP _____________________________________________________________

shinyApp(ui = ui, server = server)

## Deploy application

#rsconnect::setAccountInfo(name='amanda-ago', token='AB496E3C1B2923C7F1F9BCFC9851E8BC', secret='BPVqB6Zpx94SdxBAtE1hMLKU8JABX8dlEE7PvpSJ')
#deployApp()


