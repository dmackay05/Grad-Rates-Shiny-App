library(shiny)
library(tidyverse)

shinyUI(fluidPage(
    titlePanel("Grad Rate Explorer"),
    theme = shinythemes::shinytheme('paper'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
            htmlOutput("lea_selector")
            ),
            fluidRow(
            htmlOutput("school_selector")
            ),
            htmlOutput('group_selector'),
            htmlOutput('type_selector'),
            htmlOutput('cohort_selector'),
            flowLayout(
            downloadButton("grad_rates", "Download Grad Rates"),
            downloadButton("grad_outcomes", "Download Grad Outcomes"),
            downloadButton("completion", "Download Completion Status")),
            width = 3
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Graduation',
                         plotly::plotlyOutput('trend', width = "90%"),
                         plotly::plotlyOutput('dropout', width = "90%"),
                         tableOutput('table')),
                tabPanel('Outcome Counts',
                         tableOutput('counts'),
                         tableOutput('status'))
                ),
                width = 9
            )
        )
    )
)
