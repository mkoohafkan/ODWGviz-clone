#' Shiny UI
#'
#' UI for shiny app.
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @keywords internal
ui = fluidPage(
  titlePanel("Outlier Detection Visualization"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        actionButton("quit", "Quit", value = TRUE),
        align = "right"
      ),
      fluidRow(
        fileInput("file", "Data File", FALSE, ".csv")
      ),
      fluidRow(
        actionButton("load", "Load data")
      ),
      br(),
      fluidRow(
        selectInput("algorithms", "Algorithms", c(""),
          multiple = TRUE)
      ),
      fluidRow(
        column(6, actionButton("choose", "Confirm algorithms")),
        column(6, actionButton("flag", "Detect outliers"),
          align = "right")
      ),
      br(),
      fluidRow(p(strong("Algorithm settings")), align = "center"),
      uiOutput("arguments")
    ),
    mainPanel(
      fluidRow(
        column(4,
          selectInput("x", "X-axis", c(""))
        ),
        column(4,
          selectInput("y", "Y-axis", c(""))
        ),
        column(4,
        selectInput("plotcolor", "Algorithm", choices = c("-"),
          selected = "-")
        )
      ),
      plotlyOutput("plot")
    )
  )
)
