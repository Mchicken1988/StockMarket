library(shiny)
library(plotly)
library(RSelenium)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    h4("Choose stock"),
    fileInput("files", "Upload", multiple = FALSE, accept = c(".csv"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Input data",
                           DT::dataTableOutput("input.data")),
                  tabPanel("Chart",
                           plotlyOutput("chart"))
      )
    )
  )
)




server <- function(input, output, session) {
  
  observeEvent(input$files, {
    stock.input.df <- read.table(input$files$datapath, sep = ";", header = TRUE)
    stock.input.df <- stock.input.df[order(stock.input.df$Datum),]
    stock.input.df$Datum <- as.Date(stock.input.df$Datum, format = "%Y-%m-%d")
    stock.input.df[,-1] <- apply(stock.input.df[,-1], 2, function(x) as.numeric(gsub(",", ".", gsub("\\.", "", x))))
    
    #Add the SMA
    for (days in c(30, 100, 200)){
      stock.input.df[,paste0("SMA",days)] <- sapply(1:nrow(stock.input.df), function(i) {
      SMA <- tryCatch(mean(stock.input.df[(i-days):(i-1),]$Schlusskurs), error=function(e) {NA})
      return(SMA)
      })
      stock.input.df[,paste0("SMA",days)][1] <- NA
    }
    
    output$input.data <- DT::renderDataTable(DT::datatable(stock.input.df, options = list(pageLength = 50), rownames = FALSE))
      
    output$chart <- renderPlotly({
      plot_ly(data = stock.input.df, x = ~Datum, y = ~Schlusskurs, name = "Powercell", type = "scatter", mode = "line") %>%
        add_lines(y = ~ SMA30, name = "SMA30") %>%
        add_lines(y = ~ SMA100, name = "SMA100")%>%
        add_trace(y = ~ SMA200, name = "SMA200")
      
    })
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)



# https://community.rstudio.com/t/setting-up-rselenium/11622/6
# rs <- rsDriver(port = 4443L,
#   browser = "firefox",
#   extraCapabilities = list(
#     `mox:firefoxOptions` = list(
#       binary = "/Applications/Firefox.app/Contents/MacOS/firefox"
#     )
#   )
# )

# rsc <- rs$client
# rsc$navigate("https://community.rstudio.com/")

