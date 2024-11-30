box::use(
  bslib, 
  DT[DTOutput, renderDataTable, datatable], 
  rio[export], 
  shiny[moduleServer, NS, tags, fileInput, selectInput, 
        actionButton, downloadButton, verbatimTextOutput, icon, reactiveValues, 
        observeEvent, downloadHandler, req, withProgress, incProgress, 
        updateSelectInput, fluidRow, column,
        renderPrint], 
  sessioninfo[package_info], 
  shiny.router[route_link], 
  utils[sessionInfo], 
)


box::use(
    app/logic/xpert_magic_process_raw[process_raw], 
)


sidebar <- function(ns) {
    bslib$sidebar(
        tags$p("This tool converts raw outputs of HPV & CTNG results from GeneXpert machine into tidy tabular data for further analysis."), 
        tags$hr(), 
        
        fileInput(ns("upload"), "Select raw data in .csv", accept = ".csv"), 
        selectInput(ns("include"), "Include these data", 
                    choices = c("Sample Type"="type", "None"="none"), selected = "type"), 
        actionButton(ns("process"), "Process", icon = icon("microchip")),
        tags$hr(), 
        tags$p("Developed by Myo Minn Oo", style="font-size:12px;float:right;"), 
        tags$p("Version 3.20230206", style = "font-size:12px;float:right;"), 
    )
}



gear <- function(ns, select_ns, btn_ns) {
    bslib$accordion(
        bslib$accordion_panel(
            "Input controls", icon = icon("gear"),
            position = "right", open = FALSE, 
            selectInput(ns(select_ns), "Select columns to download:", 
                        choices = "", multiple = TRUE, width = "100%"), 
            downloadButton(ns(btn_ns), "Download Data", style = "width: 150px;"), 
        )
    )
}

body <- function(ns) {
    bslib$navset_card_tab(
        title = "", 
        # full_screen = TRUE,
        height = "100vh",
        bslib$nav_panel(
            # style = "height: calc(100vh - 60px); overflow-y: auto;", 
            tags$div(icon("table"), " HPV"),
            gear(ns, "hpv_vars", "hpv_download"), 
            verbatimTextOutput(ns("hpv_info")), 
            DTOutput(ns("hpv_data"))
        ),
        bslib$nav_panel(
            tags$div(icon("table"), " CTNG"), 
            gear(ns, "ctng_vars", "ctng_download"), 
            
            verbatimTextOutput(ns("ctng_info")), 
            DTOutput(ns("ctng_data"))
        ),
        bslib$nav_panel(
            tags$div(icon("info-circle"), "Session Info"), 
            verbatimTextOutput(ns("about1")),
            verbatimTextOutput(ns("about2"))
        ),
    )
}


#' @export
ui <- function(id) {
  ns <- NS(id)
  bslib$page_sidebar(
      title = tags$div(
          tags$h1("Xpert Magic App"), 
          tags$a(
              style = "
                position: absolute; 
              top: 10px; right: 10px; 
              display: flex; align-items: 
              center; padding: 5px; margin: 10px;",
              class = "btn btn-primary back-to-home",
              href = route_link("/"),
              "Back to Home"
          )
      ), 
      body(ns),
      sidebar = sidebar(ns),
      # fillable = TRUE,
      width = "100%",
      height = "100%",
      theme = bslib$bs_theme(bootswatch = "cosmo"),
      window_title = "{MHAS}'s Xpert Magic App",
      lang = "en"
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      
      env <- reactiveValues(hpv = NULL, ctng = NULL)
      
      output$about1 <- renderPrint({
          sessionInfo()
      })
      
      output$about2 <- renderPrint({
          package_info()
      })
      
      observeEvent(input$process, {
          req(input$upload)
          n <- 5
          
          withProgress(
              message = 'Calculation in progress',
              detail = 'This may take a while...', value = 0, {
                  df <- readLines(input$upload$datapath, skipNul = TRUE)
                  incProgress(1/n)
                  df <- suppressWarnings(process_raw(df, input$include, n))
                  incProgress(1/n)
                  hpv_vars <- names(df[["hpv"]])
                  updateSelectInput(session = session, "hpv_vars", choices = hpv_vars,
                                    selected = hpv_vars)
                  ctng_vars <- names(df[["ctng"]])
                  updateSelectInput(session = session, "ctng_vars", choices = ctng_vars,
                                    selected = ctng_vars)
                  incProgress(1/n)
              }
          )
          
          env$hpv <- df[["hpv"]]
          env$ctng <- df[["ctng"]]
      })
      
      
      # Quantification Table ----------------------------------------------------
      output$hpv_data <- renderDataTable({
          datatable(env$hpv,
                    rownames = FALSE,
                    editable = TRUE,
                    filter = list(position = "top", clear = FALSE),
                    options = list(scrollX = TRUE))
      })
      output$ctng_data <- renderDataTable({
          datatable(env$ctng,
                    rownames = FALSE,
                    editable = TRUE,
                    filter = list(position = "top", clear = FALSE),
                    options = list(scrollX = TRUE))
      })
      
      # download ----------------------------------------------------------------
      output$hpv_download <- downloadHandler(
          filename = function() paste0(
              "hpv_genexpert_processed_at_", format(Sys.time(), "%d-%b-%Y_%H.%M.%S"), ".xlsx"
          ),
          content = function(file) rio::export(env$hpv[, input$hpv_vars], file)
      )
      output$ctng_download <- downloadHandler(
          filename = function() paste0(
              "ctng_genexpert_processed_at_", format(Sys.time(), "%d-%b-%Y_%H.%M.%S"), ".xlsx"
          ),
          content = function(file) rio::export(env$ctng[, input$ctng_vars], file)
      )
      
      
      
  })
}

