box::use(
  shiny[bootstrapPage, moduleServer, NS],
  shiny.router[router_ui, router_server, route], 
)

box::use(
    app/view/page_main[insert_page_main], 
    xm = app/view/page_xpert_magic,
)


options(shiny.maxRequestSize = 30*1024^2)


#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    router_ui(
        route("/", insert_page_main()), 
        route("xpert_magic", xm$ui(ns("xpert_magic")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      router_server()
      xm$server("xpert_magic")
  })
}
