
box::use(
    shiny[tagList, tags, fluidRow, column],
    shiny.router[route_link],
)

box::use(
    app/view/hero_card[insert_hero_card],
)

#' @export
insert_page_main <- function(ns) {
    tagList(
        # Header Section
        tags$div(
            style = "background-color: #3498db; padding: 20px; text-align: center; color: #fff;",
            tags$h1("Maple Horizons Analytics and Solutions {MHAS}", style = "text-align: center;"),
            tags$p("Unlock Data-Driven Insights with your trusted partner!")
        ),
        
        # Showcase Section
        tags$div(
            style = "padding: 20px;",
            tags$h2("Explore Our Showcases", style = "color: #3498db;"),
            tags$p("Our showcase highlights a selection of our most innovative and effective solutions, designed to address the unique challenges and opportunities of our clients."),
            tags$ul(
                tags$li("Gain deeper insights into your customers, operations, and market trends"),
                tags$li("Identify areas of opportunity and optimize business processes"),
                tags$li("Develop data-driven strategies to drive growth and competitiveness")
            )
        ),
        
        # Hero Cards Section
        fluidRow(
            style="margin: 20px;", 
            column(4, insert_hero_card(
                src = '', 
                title = "Xpert Magic", 
                desc = "Convert raw outputs of HPV & CTNG results from GeneXpert machine into tidy tabular data for further analysis.", 
                path = route_link("xpert_magic")
            )),
            column(4, insert_hero_card()),
            column(4, insert_hero_card()),
        )
    )
}