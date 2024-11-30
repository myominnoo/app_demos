
box::use(
    shiny[tags], 
    shiny.router[route_link],
)


#' @export
insert_hero_card <- function(
        src = "", 
        title = "Showcase Title", 
        desc = "This is a showcase div with a title and some text.", 
        path = route_link("other")
) {
    tags$div(
        class = "showcase-column",
        style = "
        background-color: #f7f7f7; 
        padding: 20px; 
        border: 1px solid #ddd; 
        border-radius: 10px; 
        box-shadow: 0 0 10px rgba(0,0,0,0.2);",
        tags$img(
            src = src, 
            width = "100%", 
            height = "auto", 
            style = "border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.2);"
        ), 
        tags$h2(title),
        tags$p(desc), 
        tags$a(href = path, class = "btn btn-primary", "View App")
    )
}

