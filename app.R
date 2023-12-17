library(shiny)
library(bslib)
library(epoxy)
library(bsicons)

# Shiny epoxy template functions don't support inline transformations,
# so we still have to do some prep work ourselves.
bechdel <- epoxy::bechdel

as_dollars <-
    scales::label_dollar(scale_cut = scales::cut_short_scale())
bechdel$budget <- as_dollars(bechdel$budget)
bechdel$domgross <- as_dollars(bechdel$domgross)

vowels <- c("a", "e", "i", "o", "u")
bechdel$genre  <-
    paste(ifelse(substr(tolower(bechdel$genre), 1, 1) %in% vowels, "an", "a"),
          tolower(bechdel$genre))

movie_ids <- rlang::set_names(bechdel$imdb_id,
                              bechdel$title)

ui <- bslib::page_navbar(
    title = "Epoxy Demo with bslib and Movie data",
    theme = bs_theme(
        bootswatch = "superhero",
        base_font = font_google("Poppins"),
        primary = "#4E0A76",
        bg = "black",
        fg = "white"
    ),
    tabPanel("Demo",
             fluidRow(
                 column(width = 3,
                        card(
                            selectInput("movie", "Movie", movie_ids),
                            uiOutput("poster")
                        )),
                 column(width = 9,
                        card(
                            ui_epoxy_html(
                                .id = "about_movie",
                                .item_class = "movie-info",
                                h2("{{title}}"),
                                tags$p(
                                    strong("Released:"),
                                    "{{ year }} \\",
                                    strong("Rated:"),
                                    "{{ rated }} \\",
                                    strong("IMDB Rating:"),
                                    "{{ imdb_rating }}"
                                ),
                                tags$p(
                                    em("{{ title }}"),
                                    " is {{ genre }} film released in {{ year }}. It was filmed",
                                    bs_icon("camera-reels"),
                                    " in {{ country }} with a budget of {{ budget }}",
                                    bs_icon("cash-coin"),
                                    "and made {{ domgross }} at the box office",
                                    bs_icon("ticket-perforated"),
                                    ". ",
                                    em("{{ title }}"),
                                    " received a Bechdel rating",
                                    bs_icon("card-checklist"),
                                    "of ",
                                    strong("{{ bechdel_rating }}"),
                                    " for the following plot:"
                                ),
                                tags$blockquote("{{ plot }}")
                            )
                        ))
             ))
)



server <- function(input, output, session) {
    movie <- reactive({
        bechdel[bechdel$imdb_id == input$movie, ]
    })
    
    output$about_movie <- render_epoxy(.list = movie())
    output$poster <- renderUI(
        img(
            src = movie()$poster,
            alt = paste0("Poster for ", movie()$title),
            style = "max-height: 400px; max-width: 100%; margin: 0 auto; display: block;"
        )
    )
}



shinyApp(ui, server)