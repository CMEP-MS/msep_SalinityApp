library(shiny)
library(shinyWidgets)
library(shinybusy)
library(bslib)
library(mseptools)
library(plotly)
library(leaflet)

# github repo:
# https://github.com/CMEP-MS/msep_SalinityApp

ui <- page_sidebar(

    title = div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;
        background-color: #18a4ad; color: white; border-color: #0d80a4;
        padding: 18px 18px; margin: -16px -16px -16px -16px;",
        span("Mississippi Sound Salinity Explorer"),
        a(
            href = "https://github.com/CMEP-MS/msep_SalinityApp",
            target = "_blank",
            icon("github", "fa-xl"),
            style = "color: inherit; text-decoration: none;"
        )
    ),

    sidebar = sidebar(
        width = "40%",

        # Date selection inputs
        airDatepickerInput(
            inputId = "date_range",
            label = "Select Date Range",
            value = c(Sys.Date() - 30, Sys.Date()),
            range = TRUE,
            width = "100%"
        ),

        # Action button to fetch data
        actionButton(
            "get_data",
            "Get Data",
            style = "background-color: #18a4ad; color: white; border-color: #0d80a4; width: 75%;",
            class = "btn"
        ),

        # Horizontal rule for separation
        hr(),

        leafletOutput("map")

    ),

    # Main panel with map and plot in stacked layout
    card(add_busy_spinner(spin = "circle",
                          height = "100px",
                          width = "100px",
                          margins = c(20, 40)),
         plotlyOutput("plot"),
         full_screen = TRUE)


)

server <- function(input, output, session) {

    data <- eventReactive(input$get_data, {
        dat_in <- get_mssnd_data(startDate = input$date_range[1],
                                 endDate = input$date_range[2])
        dat_in
    },
    ignoreInit = TRUE)


    stns <- reactive({
        req(data())
        df <- data()$siteInfo |>
            dplyr::arrange(dec_lon_va) |>
            dplyr::mutate(clean_nm = forcats::fct_inorder(clean_nm))
        df$clean_nm
    })

    # color palette -
    # named vector for plotly
    # function of this will be made inside render leaflet for mapping
    colors_static <- reactive({
        req(stns())
        cols <- as.character(khroma::color("roma")(length(stns())))
        names(cols) <- stns()

        cols
    })

    output$map <- renderLeaflet({
        req(data(), colors_static())
        color_fun <- leaflet::colorFactor(palette = unname(colors_static()),
                                          domain = stns())

        map_mssnd_usgs(stations = data()$siteInfo,
                       color_function = color_fun)
    })

    output$plot <- renderPlotly({
        validate(
            need(input$get_data,
                 "Select a date range and push 'Get Data' in the left sidebar to see graphs here.")
        )

        plot_mssnd_salinity(data = data(),
                            colors = colors_static())
    })

}

shinyApp(ui, server)
