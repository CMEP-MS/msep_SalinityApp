library(shiny)
library(shinyWidgets)
library(bslib)
library(mseptools)
library(plotly)
library(leaflet)

ui <- page_sidebar(
    title = "Mississippi Sound Salinity Explorer",
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
            class = "btn-primary",
            width = "100%"
        ),

        # Horizontal rule for separation
        hr(),

        leafletOutput("map"),

        # Download button
        downloadButton(
            "download_data",
            "Download Data",
            class = "btn-success",
            width = "100%"
        )
    ),

    # Main panel with map and plot in stacked layout
    card(plotlyOutput("plot"),
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
        st <- sort(unique(data()$siteInfo$clean_nm))
        st <- factor(st, levels = st)
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
        plot_mssnd_salinity(data = data())
    })

}

shinyApp(ui, server)
