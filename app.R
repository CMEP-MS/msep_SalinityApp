library(shiny)
library(shinyWidgets)
library(shinybusy)
library(bslib)
library(mseptools)
library(plotly)
library(leaflet)

# github repo:
# https://github.com/CMEP-MS/msep_SalinityApp

ui <- page_navbar(

    title = "Mississippi Sound Salinity Explorer",
    theme = bs_theme(version = 5),
    navbar_options = list(bg = "#18a4ad",
                          underline = TRUE),


    nav_panel("Salinity",

              # main panel
              page_sidebar(

                  # sidebar
                  sidebar = sidebar(
                      width = "40%",

                      dateRangeInput(
                          inputId = "date_range",
                          label = "Select Date Range",
                          start = Sys.Date() - 30,
                          end = Sys.Date(),
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

                  ), # end sidebar


                  # main tabs
                  navset_card_tab(
                      id = "main_tabs",
                      # title = "Data Visualization",
                      nav_panel(
                          title = "Time Series",
                          add_busy_spinner(
                              spin = "circle",
                              height = "100px",
                              width = "100px",
                              margins = c(20, 40)
                          ),
                          plotlyOutput("plot")
                      ),
                      nav_panel(
                          title = "Map",
                          # date selection slider
                          sliderInput(
                              inputId = "date_slider",
                              label = "Map Daily Means",
                              min = as.Date(Sys.Date() - 30),
                              max = as.Date(Sys.Date()),
                              value = as.Date(Sys.Date() - 15),
                              step = 1,
                              animate = TRUE,
                              timeFormat = "%Y-%m-%d"
                          ),

                          # map
                          leafletOutput("salinity_map")
                      )
                  ),
                  full_screen = TRUE
              )

    ), # end salinity panel

    # about panel
    nav_panel("About",
              card("To be filled in")
    ), # end About panel

    nav_spacer(),
    nav_item(tags$a(shiny::icon("github"),
                    href = "https://github.com/CMEP-MS/msep_SalinityApp",
                    target = "_blank")
    )
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

    # filter by chosen date
    tomap <- reactive({
        req(input$date_slider, data())
        dplyr::left_join(data()$daily, data()$siteInfo) |>
            dplyr::filter(date == as.Date(input$date_slider))

    })

    # Create the base map only once, including the static legend
    output$salinity_map <- renderLeaflet({
        create_mssnd_basemap() |>
            add_salinity_legend()
    })

    # Observer to update only points when date changes
    observe({
        filtered_data <- tomap()

        # Update the markers
        leafletProxy("salinity_map") |>
            clearMarkers() |>
            add_salinity_points(data = filtered_data)
    })

    # observer to update date slider and make an initial map
    # when the get data button is clicked
    observeEvent(input$get_data, {

        # update the slider
        updateSliderInput(
            session,
            "date_slider",
            min = input$date_range[1],
            max = input$date_range[2],
            value = input$date_range[1]
        )

        # trigger a map update
        req(data())
        initial_data <- dplyr::left_join(data()$daily, data()$siteInfo) |>
            dplyr::filter(date == as.Date(input$date_range[1]))  # Use the first date

        leafletProxy("salinity_map") |>
            clearMarkers() |>
            add_salinity_points(data = initial_data)

    })

}

shinyApp(ui, server)
