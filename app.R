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
              card(
                  # card_header("About this app"),
                  p("This application allows users to explore salinity data in the Mississippi Sound."),
                  h4("Data sources and caveats:"),
                  tags$ol(
                      tags$li(span(strong("USGS Salinity data"), "is downloaded from the USGS web API using the `mseptools` R package, which contains a wrapper function to the USGS `dataRetrieval` package. The `mseptools` function specifically downloads and processes salinity data only. The value type downloaded is 'Instantaneous' ('uv' in USGS API parlance). Daily means are calculated from these higher-frequency readings within the `mseptools` function."),
                              tags$ol(
                                  style = "list-style-type: lower-alpha; margin-top: 8px;",
                                  tags$li(em(strong("Data Quality:")), " Data may be provisional, especially if recent - it may not have undergone QA/QC processes. If a value seems anomalously low or high, especially compared to other values on that date, there may be an equipment malfunction."),
                                  tags$li(em(strong("Data Quality:")), " Data may be provisional, especially if recent - it may not have undergone QA/QC processes. If a value seems anomalously low or high, especially compared to other values on that date, there may be an equipment malfunction."),
                                  tags$li(em(strong("Column summary:")), " some more words")
                              ))
                  ),
                  h4("Using this app:"),
                  tags$ol(
                      tags$li(
                          span(strong("Sidebar: get data"), " and see a map of the stations."),
                          tags$ol(
                              style = "list-style-type: lower-alpha; margin-top: 8px;",
                              tags$li(em(strong("Choose dates:")), " Use either the calendar interface or type the dates you wish to download."),
                              tags$li(em(strong("Get data button:")), " Nothing happens in the app until you push this button to confirm the dates you'd like to use. Then the data for the specified dates are downloaded, along with station information. Some stations do not consistently report salinity, so may appear in some date range selections and not others."),
                              tags$li(em(strong("Station map:")), " A map of the USGS stations appearing in the downloaded data appears below the date range selector. Stations are colored from west to east. Color/station combinations in this map match the color/station combinations in the time series graphs. Clicking a point on the map will show the full station name and USGS station number.")
                          )
                      ),
                      tags$li(
                          span(strong("Time Series Tab"), " "),
                          tags$ol(
                              style = "list-style-type: lower-alpha; margin-top: 8px;",
                              tags$li(em(strong("General map functionality:")), " Click on a station in the legend to make it disappear or reappear. Double-click on a station and it will be the only one that appears. Hover over part of a graph and you will see the value and station of the nearest point. Click and drag horizontally, or over the slider below both graphs, to zoom in on a date range. When you hover over the graphs, a floating bar will appear with more options as well - pan, reset boundaries/zoom levels, even download the plot. Click around!"),
                              tags$li(em(strong("All Readings:")), " these are the 'instantaneous values' in the downloaded dataset."),
                              tags$li(em(strong("Daily Mean:")), " these values are daily means of the values in the above graph. Tidal variation is averaged out by compiling to daily values.")
                          )
                      ),
                      tags$li(
                          span(strong("Map Tab"), " "),
                          tags$ol(
                              style = "list-style-type: lower-alpha; margin-top: 8px;",
                              tags$li(em(strong("Stations:")), " The points on this map represent the same stations that appear in the sidebar station map."),
                              tags$li(em(strong("Point color:")), " Points are colored by salinity value, representing the mean salinity at the station on the selected date (see 'Date selection and animation' section). Lower salinity values are yellow to light-green, and darker greens to blues represent higher salinity values."),
                              tags$li(em(strong("Date selection and animation:")), " The slider above the map can be used to select a specific date, or the 'play' button below the slider can be pressed to start an animation of the days sequentially. This animation can be paused. The date in blue above the slider bar is the date represented on the map."),
                              tags$li(em(strong("Clicking on a point:")), " When you click a point on the map, a pop-up appears with the daily mean salinity, followed by the short station name used in figure legends, and, finally, the USGS site ID number.")
                          )
                      )
                  ),

                  hr(),
                  p("This app was developed by the Mississippi Sound Estuary Program (MSEP). Please see ", tags$a("our website", href = "https://msucoastal.com/mssoundep/", target = "_blank"), " for more information."),
                  tags$small("For questions about this app, please contact ", tags$a("kim.cressman@msstate.edu", href = "mailto:kim.cressman@msstate.edu"), ".")
              )
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

        # create the map with the first date's data
        req(data())
        first_day <- dplyr::left_join(data()$daily, data()$siteInfo) |>
            dplyr::filter(date == min(data()$daily$date, na.rm = TRUE))

        create_mssnd_basemap() |>
            add_salinity_points(data = first_day) |>
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
