# create query string from table selection
create_query_string <- function(series, tags) {

  string <- vector(mode = "list", length = nrow(series))
  options("useFancyQuotes" = FALSE)

  for (i in 1:nrow(series)) {

    query_string <- NULL

    for (j in 1:ncol(series)) {

      if ((colnames(series)[j]) %in% tags) {

        if (is.null(query_string)) {

          query_string <- paste(colnames(series)[j], base::sQuote(series[i,j]),sep = "=")

        } else {

          query_string <- paste(query_string,
                                paste(colnames(series)[j], base::sQuote(series[i,j]),
                                      sep = "="), sep = "AND ")
        }

      }

    }
    string[[i]] <- paste(query_string, "GROUP BY *")
  }
  return(string)
}

influxdb_inspector <- function() {

  # check if suggested packages are available:
  required_pkgs <- c("DT", "leaflet", "miniUI", "rstudioapi", "shiny")

  invisible(sapply(required_pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("package", pkg, "needed for this function to work. Please install it."),
           call. = FALSE)
    }
  })
  )

  # define UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "influxdb inspector", left = NULL),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(title = "InfluxDb Connection",
                           icon = shiny::icon("database"),
                           miniUI::miniContentPanel(
                             shiny::fillRow(
                               list(
                                 # shiny::textInput(inputId = "group",
                                 #                  label =  "group",
                                 #                  placeholder = "name of group in config file",
                                 #                  value = "groupname"),
                                 shiny::textInput(inputId = "username",
                                           label =  "username",
                                           placeholder = "username"),
                                 shiny::passwordInput(inputId = "password",
                                                      value = "password",
                                                      label =  "password"),
                                 shiny::textInput(inputId = "host",
                                                  label =  "host",
                                                  placeholder = "www.myinfluxdbserver.com",
                                                  value = "10.11.110.202"),
                                 shiny::numericInput(inputId = "port",
                                                     label =  "port",
                                                     value = 8086)
                                 ),
                               flex = 1, width = "100%", height = "100%")
                              ),
                             miniUI::miniButtonBlock(
                               shiny::actionButton(inputId = "checkconnection",
                                                   label = "check connection")
                             )
                          ),
      miniUI::miniTabPanel(title = "Parameters",
                   icon = shiny::icon("sliders"),
                   miniUI::miniContentPanel(
                     shiny::fillRow(
                      list(
                        shiny::textInput(inputId = "database",
                               label =  "database",
                               placeholder = "the name of the dababase to query",
                               value = ""),
                        shiny::selectInput(inputId = "measurements",
                                 label =  "measurements",
                                 choices = " ",
                                 multiple = FALSE),
                        shiny::selectInput(inputId = "fieldkeys",
                                   label =  "fieldkeys",
                                   multiple = TRUE,
                                   choices = " "),
                        shiny::selectInput(inputId = "tagkeys",
                                   label =  "tagkeys",
                                   choices = " ",
                                   multiple = TRUE),
                        shiny::textInput(inputId = "result",
                                         label =  "result",
                                         placeholder = "the name of the result object
                                         in the global environment",
                                         value = "")
                        ),
                       DT::dataTableOutput("series"),
                       flex = c(2,3), width = "100%", height = "100%")),
                   miniUI::miniButtonBlock(
                     shiny::actionButton(inputId = "getseries",
                                  label = "get series")
                   )
                   ),
      miniUI::miniTabPanel("Visualize", icon = shiny::icon("area-chart"),
                           miniUI::miniContentPanel(
                             shiny::plotOutput("cars", height = "100%")
                   )
      ),
      miniUI::miniTabPanel("Data", icon = shiny::icon("table"),
                           miniUI::miniContentPanel(
                     DT::dataTableOutput("table")
                   )
      ),
      miniUI::miniTabPanel("Map", icon = shiny::icon("map-o"),
                           miniUI::miniContentPanel(padding = 0,
                                    leaflet::leafletOutput("map", height = "100%")
                   ),
                   miniUI::miniButtonBlock(
                     shiny::actionButton("resetMap", "Reset")
                   )
      )
    )
  )

  server <- function(input, output, session) {

    # database name is reactive
    dbInput <- shiny::reactive({
      # todo: multiple series?
      input$database
    })

    # measurement name is reactive
    measurementInput <- shiny::reactive({
      # todo: multiple series?
      input$measurements
    })

    # fieldkeys are reactive
    fieldkeysInput <- shiny::reactive({
      input$fieldkeys
    })

    # tagkeys are reactive
    tagkeysInput <- shiny::reactive({
      input$tagkeys
    })


    # if database changes or measurement changes:
    # 1. return series_df
    series_df_Input <- shiny::reactive({

      # catch empty values in measurement
      .measurement <- measurementInput()
      if (!is.null(.measurement)) {
        if (nchar(trimws(.measurement)) == 0) {
          .measurement <- NULL
        }
      }

      # fetch series dataframe
      series_df <- influxdbr::show_series(con = con(),
                                          db = dbInput(),
                                          measurement = .measurement)

      if (length(series_df) > 0) {
        return(series_df[[1]])
      } else {
        return(data.frame(NULL))
      }
    })

    # create new list of measurements if database changes
    shiny::observe({

      measurements <- influxdbr::show_measurements(con = con(),
                                                 db = dbInput())

      if (is.null(measurements)) measurements <- " "
      updateSelectInput(session = session,
                        inputId = "measurements",
                        choices = measurements)
    })

    # fill fieldkey selectInput
    shiny::observe({

      # catch empty values in measurement
      .measurement <- measurementInput()
      if (!is.null(.measurement)) {
        if (nchar(trimws(.measurement)) == 0) {
          .measurement <- NULL
        }
      }

      field_keys <- influxdbr::show_field_keys(con = con(),
                                               db = dbInput(),
                                               measurement = .measurement)

      if (!is.null(field_keys)) {
        field_keys <- field_keys[[1]]$fieldKey
      } else {
        field_keys <- ""
      }

      updateSelectInput(session = session,
                        inputId = "fieldkeys",
                        choices =  field_keys,
                        selected = field_keys)
    })

    # fill tagkey selectInput
    shiny::observe({

      # catch empty values in measurement
      .measurement <- measurementInput()
      if (!is.null(.measurement)) {
        if (nchar(trimws(.measurement)) == 0) {
          .measurement <- NULL
        }
      }

      tag_keys <- influxdbr::show_tag_keys(con = con(),
                                           db = dbInput(),
                                           measurement = .measurement)

      if (!length(tag_keys) > 0) tag_keys <- c("")

      updateSelectInput(session = session,
                        inputId = "tagkeys",
                        choices = tag_keys,
                        selected = tag_keys[[1]])
    })

    # create map
    output$map <- leaflet::renderLeaflet({
      force(input$resetMap)
        leaflet() %>%
          addTiles() %>%
          setView(lng = 7.6267552, lat = 51.9630088, zoom = 12) %>%
          addWMSTiles(
            "http://ows.terrestris.de/osm/service?",
            layers = "OSM-WMS",
            options = WMSTileOptions(format = "image/png", transparent = TRUE),
            attribution = "OSM-WMS © OpenStreetMap"
          )
      })

    # # create new table with series if series_df changes
    output$series <- DT::renderDataTable(series_df_Input(),
                                         filter = "top")

    # export series to .globalEnv
    shiny::observeEvent(input$getseries, {

      if (input$result == "") {

        message("Please provide a name for the return value...")

      } else {

        if (exists(input$result, envir = .GlobalEnv)) {
          message("Return value already in global Environment.
                  Please change the name for the return value.")
        } else {

          row <- as.numeric(input$series_rows_selected)

          if (!length(row) > 0) {

            message("No series selected.")

          } else {

            query <- create_query_string(series = series_df_Input()[row,],
                                         tags = tagkeysInput())

            message("executing query...")

            tmp <- lapply(query, function(q) influxdbr::influx_select(con = con(),
                                                                      db = dbInput(),
                                                                      measurement = measurementInput(),
                                                                      field_keys = fieldkeysInput(),
                                                                      where = q))

            assign(x = input$result, tmp, envir = .GlobalEnv)
            gc()
            message("done.")

          }

        }

      }

    })

    # create connection object
    con <- shiny::eventReactive(input$checkconnection, {

      ret <- list(con = influxdbr::influx_connection(host = input$host,
                                                     port = input$port,
                                                     user = input$username,
                                                     pass = input$password))
      return(ret$con)

    })

    # stop app
    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue = "Bye")
    })
  }

  # set viewer preference
  viewer <- shiny::dialogViewer("Influxdbr Rstudio Addin", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)

}
