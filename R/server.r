#' Shiny Server
#'
#' Server for shiny app.
#'
#' @import shiny
#' @import ggplot2
#' @import ODWGtools
#' @importFrom stats setNames
#' @importFrom utils lsf.str
#' @importFrom readr read_csv
#' @importFrom purrr imap map
#' @importFrom plotly ggplotly renderPlotly
#' @keywords internal
server = function(input, output, session) {
  # collect available algorithms
  odwg.ns = loadNamespace("ODWGtools")
  fnames = lsf.str(odwg.ns, pattern = "^(outlier_|rtqc_|anomaly_)")
  updateSelectInput(session, "algorithms",
    choices = as.character(fnames))
  # containers for populating UI
  data.ready = reactiveValues(ok = FALSE)
  new.df = reactiveValues(df = NULL)
  df.names = reactiveValues(names = NULL)
  algorithm.list = reactiveValues(algorithms = NULL)
  argument.uids = reactiveValues(uids = NULL)

  # load data
  observeEvent(input$load, {
    datafile = isolate(input$file$datapath)
    if (!nzchar(datafile) || is.null(datafile)) {
      showNotification("No file specified.", type = "error")
    } else {
      showModal(modalDialog("Loading data...",
        footer = NULL))
      df = try(read_csv(datafile))
      removeModal()
      if (inherits(df, "try-error")) {
        showNotification(df$message, type = "error")
      } else {
        showNotification("Data loaded successfully.", type = "message")
        df.names$names = names(df)
        updateSelectInput(session, "x", choices = df.names$names,
          selected = names(df)[1])
        updateSelectInput(session, "y", choices = df.names$names,
          selected = names(df)[2])
        data.ready$ok = TRUE
        new.df$df = df
      }
    }
  })

  # populate algorithm arguments
  observeEvent(input$choose, {
    # get list of algorithms and isolate settings
    algorithms = isolate(input$algorithms)
    x = isolate(input$x)
    y = isolate(input$y)
    # subset list of functions from ODWGtools
    functionlist = setNames(lapply(algorithms, get, pos = odwg.ns),
      algorithms)
    # get list of arguments to selected algorithms
    arglist = lapply(functionlist, formals)
    # flat list of arguments for defining UI elements
    flat.arglist = unlist(imap(arglist,
      ~ paste0(.y, ".", names(.x))), use.names = FALSE)
    argument.uids$uids = flat.arglist
    # generate UI elements
    # ui list has one element per algorithm
    # each element is list of UI elements
    output$arguments = renderUI({
      ui.list = vector("list", length(arglist))
      for (i in seq_along(ui.list)) {
        fun.name = names(arglist)[i]
        fun.args = lapply(arglist[[i]], deparse)
        # arg.inputs names are formals
        # but UI elements of arg.inputs have fun.name:: prepended
        arg.inputs = imap(fun.args,
          ~ textInput(paste0(fun.name, ".", .y), .y, value = .x))
        if (isTRUE(data.ready$ok)) {
          if ("x" %in% names(arg.inputs)) {
            arg.inputs[["x"]] = selectInput(paste0(fun.name, ".x"),
              "x", choices = df.names$names, selected = y)
          } else if ("xs" %in% names(fun.args)) {
            arg.inputs[["xs"]] = selectInput(paste0(fun.name, ".xs"),
              "xs", choices = df.names$names, selected = c(x, y),
              multiple = TRUE)
          }
        }
        # drop names of arg.inputs for UI
        names(arg.inputs) = NULL
        # wrap UI elements in row with header
        ui.list[[i]] = do.call(fluidRow,
          c(list(h4(code(fun.name))), arg.inputs))
      }
      ui.list
    })
    algorithm.list$algorithms = algorithms

  })

  # flag data
  observeEvent(input$flag, {
    if (isFALSE(data.ready$ok)) {
    showNotification("No data loaded.", type = "error")
    } else {
      showModal(modalDialog("Flagging data...",
        footer = NULL))
      # extract algorithm names and settings
      fun.list = isolate(algorithm.list$algorithms)
      uid.list = isolate(argument.uids$uids)
      # generate list of functions and associated arguments
      fun.settings = vector("list", length(fun.list))
      names(fun.settings) = fun.list
      for (i in names(fun.settings)) {
        # identify uids that match the function
        arg.names = uid.list[grep(i, uid.list, fixed = TRUE)]
        for (n in arg.names) {
          setting.name = gsub(paste0(i, "."), "", n, fixed = TRUE)
          fun.settings[[i]][[setting.name]] = isolate(input[[n]])
        }
      }
      # loop and apply functions
      for (fun in names(fun.settings)) {
        new.df$df[fun] = tryCatch({
          # create an environment containing the function arguments
          fun.expr = map(fun.settings[[fun]],
            ~ parse(text = .x))
          fun.env = new.env()
          for (arg in names(fun.expr)) {
            if (arg == "x") {
              fun.env[[arg]] = new.df$df[[fun.settings[[fun]]$x]]
            } else if (arg == "xs") {
              fun.env[[arg]] = new.df$df[fun.settings[[fun]]$xs]
            } else {
              fun.env[[arg]] = eval(fun.expr[[arg]], envir = fun.env)
            }
          }
          # evaluate the function expression
          do.call(fun, as.list(fun.env))
        }, error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        })
      }
      updateSelectInput(session, "plotcolor",
        choices = c(fun.list, "-"))
      removeModal()
    }
  })

  # plot outputs
  output$plot = renderPlotly({
    tryCatch(
      ggplotly({
        if (input$plotcolor %in% names(new.df$df)) {
          ggplot(new.df$df) +
            aes(x = !!as.name(input$x), y = !!as.name(input$y),
              color = !!as.name(input$plotcolor)) +
            geom_point()
        } else {
          ggplot(new.df$df) +
            aes(x = !!as.name(input$x), y = !!as.name(input$y)) +
            geom_point() +
            theme_bw()
        }
      }, dynamicTicks = TRUE),
      error = function(e) invisible(NULL)
    )
  })

  # quit app
  observe({
  if (input$quit)
    stopApp()
  })
}
