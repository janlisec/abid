#' @title abid_app.
#' @description \code{abid_app} will start the App.
#' @details Evaluation of MALDI measurements of digested antibodies.
#' @return A `shinyApp` object.
#' @export
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel fluidRow column selectInput fileInput tabsetPanel tabPanel p plotOutput uiOutput mainPanel helpText numericInput actionButton checkboxInput radioButtons dblclickOpts brushOpts reactiveVal isolate reactive req need observeEvent updateSelectizeInput updateNumericInput renderPrint h2 renderPlot renderUI shinyApp updateSelectInput validate
#' @importFrom bsplus use_bs_tooltip bs_embed_tooltip %>%
#' @importFrom shinyjs useShinyjs hideElement toggleElement
#' @importFrom grDevices grey
#' @importFrom graphics abline axis box mtext par rug segments text points
#' @importFrom stats median
#' @importFrom utils data
#' @importFrom DT DTOutput renderDT JS
#' @importFrom MALDIquant transformIntensity smoothIntensity removeBaseline detectPeaks mass intensity
#' @importFrom stringr str_sort
#' @importFrom plyr ldply
#' @import shiny
#' 
abid_app <- function() {
  
  # set TRUE to compile package for use at BAM Server (including tracking and Datenschutzerklärung)
  bam_server <- TRUE
  
  add_resources <- function() {
    useShinyjs()
    use_bs_tooltip()
    if (bam_server) {
      shiny::tags$head(
        shiny::HTML('<noscript><p><img src="https://agw1.bam.de/piwik/matomo.php?idsite=24&amp;rec=1" style="border:0;" alt="" /></p></noscript>'),
        shiny::HTML('<script type="text/javascript" src="https://agw1.bam.de/piwik/piwik.js" async defer></script>'),
        shiny::tags$script("
          var idSite = 24;
          var piwikTrackingApiUrl = 'https://agw1.bam.de/piwik/piwik.php';
          var _paq = _paq || [];
          _paq.push(['setTrackerUrl', piwikTrackingApiUrl]);
          _paq.push(['setSiteId', idSite]);
          _paq.push(['setDocumentTitle', document.domain + '/' + document.title]);
          _paq.push(['setDoNotTrack', true]);
          _paq.push(['trackPageView']);
          _paq.push(['enableLinkTracking']);
        ")
      )
    }
    
  }

  # Define UI for application ----
  ui <- fluidPage(
    add_resources(),
    sidebarLayout(
      sidebarPanel(
        div(
          style = "margin-bottom: 20px; width: 100%; min-height: 40px",
          div(
            style = "float: left; font-size: 20px; width: 30%",
            shiny::strong("BAM"), shiny::em("ABID")
          ),
          div(
            style = "float: right; width: 70%;",
            HTML(
              'ver', as.character(utils::packageVersion("ABID")), 
              '|', as.character(utils::packageDate("ABID")),
              '| <a href="mailto:jan.lisec@bam.de">jan.lisec@bam.de</a>',
              ifelse(bam_server, '| <a href="https://www.bam.de/Navigation/DE/Services/Datenschutz/datenschutz.html" target="_blank" rel="noopener noreferrer">BAM Datenschutzerkl\u00e4rung</a>', '')
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(inputId = "abid_par_libsource", label = "Select library", choices = list("abid_lib", "upload files"), selected = "abid_lib"),
            fileInput(inputId = "abid_par_path_libfiles", label = "Select Library Files", multiple = TRUE),
            selectInput(inputId = "abid_par_selector_lib_entry", label = "Lib ID", choices = NULL)
          ),
          column(
            width = 6,
            fileInput(inputId = "abid_par_path_newfile", label = "Load Sample"),
            fluidRow(
              column(width = 6, numericInput(inputId = "abid_par_match_dmz", label = "dmz [Da]", value = 0) %>% bs_embed_tooltip(title = "Absolute deviation allowed in testing for overlapping signals. Example: mz1=1000 and mz2=1000.01 overlap if dmz=0.01.")),
              column(width = 6, numericInput(inputId = "abid_par_match_ppm", label = "dmz [ppm]", value = 0) %>% bs_embed_tooltip(title = "Relative deviation allowed in testing for overlapping signals. Example: mz1=1000 and mz2=1000.01 overlap if ppm=10."))
            )
          )
        ),
        tabsetPanel(
          tabPanel(
            title = "Processing", p(""),
            fluidRow(
              column(width = 3, actionButton(inputId = "abid_par_reprocess_lib", label = "library", width = "100%") %>% bs_embed_tooltip(title = "Reprocess all samples from selected library with current parameter settings.")),
              column(width = 3, actionButton(inputId = "abid_par_reprocess_sample", label = "sample", width = "100%") %>% bs_embed_tooltip(title = "Reprocess new sample  with current parameter settings.")),
              column(width = 3, actionButton(inputId = "abid_par_reprocess_both", label = "both", width = "100%") %>% bs_embed_tooltip(title = "Reprocess all samples from selected library and new sample with current parameter settings."))
            ),
            p(""),
            fluidRow(
              column(width = 3, checkboxInput(inputId = "abid_par_transform_apply", label = "transform", value = FALSE) %>% bs_embed_tooltip(title = "Select to include transformation step in spectra processing.")),
              column(width = 3, selectInput(inputId = "abid_par_transform_method", label = "method", choices = c("sqrt", "log2", "log10"), selected = "sqrt") %>% bs_embed_tooltip(title = "Transformation method [suggested: sqrt]")),
              column(width = 3, checkboxInput(inputId = "abid_par_smoothing_apply", label = "smoothing", value = TRUE) %>% bs_embed_tooltip(title = "Select to include smoothing step in spectra processing.")),
              column(width = 3, numericInput(inputId = "abid_par_smoothing_halfWindowSize", label = "hWS", value = 1, min = 0, max = 50, step = 1) %>% bs_embed_tooltip(title = "Smoothing parameter: 'half window size' of peak."))
            ),
            fluidRow(
              column(width = 3, checkboxInput(inputId = "abid_par_baseline_apply", label = "baseline correction", value = TRUE) %>% bs_embed_tooltip(title = "Select to include baseline correction step in spectra processing.")),
              column(width = 3, selectInput(inputId = "abid_par_baseline_method", label = "method", choices = c("SNIP", "TopHat", "ConvexHull", "median"), selected = "TopHat") %>% bs_embed_tooltip(title = "Select method for baseline estimation.")),
              column(width = 3, checkboxInput(inputId = "abid_par_filter_monoiso", label = "keep only monoisotopic peaks", value = TRUE)),
              column(width = 3, numericInput(inputId = "abid_par_filter_monoiso_gap", label = "gap size [Da]", value = 1.1))
            ),
            fluidRow(
              column(width = 3, checkboxInput(inputId = "abid_par_peakpicking_apply", label = "peak picking", value = TRUE) %>% bs_embed_tooltip(title = "Select to include peak picking step in spectra processing.")),
              column(width = 3, numericInput(inputId = "abid_par_peakpicking_halfWindowSize", label = "hWS", value = 1) %>% bs_embed_tooltip(title = "Peak picking parameter: 'half window size' of peak [suggested: 1..5].")),
              column(width = 3, numericInput(inputId = "abid_par_peakpicking_SNR", label = "SNR", value = 10) %>% bs_embed_tooltip(title = "Peak picking parameter: 'Signal/Noise ratio' [suggested: 2..10]."))
            )
          ),
          tabPanel(
            title = "Display", p(""),
            fluidRow(
              column(width = 4, radioButtons(inputId = "abid_par_specplot_showmass", label = "show peak m/z", choices = list("always", "on_zoom", "never"), selected = "on_zoom")),
              column(width = 4, radioButtons(inputId = "abid_par_specplot_precision", label = "m/z precision", choices = list(0, 1, 4), inline = TRUE)),
              column(width = 4, radioButtons(inputId = "abid_par_specplot_plottype", label = "plot type", choices = list("raw", "head2tail"), selected = "raw"))
            ),
            fluidRow(
              column(width = 4, checkboxInput(inputId = "abid_par_testplot_show", label = "show mass deviation plot", value = FALSE)),
              column(width = 4, radioButtons(inputId = "abid_par_testplot_win", label = "m/z dev plot window", choices = list(1, 2, 10), inline = TRUE)),
              column(width = 4, radioButtons(inputId = "abid_par_subclasspeaks_show", label = "show subclass peak positions", choices = list("none", "all", "overlapping"), selected = "overlapping"))
            )
          ),
          id = "abid_par_panel",
          selected = "Display",
          type = "tabs"
        )
      ),

      # Show a specplot
      mainPanel(
        # verbatimTextOutput("test"),
        plotOutput("abid_specplot_lib", height = "300px", dblclick = dblclickOpts(id = "abid_specplot_lib_dblclick"), brush = brushOpts(id = "abid_specplot_lib_brush", direction = "x", resetOnNew = TRUE)) %>% bs_embed_tooltip(title = "You may select a mass range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "left"),
        uiOutput("abid_specplot_new") %>% bs_embed_tooltip(title = "You may select a mass range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "left"),
        uiOutput("abid_testplot"),
        tabsetPanel(
          tabPanel(
            title = "Library comparison", p(""),
            DTOutput("abid_table_lib") # %>% bs_embed_tooltip(title = "You may click on any row to show the corresponding spectrum.")
          ),
          tabPanel(
            title = "Peak list of test sample",
            DTOutput("abid_table_new_peaks") # %>% bs_embed_tooltip(title = "You may click on any row to zoom to the corresponding spectrum.")
          ),
          tabPanel(
            title = "Subclass of test sample",
            DTOutput("abid_table_subclass_prediction")
          ),
          id = "abid_tabPanel_tables"
        )
      )
    )
  )

  # Define server logic ----
  server <- function(input, output, session) {

    ### setup Options ############################################################
    # increase maximum file size for upload
    options(shiny.maxRequestSize = 30 * 1024^2) # BrukerFlex Files are >5MB
    # hide UI Elements for 'new sample'
    hideElement(id = "abid_par_match_dmz")
    hideElement(id = "abid_par_match_ppm")

    ### setup reactive Values ####################################################
    # user can make columns invisible on demand
    col_invisible <- reactiveVal(NULL)
    # will contain peaks of AB subclass if defined
    subclasspeaks <- reactiveVal(NULL)
    # setup plot range (min, max)
    spec_plots_xmin <- reactiveVal(NA)
    spec_plots_xmax <- reactiveVal(NA)


    ### load libraries ###########################################################
    # this is the prepared consensus peak list of Dennis
    subclass_peak_list <- ABID::subclass_peak_list
    # data_env <- new.env()
    # data(subclass_peak_list, package = "ABID", envir = data_env)
    # subclass_peak_list <- get("subclass_peak_list", envir = data_env)

    ### internal functions #######################################################
    # define the (pre) processing steps in a functions
    MALDIquant_pre_process <- function(x) {
      if (isolate(input$abid_par_transform_apply)) {
        x <- transformIntensity(
          object = x,
          method = isolate(input$abid_par_transformation_method)
        )
      }
      if (isolate(input$abid_par_smoothing_apply)) {
        x <- smoothIntensity(
          object = x,
          method = "MovingAverage",
          halfWindowSize = isolate(input$abid_par_smoothing_halfWindowSize)
        )
      }
      if (isolate(input$abid_par_baseline_apply)) {
        x <- removeBaseline(
          object = x,
          method = isolate(input$abid_par_baseline_method)
        )
      }
      return(x)
    }

    # peak detection function
    MALDIquant_peaks <- function(x) {
      detectPeaks(
        object = x,
        method = "MAD",
        halfWindowSize = isolate(input$abid_par_peakpicking_halfWindowSize),
        SNR = isolate(input$abid_par_peakpicking_SNR)
      )
    }

    # define the (post) processing steps in a functions
    MALDIquant_post_process <- function(x) {
      # search for MIDs which hint at monoisotopic peak groups
      test <- any(abs(diff(diff(x@mass))) < 0.2)
      if (test) {
        # x@mass[c(T,T,round(test)==0)]
        gr <- GetGroupFactor(x = x@mass, gap = input$abid_par_filter_monoiso_gap)
        out <- ldply(
          split(data.frame(x@mass, x@intensity, x@snr), gr),
          function(y) {
            y[which.max(y[, 2]), ]
          },
          .id = NULL
        )
        x@mass <- out[, 1]
        x@intensity <- out[, 2]
        x@snr <- out[, 3]
      }
      return(x)
    }

    ### reactives ################################################################
    # load library of non-processed raw data
    abid_lib_spectra_raw <- reactive({
      req(input$abid_par_libsource)
      if (input$abid_par_libsource == "upload files" && !is.null(input$abid_par_path_libfiles)) {
        out <- lapply(input$abid_par_path_libfiles$datapath, readABSpec)
        for (i in 1:length(out)) {
          MALDIquant::metaData(out[[i]])[["File"]] <- input$abid_par_path_libfiles$name[i]
        }
      } else {
        # stripped the possibility to load different libs
        out <- ABID::abid_lib
      }
      message("Loading Lib Spectra")
      tmp <- range(sapply(out, function(x) {
        range(x@mass, na.rm = TRUE)
      }), na.rm = TRUE)
      spec_plots_xmax(max(isolate(spec_plots_xmax()), tmp, na.rm = TRUE))
      spec_plots_xmin(min(isolate(spec_plots_xmin()), tmp, na.rm = TRUE))
      return(out)
    })

    # provide spectra based on processed raw data
    abid_lib_spectra <- reactive({
      input$abid_par_reprocess_lib
      input$abid_par_reprocess_both
      req(abid_lib_spectra_raw())
      MALDIquant_pre_process(abid_lib_spectra_raw())
    })

    abid_new_spectra_raw <- reactive({
      req(input$abid_par_path_newfile)
      message("Loading Test Spectrum")
      out <- readABSpec(file = input$abid_par_path_newfile$datapath)
      # rng <- 100*c(floor(min(out@mass)/100),ceiling(max(out@mass)/100))
      dmz <- round(10 * median(diff(out@mass)), 3)
      updateNumericInput(session = session, inputId = "abid_par_match_dmz", value = dmz)
      # updateNumericInput(session = session, inputId = "abid_par_match_ppm", value = dmz)
      return(out)
    })

    abid_new_spectra <- reactive({
      input$abid_par_reprocess_sample
      input$abid_par_reprocess_both
      req(abid_new_spectra_raw())
      MALDIquant_pre_process(abid_new_spectra_raw())
    })

    abid_lib_peaks_pre <- reactive({
      req(abid_lib_spectra())
      if (isolate(input$abid_par_peakpicking_apply)) {
        lapply(abid_lib_spectra(), MALDIquant_peaks)
      } else {
        NULL
      }
    })

    abid_lib_peaks <- reactive({
      req(abid_lib_peaks_pre())
      if (input$abid_par_filter_monoiso) {
        lapply(abid_lib_peaks_pre(), MALDIquant_post_process)
      } else {
        abid_lib_peaks_pre()
      }
    })

    abid_new_peaks_pre <- reactive({
      req(abid_new_spectra())
      if (isolate(input$abid_par_peakpicking_apply)) {
        MALDIquant_peaks(abid_new_spectra())
      } else {
        NULL
      }
    })

    abid_new_peaks <- reactive({
      req(abid_new_peaks_pre())
      if (input$abid_par_filter_monoiso) {
        MALDIquant_post_process(abid_new_peaks_pre())
      } else {
        abid_new_peaks_pre()
      }
    })

    abid_table_new_peaks_pre <- reactive({
      validate(need(input$abid_par_path_newfile, message = "Please upload a sample"))
      validate(need(abid_new_peaks(), message = "Please apply peak picking to sample"))
      out <- MALDIquant::as.matrix(abid_new_peaks())
      prec <- 0
      test <- TRUE
      while (test | prec >= 5) {
        if (any(duplicated(round(out[, 2], prec)))) {
          prec <- prec + 1
        } else {
          test <- FALSE
        }
      }
      out[, 2] <- round(out[, 2], prec)
      return(out)
    })

    abid_table_subclass_prediction_pre <- reactive({
      validate(need(input$abid_par_path_newfile, message = "Please upload a sample"))
      validate(need(abid_new_peaks(), message = "Please apply peak picking to sample"))
      l2 <- MALDIquant::as.matrix(abid_new_peaks())
      # adjust mass deviation windows
      for (i in 1:length(subclass_peak_list)) {
        subclass_peak_list[[i]][, 2] <- sapply(subclass_peak_list[[i]][, 1] * input$abid_par_match_ppm / 10^6, function(y) {
          max(y, input$abid_par_match_dmz)
        })
      }
      # find overlapping peaks
      ovlp <- lapply(subclass_peak_list, function(l1) {
        sapply(1:nrow(l1), function(i) {
          ifelse(any(abs(l2[, 1] - l1[i, 1]) < l1[i, 2]), which.min(abs(l2[, 1] - l1[i, 1])), NA)
        })
      })
      out <- cbind(attr(subclass_peak_list, "info"),
        "n_peaks" = sapply(subclass_peak_list, nrow),
        "n_overlap" = sapply(ovlp, function(x) {
          sum(is.finite(x))
        })
      )
      out <- out[order(out[, "n_overlap"], decreasing = TRUE), ]
      attr(out, "ovlp") <- ovlp
      return(out)
    })

    abid_lib_entry_names <- reactive({
      sapply(abid_lib_spectra_raw(), function(x) {
        MALDIquant::metaData(x)[[1]]
      })
    })

    abid_table_lib_pre <- reactive({
      # req(abid_lib_spectra_raw())
      validate(need(abid_lib_spectra_raw(), message = "Please select library"))
      out <- ldply(abid_lib_spectra_raw(), function(x) {
        unlist(MALDIquant::metaData(x))
      })
      colnames(out)[1] <- "ID"
      out[, "ID"] <- factor(out[, "ID"], levels = str_sort(out[, "ID"], numeric = TRUE))
      # add numer of peak column if peaks were determined
      # browser()
      if (!is.null(abid_lib_peaks())) {
        out[, "n_peaks"] <- as.numeric(sapply(abid_lib_peaks(), length))
        # add peak comparison columns if test file/peaks is present
        if (!is.null(input$abid_par_path_newfile) && !is.null(abid_new_peaks())) {
          out <- cbind(
            out,
            ldply(abid_lib_peaks(), function(x) {
              get_overlap(x = x, y = abid_new_peaks(), type = "all", dmz = input$abid_par_match_dmz, ppm = input$abid_par_match_ppm)
            })
          )
          # sort table for best matching lib spectra and select this one for comparison
          out <- out[order(out[, "rel"], decreasing = TRUE), ]
          if (!isolate(input$abid_par_selector_lib_entry) %in% isolate(abid_lib_entry_names())) {
            # keep the currently selected peak but change to best hit in case that different library is selected
            # it may be preferred to switch to best hit after every reprocessing though
            updateSelectizeInput(session, inputId = "abid_par_selector_lib_entry", selected = out[1, "ID"])
          }
        }
      }
      # sort according to 'ID' if no comparison took place
      if (!"rel" %in% colnames(out)) out <- out[order(out[, "ID"]), ]
      # remove some column leftovers from the meta data
      out <- out[, !(colnames(out) %in% c("m/z", "masses"))]
      return(out)
    })

    abid_status_valid_lib_selected <- reactive({
      test <- !is.null(input$abid_par_libsource)
      test <- test & !is.null(abid_lib_spectra_raw())
      test <- test & !is.null(abid_lib_entry_names())
      test <- test & !is.null(input$abid_par_selector_lib_entry)
      test <- test & any(abid_lib_entry_names() == input$abid_par_selector_lib_entry)
      return(test)
    })

    ### observers on input fields ################################################
    # changes upon lib source selection
    observeEvent(input$abid_par_libsource, {
      toggleElement(id = "abid_par_path_libfiles", condition = input$abid_par_libsource == "upload files")
      # reset column visibility filter
      if (input$abid_par_libsource == "abid_lib") {
        col_invisible(0)
      } else {
        col_invisible(NULL)
      }
    })

    # ...
    observeEvent(input$abid_par_path_newfile, {
      # print(input$abid_par_path_newfile)
      toggleElement(id = "abid_par_match_dmz", condition = !is.null(input$abid_par_path_newfile))
      toggleElement(id = "abid_par_match_ppm", condition = !is.null(input$abid_par_path_newfile))
    })

    # ...
    observeEvent(input$abid_par_testplot_show, {
      toggleElement(id = "abid_testplot", condition = input$abid_par_testplot_show)
    })

    # ...
    observeEvent(
      {
        input$abid_table_subclass_prediction_rows_selected
        input$abid_par_subclasspeaks_show
      },
      {
        if (input$abid_par_subclasspeaks_show == "none") {
          subclasspeaks(NULL)
        } else {
          id_selected <- paste(abid_table_subclass_prediction_pre()[input$abid_table_subclass_prediction_rows_selected, 2:3], collapse = "_")
          if (input$abid_par_subclasspeaks_show == "overlapping") {
            flt <- is.finite(attr(abid_table_subclass_prediction_pre(), "ovlp")[[id_selected]])
            out <- as.numeric(subclass_peak_list[[id_selected]][, 1][flt])
          } else {
            out <- as.numeric(subclass_peak_list[[id_selected]][, 1])
          }
          subclasspeaks(out)
        }
      }
    )

    # ...
    observeEvent(input$abid_table_lib_state$columns, {
      col_invisible(which(!sapply(input$abid_table_lib_state$columns, function(x) {
        x$visible
      })) - 1)
    })

    # ...
    observeEvent(input$abid_table_new_peaks_rows_selected, {
      req(abid_table_new_peaks_pre())
      mz <- as.integer(abid_table_new_peaks_pre()[input$abid_table_new_peaks_rows_selected, 1])
      spec_plots_xmin(mz - 25)
      spec_plots_xmax(mz + 25)
    })

    # ...
    observeEvent(input$abid_table_lib_rows_selected, {
      req(abid_table_lib_pre())
      selected <- abid_table_lib_pre()[input$abid_table_lib_rows_selected, "ID"]
      updateSelectInput(session, "abid_par_selector_lib_entry", selected = selected)
    })

    # ...
    observeEvent(input$abid_specplot_lib_brush, {
      spec_plots_xmin(floor(input$abid_specplot_lib_brush$xmin))
      spec_plots_xmax(ceiling(input$abid_specplot_lib_brush$xmax))
    })

    # ...
    observeEvent(input$abid_specplot_new_brush, {
      spec_plots_xmin(floor(input$abid_specplot_new_brush$xmin))
      spec_plots_xmax(ceiling(input$abid_specplot_new_brush$xmax))
    })

    # ...
    observeEvent(input$abid_specplot_lib_dblclick, {
      idx <- which(isolate(abid_lib_entry_names()) == isolate(input$abid_par_selector_lib_entry))
      rng <- range(mass(isolate(abid_lib_spectra())[[idx]]))
      spec_plots_xmin(rng[1])
      spec_plots_xmax(rng[2])
    })

    # ...
    observeEvent(input$abid_specplot_new_dblclick, {
      rng <- range(mass(isolate(abid_new_spectra())))
      spec_plots_xmin(rng[1])
      spec_plots_xmax(rng[2])
    })

    ### observers on reactives ###################################################
    observeEvent(abid_lib_entry_names(), {
      choices <- str_sort(abid_lib_entry_names(), numeric = TRUE)
      updateSelectInput(session, "abid_par_selector_lib_entry", choices = choices)
    })

    ### outputs ##################################################################
    ## tables
    # table of peaks of 'new sample'
    output$abid_table_new_peaks <- renderDT(
      {
        abid_table_new_peaks_pre()
      },
      selection = list(mode = "single", target = "row"),
      rownames = NULL
    )

    output$abid_table_subclass_prediction <- renderDT(
      {
        abid_table_subclass_prediction_pre()
      },
      selection = list(mode = "single", target = "row"),
      rownames = NULL
    ) # , selected=1

    output$abid_table_lib <- renderDT(
      {
        abid_table_lib_pre()
      },
      extensions = "Buttons",
      options = list(
        stateSave = TRUE,
        stateLoadParams = JS("function (settings, data) {return false;}"),
        dom = "Bfrtip", buttons = I("colvis"),
        columnDefs = list(list(visible = FALSE, targets = isolate(col_invisible())))
      ),
      selection = list(mode = "single", target = "row"),
      rownames = NULL
    )

    ## plots
    # ...
    output$abid_specplot_new_pre <- renderPlot({
      req(input$abid_par_path_newfile, abid_new_spectra())
      ptr <- input$abid_par_specplot_plottype == "raw"
      m <- ifelse(ptr, 1, -1) # for head2tail plot everything in negative direction
      sm <- mass(abid_new_spectra())
      si <- intensity(abid_new_spectra())
      flt <- sm >= spec_plots_xmin() & sm <= spec_plots_xmax()
      zoom_status <- diff(range(sm)) > 1.05 * (spec_plots_xmax() - spec_plots_xmin())
      xlim <- c(spec_plots_xmin(), spec_plots_xmax())
      par(mar = c(2.5, 2.5, ifelse(ptr, 0.5, 0), 0.5))
      plot(x = sm[flt], y = m * si[flt], type = "l", xaxs = "i", xlab = "", ylab = "", xlim = xlim, col = ifelse(ptr, 1, grey(0.9)))
      mp <- NULL
      if (!is.null(abid_new_peaks()) && length(mass(abid_new_peaks())) >= 1) mp <- mass(abid_new_peaks())
      if (!is.null(mp)) {
        mi <- m * intensity(abid_new_peaks())
        points(x = mp, y = mi, col = 4, pch = 4)
        if (!ptr) segments(x0 = mp, y0 = rep(0, length(mi)), y1 = mi)
        mtext(text = paste("n_peaks =", length(mp)), side = ifelse(ptr, 3, 1), line = -1.2, adj = 0.02)
        if (input$abid_par_specplot_showmass == "always" | (input$abid_par_specplot_showmass == "on_zoom" & zoom_status)) {
          text(x = mp, y = mi, labels = round(mp, as.numeric(input$abid_par_specplot_precision)), pos = 4)
        }
        if (!is.null(abid_lib_spectra_raw()) && !is.null(abid_lib_peaks()) && !is.null(input$abid_par_selector_lib_entry) && input$abid_par_selector_lib_entry %in% abid_lib_entry_names()) {
          # color overlapping peaks as green
          idx <- which(abid_lib_entry_names() == input$abid_par_selector_lib_entry)
          lmp <- mass(abid_lib_peaks()[[idx]])
          if (length(lmp) >= 1) {
            flt <- sapply(mp, function(x) {
              any(abs(lmp - x) <= max(input$abid_par_match_dmz, x * input$abid_par_match_ppm / 10^6))
            })
            if (any(flt)) points(x = mp[flt], y = mi[flt], bg = 3, pch = 21)
            mtext(text = paste("peak overlap =", sum(flt)), side = ifelse(ptr, 3, 1), line = -2.4, adj = 0.02)
          }
        }
      }
      if (!is.null(subclasspeaks())) {
        rug(x = subclasspeaks(), col = 2, lwd = 2, quiet = TRUE)
      }
      mtext(text = input$abid_par_path_newfile$name, side = ifelse(ptr, 3, 1), line = -1.2, adj = 0.98, font = 2)
      mtext(text = "(sample)", side = ifelse(ptr, 3, 1), line = -2.4, adj = 0.98, font = 3)
    })

    # ...
    output$abid_specplot_lib <- renderPlot({
      if (abid_status_valid_lib_selected()) {
        message("Plot Lib Spectra")
        idx <- which(abid_lib_entry_names() == input$abid_par_selector_lib_entry)
        ptr <- input$abid_par_specplot_plottype == "raw"
        sm <- mass(abid_lib_spectra()[[idx]])
        si <- intensity(abid_lib_spectra()[[idx]])
        flt <- sm >= spec_plots_xmin() & sm <= spec_plots_xmax()
        xlim <- c(spec_plots_xmin(), spec_plots_xmax())
        zoom_status <- diff(range(sm)) > 1.05 * (spec_plots_xmax() - spec_plots_xmin())
        par(mar = c(ifelse(ptr, 0.5, 0), 2.5, 2.5, 0.5))
        plot(x = sm[flt], y = si[flt], type = "l", xaxs = "i", xlab = "", ylab = "", xlim = xlim, axes = F, col = ifelse(ptr, 1, grey(0.9)))
        axis(2)
        axis(3)
        box()
        if (!is.null(abid_lib_peaks())) {
          pm <- mass(abid_lib_peaks()[[idx]])
          pi <- intensity(abid_lib_peaks()[[idx]])
          points(x = pm, y = pi, col = "red", pch = 4)
          if (!ptr) segments(x0 = pm, y0 = rep(0, length(pi)), y1 = pi)
          if (input$abid_par_specplot_showmass == "always" | (input$abid_par_specplot_showmass == "on_zoom" & zoom_status)) {
            text(x = pm, y = pi, labels = round(pm, as.numeric(input$abid_par_specplot_precision)), pos = 4)
          }
          mtext(text = paste("n_peaks =", length(pm)), side = 3, line = -1.2, adj = 0.02, font = 1)
        }
        mtext(text = input$abid_par_selector_lib_entry, side = 3, line = -1.2, adj = 0.98, font = 2)
        mtext(text = "(library)", side = 3, line = -2.4, adj = 0.98, font = 3)
      } else {
        plot(0, 0, axes = F, ann = F, type = "n")
        text(0, 0, "Please select a library")
      }
    })

    # ...
    output$abid_specplot_new <- renderUI({
      if (!is.null(abid_new_spectra())) {
        plotOutput(
          outputId = "abid_specplot_new_pre",
          height = "300px",
          dblclick = dblclickOpts(id = "abid_specplot_new_dblclick"),
          brush = brushOpts(id = "abid_specplot_new_brush", direction = "x", resetOnNew = TRUE)
        )
      }
    })

    # ...
    output$abid_testplot_pre <- renderPlot({
      req(
        input$abid_par_path_newfile,
        abid_new_spectra(),
        abid_lib_peaks(),
        abid_new_peaks(),
        input$abid_par_selector_lib_entry,
        abid_lib_entry_names()
      )
      if (any(abid_lib_entry_names() == input$abid_par_selector_lib_entry)) {
        idx <- which(abid_lib_entry_names() == input$abid_par_selector_lib_entry)
        flt <- mass(abid_lib_spectra()[[idx]]) >= spec_plots_xmin() & mass(abid_lib_spectra()[[idx]]) <= spec_plots_xmax()
        x_rng <- range(mass(abid_lib_spectra()[[idx]])[flt])
        mz_test <- mass(abid_lib_peaks()[[idx]])
        d_mz <- sapply(mass(abid_new_peaks()), function(x) {
          c(x - mz_test)[which.min(abs(x - mz_test))]
        })
        par(mar = c(2, 2, 0, 0) + 0.5)
        plot(x = mass(abid_new_peaks()), y = d_mz, ylim = c(-1, 1) * as.numeric(input$abid_par_testplot_win) * input$abid_par_match_dmz, xlim = x_rng, type = "p", xaxs = "i", xlab = "", ylab = "")
        abline(h = c(-1, 1) * input$abid_par_match_dmz, col = grey(0.8))
      }
    })

    # ...
    output$abid_testplot <- renderUI({
      if (!is.null(abid_new_spectra())) {
        plotOutput("abid_testplot_pre")
      }
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
  
}
