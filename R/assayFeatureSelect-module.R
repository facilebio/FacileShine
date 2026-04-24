#' A module to pull out features from an assay with optional GeneSetDb support.
#' 
#' @export
assayFeatureSelectServer <- function(id, rfds, gdb = reactive(NULL), ...,
                                     exclude = NULL, debug = FALSE) {
  assert_class(rfds, "ReactiveFacileDataStore")
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(
      rfds_name = "__initializing__",
      selected = .no_features(),
      # "labeled" API
      name = "__initializing__",
      label = "__initializing__")
    
    assay <- assaySelectServer("assay", rfds, debug = FALSE, ...)
    
    in_sync <- reactive({
      req(assay$in_sync())
    })
    
    # features_all <- eventReactive(assay$selected(), {
    #   req(in_sync())
    #   features(rfds$fds(), assay_name = assay$selected())
    # })
    features_all <- shiny::reactive({
      shiny::req(in_sync())
      rfds$fds() |> 
        FacileData::features(assay_name = assay$selected()) |> 
        dplyr::mutate(
          name = ifelse(is.na(.data$name), .data$feature_id, .data$name))
    }) |> 
      shiny::bindEvent(assay$selected())

    observeEvent(features_all(), {
      fall <- features_all()
      choices <- setNames(fall$feature_id, fall$name)
      selected <- input$features
      if (unselected(selected) && nrow(state$selected) > 0L) {
        selected <- state$selected$feature_id
      }
      overlap <- intersect(selected, fall$feature_id)
      if (length(overlap) > 0L) {
        keep <- tibble(assay = assay$selected(), feature_id = overlap)
        out <- semi_join(fall, keep, by = c("assay", "feature_id"))
      } else {
        out <- .no_features()
        overlap <- NULL
      }
      if (!setequal(
        paste(out$assay, out$feature_id),
        paste(state$selected$assay, state$selected$feature_id)
      )) {
        state$selected <- out
      }
      shiny::updateSelectizeInput(
        session, "features",
        choices = choices,
        selected = overlap,
        server = TRUE
      )
    })
    
    observeEvent(input$features, {
      fall <- features_all()
      req(nrow(fall))
      ifeatures <- input$features

      if (unselected(ifeatures)) {
        out <- .no_features()
      } else {
        fx <- tibble(assay = assay$selected(), feature_id = ifeatures)
        out <- semi_join(fall, fx, by = c("assay", "feature_id"))
      }
      
      update <- !setequal(
        paste(out$assay, out$feature_id),
        paste(state$selected$assay, state$selected$feature_id))

      if (update) {
        ftrace("updating selected features: ", 
               paste(out$feature_id, collapse = ","))
        state$selected <- out
      }
      
    }, ignoreNULL = FALSE)
    
    selected <- reactive({
      req(in_sync())
      semi_join(state$selected, features_all(), by = c("assay", "feature_id"))
    })
  
    # ................................................................. genesets
    observe({
      # Only show the UI element if a GeneSetDb was passed in.
      shinyjs::toggleElement("genesetbox", condition = !is.null(gdb()))
    })
    
    geneset <- callModule(
      sparrow.shiny::reactiveGeneSetSelect, "geneset", gdb, ...)
    
    # We tend to use the same genesets between transcriptomics and proteomics
    # (based on ensembl id). When users switch to proteomics, firing on
    # assay$selected() gives us a shot to update the geneset again, unless
    # the proteomics assay covers all of the same features as transcriptomics.
    observeEvent({ geneset$membership(); assay$selected() }, {
      in_sync()
      fall <- req(features_all())
      req(nrow(fall))
      
      gfeatures <- geneset$membership()
      out <- semi_join(fall, gfeatures, by = "feature_id")
      
      if (!setequal(out$feature_id, state$selected$feature_id)) {
        # state$selected <- out
        choices <- setNames(features_all()$feature_id, features_all()$name)
        updateSelectizeInput(session, "features", selected = out$feature_id,
                             choices = choices, server = TRUE)
      }
    }, ignoreNULL = TRUE)
    
    vals <- list(
      selected = selected,
      features_all = features_all,
      assay = assay,
      in_sync = in_sync,
      .state = state,
      .ns = session$ns)
    class(vals) <- c("AssayFeatureSelectModule", "FacileDataAPI", "Labeled")    
    vals
  })
}

#' @noRd
#' @export
initialized.AssayFeatureSelectModule <- function(x, ...) {
  check <- c("assay_names", "assay_info")
  ready <- sapply(check, \(s) !unselected(x$.state[[s]]))
  initialized(x$assay) && all(ready) && is(x$features_all(), "tbl")
}

#' @noRd
#' @export
from_fds.AssayFeatureSelectModule <- function(x, rfds, ...) {
  .Deprecated("Use x$in_sync() instead")
  if (!x$assay$in_sync()) return(FALSE)
  isolate(x[[".state"]]$rfds_name == name(rfds))
}

#' @export
#' @noRd
#' @importFrom shiny selectInput selectizeInput
#' @rdname assayFeatureSelect
#' @param multiple whether the `"features"` input accepts multiple values
#' @param selectizeOptions a list of options passed to the `"features"`
#'   `selectizeInput`; defaults enable remove buttons and multi-value paste by
#'   feature id or label
assayFeatureSelectInput <- function(id, label = NULL, multiple = TRUE,
                                    selectizeOptions = list(), ...) {
  ns <- NS(id)
  
  if (is.null(label)) {
    assay.style <- ""
  } else {
    assay.style <- "padding-top: 1.7em"
  }
  
  out <- tagList(
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::tagList(
          selectizeInput(ns("features"), label = label, choices = NULL,
                         multiple = multiple,
                         options = .assay_feature_selectize_options(
                           selectizeOptions
                         )),
          shiny::tags$div(
            id = ns("features_feedback"),
            class = "text-muted",
            role = "status",
            style = "display:none; margin-top: 0.35em; font-size: 0.9em;"
          ))),
      shiny::column(
        width = 3,
        shiny::tags$div(
          style = assay.style,
          assaySelectInput(ns("assay"), label = NULL, choices = NULL)))),
    shinyjs::hidden(
      shiny::tags$div(
        id = ns("genesetbox"),
        sparrow.shiny::reactiveGeneSetSelectUI(ns("geneset"))))
  )
  
  out
}

# Labeled API ==================================================================

#' @noRd
#' @export
name.AssayFeatureSelectModule <- function(x, ...) {
  xf <- x[["selected"]]()
  out <- if (nrow(xf) == 0) {
    "nothing"
  } else if (nrow(xf) == 1) {
    xf$name
  } else {
    "score"
  }
  make.names(x[[".ns"]](out))
}

#' @noRd
#' @export
label.AssayFeatureSelectModule <- function(x, ...) {
  xf <- x[["selected"]]()
  if (nrow(xf) == 0) {
    "nothing"
  } else if (nrow(xf) == 1) {
    xf$name
  } else if (nrow(xf) < 10) {
    paste(xf$name, collapse = ",")
  } else {
    "score"
  }
}

# Random =======================================================================
.no_features <- function() {
  tibble(
    assay = character(),
    feature_id = character(),
    name = character())
}

.assay_feature_selectize_options <- function(selectizeOptions = list()) {
  init_handler <- I(
    "function() {
      var s = this;
      var copyText = function(txt) {
        var copyFallback = function() {
          var ta = document.createElement('textarea');
          ta.value = txt;
          ta.setAttribute('readonly', '');
          ta.style.position = 'absolute';
          ta.style.left = '-9999px';
          document.body.appendChild(ta);
          ta.select();
          try {
            document.execCommand('copy');
          } finally {
            document.body.removeChild(ta);
            s.focus();
          }
        };
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(txt).catch(copyFallback);
        } else {
          copyFallback();
        }
      };
      var feedbackId = function() {
        return s.$input.attr('id') + '_feedback';
      };
      var feedbackNode = function() {
        return document.getElementById(feedbackId());
      };
      var clearFeedback = function() {
        var node = feedbackNode();
        if (!node) return;
        node.textContent = '';
        node.style.display = 'none';
      };
      var armFeedbackClear = function() {
        s.$control_input.off('.assayFeatureSelectFeedback');
        s.$control.off('.assayFeatureSelectFeedback');
        s.$control_input.one('keydown.assayFeatureSelectFeedback',
                             clearFeedback);
        s.$control.one('mousedown.assayFeatureSelectFeedback', clearFeedback);
      };
      var invalidMessage = function(vals) {
        if (vals.length === 1) {
          return vals[0] + ' is invalid and was not appended to the pasted list';
        }
        return vals.join(', ') +
          ' are invalid and were not appended to the pasted list';
      };
      var showFeedback = function(txt) {
        var node = feedbackNode();
        if (!node) return;
        node.textContent = txt;
        node.style.display = 'block';
        armFeedbackClear();
      };
      var lookupKey = function(v) {
        if (s.options[v]) return v;
        var key = null;
        Object.keys(s.options).some(function(k) {
          var opt = s.options[k];
          var label = opt && (opt.text || opt.label);
          if (label === v) {
            key = k;
            return true;
          }
          return false;
        });
        return key;
      };
      var itemLabel = function(key) {
        var opt = s.options[key];
        if (!opt) return key;
        return opt.text || opt.label || key;
      };
      var resolveKeys = function(vals, done) {
        var out = [];
        var i = 0;
        var next = function() {
          if (i >= vals.length) {
            done(out);
            return;
          }
          var v = vals[i++];
          var key = lookupKey(v);
          if (key) {
            out.push(key);
            next();
            return;
          }
          if (!s.settings.load) {
            next();
            return;
          }
          s.load(function(callback) {
            s.settings.load.call(s, v, function(results) {
              callback(results);
              key = lookupKey(v);
              if (key) out.push(key);
              next();
            });
          });
        };
        next();
      };
      s.$control_input.on('keydown.assayFeatureSelectCopy', function(e) {
        var isCopy = (e.ctrlKey || e.metaKey) &&
          !e.altKey &&
          !e.shiftKey &&
          (e.key === 'c' || e.key === 'C' || e.keyCode === 67);
        if (!isCopy) return;
        if (s.items.length === 0) return;
        if (s.$control_input.val()) return;
        e.preventDefault();
        copyText(s.items.map(itemLabel).join('\\n'));
      });
      s.$control_input[0].addEventListener('paste', function(e) {
        var cd = (e.originalEvent || e).clipboardData;
        if (!cd) return;
        var txt = cd.getData('text');
        if (!txt) return;
        var vals = txt.split(/\\r?\\n|,|\\t|;/)
          .map(function(x) { return x.trim(); })
          .filter(Boolean);
        if (vals.length < 2) return;
        if (s.settings.maxItems === 1) return;
        e.preventDefault();
        e.stopPropagation();
        if (e.stopImmediatePropagation) e.stopImmediatePropagation();
        clearFeedback();
        resolveKeys(vals, function(keys) {
          var seen = {};
          var invalidSeen = {};
          var invalid = vals.filter(function(v) {
            if (lookupKey(v)) return false;
            if (invalidSeen[v]) return false;
            invalidSeen[v] = true;
            return true;
          });
          keys = keys.filter(function(key) {
            if (!key) return false;
            if (seen[key]) return false;
            seen[key] = true;
            return true;
          });
          if (keys.length === 0) {
            if (invalid.length) showFeedback(invalidMessage(invalid));
            return;
          }
          var next = s.items.slice();
          keys.forEach(function(key) {
            if (next.indexOf(key) === -1) next.push(key);
          });
          // Keep the selectize widget and Shiny input binding in sync so
          // downstream visualizations react after multi-value paste.
          s.setValue(next, false);
          s.setTextboxValue('');
          if (invalid.length) showFeedback(invalidMessage(invalid));
        });
      }, true);
    }"
  )
  default_selectize_options <- list(
    plugins = list("remove_button"),
    onInitialize = init_handler
  )
  utils::modifyList(default_selectize_options, selectizeOptions)
}
