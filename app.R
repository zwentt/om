# Total Factor Productivity (TFP) Interactive Analysis App
# ---------------------------------------------------------
# Requirements: shiny, bslib, shinyjs, DT
# Run with: shiny::runApp()

# --------------------
# Libraries
# --------------------
library(shiny)
library(bslib)
library(shinyjs)
library(DT)

# --------------------
# Helpers
# --------------------
currency <- function(x) {
  if (length(x) == 0 || is.na(x)) return("$0.00")
  paste0("$", formatC(x, format = "f", big.mark = ",", digits = 2))
}

pct <- function(x, digits = 1) {
  if (is.na(x)) return("-")
  paste0(sprintf("%0.*f", digits, x), "%")
}

kpi_box <- function(title, value, subtitle = NULL, level = "neutral") {
  colors <- list(
    good = "#0f9d58",  # green
    warn = "#f4b400",  # yellow
    bad  = "#db4437",  # red
    neutral = "#5f6368" # gray
  )
  bg <- colors[[level]] %||% colors$neutral
  div(
    class = "kpi-box",
    style = paste(
      "border-radius: 14px; padding: 18px 22px; color: white;",
      "box-shadow: 0 6px 18px rgba(0,0,0,0.08);",
      sprintf("background:%s;", bg)
    ),
    div(style = "font-size:14px; opacity:.9; font-weight:600; text-transform:uppercase; letter-spacing:.6px;", title),
    div(style = "font-size:44px; font-weight:800; line-height:1.15; margin-top:4px;", value),
    if (!is.null(subtitle)) div(style = "margin-top:6px; font-size:13px; opacity:.92;", subtitle)
  )
}

# default operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Resource panel UI factory
resourcePanel <- function(idx, default_name, panel_color) {
  ns <- function(id) paste0("res", idx, "_", id)
  card(
    class = "resource-card",
    style = sprintf("border-left: 6px solid %s;", panel_color),
    card_header(h5(sprintf("Resource %d", idx), class = "m-0")),
    layout_columns(
      col_widths = c(4, 4, 4),
      # Activation toggle & name
      div(
        class = "mb-2",
        checkboxInput(ns("active"), label = paste0("Activate ", default_name), value = idx <= 3)
      ),
      textInput(ns("name"), "Resource Name", value = default_name, placeholder = "e.g., Labor, Materials, Electricity"),
      # Unit Cost controls
      div(
        class = "mt-1",
        tags$label("Unit Cost ($/unit)"),
        sliderInput(ns("cost_slider"), NULL, min = 0, max = 1000, value = c(10, 50, 12, 1.5, 10)[idx], step = 0.01),
        numericInput(ns("cost_num"), NULL, value = c(10, 50, 0.12, 1.5, 10)[idx], min = 0, step = 0.01)
      ),
      # Units controls
      div(
        class = "mt-1",
        tags$label("Total Units"),
        sliderInput(ns("units_slider"), NULL, min = 0, max = 100000, value = c(3000, 2500, 50000, 1000, 200)[idx], step = 1),
        numericInput(ns("units_num"), NULL, value = c(3000, 2500, 50000, 1000, 200)[idx], min = 0, step = 1)
      ),
      # Calculated total
      div(
        class = "mt-2",
        uiOutput(ns("calc_total"))
      )
    )
  )
}

# --------------------
# UI
# --------------------
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Inter")
)

ui <- page_fluid(
  theme = app_theme,
  useShinyjs(),
  tags$head(tags$style(HTML("\n  .resource-card { margin-bottom: 16px; }\n  .calc-steps code { background: #f8f9fa; padding: 2px 6px; border-radius: 6px; }\n  .help-muted { color: #6c757d; }\n  .zone-title { font-weight: 700; font-size: 18px; margin-bottom: 8px; }\n  .card-header { background: #f7f9fc; }\n  @media (max-width: 768px){ .kpi-box div:nth-child(2){ font-size: 36px !important; } }\n"))),

  # Header Zone
  card(
    card_header(h3("Total Factor Productivity (TFP) – Interactive Analysis App", class = "m-0")),
    p(class = "help-muted m-0", "Explore how changes in input resources impact Total Factor Productivity in real time. TFP = Output / (Sum of Inputs).")
  ),

  layout_columns(
    col_widths = c(6, 6),

    # Left Column: Output + Resources
    card(
      card_header(div(class = "zone-title", "Output Zone")),
      layout_columns(
        col_widths = c(6, 6),
        div(
          tags$label("Output Value ($)"),
          sliderInput("output_slider", NULL, min = 0, max = 1000000, value = 250000, step = 500),
          numericInput("output_num", NULL, value = 250000, min = 0, step = 100)
        ),
        div(
          selectInput(
            "preset", "Example Presets",
            choices = c(
              "Custom (current)" = "custom",
              "Manufacturing – Labor Intensive" = "mfg_labor",
              "Services – Energy Efficient" = "services_energy",
              "Capital Heavy" = "capital_heavy"
            ), selected = "custom"
          ),
          helpText("Choose a preset to see a sample scenario. Modify any values to perform what‑if analysis.")
        )
      ),
      hr(),
      card_header(div(class = "zone-title", "Resource Input Zones"), p(class = "help-muted m-0", "Activate up to five resource categories. Each resource: Total = Unit Cost × Total Units.")),
      resourcePanel(1, "Labor", "#00b894"),
      resourcePanel(2, "Materials", "#0984e3"),
      resourcePanel(3, "Energy", "#fdcb6e"),
      resourcePanel(4, "Capital", "#e17055"),
      resourcePanel(5, "Miscellaneous", "#6c5ce7"),
      layout_columns(
        col_widths = c(6,6),
        actionButton("reset_current", "Reset Current Calculation", icon = icon("arrow-rotate-left"), class = "btn btn-outline-secondary"),
        actionButton("clear_history", "Clear History", icon = icon("trash"), class = "btn btn-outline-danger")
      )
    ),

    # Right Column: Calculation display + Save + History
    card(
      card_header(div(class = "zone-title", "Calculation Display Zone")),
      uiOutput("tfp_kpi"),
      br(),
      div(class = "calc-steps", htmlOutput("calc_steps")),
      div(class = "mt-3", p(class = "help-muted", "Interpretation: TFP > 1 suggests higher productivity (more output per dollar of input). TFP < 1 suggests room for efficiency improvements.")),
      layout_columns(
        col_widths = c(6, 6),
        actionButton("save_calc", "Save Calculation", icon = icon("floppy-disk"), class = "btn btn-success"),
        actionButton("new_calc", "New Calculation (Clear Inputs)", icon = icon("broom"), class = "btn btn-outline-primary")
      ),
      hr(),
      card_header(div(class = "zone-title", "History Zone")),
      p(class = "help-muted", "Saved calculations persist during this session. Compare scenarios and track % change in TFP."),
      DTOutput("history_table"),
      br(),
      p(class = "help-muted", "Tip: Click View to inspect the exact inputs used for any saved calculation.")
    )
  )
)

# --------------------
# Server
# --------------------
server <- function(input, output, session) {
  # ---------- Defaults & reactive state ----------
  rv <- reactiveValues(
    history = NULL,         # data.frame with ID, Timestamp, Output, Inputs, TFP, PctChange
    details = list(),       # per-ID list of resource details
    next_id = 1
  )

  # ---------- Synchronize sliders and numeric inputs ----------
  # Output sync
  observeEvent(input$output_slider, ignoreInit = TRUE, {
    if (!isTRUE(all.equal(input$output_num, input$output_slider))) {
      updateNumericInput(session, "output_num", value = input$output_slider)
    }
  })
  observeEvent(input$output_num, ignoreInit = TRUE, {
    if (!isTRUE(all.equal(input$output_slider, input$output_num))) {
      updateSliderInput(session, "output_slider", value = input$output_num)
    }
  })

  # Resource sync (cost & units) for each of the 5 resources
  for (i in 1:5) local({
    idx <- i
    ns <- function(id) paste0("res", idx, "_", id)

    # Toggle enable/disable based on active
    observe({
      active <- isTRUE(input[[ns("active")]])
      toggleState(ns("name"), condition = active)
      toggleState(ns("cost_slider"), condition = active)
      toggleState(ns("cost_num"), condition = active)
      toggleState(ns("units_slider"), condition = active)
      toggleState(ns("units_num"), condition = active)
    })

    # Cost sync
    observeEvent(input[[ns("cost_slider")]], ignoreInit = TRUE, {
      if (!isTRUE(all.equal(input[[ns("cost_num")]], input[[ns("cost_slider")]]))) {
        updateNumericInput(session, ns("cost_num"), value = input[[ns("cost_slider")]])
      }
    })
    observeEvent(input[[ns("cost_num")]], ignoreInit = TRUE, {
      if (!isTRUE(all.equal(input[[ns("cost_slider")]], input[[ns("cost_num")]]))) {
        updateSliderInput(session, ns("cost_slider"), value = input[[ns("cost_num")]])
      }
    })

    # Units sync
    observeEvent(input[[ns("units_slider")]], ignoreInit = TRUE, {
      if (!isTRUE(all.equal(input[[ns("units_num")]], input[[ns("units_slider")]]))) {
        updateNumericInput(session, ns("units_num"), value = input[[ns("units_slider")]])
      }
    })
    observeEvent(input[[ns("units_num")]], ignoreInit = TRUE, {
      if (!isTRUE(all.equal(input[[ns("units_slider")]], input[[ns("units_num")]]))) {
        updateSliderInput(session, ns("units_slider"), value = input[[ns("units_num")]])
      }
    })

    # Calculated total display
    output[[ns("calc_total")]] <- renderUI({
      active <- isTRUE(input[[ns("active")]])
      cost <- as.numeric(input[[ns("cost_num")]] %||% 0)
      units <- as.numeric(input[[ns("units_num")]] %||% 0)
      total <- if (active) cost * units else 0
      name <- input[[ns("name")]] %||% paste("Resource", idx)
      div(
        HTML(paste0(
          "<strong>Calculated Total</strong>: ", currency(total),
          if (active) paste0(
            "<div class='help-muted'>", htmltools::htmlEscape(name),
            ": ", currency(cost), " × ", format(units, big.mark=","),
            " = ", currency(total), "</div>") else
            "<div class='help-muted'>(Inactive)</div>"
        ))
      )
    })
  })

  # ---------- Preset scenarios ----------
  observeEvent(input$preset, {
    if (input$preset == "mfg_labor") {
      updateSliderInput(session, "output_slider", value = 300000)
      updateNumericInput(session, "output_num", value = 300000)
      # Labor heavy
      updateCheckboxInput(session, "res1_active", value = TRUE)
      updateTextInput(session, "res1_name", value = "Labor Hours")
      updateSliderInput(session, "res1_cost_slider", value = 25)
      updateNumericInput(session, "res1_cost_num", value = 25)
      updateSliderInput(session, "res1_units_slider", value = 6000)
      updateNumericInput(session, "res1_units_num", value = 6000)
      # Materials
      updateCheckboxInput(session, "res2_active", value = TRUE)
      updateTextInput(session, "res2_name", value = "Raw Materials")
      updateSliderInput(session, "res2_cost_slider", value = 50)
      updateNumericInput(session, "res2_cost_num", value = 50)
      updateSliderInput(session, "res2_units_slider", value = 2000)
      updateNumericInput(session, "res2_units_num", value = 2000)
      # Energy
      updateCheckboxInput(session, "res3_active", value = TRUE)
      updateTextInput(session, "res3_name", value = "Electricity (kWh)")
      updateSliderInput(session, "res3_cost_slider", value = 0.12)
      updateNumericInput(session, "res3_cost_num", value = 0.12)
      updateSliderInput(session, "res3_units_slider", value = 80000)
      updateNumericInput(session, "res3_units_num", value = 80000)
      # Capital
      updateCheckboxInput(session, "res4_active", value = TRUE)
      updateTextInput(session, "res4_name", value = "Capital (Machine Hours)")
      updateSliderInput(session, "res4_cost_slider", value = 2)
      updateNumericInput(session, "res4_cost_num", value = 2)
      updateSliderInput(session, "res4_units_slider", value = 2000)
      updateNumericInput(session, "res4_units_num", value = 2000)
      # Misc
      updateCheckboxInput(session, "res5_active", value = FALSE)
    }
    if (input$preset == "services_energy") {
      updateSliderInput(session, "output_slider", value = 180000)
      updateNumericInput(session, "output_num", value = 180000)
      updateCheckboxInput(session, "res1_active", value = TRUE)
      updateTextInput(session, "res1_name", value = "Labor Hours")
      updateSliderInput(session, "res1_cost_slider", value = 35)
      updateNumericInput(session, "res1_cost_num", value = 35)
      updateSliderInput(session, "res1_units_slider", value = 2500)
      updateNumericInput(session, "res1_units_num", value = 2500)
      updateCheckboxInput(session, "res2_active", value = TRUE)
      updateTextInput(session, "res2_name", value = "Supplies")
      updateSliderInput(session, "res2_cost_slider", value = 20)
      updateNumericInput(session, "res2_cost_num", value = 20)
      updateSliderInput(session, "res2_units_slider", value = 1500)
      updateNumericInput(session, "res2_units_num", value = 1500)
      updateCheckboxInput(session, "res3_active", value = TRUE)
      updateTextInput(session, "res3_name", value = "Electricity (kWh)")
      updateSliderInput(session, "res3_cost_slider", value = 0.10)
      updateNumericInput(session, "res3_cost_num", value = 0.10)
      updateSliderInput(session, "res3_units_slider", value = 30000)
      updateNumericInput(session, "res3_units_num", value = 30000)
      updateCheckboxInput(session, "res4_active", value = FALSE)
      updateCheckboxInput(session, "res5_active", value = FALSE)
    }
    if (input$preset == "capital_heavy") {
      updateSliderInput(session, "output_slider", value = 400000)
      updateNumericInput(session, "output_num", value = 400000)
      updateCheckboxInput(session, "res1_active", value = TRUE)
      updateTextInput(session, "res1_name", value = "Labor Hours")
      updateSliderInput(session, "res1_cost_slider", value = 20)
      updateNumericInput(session, "res1_cost_num", value = 20)
      updateSliderInput(session, "res1_units_slider", value = 2000)
      updateNumericInput(session, "res1_units_num", value = 2000)
      updateCheckboxInput(session, "res2_active", value = TRUE)
      updateTextInput(session, "res2_name", value = "Materials")
      updateSliderInput(session, "res2_cost_slider", value = 35)
      updateNumericInput(session, "res2_cost_num", value = 35)
      updateSliderInput(session, "res2_units_slider", value = 1800)
      updateNumericInput(session, "res2_units_num", value = 1800)
      updateCheckboxInput(session, "res3_active", value = TRUE)
      updateTextInput(session, "res3_name", value = "Electricity (kWh)")
      updateSliderInput(session, "res3_cost_slider", value = 0.14)
      updateNumericInput(session, "res3_cost_num", value = 0.14)
      updateSliderInput(session, "res3_units_slider", value = 60000)
      updateNumericInput(session, "res3_units_num", value = 60000)
      updateCheckboxInput(session, "res4_active", value = TRUE)
      updateTextInput(session, "res4_name", value = "Capital (Machine Hours)")
      updateSliderInput(session, "res4_cost_slider", value = 5)
      updateNumericInput(session, "res4_cost_num", value = 5)
      updateSliderInput(session, "res4_units_slider", value = 10000)
      updateNumericInput(session, "res4_units_num", value = 10000)
      updateCheckboxInput(session, "res5_active", value = FALSE)
    }
  })

  # ---------- Current calculation reactive ----------
  resource_data <- reactive({
    res <- lapply(1:5, function(idx){
      ns <- function(id) paste0("res", idx, "_", id)
      active <- isTRUE(input[[ns("active")]])
      list(
        idx = idx,
        active = active,
        name = (input[[ns("name")]] %||% paste("Resource", idx)),
        cost = if (active) as.numeric(input[[ns("cost_num")]] %||% 0) else 0,
        units = if (active) as.numeric(input[[ns("units_num")]] %||% 0) else 0
      )
    })
    do.call(rbind, lapply(res, function(x){
      data.frame(
        idx = x$idx,
        active = x$active,
        name = x$name,
        cost = as.numeric(x$cost),
        units = as.numeric(x$units),
        total = as.numeric(x$cost) * as.numeric(x$units),
        stringsAsFactors = FALSE
      )
    }))
  })

  calc_values <- reactive({
    out_val <- as.numeric(input$output_num %||% 0)
    rd <- resource_data()
    total_inputs <- sum(pmax(0, rd$total), na.rm = TRUE)
    list(
      output = out_val,
      total_inputs = total_inputs,
      tfp = if (total_inputs > 0) out_val / total_inputs else NA_real_,
      resources = rd
    )
  })

  # ---------- TFP KPI & Calculation steps ----------
  output$tfp_kpi <- renderUI({
    cv <- calc_values()
    tfp <- cv$tfp
    level <- if (is.na(tfp)) "neutral" else if (tfp > 1.01) "good" else if (tfp < 0.99) "bad" else "warn"
    kpi_box(
      title = "TFP (Output / Total Inputs)",
      value = if (is.na(tfp)) "N/A" else sprintf("%.4f", tfp),
      subtitle = paste0("Output: ", currency(cv$output), " · Total Inputs: ", currency(cv$total_inputs)),
      level = level
    )
  })

  output$calc_steps <- renderText({
    cv <- calc_values()
    rd <- cv$resources
    step1 <- paste0(
      apply(rd, 1, function(r){
        nm <- htmltools::htmlEscape(r["name"]) ; ct <- as.numeric(r["cost"]) ; un <- as.numeric(r["units"]) ; tt <- as.numeric(r["total"]) ; act <- as.logical(r["active"]) ;
        if (act) sprintf(
          "<code>%s</code>: %s × %s = <strong>%s</strong>",
          nm, currency(ct), format(un, big.mark=","), currency(tt)
        ) else sprintf("<code>%s</code>: (inactive)", nm)
      }), collapse = "<br>"
    )
    step2 <- sprintf("<strong>Total Inputs</strong> = %s", currency(cv$total_inputs))
    step3 <- if (is.na(cv$tfp)) {
      "<strong>TFP</strong> = Output ÷ Total Inputs = <em>undefined (Total Inputs must be > 0)</em>"
    } else {
      sprintf("<strong>TFP</strong> = %s ÷ %s = <strong>%s</strong>", currency(cv$output), currency(cv$total_inputs), sprintf("%.4f", cv$tfp))
    }
    HTML(paste(step1, "<hr>", step2, "<br>", step3))
  })

  # Enable Save only when valid
  observe({
    cv <- calc_values()
    valid <- is.finite(cv$tfp) && cv$tfp >= 0
    toggleState("save_calc", condition = valid)
  })

  # ---------- Save Calculation & History ----------
  observeEvent(input$save_calc, {
    cv <- calc_values()
    if (!(is.finite(cv$tfp) && cv$total_inputs > 0)) return(NULL)

    # compute pct change vs previous
    prev_tfp <- if (!is.null(rv$history) && nrow(rv$history) > 0) tail(rv$history$TFP, 1) else NA_real_
    pct_change <- if (is.na(prev_tfp) || prev_tfp == 0) NA_real_ else (cv$tfp - prev_tfp) / prev_tfp * 100

    # Build new row
    id <- rv$next_id
    ts <- format(Sys.time(), "%m/%d/%Y %H:%M")
    new_row <- data.frame(
      ID = id,
      `Calculation #` = paste0("Calc #", id),
      Timestamp = ts,
      `Output ($)` = cv$output,
      `Total Inputs ($)` = cv$total_inputs,
      `TFP` = round(cv$tfp, 4),
      `%% Change` = pct_change,
      stringsAsFactors = FALSE
    )

    # Append with limit 50
    if (is.null(rv$history)) rv$history <- new_row else rv$history <- rbind(rv$history, new_row)
    if (nrow(rv$history) > 50) {
      # drop oldest
      drop_id <- rv$history$ID[1]
      rv$history <- rv$history[-1, , drop = FALSE]
      rv$details[[as.character(drop_id)]] <- NULL
    }

    # Store resource details
    rv$details[[as.character(id)]] <- list(
      output = cv$output,
      resources = cv$resources
    )

    rv$next_id <- id + 1

    showNotification("Calculation saved to history.", type = "message")
  })

  # New calculation (clear inputs to defaults but keep history)
  observeEvent(input$new_calc, {
    updateSliderInput(session, "output_slider", value = 250000)
    updateNumericInput(session, "output_num", value = 250000)
    # Reset resources to initial defaults
    defaults <- list(
      list(name="Labor", cost=10, units=3000, active=TRUE),
      list(name="Materials", cost=50, units=2500, active=TRUE),
      list(name="Energy", cost=0.12, units=50000, active=TRUE),
      list(name="Capital", cost=1.5, units=1000, active=TRUE),
      list(name="Miscellaneous", cost=10, units=200, active=TRUE)
    )
    for (i in 1:5) {
      ns <- function(id) paste0("res", i, "_", id)
      updateCheckboxInput(session, ns("active"), value = defaults[[i]]$active)
      updateTextInput(session, ns("name"), value = defaults[[i]]$name)
      updateSliderInput(session, ns("cost_slider"), value = defaults[[i]]$cost)
      updateNumericInput(session, ns("cost_num"), value = defaults[[i]]$cost)
      updateSliderInput(session, ns("units_slider"), value = defaults[[i]]$units)
      updateNumericInput(session, ns("units_num"), value = defaults[[i]]$units)
    }
  })

  # Reset current calculation (same as new_calc but keep names/actives)
  observeEvent(input$reset_current, {
    # Keep active flags and names; reset numeric values
    for (i in 1:5) {
      ns <- function(id) paste0("res", i, "_", id)
      # Reset costs and units to modest defaults
      updateSliderInput(session, ns("cost_slider"), value = 10)
      updateNumericInput(session, ns("cost_num"), value = 10)
      updateSliderInput(session, ns("units_slider"), value = 1000)
      updateNumericInput(session, ns("units_num"), value = 1000)
    }
    updateSliderInput(session, "output_slider", value = 100000)
    updateNumericInput(session, "output_num", value = 100000)
  })

  # Clear history with confirmation
  observeEvent(input$clear_history, {
    showModal(modalDialog(
      title = "Clear History",
      "This will remove all saved calculations. Proceed?",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Yes, clear", class = "btn btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_clear, {
    removeModal()
    rv$history <- NULL
    rv$details <- list()
    rv$next_id <- 1
    showNotification("History cleared.", type = "message")
  })

  # ---------- History Table Rendering with per-row actions ----------
  output$history_table <- renderDT({
    df <- rv$history
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No saved calculations yet."), rownames = FALSE, options = list(dom = 't')))
    }

    # Display columns with formatting
    view_btn <- '<button class="btn btn-sm btn-outline-primary view">View</button>'
    del_btn  <- '<button class="btn btn-sm btn-outline-danger delete">Delete</button>'

    dt <- df
    dt$`Output ($)` <- sapply(dt$`Output ($)`, currency)
    dt$`Total Inputs ($)` <- sapply(dt$`Total Inputs ($)`, currency)
    dt$`TFP` <- sprintf("%.4f", dt$`TFP`)
    dt$`% Change` <- sapply(dt$`% Change`, function(x){
      if (is.na(x)) return("-")
      sign <- ifelse(x > 0, "+", ifelse(x < 0, "-", ""))
      col <- ifelse(x > 0, "#0f9d58", ifelse(x < 0, "#db4437", "#5f6368"))
      paste0('<span style="color:', col, '">', sign, sprintf("%.1f", abs(x)), '%</span>')
    })
    dt$Action <- paste(view_btn, del_btn)

    datatable(
      dt[, c("ID", "Calculation #", "Timestamp", "Output ($)", "Total Inputs ($)", "TFP", "% Change", "Action")],
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      class = "stripe hover",
      options = list(
        dom = 'tip',
        pageLength = 8,
        columnDefs = list(
          list(visible = FALSE, targets = 0), # hide ID
          list(orderable = FALSE, targets = ncol(dt))
        ),
        # JS to capture button clicks
        fnDrawCallback = htmlwidgets::JS(
          "function(){",
          "  var table = this.api();",
          "  table.table().off('click', 'button.view');",
          "  table.table().off('click', 'button.delete');",
          "  table.table().on('click', 'button.view', function(){",
          "    var $tr = $(this).closest('tr');",
          "    var row = table.row($tr);",
          "    var id = row.data()[0];",
          "    Shiny.setInputValue('history_action', {type:'view', id:id, nonce:Math.random()});",
          "  });",
          "  table.table().on('click', 'button.delete', function(){",
          "    var $tr = $(this).closest('tr');",
          "    var row = table.row($tr);",
          "    var id = row.data()[0];",
          "    Shiny.setInputValue('history_action', {type:'delete', id:id, nonce:Math.random()});",
          "  });",
          "}"
        )
      )
    )
  })

  # Handle per-row actions
  observeEvent(input$history_action, {
    action <- input$history_action$type
    id <- as.integer(input$history_action$id)
    if (is.na(id)) return(NULL)

    if (action == 'view') {
      det <- rv$details[[as.character(id)]]
      if (is.null(det)) return(NULL)
      rd <- det$resources
      # Build details HTML
      rows <- apply(rd, 1, function(r){
        if (as.logical(r["active"])) {
          sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                  htmltools::htmlEscape(r["name"]), currency(as.numeric(r["cost"])),
                  format(as.numeric(r["units"]), big.mark=","), currency(as.numeric(r["total"])) )
        } else {
          sprintf("<tr class='text-muted'><td>%s</td><td colspan='3'>(inactive)</td></tr>", htmltools::htmlEscape(r["name"]))
        }
      })
      showModal(modalDialog(
        title = paste0("Details for Calc #", id),
        HTML(paste0(
          "<p><strong>Output:</strong> ", currency(det$output), "</p>",
          "<table class='table table-sm'><thead><tr><th>Resource</th><th>Unit Cost</th><th>Units</th><th>Total</th></tr></thead><tbody>",
          paste(rows, collapse = ""), "</tbody></table>"
        )),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }

    if (action == 'delete') {
      showModal(modalDialog(
        title = paste0("Delete Calc #", id, "?"),
        "This will permanently remove the selected calculation from history.",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Delete", class = "btn btn-danger")
        )
      ))
      # store ID pending deletion
      rv$pending_delete_id <- id
    }
  })

  observeEvent(input$confirm_delete, {
    removeModal()
    id <- rv$pending_delete_id
    if (is.null(id)) return(NULL)
    keep <- rv$history$ID != id
    rv$history <- rv$history[keep, , drop = FALSE]
    rv$details[[as.character(id)]] <- NULL
    rv$pending_delete_id <- NULL
    showNotification(paste0("Deleted Calc #", id, "."), type = "warning")
  })
}

# --------------------
# App
# --------------------
shinyApp(ui, server)
