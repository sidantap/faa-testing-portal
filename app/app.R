# Drug and Alcohol Testing Portal: a from-scratch rebuild with made-up data.
# Runs entirely in the browser with Shinylive; changes last for the visit only.
library(shiny)
library(bslib)
source("randomizer.R")

read_data <- function(f) read.csv(file.path("data", f), stringsAsFactors = FALSE, check.names = FALSE)
EMP_COLS <- c("EmployeeID", "Name", "Occupation", "Base", "Status", "HireDate")
BASES <- c("CVG", "MIA", "ANC", "ORD", "LAX")
STATUSES <- c("Active", "LOA", "Terminated")
YEAR <- 2026
now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M")
occ_label <- function(code) unname(OCCUPATIONS[code])
csv_download <- function(name, data_fn) downloadHandler(
  filename = function() if (is.function(name)) name() else name,
  content = function(file) write.csv(data_fn(), file, row.names = FALSE)
)

kpi <- function(title, output_id, note = NULL) value_box(
  title = title, value = textOutput(output_id, inline = TRUE),
  if (!is.null(note)) p(note, class = "small mb-0"), theme = "light"
)

ui <- page_navbar(
  title = "Drug and Alcohol Testing Portal",
  theme = bs_theme(version = 5, primary = "#1f4e79", "navbar-bg" = "#14213d", base_font = font_collection("system-ui", "sans-serif")),
  window_title = "DAT Portal demo",
  header = div(class = "container-fluid pt-2",
               div(class = "alert alert-secondary py-2 mb-2 small",
                   "Demo with made-up employees. Rebuilt from scratch from an internal app I built at Atlas Air; changes reset when you reload.")),

  nav_panel("Home",
    layout_columns(col_widths = c(8, 4),
      h4("Key Performance Indicators", class = "mt-2"),
      div(class = "text-end mt-2",
          actionButton("edit_goals", "Edit annual goals", class = "btn-sm btn-outline-primary"),
          actionButton("show_hc", "Quarterly headcount", class = "btn-sm btn-outline-primary"))
    ),
    layout_column_wrap(width = "200px", fill = FALSE,
      kpi("Total active employees", "total_employees"),
      kpi("Annual drug goal", "drug_goal"),
      kpi("Annual alcohol goal", "alcohol_goal"),
      kpi("Last qtr rollover", "rollover_last_quarter"),
      kpi("Quarterly drug goal", "quarter_drug_goal"),
      kpi("Quarterly alcohol goal", "quarter_combo_goal"),
      kpi("Completed this quarter", "completed_this_quarter")
    ),
    card(
      card_header("Randomizer Configuration"),
      layout_columns(col_widths = c(3, 3, 3, 3),
        numericInput("combo_percent", "Drug + alcohol % per year", 10, min = 0, max = 100),
        numericInput("drug_percent", "Drug % per year", 25, min = 0, max = 100),
        selectInput("quarter", "Quarter", paste0("Q", 1:4), selected = "Q4"),
        div(class = "pt-4", actionButton("run_randomizer", "Generate Random Selection", class = "btn-primary w-100"))
      )
    ),
    card(
      card_header("Random Selection Results"),
      uiOutput("results_summary"),
      tableOutput("results_table"),
      uiOutput("results_actions")
    )
  ),

  nav_panel("Audit Log",
    card(
      card_header("Audit Log – Randomizer Runs"),
      layout_columns(col_widths = c(3, 3, 6),
        dateInput("date_from", "From", value = paste0(YEAR, "-01-01")),
        dateInput("date_to", "To", value = Sys.Date()),
        div(class = "pt-4", downloadButton("download_audit", "Download audit log", class = "btn-outline-primary"))
      ),
      tableOutput("audit_table")
    ),
    card(card_header("Employee changes"), tableOutput("change_table"))
  ),

  nav_panel("Employee Management",
    layout_columns(col_widths = c(6, 6),
      card(
        card_header("Manage Employee Pool"),
        fileInput("file_upload", "Upload an employee list (CSV)", accept = ".csv"),
        div(downloadButton("download_template", "Template", class = "btn-sm btn-outline-secondary"),
            downloadButton("download_master_pool", "Master pool", class = "btn-sm btn-outline-secondary")),
        uiOutput("upload_result")
      ),
      card(
        card_header("Add New Employee"),
        layout_columns(col_widths = c(6, 6),
          textInput("employee_name", "Full name"),
          selectInput("employee_occupation", "Occupation", setNames(names(OCCUPATIONS), OCCUPATIONS)),
          selectInput("employee_base", "Base", BASES),
          selectInput("employee_status", "Status", STATUSES)
        ),
        div(actionButton("clear_form", "Clear Form", class = "btn-outline-secondary"),
            actionButton("add_employee", "Add Employee", class = "btn-primary"))
      )
    ),
    card(
      card_header("Employee pool"),
      layout_columns(col_widths = c(3, 3, 6),
        selectInput("occupation_filter", "Occupation", c(All = "", setNames(names(OCCUPATIONS), OCCUPATIONS))),
        selectInput("base_filter", "Base", c(All = "", BASES)),
        radioButtons("status_filter", "Status", c("All Employees" = "", "Active", "Leave of Absence (LOA)" = "LOA", "Terminated"), inline = TRUE)
      ),
      layout_columns(col_widths = c(6, 6),
        selectizeInput("pick_employee", "Select an employee to change", choices = NULL,
                       options = list(placeholder = "Type a name or ID")),
        div(class = "pt-4",
            actionButton("edit_employee", "Edit", class = "btn-outline-primary"),
            actionButton("remove_employee", "Remove", class = "btn-outline-danger"),
            actionButton("rehire_employee", "Rehire", class = "btn-outline-success"))
      ),
      tableOutput("employee_table")
    )
  )
)

server <- function(input, output, session) {
  db <- reactiveValues(
    employees = read_data("employees.csv"),
    history   = read_data("selection_history.csv"),
    completed = read_data("completed_tests.csv"),
    audit     = read_data("audit_log.csv"),
    settings  = read_data("settings.csv"),
    changes   = data.frame(ChangedAt = character(), EmployeeID = character(), Change = character()),
    result    = NULL
  )
  log_change <- function(id, what) db$changes <- rbind(data.frame(ChangedAt = now_stamp(), EmployeeID = id, Change = what), db$changes)
  qnum <- reactive(as.integer(sub("Q", "", input$quarter)))

  # Saved annual goals feed the configuration inputs
  observe({
    s <- isolate(db$settings); s <- s[s$Year == YEAR, ]
    if (nrow(s)) {
      updateNumericInput(session, "drug_percent", value = s$DrugPct[1])
      updateNumericInput(session, "combo_percent", value = s$DrugAlcoholPct[1])
    }
  })

  plan <- reactive({
    req(input$drug_percent, input$combo_percent)
    quarter_plan(db$employees, db$history, db$completed, db$audit, YEAR, qnum(),
                 input$drug_percent, input$combo_percent)
  })

  # ---- KPIs ----
  output$total_employees <- renderText(plan()$headcount_now)
  output$drug_goal <- renderText(plan()$annual_drug)
  output$alcohol_goal <- renderText(plan()$annual_combo)
  output$rollover_last_quarter <- renderText(sum(plan()$rollover))
  output$quarter_drug_goal <- renderText(plan()$base_drug)
  output$quarter_combo_goal <- renderText(plan()$base_combo)
  output$completed_this_quarter <- renderText({
    c <- db$completed; sum(c$Year == YEAR & c$Quarter == qnum() & c$TestStatus == "completed")
  })

  observeEvent(input$edit_goals, showModal(modalDialog(
    title = sprintf("Edit Annual Goals — %s", YEAR),
    numericInput("modal_drug", "Drug % per year", input$drug_percent, 0, 100),
    numericInput("modal_combo", "Drug + alcohol % per year", input$combo_percent, 0, 100),
    footer = tagList(modalButton("Cancel"), actionButton("confirm_goals", paste("Save for", YEAR), class = "btn-primary"))
  )))
  observeEvent(input$confirm_goals, {
    db$settings <- rbind(db$settings[db$settings$Year != YEAR, ],
                         data.frame(Year = YEAR, DrugPct = input$modal_drug, DrugAlcoholPct = input$modal_combo))
    updateNumericInput(session, "drug_percent", value = input$modal_drug)
    updateNumericInput(session, "combo_percent", value = input$modal_combo)
    removeModal()
    showNotification("Annual goals saved.", type = "message")
  })

  observeEvent(input$show_hc, {
    p <- plan(); a <- db$audit[db$audit$Year == YEAR & db$audit$Quarter < qnum(), ]
    hc <- rbind(data.frame(Quarter = paste0("Q", a$Quarter), Headcount = a$PoolSize, Source = "recorded at that draw"),
                data.frame(Quarter = input$quarter, Headcount = p$headcount_now, Source = "current active pool"))
    showModal(modalDialog(
      title = sprintf("Quarterly Headcount — %s (Q1..Q%s)", YEAR, qnum()),
      renderTable(hc),
      p(sprintf("Average headcount: %.1f. Annual goals are this average times each testing rate, rounded up.", p$avg_headcount)),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  # ---- Randomizer ----
  observeEvent(input$run_randomizer, {
    p <- plan()
    if (p$headcount_now == 0) return(showNotification("No active employees to select from.", type = "error"))
    seed <- sample.int(99999, 1)
    db$result <- list(plan = p, seed = seed, quarter = qnum(), at = now_stamp(),
                      selection = draw_selection(p, seed), saved = FALSE)
  })

  output$results_summary <- renderUI({
    r <- db$result
    if (is.null(r)) return(div(class = "text-center text-muted py-4",
      h5("Awaiting Configuration"), p("Set the rates and quarter, then generate a random selection.")))
    s <- r$selection; p <- r$plan
    div(class = "d-flex flex-wrap gap-2 mb-2",
      span(class = "badge text-bg-primary", sum(s$TestFor == COMBO), " drug + alcohol"),
      span(class = "badge text-bg-secondary", sum(s$TestFor == DRUG), " drug only"),
      span(class = "badge text-bg-light", "pool ", p$headcount_now),
      span(class = "badge text-bg-light", "rollover ", p$rollover[["combo"]], " d+a / ", p$rollover[["drug"]], " drug"),
      span(class = "badge text-bg-light", "cap ", p$cap),
      span(class = "badge text-bg-light", "seed ", r$seed))
  })
  output$results_table <- renderTable({
    req(db$result); s <- db$result$selection
    transform(s, Occupation = occ_label(s$Occupation))
  })
  output$results_actions <- renderUI({
    req(db$result)
    div(class = "d-flex gap-2",
        downloadButton("download_results", "Download Results", class = "btn-outline-primary"),
        actionButton("save_results", if (db$result$saved) "Saved" else "Save Results", class = "btn-success",
                     disabled = if (db$result$saved) "disabled" else NULL),
        actionButton("clear_results", "Clear Results", class = "btn-outline-secondary"))
  })
  output$download_results <- csv_download(
    function() paste0("selection_", YEAR, "_Q", db$result$quarter, ".csv"),
    function() db$result$selection
  )
  observeEvent(input$save_results, {
    r <- db$result; req(r, !r$saved)
    db$history <- rbind(db$history, data.frame(EmployeeID = r$selection$EmployeeID, Year = YEAR, Quarter = r$quarter,
                                               TestFor = r$selection$TestFor, SelectedAt = r$at))
    db$audit <- rbind(db$audit, data.frame(RunAt = r$at, Year = YEAR, Quarter = r$quarter, PoolSize = r$plan$headcount_now,
                                           DrugAlcohol = sum(r$selection$TestFor == COMBO), DrugOnly = sum(r$selection$TestFor == DRUG),
                                           Seed = r$seed, User = "you (demo)"))
    db$result$saved <- TRUE
    showNotification("Saved to selection history and the audit log.", type = "message")
  })
  observeEvent(input$clear_results, db$result <- NULL)

  # ---- Audit log ----
  audit_view <- reactive({
    a <- db$audit; d <- as.Date(substr(a$RunAt, 1, 10))
    a <- a[d >= input$date_from & d <= input$date_to, ]
    a[order(a$RunAt, decreasing = TRUE), ]
  })
  output$audit_table <- renderTable(audit_view())
  output$download_audit <- csv_download("audit_log.csv", audit_view)
  output$change_table <- renderTable({
    if (nrow(db$changes)) db$changes else data.frame(Note = "No employee changes yet in this session.")
  })

  # ---- Employee management ----
  emp_view <- reactive({
    e <- db$employees
    if (nzchar(input$occupation_filter)) e <- e[e$Occupation == input$occupation_filter, ]
    if (nzchar(input$base_filter)) e <- e[e$Base == input$base_filter, ]
    if (nzchar(input$status_filter)) e <- e[e$Status == input$status_filter, ]
    e
  })
  output$employee_table <- renderTable({
    e <- emp_view(); transform(e, Occupation = occ_label(e$Occupation))
  })
  observe({
    e <- db$employees
    updateSelectizeInput(session, "pick_employee", choices = setNames(e$EmployeeID, paste(e$EmployeeID, e$Name, "·", e$Status)),
                         selected = isolate(input$pick_employee), server = FALSE)
  })
  picked <- reactive({ req(input$pick_employee); db$employees[db$employees$EmployeeID == input$pick_employee, ] })

  observeEvent(input$clear_form, {
    updateTextInput(session, "employee_name", value = "")
    updateSelectInput(session, "employee_status", selected = "Active")
  })
  observeEvent(input$add_employee, {
    name <- trimws(input$employee_name)
    if (!nzchar(name)) return(showNotification("Enter a name first.", type = "warning"))
    ids <- as.integer(sub("E", "", db$employees$EmployeeID))
    id <- paste0("E", max(ids) + 1)
    db$employees <- rbind(data.frame(EmployeeID = id, Name = name, Occupation = input$employee_occupation, Base = input$employee_base,
                                     Status = input$employee_status, HireDate = as.character(Sys.Date())), db$employees)
    log_change(id, paste("added as", input$employee_status, occ_label(input$employee_occupation), "at", input$employee_base))
    updateTextInput(session, "employee_name", value = "")
    showNotification(paste("Added", name, "as", id), type = "message")
  })

  observeEvent(input$edit_employee, {
    e <- picked()
    showModal(modalDialog(
      title = paste("Edit", e$Name, "·", e$EmployeeID),
      selectInput("edit_occupation", "Occupation", setNames(names(OCCUPATIONS), OCCUPATIONS), e$Occupation),
      selectInput("edit_base", "Base", BASES, e$Base),
      selectInput("edit_status", "Status", STATUSES, e$Status),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit", "Save Changes", class = "btn-primary"))
    ))
  })
  observeEvent(input$confirm_edit, {
    e <- picked(); i <- db$employees$EmployeeID == e$EmployeeID
    diffs <- c(if (e$Occupation != input$edit_occupation) paste("occupation", e$Occupation, "→", input$edit_occupation),
               if (e$Base != input$edit_base) paste("base", e$Base, "→", input$edit_base),
               if (e$Status != input$edit_status) paste("status", e$Status, "→", input$edit_status))
    db$employees$Occupation[i] <- input$edit_occupation
    db$employees$Base[i] <- input$edit_base
    db$employees$Status[i] <- input$edit_status
    if (length(diffs)) log_change(e$EmployeeID, paste(diffs, collapse = "; "))
    removeModal()
  })
  observeEvent(input$remove_employee, {
    e <- picked()
    if (e$Status == "Terminated") return(showNotification("Already terminated.", type = "warning"))
    showModal(modalDialog(title = "Confirm Removal",
      p(sprintf("Mark %s (%s) as terminated? They leave the testing pool but stay on record.", e$Name, e$EmployeeID)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_remove", "Remove Employee", class = "btn-danger"))))
  })
  observeEvent(input$confirm_remove, {
    e <- picked(); db$employees$Status[db$employees$EmployeeID == e$EmployeeID] <- "Terminated"
    log_change(e$EmployeeID, paste("status", e$Status, "→ Terminated")); removeModal()
  })
  observeEvent(input$rehire_employee, {
    e <- picked()
    if (e$Status != "Terminated") return(showNotification("Only terminated employees can be rehired.", type = "warning"))
    showModal(modalDialog(title = "Confirm Rehire",
      p(sprintf("Rehire %s (%s) as active? They rejoin the testing pool.", e$Name, e$EmployeeID)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_rehire", "Rehire Employee", class = "btn-success"))))
  })
  observeEvent(input$confirm_rehire, {
    e <- picked(); db$employees$Status[db$employees$EmployeeID == e$EmployeeID] <- "Active"
    log_change(e$EmployeeID, "status Terminated → Active (rehired)"); removeModal()
  })

  output$download_template <- csv_download("employee_upload_template.csv",
    function() data.frame(EmployeeID = "E2001", Name = "Jordan Lee", Occupation = "FCR", Base = "CVG", Status = "Active", HireDate = "2026-01-15"))
  output$download_master_pool <- csv_download("master_employee_pool.csv", function() db$employees)

  output$upload_result <- renderUI(NULL)
  observeEvent(input$file_upload, {
    up <- tryCatch(read.csv(input$file_upload$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    problems <- c(
      if (is.null(up)) "The file could not be read as CSV.",
      if (!is.null(up) && !all(EMP_COLS %in% names(up))) paste("Missing columns:", paste(setdiff(EMP_COLS, names(up)), collapse = ", ")),
      if (!is.null(up) && all(EMP_COLS %in% names(up)) && any(!up$Occupation %in% names(OCCUPATIONS))) "Occupation must be one of ACD, ACM, FCR, FIN, GSC.",
      if (!is.null(up) && all(EMP_COLS %in% names(up)) && any(!up$Status %in% STATUSES)) "Status must be Active, LOA or Terminated."
    )
    if (length(problems)) {
      output$upload_result <- renderUI(div(class = "alert alert-danger mt-2 mb-0", tags$ul(lapply(problems, tags$li))))
      return()
    }
    up <- up[, EMP_COLS]
    existing <- up$EmployeeID %in% db$employees$EmployeeID
    for (k in which(existing)) {
      i <- db$employees$EmployeeID == up$EmployeeID[k]
      if (db$employees$Status[i] != up$Status[k]) log_change(up$EmployeeID[k], paste("status", db$employees$Status[i], "→", up$Status[k], "(upload)"))
      db$employees[i, EMP_COLS] <- up[k, EMP_COLS]
    }
    if (any(!existing)) {
      db$employees <- rbind(up[!existing, ], db$employees)
      for (id in up$EmployeeID[!existing]) log_change(id, "added (upload)")
    }
    output$upload_result <- renderUI(div(class = "alert alert-success mt-2 mb-0",
      sprintf("%d rows processed: %d updated, %d added.", nrow(up), sum(existing), sum(!existing))))
  })
}

shinyApp(ui, server)
