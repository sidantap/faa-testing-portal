# Drug and Alcohol Testing Portal: a from-scratch rebuild with made-up data.
# Layout follows the original app: dark header with tab buttons, KPI cards with
# annual/quarterly ribbons, and card sections. Runs in the browser with Shinylive;
# changes last for the visit only.
library(shiny)
source("randomizer.R")

read_data <- function(f) read.csv(file.path("data", f), stringsAsFactors = FALSE, check.names = FALSE)
EMP_COLS <- c("EmployeeID", "Name", "Occupation", "Base", "Position", "Status", "HireDate")
BASES <- c("JFK", "MIA", "CVG", "ANC")
QUARTERS <- c("Q1 - January to March" = "Q1", "Q2 - April to June" = "Q2",
              "Q3 - July to September" = "Q3", "Q4 - October to December" = "Q4")
YEAR <- 2026
now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M")
h <- function(x) htmltools::htmlEscape(as.character(x))
csv_download <- function(name, data_fn) downloadHandler(
  filename = function() if (is.function(name)) name() else name,
  content = function(file) write.csv(data_fn(), file, row.names = FALSE)
)
field <- function(label, input) div(class = "input-group", tags$label(class = "input-label", label), input)

# An HTML table with optional per-column cell formatters
html_table <- function(df, format = list()) {
  if (!nrow(df)) return(NULL)
  head <- tags$tr(lapply(names(df), tags$th))
  rows <- lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(names(df), function(col) {
    v <- df[[col]][i]
    tags$td(if (!is.null(format[[col]])) HTML(format[[col]](v, df[i, ])) else v)
  })))
  div(class = "table-wrap", tags$table(class = "dat", tags$thead(head), tags$tbody(rows)))
}
test_pill <- function(v, row) sprintf('<span class="test-pill %s">%s</span>', if (v == COMBO) "test-combo" else "test-drug", h(v))
status_badge <- function(v, row = NULL) sprintf('<span class="status-badge status-%s">%s</span>', tolower(v), h(v))

kpi_card <- function(title, value_id, icon, ribbon = NULL, badge_id = NULL, id = NULL, clickable = NULL, tone = "") {
  div(class = paste("kpi-card", if (!is.null(clickable)) "clickable"), id = id, title = clickable,
      if (!is.null(ribbon)) div(class = paste0("kpi-top-bar kpi-top-", tolower(ribbon)), ribbon),
      div(class = "kpi-header", div(class = paste("kpi-icon-wrapper", tone), icon), h3(class = "kpi-title", title)),
      p(class = "kpi-value", textOutput(value_id, inline = TRUE)),
      if (!is.null(badge_id)) div(class = "kpi-badge", uiOutput(badge_id, inline = TRUE)))
}

ui <- fluidPage(
  title = "Drug and Alcohol Testing Portal",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('nav', function(page) {
        document.querySelectorAll('.nav-tab').forEach(function(b) { b.classList.toggle('active', b.dataset.page === page); });
      });
      document.addEventListener('dblclick', function(e) {
        if (e.target.closest('#kpi_annual_drug_card, #kpi_annual_combo_card')) Shiny.setInputValue('kpi_goals', Date.now(), {priority: 'event'});
      });
      document.addEventListener('click', function(e) {
        if (e.target.closest('#kpi_headcount_card')) Shiny.setInputValue('kpi_hc', Date.now(), {priority: 'event'});
        var b = e.target.closest('[data-row-action]');
        if (b) Shiny.setInputValue('row_action', {id: b.dataset.id, act: b.dataset.rowAction}, {priority: 'event'});
      });
    "))
  ),

  div(class = "header",
    div(class = "header-content",
      div(class = "logo-section",
        div(class = "logo-mark", "DAT"),
        h1(class = "dashboard-title", "DRUG AND ALCOHOL TESTING PORTAL",
           span(class = "dashboard-sub", "FAA random testing, demo with made-up employees"))),
      div(class = "nav-tabs",
        actionButton("nav_home", "Home", class = "nav-tab active", `data-page` = "home"),
        actionButton("nav_audit", "Audit Log", class = "nav-tab", `data-page` = "audit"),
        actionButton("nav_employees", "Employee Management", class = "nav-tab", `data-page` = "employees"))
    )
  ),

  div(class = "main-content",
    p(class = "demo-note", "Rebuilt from scratch from an internal app I built at Atlas Air. Every employee here is made up, and changes reset when you reload."),

    # ---------------- Home ----------------
    conditionalPanel("output.current_page == 'home'", div(style = "display:grid;gap:20px",
      div(class = "card",
        div(class = "section-header",
          h2(class = "section-title", "Key Performance Indicators"),
          span(class = "hint", "Double-click an annual goal to change the rates. Click Total Active Employees for the headcount behind them.")),
        div(class = "kpi-grid",
          kpi_card("Total Active Employees", "total_employees", "HC", badge_id = "avg_headcount",
                   id = "kpi_headcount_card", clickable = "Click to see quarterly headcount"),
          kpi_card("Annual Drug Goal", "drug_goal", "D", "ANNUAL", "completed_annual_drug",
                   id = "kpi_annual_drug_card", clickable = "Double-click to configure"),
          kpi_card("Annual Alcohol Goal", "alcohol_goal", "D&A", "ANNUAL", "completed_annual_combo",
                   id = "kpi_annual_combo_card", clickable = "Double-click to configure"),
          kpi_card("Last Qtr Rollover", "rollover_last_quarter", "!", badge_id = "rollover_split", tone = "amber"),
          kpi_card("Quarterly Drug Goal", "quarter_drug_goal", "D", "QUARTERLY", "completed_quarterly_drug", tone = "teal"),
          kpi_card("Quarterly Alcohol Goal", "quarter_combo_goal", "D&A", "QUARTERLY", "completed_quarterly_combo", tone = "teal")
        )
      ),
      div(class = "card",
        h2(class = "section-title", "Randomizer Configuration"),
        div(class = "form-grid",
          field("Drug & Alcohol (Combo) Test %", numericInput("combo_percent", NULL, 10, min = 0, max = 100)),
          field("Drug Test %", numericInput("drug_percent", NULL, 25, min = 0, max = 100)),
          field("Quarter", selectInput("quarter", NULL, QUARTERS, selected = "Q4", selectize = FALSE))
        ),
        div(class = "button-container", actionButton("run_randomizer", "Generate Random Selection", class = "run-button"))
      ),
      div(class = "card", uiOutput("results_panel"))
    )),

    # ---------------- Audit log ----------------
    conditionalPanel("output.current_page == 'audit'", div(style = "display:grid;gap:20px",
      div(class = "page-header", h2(class = "section-title", "Audit Log – Randomizer Runs"),
          downloadButton("download_audit", "Download Audit Log", class = "download-button")),
      div(class = "card", div(class = "audit-filter-row-three",
        field("From", dateInput("date_from", NULL, value = paste0(YEAR, "-01-01"))),
        field("To", dateInput("date_to", NULL, value = Sys.Date())),
        field("Performed By", textInput("performed_by", NULL, placeholder = "Enter a user name")))),
      div(class = "card", uiOutput("audit_table")),
      div(class = "card", h2(class = "section-title", "Employee Changes"), uiOutput("change_table"))
    )),

    # ---------------- Employee management ----------------
    conditionalPanel("output.current_page == 'employees'", div(style = "display:grid;gap:20px",
      div(class = "page-header",
        h2(class = "section-title", "Manage Employee Pool"),
        div(class = "page-actions",
          downloadLink("download_template", "Download template", class = "template-link"),
          fileInput("file_upload", NULL, accept = ".csv", buttonLabel = "Upload CSV", placeholder = NULL),
          downloadButton("download_master_pool", "Download Master Pool", class = "download-button"))),
      uiOutput("upload_result"),
      div(class = "card",
        h2(class = "section-title", "Add New Employee"),
        div(class = "form-grid-6",
          field("Employee ID", textInput("employee_id", NULL, placeholder = "Auto if blank")),
          field("Full Name", textInput("employee_name", NULL, placeholder = "First Last")),
          field("Occupation", selectInput("employee_occupation", NULL, names(OCCUPATIONS), selectize = FALSE)),
          field("Base", selectInput("employee_base", NULL, BASES, selectize = FALSE)),
          field("Status", selectInput("employee_status", NULL, c("Active" = "Active", "Leave of Absence (LOA)" = "LOA"), selectize = FALSE)),
          field("Position", textInput("position", NULL, placeholder = "e.g. First Officer"))
        ),
        div(class = "form-actions",
          actionButton("clear_form", "Clear Form", class = "btn-clear"),
          actionButton("add_employee", "Add Employee", class = "btn-atlas-primary"))
      ),
      div(class = "card",
        div(class = "filter-controls",
          field("Search", textInput("search_filter", NULL, placeholder = "ID, name, occupation, position")),
          field("Occupations", selectInput("occupation_filter", NULL, c("All Occupations" = "all", names(OCCUPATIONS)), selectize = FALSE)),
          field("Base", selectInput("base_filter", NULL, c("All Bases" = "all", BASES), selectize = FALSE))),
        uiOutput("status_tabs"),
        div(style = "margin-top:16px", uiOutput("employee_table"))
      )
    ))
  )
)

server <- function(input, output, session) {
  db <- reactiveValues(
    employees = read_data("employees.csv"),
    history   = read_data("selection_history.csv"),
    completed = read_data("completed_tests.csv"),
    audit     = read_data("audit_log.csv"),
    changes   = data.frame(ChangedAt = character(), EmployeeID = character(), Change = character(), By = character()),
    result    = NULL
  )
  log_change <- function(id, what) db$changes <- rbind(data.frame(ChangedAt = now_stamp(), EmployeeID = id, Change = what, By = "you (demo)"), db$changes)

  # ---- navigation ----
  page <- reactiveVal("home")
  output$current_page <- reactive(page())
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
  go <- function(p) { page(p); session$sendCustomMessage("nav", p) }
  observeEvent(input$nav_home, go("home"))
  observeEvent(input$nav_audit, go("audit"))
  observeEvent(input$nav_employees, go("employees"))

  # ---- goals and KPIs ----
  qnum <- reactive(as.integer(sub("Q", "", input$quarter)))
  plan <- reactive({
    req(input$drug_percent, input$combo_percent)
    quarter_plan(db$employees, db$history, db$completed, db$audit, YEAR, qnum(), input$drug_percent, input$combo_percent)
  })
  done_count <- function(quarters, test = NULL) {
    c <- db$completed; hs <- db$history[db$history$Year == YEAR & db$history$Quarter %in% quarters, ]
    if (!is.null(test)) hs <- hs[hs$TestFor == test, ]
    sum(hs$EmployeeID %in% c$EmployeeID[c$Year == YEAR & c$Quarter %in% quarters & c$TestStatus == "completed"])
  }
  badge <- function(n) HTML(sprintf("<b>%s</b> completed", n))

  output$total_employees <- renderText(plan()$headcount_now)
  output$avg_headcount <- renderUI(HTML(sprintf("<b>%s</b> avg headcount", ceiling(plan()$avg_headcount))))
  output$drug_goal <- renderText(plan()$annual_drug)
  output$alcohol_goal <- renderText(plan()$annual_combo)
  output$completed_annual_drug <- renderUI(badge(done_count(1:4)))   # every test includes a drug test
  output$completed_annual_combo <- renderUI(badge(done_count(1:4, COMBO)))
  output$rollover_last_quarter <- renderText(sum(plan()$rollover))
  output$rollover_split <- renderUI(HTML(sprintf("<b>%s</b> drug, <b>%s</b> combo", plan()$rollover[["drug"]], plan()$rollover[["combo"]])))
  output$quarter_drug_goal <- renderText(plan()$base_drug)
  output$quarter_combo_goal <- renderText(plan()$base_combo)
  output$completed_quarterly_drug <- renderUI(badge(done_count(qnum())))
  output$completed_quarterly_combo <- renderUI(badge(done_count(qnum(), COMBO)))

  observeEvent(input$kpi_goals, showModal(modalDialog(
    title = sprintf("Edit Annual Goals — %s", YEAR),
    p("Testing rates for the year. Annual goals are the average headcount times each rate, rounded up."),
    div(class = "form-grid",
      field("Drug & Alcohol (Combo) Test %", numericInput("modal_combo", NULL, input$combo_percent, 0, 100)),
      field("Drug Test %", numericInput("modal_drug", NULL, input$drug_percent, 0, 100))),
    footer = tagList(modalButton("Cancel"), actionButton("confirm_goals", paste("Save for", YEAR), class = "btn-atlas-primary")),
    easyClose = TRUE
  )))
  observeEvent(input$confirm_goals, {
    updateNumericInput(session, "drug_percent", value = input$modal_drug)
    updateNumericInput(session, "combo_percent", value = input$modal_combo)
    removeModal(); showNotification(sprintf("Annual goals saved for %s.", YEAR), type = "message")
  })
  observeEvent(input$kpi_hc, {
    p <- plan(); a <- db$audit[db$audit$Year == YEAR & db$audit$Quarter < qnum(), ]
    hc <- data.frame(Quarter = c(paste0("Q", a$Quarter), input$quarter), Headcount = c(a$PoolSize, p$headcount_now),
                     Source = c(rep("recorded at that quarter's draw", nrow(a)), "current active pool"))
    showModal(modalDialog(
      title = sprintf("Quarterly Headcount — %s (Q1..Q%s)", YEAR, qnum()),
      html_table(hc),
      p(style = "margin-top:12px", sprintf("Average headcount: %.1f", p$avg_headcount)),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  # ---- randomizer ----
  observeEvent(input$run_randomizer, {
    p <- plan()
    if (p$headcount_now == 0) return(showNotification("There are no active employees to select from.", type = "error"))
    seed <- sample.int(99999, 1)
    db$result <- list(plan = p, seed = seed, quarter = qnum(), at = now_stamp(), selection = draw_selection(p, seed), saved = FALSE)
  })
  output$results_panel <- renderUI({
    r <- db$result
    if (is.null(r)) return(div(class = "placeholder-content",
      div(class = "placeholder-icon", HTML('<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" aria-hidden="true"><rect x="5" y="3" width="14" height="18" rx="2"/><path d="M9 3h6v3H9zM8.5 11h7M8.5 15h5"/></svg>')),
      h3(class = "placeholder-title", "Awaiting Configuration"),
      p(class = "placeholder-text", "Configure and run the randomizer to generate employee selection results.")))
    s <- r$selection
    tagList(
      h2(class = "section-title", "Random Selection Results"),
      p(class = "run-meta", sprintf("%s Q%s, pool of %s, rollover %s drug and %s combo, cap %s, seed %s, run %s",
        YEAR, r$quarter, r$plan$headcount_now, r$plan$rollover[["drug"]], r$plan$rollover[["combo"]], r$plan$cap, r$seed, r$at)),
      div(class = "summary-grid",
        div(class = "summary-card", span(class = "summary-value", sum(s$TestFor == DRUG)), span(class = "summary-label", "Drug Only Tests")),
        div(class = "summary-card", span(class = "summary-value", sum(s$TestFor == COMBO)), span(class = "summary-label", "Combo Tests")),
        div(class = "summary-card total", span(class = "summary-value", nrow(s)), span(class = "summary-label", "Total Selected"))),
      html_table(setNames(s, c("Employee ID", "Name", "Occupation", "Base", "Position", "Test For")), list(`Test For` = test_pill)),
      div(class = "results-actions",
        downloadButton("download_results", "Download Results", class = "download-button"),
        actionButton("save_results", if (r$saved) "Saved" else "Save Results", class = "action-button save",
                     disabled = if (r$saved) "disabled" else NULL),
        actionButton("clear_results", "Clear Results", class = "action-button clear"))
    )
  })
  output$download_results <- csv_download(function() paste0("selection_", YEAR, "_Q", db$result$quarter, ".csv"), function() db$result$selection)
  observeEvent(input$save_results, {
    r <- db$result; req(r, !r$saved)
    db$history <- rbind(db$history, data.frame(EmployeeID = r$selection$EmployeeID, Year = YEAR, Quarter = r$quarter, TestFor = r$selection$TestFor, SelectedAt = r$at))
    db$audit <- rbind(db$audit, data.frame(RunAt = r$at, Year = YEAR, Quarter = r$quarter, PoolSize = r$plan$headcount_now,
      DrugAlcohol = sum(r$selection$TestFor == COMBO), DrugOnly = sum(r$selection$TestFor == DRUG), Seed = r$seed, User = "you (demo)"))
    db$result$saved <- TRUE
    showNotification("Saved to the selection history and audit log.", type = "message")
  })
  observeEvent(input$clear_results, db$result <- NULL)

  # ---- audit log ----
  audit_view <- reactive({
    a <- db$audit; d <- as.Date(substr(a$RunAt, 1, 10))
    keep <- d >= input$date_from & d <= input$date_to
    if (nzchar(input$performed_by)) keep <- keep & grepl(input$performed_by, a$User, ignore.case = TRUE, fixed = TRUE)
    a <- a[keep, ]; a[order(a$RunAt, decreasing = TRUE), ]
  })
  output$audit_table <- renderUI({
    a <- audit_view()
    if (!nrow(a)) return(p(class = "hint", "No randomizer runs match these filters."))
    a$Period <- paste0(a$Year, " Q", a$Quarter)
    html_table(setNames(a[, c("RunAt", "Period", "PoolSize", "DrugAlcohol", "DrugOnly", "Seed", "User")],
                        c("Run At", "Period", "Pool Size", "Combo", "Drug Only", "Seed", "Performed By")))
  })
  output$download_audit <- csv_download("audit_log.csv", audit_view)
  output$change_table <- renderUI({
    if (!nrow(db$changes)) return(p(class = "hint", "No employee changes yet in this session. Add, edit or remove someone in Employee Management."))
    html_table(setNames(db$changes, c("Changed At", "Employee ID", "Change", "By")))
  })

  # ---- employee management ----
  status_filter <- reactiveVal("all")
  PAGE_SIZE <- 10
  emp_page <- reactiveVal(1)
  observeEvent(list(input$search_filter, input$occupation_filter, input$base_filter, status_filter()), emp_page(1))
  observeEvent(input$emp_prev, emp_page(max(1, emp_page() - 1)))
  observeEvent(input$emp_next, emp_page(emp_page() + 1))
  filtered <- reactive({
    e <- db$employees
    if (input$occupation_filter != "all") e <- e[e$Occupation == input$occupation_filter, ]
    if (input$base_filter != "all") e <- e[e$Base == input$base_filter, ]
    q <- trimws(input$search_filter)
    if (nzchar(q)) e <- e[grepl(q, paste(e$EmployeeID, e$Name, e$Occupation, e$Position), ignore.case = TRUE, fixed = TRUE), ]
    e
  })
  output$status_tabs <- renderUI({
    e <- filtered()
    counts <- c(all = nrow(e), Active = sum(e$Status == "Active"), LOA = sum(e$Status == "LOA"), Terminated = sum(e$Status == "Terminated"))
    labels <- c(all = "All Employees", Active = "Active", LOA = "Leave of Absence (LOA)", Terminated = "Terminated")
    div(class = "status-tabs", lapply(names(labels), function(s)
      actionButton(paste0("status_", tolower(s)), tagList(labels[[s]], span(class = "count", counts[[s]])),
                   class = paste("status-tab", if (status_filter() == s) "active"))))
  })
  observeEvent(input$status_all, status_filter("all"))
  observeEvent(input$status_active, status_filter("Active"))
  observeEvent(input$status_loa, status_filter("LOA"))
  observeEvent(input$status_terminated, status_filter("Terminated"))
  output$employee_table <- renderUI({
    e <- filtered(); if (status_filter() != "all") e <- e[e$Status == status_filter(), ]
    if (!nrow(e)) return(p(class = "hint", "No employees match these filters."))
    pages <- ceiling(nrow(e) / PAGE_SIZE); pg <- min(emp_page(), pages)
    shown <- e[((pg - 1) * PAGE_SIZE + 1):min(pg * PAGE_SIZE, nrow(e)), ]
    total <- nrow(e); e <- shown
    e$Actions <- e$EmployeeID
    tagList(
      html_table(setNames(e[, c("EmployeeID", "Name", "Occupation", "Base", "Position", "Status", "Actions")],
                          c("Employee ID", "Name", "Occupation", "Base", "Position", "Status", "Actions")),
        list(Status = status_badge, Actions = function(id, row) {
          paste0(sprintf('<button type="button" class="row-btn" data-row-action="edit" data-id="%s">Edit</button>', id),
                 if (row$Status == "Terminated") sprintf('<button type="button" class="row-btn ok" data-row-action="rehire" data-id="%s">Rehire</button>', id)
                 else sprintf('<button type="button" class="row-btn danger" data-row-action="remove" data-id="%s">Remove</button>', id))
        })),
      div(class = "pager",
        span(class = "table-foot", sprintf("Showing %s to %s of %s employees", (pg - 1) * PAGE_SIZE + 1, (pg - 1) * PAGE_SIZE + nrow(e), total)),
        div(actionButton("emp_prev", "Previous", disabled = if (pg <= 1) "disabled" else NULL),
            span(class = "table-foot", sprintf(" Page %s of %s ", pg, pages)),
            actionButton("emp_next", "Next", disabled = if (pg >= pages) "disabled" else NULL))))
  })

  observeEvent(input$clear_form, {
    for (id in c("employee_id", "employee_name", "position")) updateTextInput(session, id, value = "")
    updateSelectInput(session, "employee_status", selected = "Active")
  })
  observeEvent(input$add_employee, {
    name <- trimws(input$employee_name)
    if (!nzchar(name)) return(showNotification("Enter the employee's full name.", type = "warning"))
    id <- trimws(input$employee_id)
    if (!nzchar(id)) id <- paste0("E", max(as.integer(sub("\\D+", "", db$employees$EmployeeID)), na.rm = TRUE) + 1)
    if (id %in% db$employees$EmployeeID) return(showNotification(paste("Employee ID", id, "is already in the pool."), type = "warning"))
    db$employees <- rbind(data.frame(EmployeeID = id, Name = name, Occupation = input$employee_occupation, Base = input$employee_base,
      Position = trimws(input$position), Status = input$employee_status, HireDate = as.character(Sys.Date())), db$employees)
    log_change(id, sprintf("added as %s %s at %s", input$employee_status, input$employee_occupation, input$employee_base))
    for (f in c("employee_id", "employee_name", "position")) updateTextInput(session, f, value = "")
    showNotification(sprintf("Added %s (%s).", name, id), type = "message")
  })

  person <- function(id) db$employees[db$employees$EmployeeID == id, ]
  person_card <- function(e) div(class = "confirm-person", HTML(status_badge(e$Status)),
    div(tags$b(e$Name), span(class = "hint", sprintf("%s, %s, %s, %s", e$EmployeeID, unname(OCCUPATIONS[e$Occupation]), e$Base, e$Position))))
  target <- reactiveVal(NULL)
  observeEvent(input$row_action, {
    a <- input$row_action; e <- person(a$id); req(nrow(e) == 1); target(a$id)
    if (a$act == "edit") showModal(modalDialog(title = paste("Edit Employee —", e$Name),
      div(class = "form-grid",
        field("Occupation", selectInput("edit_occupation", NULL, names(OCCUPATIONS), e$Occupation, selectize = FALSE)),
        field("Base", selectInput("edit_base", NULL, BASES, e$Base, selectize = FALSE)),
        field("Position", textInput("edit_position", NULL, e$Position)),
        field("Status", selectInput("edit_status", NULL, c("Active", "LOA", "Terminated"), e$Status, selectize = FALSE))),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit", "Save Changes", class = "btn-atlas-primary")), easyClose = TRUE))
    if (a$act == "remove") showModal(modalDialog(title = "Confirm Removal",
      div(class = "confirm-body", p("This employee leaves the testing pool. Their record and history stay on file."), person_card(e)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_remove", "Remove Employee", class = "btn btn-danger"))))
    if (a$act == "rehire") showModal(modalDialog(title = "Confirm Rehire",
      div(class = "confirm-body", p("This employee rejoins the testing pool as Active."), person_card(e)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_rehire", "Rehire Employee", class = "btn btn-success"))))
  })
  set_fields <- function(id, values, note) {
    i <- db$employees$EmployeeID == id; old <- db$employees[i, ]
    diffs <- unlist(lapply(names(values), function(k) if (!identical(as.character(old[[k]]), as.character(values[[k]])))
      sprintf("%s %s to %s", tolower(k), old[[k]], values[[k]])))
    for (k in names(values)) db$employees[i, k] <- values[[k]]
    if (length(diffs)) log_change(id, paste0(paste(diffs, collapse = "; "), note))
  }
  observeEvent(input$confirm_edit, { set_fields(target(), list(Occupation = input$edit_occupation, Base = input$edit_base,
    Position = input$edit_position, Status = input$edit_status), ""); removeModal() })
  observeEvent(input$confirm_remove, { set_fields(target(), list(Status = "Terminated"), " (removed)"); removeModal() })
  observeEvent(input$confirm_rehire, { set_fields(target(), list(Status = "Active"), " (rehired)"); removeModal() })

  output$download_template <- csv_download("employee_upload_template.csv", function()
    data.frame(EmployeeID = "E2001", Name = "Jordan Lee", Occupation = "FCR", Base = "CVG", Position = "First Officer", Status = "Active", HireDate = "2026-01-15"))
  output$download_master_pool <- csv_download("master_employee_pool.csv", function() db$employees)

  output$upload_result <- renderUI(NULL)
  observeEvent(input$file_upload, {
    up <- tryCatch(read.csv(input$file_upload$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    problems <- if (is.null(up)) "The file could not be read as CSV." else c(
      if (!all(EMP_COLS %in% names(up))) paste("Missing columns:", paste(setdiff(EMP_COLS, names(up)), collapse = ", ")),
      if (all(EMP_COLS %in% names(up)) && any(!up$Occupation %in% names(OCCUPATIONS))) "Occupation must be ACD, ACM, FCR, FIN or GSC.",
      if (all(EMP_COLS %in% names(up)) && any(!up$Status %in% c("Active", "LOA", "Terminated"))) "Status must be Active, LOA or Terminated.")
    if (length(problems)) {
      output$upload_result <- renderUI(div(class = "card", style = "border-color:var(--red)",
        tags$b("Upload not applied."), lapply(problems, div), div(class = "hint", "Download the template to see the expected columns.")))
      return()
    }
    up <- up[, EMP_COLS]; existing <- up$EmployeeID %in% db$employees$EmployeeID
    for (k in which(existing)) set_fields(up$EmployeeID[k], as.list(up[k, setdiff(EMP_COLS, "EmployeeID")]), " (upload)")
    if (any(!existing)) { db$employees <- rbind(up[!existing, ], db$employees); for (id in up$EmployeeID[!existing]) log_change(id, "added (upload)") }
    output$upload_result <- renderUI(div(class = "card", style = "border-color:var(--green)",
      sprintf("Upload applied: %d rows, %d updated, %d added. Changes are listed in the Audit Log.", nrow(up), sum(existing), sum(!existing))))
  })
}

shinyApp(ui, server)
