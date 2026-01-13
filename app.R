# =========================
# ParkEase (SQLITE + EDIT ACTIVE RESERVATIONS + ANALYTICS + HISTORY + SIDEBAR NAV)
# - SQLite (RSQLite) backend
# - Auto-create tables + indexes
# - Fresh DB connection per query
# - Edit Active reservations + End w/ ticket + Analytics
# - Sidebar navigation (functional)
# - Reservation History page + Filters + Export Excel/PDF
# - FIX: Ticket prints clean (only ticket, no dashboard UI)
# - FIX: No JS leakage on login
# - UI polish: subtle animations + cleaner analytics plots on dark UI
# =========================

library(shiny)
library(shinyWidgets)
library(DT)
library(DBI)
library(RSQLite)
library(digest)
library(ggplot2)
library(plotly)

APP_TZ <- Sys.getenv("APP_TZ", "Asia/Manila")
Sys.setenv(TZ = APP_TZ)

# ---------------- LOGIN CREDENTIALS ----------------
ADMIN_USER <- "admin"
ADMIN_PASS_HASH <- digest("parkease123", algo = "sha256")

# ================== SQLITE DB HELPERS (DEPLOY-SAFE) ==================
SEED_DB <- Sys.getenv("SEED_DB", "parkease_seed.sqlite")
LIVE_DB <- Sys.getenv("LIVE_DB", "parkease_live.sqlite")

get_db_path <- function() {
  live_path <- file.path(tempdir(), LIVE_DB)
  if (!file.exists(live_path)) {
    seed_path <- file.path(getwd(), SEED_DB)
    if (file.exists(seed_path)) file.copy(seed_path, live_path, overwrite = TRUE)
  }
  live_path
}

get_con <- function() {
  db_path <- get_db_path()
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  try(dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
  try(dbExecute(con, "PRAGMA foreign_keys = ON;"), silent = TRUE)
  try(dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
  con
}

db_get <- function(sql, params = NULL) {
  con <- NULL
  on.exit({ if (!is.null(con)) try(dbDisconnect(con), silent = TRUE) }, add = TRUE)
  con <- get_con()
  if (is.null(params)) dbGetQuery(con, sql) else dbGetQuery(con, sql, params = params)
}

db_exec <- function(sql, params = NULL) {
  con <- NULL
  on.exit({ if (!is.null(con)) try(dbDisconnect(con), silent = TRUE) }, add = TRUE)
  con <- get_con()
  if (is.null(params)) dbExecute(con, sql) else dbExecute(con, sql, params = params)
}

ensure_tables <- function() {
  db_exec(paste(
    "CREATE TABLE IF NOT EXISTS parking_slots (",
    "  slot_id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "  slot_no TEXT NOT NULL UNIQUE,",
    "  zone    TEXT NOT NULL,",
    "  type    TEXT NOT NULL,",
    "  rate    REAL NOT NULL DEFAULT 0,",
    "  is_available INTEGER NOT NULL DEFAULT 1,",
    "  created_at TEXT DEFAULT (datetime('now'))",
    ");",
    sep = "\n"
  ))
  
  db_exec(paste(
    "CREATE TABLE IF NOT EXISTS reservations (",
    "  reservation_id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "  slot_id INTEGER NOT NULL,",
    "  vehicle_no TEXT NOT NULL,",
    "  driver_name TEXT NOT NULL,",
    "  start_time TEXT NOT NULL,",
    "  time_out TEXT NULL,",
    "  duration_hours INTEGER NULL,",
    "  total_fee REAL NULL,",
    "  status TEXT NOT NULL DEFAULT 'Active',",
    "  created_at TEXT DEFAULT (datetime('now')),",
    "  FOREIGN KEY(slot_id) REFERENCES parking_slots(slot_id)",
    ");",
    sep = "\n"
  ))
  
  db_exec("CREATE INDEX IF NOT EXISTS idx_reservations_slot_id ON reservations(slot_id);")
  db_exec("CREATE INDEX IF NOT EXISTS idx_reservations_status  ON reservations(status);")
  db_exec("CREATE INDEX IF NOT EXISTS idx_reservations_start   ON reservations(start_time);")
  db_exec("CREATE INDEX IF NOT EXISTS idx_reservations_timeout ON reservations(time_out);")
}

# --------- TIME HELPERS ----------
to_posix <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  suppressWarnings(as.POSIXct(x, tz = Sys.timezone()))
}
fmt_ts <- function(x) format(x, "%Y-%m-%d %H:%M:%S")

rate_for_type <- function(type) {
  switch(type,
         "Regular"  = 30,
         "Compact"  = 20,
         "Electric" = 60,
         "Disabled" = 0,
         30)
}

# --------- DARK PLOT THEME (Analytics UI fix) ----------
theme_dark_dashboard <- function() {
  bg    <- "#020617"
  fg    <- "#E5E7EB"
  muted <- "#CBD5F5"
  gridc <- grDevices::adjustcolor("#94A3B8", alpha.f = 0.12)
  
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      legend.background = element_rect(fill = bg, color = NA),
      legend.key = element_rect(fill = bg, color = NA),
      
      text = element_text(color = fg, family = "Segoe UI"),
      plot.title = element_text(face = "bold", size = 14, color = fg),
      axis.title = element_text(color = muted),
      axis.text  = element_text(color = muted),
      
      panel.grid.major = element_line(color = gridc),
      panel.grid.minor = element_blank(),
      
      axis.ticks = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(rel="stylesheet",
              href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),
    
    # ✅ Only JS here (no CSS). Prevents code leakage on login.
    tags$script(HTML("
      // 👁 Password toggle (safe)
      $(document).on('click', '#toggle_pass', function() {
        var input = document.getElementById('login_pass');
        if (!input) return;

        if (input.type === 'password') {
          input.type = 'text';
          this.classList.remove('fa-eye');
          this.classList.add('fa-eye-slash');
        } else {
          input.type = 'password';
          this.classList.remove('fa-eye-slash');
          this.classList.add('fa-eye');
        }
      });

      // Session persistence - store login when successful
      window.storeLoginSession = function() {
        localStorage.setItem('parkease_logged_in', 'true');
        localStorage.setItem('parkease_session_time', new Date().getTime());
      };

      window.clearLoginSession = function() {
        localStorage.removeItem('parkease_logged_in');
        localStorage.removeItem('parkease_session_time');
      };

      window.hasLoginSession = function() {
        return localStorage.getItem('parkease_logged_in') === 'true';
      };

      // Custom message handlers for login persistence
      Shiny.addCustomMessageHandler('store_session', function(message) {
        window.storeLoginSession();
      });

      Shiny.addCustomMessageHandler('clear_session', function(message) {
        window.clearLoginSession();
      });

      // Check for session on app load and notify Shiny
      $(document).ready(function() {
        setTimeout(function() {
          if (window.hasLoginSession()) {
            Shiny.setInputValue('restore_session', true, {priority: 'event'});
          }
        }, 100);
      });

      // Ticket print (CSS handles printing ONLY the ticket)
      window.printTicket = function() {
        window.focus();
        window.print();
      };
    "))
  ),
  uiOutput("app_ui")
)

# ---------------- SERVER ----------------
server <- function(input, output, session){
  
  logged_in <- reactiveVal(FALSE)
  refresh <- reactiveVal(0)
  current_page <- reactiveVal("overview") # overview | add | reserve | history
  
  tryCatch(ensure_tables(), error = function(e) {
    showNotification(paste("DB init error:", conditionMessage(e)), type="error", duration=NULL)
  })
  
  # ---------- LOGIN UI ----------
  login_ui <- function(){
    div(class="login-wrap",
        div(class="login-card",
            h2("ParkEase Login", class="login-title"),
            textInput("login_user","Username"),
            div(style="position:relative;",
                passwordInput("login_pass","Password"),
                tags$i(class="fa fa-eye", id="toggle_pass",
                       style="position:absolute; right:12px; top:38px; cursor:pointer; color:#9ca3af;")
            ),
            actionButton("login_btn","Sign In", class="btn-primary"),
            br(), br(),
            textOutput("login_error")
        )
    )
  }
  
  # ---------- SIDEBAR ----------
  sidebar_ui <- function() {
    pg <- current_page()
    btn_class <- function(name) if (identical(pg, name)) "side-btn active" else "side-btn"
    
    div(class="sidebar",
        actionButton("nav_overview","Overview", class=btn_class("overview")),
        hr(),
        actionButton("nav_add","➕ Add Slot", class=btn_class("add")),
        actionButton("nav_reserve","⏱ Reserve Slot", class=btn_class("reserve")),
        actionButton("nav_history","🧾 Reservation History", class=btn_class("history"))
    )
  }
  
  # ---------- PAGE UIs ----------
  page_overview_ui <- function() {
    tagList(
      h2("Parking Management"),
      p("Monitor and manage parking slots in real time"),
      
      div(class="stats-row",
          div(class="stat-card green", span("Total Slots"), h1(textOutput("total_slots"))),
          div(class="stat-card blue", span("Available"), h1(textOutput("available_slots"))),
          div(class="stat-card orange", span("Occupied"), h1(textOutput("occupied_slots"))),
          div(class="stat-card purple", span("Revenue Today"), h1(textOutput("revenue_today")))
      ),
      
      div(class="form-row",
          div(class="panel",
              h3("Add Parking Slot"),
              textInput("slot_no","Slot Number","A-101"),
              selectInput("zone","Zone",c("Zone A","Zone B","Zone C")),
              selectInput("type","Type",c("Regular","Compact","Electric","Disabled")),
              actionButton("add_slot","Add Slot",class="btn-primary")
          ),
          
          div(class="panel",
              h3("Reserve Slot"),
              selectInput("res_zone","Zone",c("Zone A","Zone B","Zone C"), selected="Zone A"),
              selectInput("res_type","Type",c("Regular","Compact","Electric","Disabled"), selected="Regular"),
              selectInput("slot_sel","Available Slot",choices=c("Loading..."="")),
              textInput("vehicle","Vehicle Number"),
              textInput("driver","Driver Name"),
              actionButton("reserve","Reserve Slot",class="btn-success")
          )
      ),
      
      div(class="panel panel-bright",
          h3("🕒 Active Reservations"),
          DTOutput("active_table")
      ),
      
      div(class="panel panel-bright",
          h3("📊 Analytics"),
          tabsetPanel(
            tabPanel("Revenue",
                     plotlyOutput("daily_revenue", height=260),
                     plotlyOutput("weekly_revenue", height=260),
                     plotlyOutput("monthly_revenue", height=260)
            ),
            tabPanel("Most Used Zones",
                     plotlyOutput("zone_usage", height=320)
            ),
            tabPanel("Peak Hours",
                     plotlyOutput("peak_hours", height=320)
            )
          )
      )
    )
  }
  
  page_add_ui <- function() {
    tagList(
      h2("➕ Add Slot"),
      p("Create new parking slots and review the slot list."),
      div(class="panel",
          h3("Add Parking Slot"),
          textInput("slot_no","Slot Number","A-101"),
          selectInput("zone","Zone",c("Zone A","Zone B","Zone C")),
          selectInput("type","Type",c("Regular","Compact","Electric","Disabled")),
          actionButton("add_slot","Add Slot",class="btn-primary")
      ),
      div(class="panel panel-bright",
          h3("🅿 Parking Slots"),
          DTOutput("slots_table")
      )
    )
  }
  
  page_reserve_ui <- function() {
    tagList(
      h2("⏱ Reserve Slot"),
      p("Reserve available slots and manage active reservations."),
      div(class="panel",
          h3("Reserve Slot"),
          selectInput("res_zone","Zone",c("Zone A","Zone B","Zone C"), selected="Zone A"),
          selectInput("res_type","Type",c("Regular","Compact","Electric","Disabled"), selected="Regular"),
          selectInput("slot_sel","Available Slot",choices=c("Loading..."="")),
          textInput("vehicle","Vehicle Number"),
          textInput("driver","Driver Name"),
          actionButton("reserve","Reserve Slot",class="btn-success")
      ),
      div(class="panel panel-bright",
          h3("🕒 Active Reservations"),
          DTOutput("active_table")
      )
    )
  }
  
  page_history_ui <- function() {
    tagList(
      h2("🧾 Reservation History"),
      p("View completed reservations with filters and export options."),
      
      div(class="panel",
          h3("Filters"),
          fluidRow(
            column(6,
                   dateRangeInput("hist_date","Date Range (Time Out)",
                                  start = Sys.Date() - 30, end = Sys.Date())
            ),
            column(2,
                   selectInput("hist_zone","Zone",
                               choices = c("All Zones","Zone A","Zone B","Zone C"),
                               selected = "All Zones")
            ),
            column(2,
                   selectInput("hist_type","Type",
                               choices = c("All Types","Regular","Compact","Electric","Disabled"),
                               selected = "All Types")
            ),
            column(2,
                   textInput("hist_vehicle","Vehicle", placeholder = "e.g., ABC-123")
            )
          ),
          fluidRow(
            column(6, selectInput("export_format", "Export Format", 
                                  choices = c("Excel (.xlsx)" = "excel", "PDF (.pdf)" = "pdf"),
                                  selected = "excel")),
            column(6, downloadButton("export_data", "Export Data", class="btn-success"))
          )
      ),
      
      div(class="panel panel-bright",
          h3("Completed Reservations"),
          DTOutput("history_table")
      )
    )
  }
  
  main_ui <- function(){
    tagList(
      div(class="topbar",
          div(class="brand","ParkEase"),
          actionButton("logout","Logout", class="btn-end")
      ),
      div(class="layout",
          sidebar_ui(),
          div(class="content", uiOutput("page_ui"))
      )
    )
  }
  
  output$app_ui <- renderUI({ if (logged_in()) main_ui() else login_ui() })
  
  output$page_ui <- renderUI({
    req(logged_in())
    pg <- current_page()
    if (pg == "overview") return(page_overview_ui())
    if (pg == "add")      return(page_add_ui())
    if (pg == "reserve")  return(page_reserve_ui())
    if (pg == "history")  return(page_history_ui())
    page_overview_ui()
  })
  
  observeEvent(input$nav_overview, { req(logged_in()); current_page("overview") })
  observeEvent(input$nav_add,      { req(logged_in()); current_page("add") })
  observeEvent(input$nav_reserve,  { req(logged_in()); current_page("reserve") })
  observeEvent(input$nav_history,  { req(logged_in()); current_page("history") })
  
  observeEvent(input$login_btn,{
    req(input$login_user, input$login_pass)
    if (digest(input$login_pass,"sha256")==ADMIN_PASS_HASH && input$login_user==ADMIN_USER) {
      logged_in(TRUE)
      output$login_error <- renderText("")
      current_page("overview")
      refresh(refresh() + 1)
      # Store session in browser localStorage
      session$sendCustomMessage("store_session", list())
    } else {
      output$login_error <- renderText("Invalid username or password")
    }
  })
  
  observeEvent(input$logout,{ 
    logged_in(FALSE)
    # Clear session from browser localStorage
    session$sendCustomMessage("clear_session", list())
  })
  
  # Restore session if user was previously logged in
  observeEvent(input$restore_session, {
    if (!logged_in()) {
      logged_in(TRUE)
      current_page("overview")
      refresh(refresh() + 1)
    }
  })
  
  # ---------- DATA ----------
  slots <- reactive({
    refresh()
    tryCatch(db_get("SELECT * FROM parking_slots"), error = function(e) data.frame())
  })
  
  active_reservations <- reactive({
    refresh()
    sql <- paste(
      "SELECT r.reservation_id, r.slot_id, s.slot_no, s.zone, s.type,",
      "       r.vehicle_no, r.driver_name, r.start_time",
      "FROM reservations r",
      "JOIN parking_slots s ON r.slot_id=s.slot_id",
      "WHERE r.status='Active'",
      "ORDER BY r.start_time DESC",
      sep="\n"
    )
    tryCatch(db_get(sql), error = function(e) data.frame())
  })
  
  completed <- reactive({
    refresh()
    sql <- paste(
      "SELECT r.*, s.zone",
      "FROM reservations r",
      "JOIN parking_slots s ON r.slot_id=s.slot_id",
      "WHERE r.status='Completed' AND r.time_out IS NOT NULL",
      "ORDER BY r.time_out DESC",
      sep="\n"
    )
    tryCatch(db_get(sql), error = function(e) data.frame())
  })
  
  history_raw <- reactive({
    refresh()
    sql <- paste(
      "SELECT r.reservation_id, s.slot_no, s.zone, s.type,",
      "       r.vehicle_no, r.driver_name, r.start_time, r.time_out,",
      "       r.duration_hours, r.total_fee",
      "FROM reservations r",
      "JOIN parking_slots s ON r.slot_id=s.slot_id",
      "WHERE r.status='Completed' AND r.time_out IS NOT NULL",
      "ORDER BY r.time_out DESC",
      sep="\n"
    )
    tryCatch(db_get(sql), error = function(e) data.frame())
  })
  
  # ---------- AUTO-GENERATE SLOT NUMBER ----------
  generate_next_slot_no <- function(zone, type) {
    # Map zone to letter
    zone_letter <- switch(zone,
                          "Zone A" = "A",
                          "Zone B" = "B",
                          "Zone C" = "C",
                          "A")
    
    # Map type to abbreviation
    type_abbr <- switch(type,
                        "Regular" = "R",
                        "Compact" = "C",
                        "Electric" = "E",
                        "Disabled" = "D",
                        "R")
    
    prefix <- paste0(zone_letter, "-", type_abbr, "-")
    
    # Query database for existing slots with same zone-type prefix
    sql <- paste(
      "SELECT slot_no FROM parking_slots",
      "WHERE slot_no LIKE ?",
      "ORDER BY slot_no DESC",
      "LIMIT 1",
      sep = "\n"
    )
    
    result <- tryCatch(
      db_get(sql, params = list(paste0(prefix, "%"))),
      error = function(e) data.frame()
    )
    
    # Extract number and increment
    if (nrow(result) > 0) {
      last_slot <- result$slot_no[1]
      last_num <- as.numeric(gsub(paste0("^", gsub("-", "\\\\-", prefix)), "", last_slot))
      next_num <- last_num + 1
    } else {
      next_num <- 1
    }
    
    paste0(prefix, sprintf("%03d", next_num))
  }
  
  # Observer to auto-update slot number when zone or type changes
  observe({
    req(logged_in(), input$zone, input$type)
    new_slot_no <- generate_next_slot_no(input$zone, input$type)
    updateTextInput(session, "slot_no", value = new_slot_no)
  })
  
  # ---------- STATS ----------
  output$total_slots <- renderText(nrow(slots()))
  output$available_slots <- renderText({
    s <- slots()
    if (nrow(s) == 0 || is.null(s$is_available)) return(0)
    sum(as.logical(as.integer(s$is_available)), na.rm=TRUE)
  })
  output$occupied_slots <- renderText({
    s <- slots()
    if (nrow(s) == 0 || is.null(s$is_available)) return(0)
    sum(!as.logical(as.integer(s$is_available)), na.rm=TRUE)
  })
  output$revenue_today <- renderText({
    refresh()
    sql <- paste(
      "SELECT COALESCE(SUM(total_fee),0) AS total",
      "FROM reservations",
      "WHERE time_out IS NOT NULL",
      "  AND date(time_out) = date('now','localtime')",
      sep = "\n"
    )
    q <- tryCatch(db_get(sql), error = function(e) data.frame(total = 0))
    total <- if (nrow(q) == 0) 0 else q$total[1]
    paste0("₱", formatC(as.numeric(total), digits = 2, format = "f"))
  })
  
  # ---------- ANALYTICS (dark-friendly + colors restored) ----------
  output$daily_revenue <- renderPlotly({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$time_out <- to_posix(df$time_out)
    df$date <- as.Date(df$time_out)
    agg <- aggregate(total_fee ~ date, df, sum)
    
    p <- ggplot(agg, aes(x=date, y=total_fee, group=1, text = paste0("Date: ", date, "<br>Revenue: ₱", formatC(total_fee, digits=2, format="f")))) +
      geom_line(color="#22c55e", linewidth=1.2) +
      geom_point(color="#22c55e", size=3) +
      labs(title="Daily Revenue", y="₱", x="Date") +
      theme_dark_dashboard()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  })
  
  output$weekly_revenue <- renderPlotly({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$time_out <- to_posix(df$time_out)
    df$week <- format(as.Date(df$time_out), "%Y-W%U")
    agg <- aggregate(total_fee ~ week, df, sum)
    
    p <- ggplot(agg, aes(week, total_fee, text = paste0("Week: ", week, "<br>Revenue: ₱", formatC(total_fee, digits=2, format="f")))) +
      geom_col(fill="#3b82f6") +
      labs(title="Weekly Revenue", y="₱", x="Week") +
      theme_dark_dashboard()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  })
  
  output$monthly_revenue <- renderPlotly({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$time_out <- to_posix(df$time_out)
    df$month <- format(as.Date(df$time_out), "%Y-%m")
    agg <- aggregate(total_fee ~ month, df, sum)
    
    p <- ggplot(agg, aes(month, total_fee, text = paste0("Month: ", month, "<br>Revenue: ₱", formatC(total_fee, digits=2, format="f")))) +
      geom_col(fill="#a855f7") +
      labs(title="Monthly Revenue", y="₱", x="Month") +
      theme_dark_dashboard()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  })
  
  output$zone_usage <- renderPlotly({
    df <- completed()
    if (nrow(df)==0 || is.null(df$zone)) return(NULL)
    agg <- aggregate(reservation_id ~ zone, df, length)
    names(agg)[2] <- "count"
    
    p <- ggplot(agg, aes(zone, count, text = paste0("Zone: ", zone, "<br>Reservations: ", count))) +
      geom_col(fill="#f97316") +
      labs(title="Most Used Zones", y="Reservations", x="Zone") +
      theme_dark_dashboard()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  })
  
  output$peak_hours <- renderPlotly({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$start_time <- to_posix(df$start_time)
    df$hour <- format(df$start_time, "%H:00")
    agg <- aggregate(reservation_id ~ hour, df, length)
    names(agg)[2] <- "count"
    
    p <- ggplot(agg, aes(hour, count, text = paste0("Time: ", hour, "<br>Reservations: ", count))) +
      geom_col(fill="#22c55e") +
      labs(title="Peak Hours", y="Reservations", x="Hour") +
      theme_dark_dashboard()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE, scrollZoom = FALSE)
  })
  
  # ---------- RESERVE CASCADE ----------
  observe({
    # Trigger on page change, zone, type, and refresh
    current_page()
    refresh()
    req(logged_in(), input$res_zone, input$res_type)
    
    sql <- paste(
      "SELECT slot_id, slot_no",
      "FROM parking_slots",
      "WHERE is_available=1 AND zone=? AND type=?",
      "ORDER BY slot_no",
      sep="\n"
    )
    
    avail <- tryCatch(db_get(sql, params=list(input$res_zone, input$res_type)),
                      error=function(e) data.frame())
    
    if (nrow(avail)==0) {
      updateSelectInput(session,"slot_sel", choices=c("No available slots"=""), selected="")
    } else {
      updateSelectInput(session,"slot_sel",
                        choices=setNames(as.character(avail$slot_id), avail$slot_no),
                        selected=as.character(avail$slot_id[1]))
    }
  })
  
  # ---------- ADD SLOT ----------
  observeEvent(input$add_slot,{
    req(logged_in(), input$slot_no, input$zone, input$type)
    rate <- rate_for_type(input$type)
    
    # Check if slot already exists
    existing <- tryCatch(
      db_get("SELECT slot_id FROM parking_slots WHERE slot_no=? LIMIT 1", params=list(input$slot_no)),
      error=function(e) data.frame()
    )
    
    if (nrow(existing) > 0) {
      showNotification("This slot number already exists. Please use a different number.", type="error")
      return()
    }
    
    tryCatch({
      db_exec(
        "INSERT INTO parking_slots(slot_no,zone,type,rate,is_available) VALUES (?,?,?,?,1)",
        params=list(input$slot_no, input$zone, input$type, rate)
      )
      refresh(refresh()+1)
      # Clear form fields
      updateTextInput(session, "slot_no", value = "")
      updateSelectInput(session, "zone", selected = "Zone A")
      updateSelectInput(session, "type", selected = "Regular")
      showNotification("Slot added!", type="message")
    }, error=function(e){
      showNotification(paste("Error adding slot:", conditionMessage(e)), type="error")
    })
  })
  
  # ---------- RESERVE ----------
  observeEvent(input$reserve,{
    req(logged_in(), input$slot_sel, input$vehicle, input$driver)
    if (input$slot_sel == "") return()
    
    tryCatch({
      start_time <- Sys.time()
      
      db_exec(
        "INSERT INTO reservations(slot_id,vehicle_no,driver_name,start_time,status) VALUES (?,?,?,?, 'Active')",
        params=list(as.integer(input$slot_sel), input$vehicle, input$driver, fmt_ts(start_time))
      )
      
      db_exec("UPDATE parking_slots SET is_available=0 WHERE slot_id=?",
              params=list(as.integer(input$slot_sel)))
      
      refresh(refresh()+1)
      # Clear form fields
      updateTextInput(session, "vehicle", value = "")
      updateTextInput(session, "driver", value = "")
      showNotification("Reservation created!", type="message")
    }, error=function(e){
      showNotification(paste("Reserve error:", conditionMessage(e)), type="error")
    })
  })
  
  # =========================
  # EDIT ACTIVE RESERVATION
  # =========================
  edit_state <- reactiveValues(res_id=NULL, old_slot_id=NULL)
  
  update_edit_slots <- function(zone, type, current_slot_id) {
    sql <- paste(
      "SELECT slot_id, slot_no",
      "FROM parking_slots",
      "WHERE ((is_available=1 AND zone=? AND type=?) OR slot_id=?)",
      "ORDER BY slot_no",
      sep="\n"
    )
    avail <- tryCatch(db_get(sql, params=list(zone, type, as.integer(current_slot_id))),
                      error=function(e) data.frame())
    if (nrow(avail)==0) {
      updateSelectInput(session, "edit_slot", choices=c("No available slots"=""), selected="")
    } else {
      updateSelectInput(session, "edit_slot",
                        choices=setNames(as.character(avail$slot_id), avail$slot_no),
                        selected=as.character(current_slot_id))
    }
  }
  
  observeEvent(input$edit_reservation, {
    req(logged_in(), input$edit_reservation)
    res_id <- as.integer(input$edit_reservation)
    
    sql <- paste(
      "SELECT r.reservation_id, r.slot_id, r.vehicle_no, r.driver_name, r.start_time,",
      "       s.zone, s.type, s.slot_no",
      "FROM reservations r",
      "JOIN parking_slots s ON r.slot_id=s.slot_id",
      "WHERE r.reservation_id=? AND r.status='Active'",
      "LIMIT 1",
      sep="\n"
    )
    info <- tryCatch(db_get(sql, params=list(res_id)), error=function(e) data.frame())
    if (nrow(info)==0) {
      showNotification("Reservation not found / not active.", type="error")
      return()
    }
    
    edit_state$res_id <- res_id
    edit_state$old_slot_id <- as.integer(info$slot_id[1])
    
    showModal(modalDialog(
      title = paste0("Edit Active Reservation #", res_id),
      easyClose = TRUE,
      size = "m",
      footer = tagList(
        actionButton("save_edit", "Save Changes", class="btn-success"),
        modalButton("Cancel")
      ),
      fluidRow(
        column(6, selectInput("edit_zone","Zone",c("Zone A","Zone B","Zone C"), selected=info$zone[1])),
        column(6, selectInput("edit_type","Type",c("Regular","Compact","Electric","Disabled"), selected=info$type[1]))
      ),
      selectInput("edit_slot","Slot",choices=c("Loading..."="")),
      fluidRow(
        column(6, textInput("edit_vehicle","Vehicle Number", value=info$vehicle_no[1])),
        column(6, textInput("edit_driver","Driver Name", value=info$driver_name[1]))
      )
    ))
    
    update_edit_slots(info$zone[1], info$type[1], info$slot_id[1])
  })
  
  observeEvent(list(input$edit_zone, input$edit_type), {
    req(edit_state$res_id, input$edit_zone, input$edit_type)
    current_sel <- input$edit_slot
    if (is.null(current_sel) || current_sel == "") current_sel <- edit_state$old_slot_id
    update_edit_slots(input$edit_zone, input$edit_type, as.integer(current_sel))
  }, ignoreInit = TRUE)
  
  observeEvent(input$save_edit, {
    req(edit_state$res_id, input$edit_zone, input$edit_type, input$edit_slot, input$edit_vehicle, input$edit_driver)
    if (input$edit_slot == "") {
      showNotification("Please select a valid slot.", type="error")
      return()
    }
    
    res_id <- as.integer(edit_state$res_id)
    old_slot_id <- as.integer(edit_state$old_slot_id)
    new_slot_id <- as.integer(input$edit_slot)
    
    ok <- tryCatch(db_get("SELECT slot_id, is_available FROM parking_slots WHERE slot_id=? LIMIT 1",
                          params=list(new_slot_id)),
                   error=function(e) data.frame())
    if (nrow(ok)==0) {
      showNotification("Selected slot not found.", type="error")
      return()
    }
    if (new_slot_id != old_slot_id && as.integer(ok$is_available[1]) != 1) {
      showNotification("That slot is not available anymore.", type="error")
      return()
    }
    
    new_rate <- rate_for_type(input$edit_type)
    
    tryCatch({
      if (new_slot_id != old_slot_id) {
        db_exec("UPDATE parking_slots SET is_available=1 WHERE slot_id=?", params=list(old_slot_id))
        db_exec("UPDATE parking_slots SET is_available=0 WHERE slot_id=?", params=list(new_slot_id))
      } else {
        db_exec("UPDATE parking_slots SET is_available=0 WHERE slot_id=?", params=list(old_slot_id))
      }
      
      db_exec("UPDATE parking_slots SET zone=?, type=?, rate=? WHERE slot_id=?",
              params=list(input$edit_zone, input$edit_type, new_rate, new_slot_id))
      
      db_exec("UPDATE reservations SET slot_id=?, vehicle_no=?, driver_name=? WHERE reservation_id=? AND status='Active'",
              params=list(new_slot_id, input$edit_vehicle, input$edit_driver, res_id))
      
      removeModal()
      refresh(refresh()+1)
      showNotification("Reservation updated!", type="message")
    }, error=function(e){
      showNotification(paste("Edit save error:", conditionMessage(e)), type="error")
    })
  })
  
  # ---------- END + TICKET ----------
  observeEvent(input$end_reservation,{
    req(logged_in(), input$end_reservation)
    
    sql <- paste(
      "SELECT r.*, s.slot_no, s.rate",
      "FROM reservations r",
      "JOIN parking_slots s ON r.slot_id=s.slot_id",
      "WHERE r.reservation_id=?",
      "LIMIT 1",
      sep="\n"
    )
    info <- tryCatch(db_get(sql, params=list(as.integer(input$end_reservation))),
                     error=function(e) data.frame())
    if (nrow(info)==0) return()
    
    info$start_time <- to_posix(info$start_time)
    
    time_out <- Sys.time()
    hours <- ceiling(as.numeric(difftime(time_out, info$start_time, units="hours")))
    if (is.na(hours) || hours < 1) hours <- 1
    
    rate <- as.numeric(info$rate[1])
    total <- hours * rate
    
    tryCatch({
      db_exec(paste(
        "UPDATE reservations",
        "SET status='Completed',",
        "    time_out=?,",
        "    duration_hours=?,",
        "    total_fee=?",
        "WHERE reservation_id=?",
        sep="\n"
      ), params=list(
        fmt_ts(time_out),
        as.integer(hours),
        as.numeric(total),
        as.integer(input$end_reservation)
      ))
      
      db_exec("UPDATE parking_slots SET is_available=1 WHERE slot_id=?",
              params=list(as.integer(info$slot_id[1])))
      
      # Store ticket info for PDF download
      session$userData$ticket_data <- list(
        slot_no = info$slot_no[1],
        vehicle_no = info$vehicle_no[1],
        driver_name = info$driver_name[1],
        start_time = format(info$start_time[1], "%Y-%m-%d %H:%M:%S"),
        time_out = format(time_out, "%Y-%m-%d %H:%M:%S"),
        hours = hours,
        rate = rate,
        total = total,
        res_id = as.integer(input$end_reservation)
      )
      
      showModal(modalDialog(
        easyClose = TRUE,
        footer = tagList(
          downloadButton("download_ticket", "🖨 Download Ticket PDF", class="btn-success"),
          modalButton("Close")
        ),
        div(id="ticket_area", class="ticket", style="background: #fff; color: #111827;",
            h3("ParkEase Ticket"),
            hr(),
            p(strong("Slot:"), span(info$slot_no[1])),
            p(strong("Vehicle:"), span(info$vehicle_no[1])),
            p(strong("Driver:"), span(info$driver_name[1])),
            p(strong("Time In:"), span(format(info$start_time[1], "%Y-%m-%d %H:%M:%S"))),
            p(strong("Time Out:"), span(format(time_out, "%Y-%m-%d %H:%M:%S"))),
            p(strong("Duration:"), span(paste0(hours, " hour(s)"))),
            p(strong("Rate/hr:"), span(paste0("₱", formatC(rate, digits=2, format="f")))),
            hr(),
            h3(paste0("Total: ₱", formatC(total, digits=2, format="f")))
        )
      ))
      
      refresh(refresh()+1)
    }, error=function(e){
      showNotification(paste("End reservation error:", conditionMessage(e)), type="error")
    })
  })
  
  # ---------- TABLE: ACTIVE ----------
  output$active_table <- renderDT({
    df <- active_reservations()
    if (nrow(df)==0) {
      return(datatable(data.frame(Message="No active reservations"),
                       options=list(dom="t"), rownames=FALSE))
    }
    
    df$Edit <- paste0(
      "<button class='btn-primary' style='padding:6px 10px; border-radius:8px;' data-edit='",
      df$reservation_id,"'>Edit</button>"
    )
    df$End <- paste0(
      "<button class='btn-end' data-end='", df$reservation_id, "'>End</button>"
    )
    
    datatable(
      df[,c("slot_no","zone","type","vehicle_no","driver_name","Edit","End")],
      escape=FALSE, selection="none",
      options=list(
        dom="t",
        rowCallback=JS(
          "function(row, data){
             $('button[data-end]', row).off('click').on('click', function(){
               Shiny.setInputValue('end_reservation', $(this).data('end'), {priority:'event'});
             });
             $('button[data-edit]', row).off('click').on('click', function(){
               Shiny.setInputValue('edit_reservation', $(this).data('edit'), {priority:'event'});
             });
           }"
        )
      )
    )
  })
  
  # ---------- TABLE: SLOTS ----------
  output$slots_table <- renderDT({
    s <- slots()
    if (nrow(s) == 0) {
      return(datatable(data.frame(Message="No slots yet"),
                       options=list(dom="t"), rownames=FALSE))
    }
    s$is_available <- ifelse(as.integer(s$is_available) == 1, "Yes", "No")
    datatable(
      s[, c("slot_no","zone","type","rate","is_available")],
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip")
    )
  })
  
  # ---------- HISTORY FILTERED ----------
  history_filtered <- reactive({
    df <- history_raw()
    if (nrow(df) == 0) return(df)
    
    df$time_out_posix <- to_posix(df$time_out)
    df$time_out_date  <- as.Date(df$time_out_posix)
    
    if (!is.null(input$hist_date) && length(input$hist_date) == 2 &&
        !any(is.na(input$hist_date))) {
      df <- df[df$time_out_date >= input$hist_date[1] & df$time_out_date <= input$hist_date[2], , drop=FALSE]
    }
    
    if (!is.null(input$hist_zone) && input$hist_zone != "All Zones") {
      df <- df[df$zone == input$hist_zone, , drop=FALSE]
    }
    
    if (!is.null(input$hist_type) && input$hist_type != "All Types") {
      df <- df[df$type == input$hist_type, , drop=FALSE]
    }
    
    if (!is.null(input$hist_vehicle) && nzchar(trimws(input$hist_vehicle))) {
      pat <- trimws(input$hist_vehicle)
      df <- df[grepl(pat, df$vehicle_no, ignore.case = TRUE), , drop=FALSE]
    }
    
    df
  })
  
  output$history_table <- renderDT({
    req(logged_in())
    df <- history_filtered()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message="No completed reservations found for the selected filters."),
                       options=list(dom="t"), rownames=FALSE))
    }
    
    show_df <- df[, c("reservation_id","slot_no","zone","type","vehicle_no","driver_name",
                      "start_time","time_out","duration_hours","total_fee")]
    show_df$total_fee <- paste0("₱", formatC(as.numeric(show_df$total_fee), digits = 2, format = "f"))
    
    datatable(show_df, rownames = FALSE, options = list(pageLength = 10, dom = "tip"))
  })
  
  # ---------- UNIFIED EXPORT HANDLER ----------
  output$export_data <- downloadHandler(
    filename = function() {
      format_type <- input$export_format
      ext <- if (format_type == "excel") ".xlsx" else ".pdf"
      paste0("parkease_reservation_history_", format(Sys.Date(), "%Y-%m-%d"), ext)
    },
    content = function(file) {
      df <- history_filtered()
      if (nrow(df) == 0) stop("No data to export.")
      
      format_type <- input$export_format
      
      if (format_type == "excel") {
        # Export to Excel
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          stop("Package 'openxlsx' is required. Install: install.packages('openxlsx')")
        }
        
        out <- df[, c("reservation_id","slot_no","zone","type","vehicle_no","driver_name",
                      "start_time","time_out","duration_hours","total_fee")]
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "History")
        openxlsx::writeData(wb, "History", out)
        openxlsx::setColWidths(wb, "History", cols = 1:ncol(out), widths = "auto")
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        # Export to PDF
        if (!requireNamespace("gridExtra", quietly = TRUE) ||
            !requireNamespace("grid", quietly = TRUE)) {
          stop("Install required packages: install.packages(c('gridExtra'))")
        }
        
        out <- df[, c("reservation_id","slot_no","zone","type","vehicle_no","driver_name",
                      "start_time","time_out","duration_hours","total_fee")]
        out$total_fee <- paste0("₱", formatC(as.numeric(out$total_fee), digits = 2, format = "f"))
        
        grDevices::pdf(file, width = 11.69, height = 8.27)  # A4 landscape
        grid::grid.newpage()
        grid::grid.text("ParkEase — Reservation History (Completed)",
                        y = 0.97, gp = grid::gpar(fontsize = 16, fontface = "bold"))
        
        tbl <- gridExtra::tableGrob(out, rows = NULL,
                                    theme = gridExtra::ttheme_default(base_size = 8))
        grid::pushViewport(grid::viewport(y = 0.48, height = 0.82))
        grid::grid.draw(tbl)
        grid::popViewport()
        grDevices::dev.off()
      }
    }
  )
  
  # ---------- DOWNLOAD TICKET PDF ----------
  output$download_ticket <- downloadHandler(
    filename = function() {
      ticket <- session$userData$ticket_data
      if (is.null(ticket)) return("ticket.pdf")
      paste0("ParkEase_Ticket_", ticket$res_id, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      ticket <- session$userData$ticket_data
      if (is.null(ticket)) {
        stop("Ticket data not found.")
      }
      
      if (!requireNamespace("grid", quietly = TRUE)) {
        stop("Install required package: install.packages('grid')")
      }
      
      grDevices::pdf(file, width = 3.5, height = 4.5)  # Ticket size
      grid::grid.newpage()
      
      # Set up viewport for the ticket
      grid::pushViewport(grid::viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.95, just = c("center", "center")))
      
      # Title
      grid::grid.text("ParkEase Ticket",
                      y = 0.95, gp = grid::gpar(fontsize = 14, fontface = "bold"))
      
      # Line separator
      grid::grid.lines(c(0.05, 0.95), c(0.92, 0.92), gp = grid::gpar(lty = "dashed", col = "black"))
      
      # Ticket details
      y_pos <- 0.88
      line_height <- 0.08
      
      grid::grid.text("Slot:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 11, fontface = "bold"))
      grid::grid.text(ticket$slot_no, x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 11))
      y_pos <- y_pos - line_height
      
      grid::grid.text("Vehicle:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 11, fontface = "bold"))
      grid::grid.text(ticket$vehicle_no, x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 11))
      y_pos <- y_pos - line_height
      
      grid::grid.text("Driver:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 11, fontface = "bold"))
      grid::grid.text(ticket$driver_name, x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 11))
      y_pos <- y_pos - line_height
      
      grid::grid.text("Time In:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 10, fontface = "bold"))
      grid::grid.text(ticket$start_time, x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 10))
      y_pos <- y_pos - line_height
      
      grid::grid.text("Time Out:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 10, fontface = "bold"))
      grid::grid.text(ticket$time_out, x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 10))
      y_pos <- y_pos - line_height
      
      grid::grid.text("Duration:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 10, fontface = "bold"))
      grid::grid.text(paste0(ticket$hours, " hour(s)"), x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 10))
      y_pos <- y_pos - line_height
      
      grid::grid.text("Rate/hr:", x = 0.05, y = y_pos, just = c("left", "center"), gp = grid::gpar(fontsize = 10, fontface = "bold"))
      grid::grid.text(paste0("₱", formatC(ticket$rate, digits = 2, format = "f")), x = 0.95, y = y_pos, just = c("right", "center"), gp = grid::gpar(fontsize = 10))
      y_pos <- y_pos - line_height
      
      # Line separator
      grid::grid.lines(c(0.05, 0.95), c(y_pos + 0.02, y_pos + 0.02), gp = grid::gpar(lty = "dashed", col = "black"))
      y_pos <- y_pos - line_height * 1.2
      
      # Total amount box
      box_y <- y_pos
      grid::grid.rect(x = 0.5, y = box_y, width = 0.9, height = 0.12, just = c("center", "center"),
                      gp = grid::gpar(fill = "#111827", col = "black", lwd = 1))
      
      grid::grid.text(paste0("Total: ₱", formatC(ticket$total, digits = 2, format = "f")),
                      x = 0.5, y = box_y, just = c("center", "center"),
                      gp = grid::gpar(fontsize = 13, fontface = "bold", col = "white"))
      
      grid::popViewport()
      grDevices::dev.off()
    }
  )
}

shinyApp(ui, server)
