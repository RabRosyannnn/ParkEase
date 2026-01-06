# =========================
# ParkEase (SQLITE + EDIT ACTIVE RESERVATIONS + ANALYTICS)
# - SQLite (RSQLite) backend
# - Auto-create tables + indexes
# - Fresh DB connection per query
# - Edit Active reservations + End w/ ticket + Analytics
# =========================

library(shiny)
library(shinyWidgets)
library(DT)
library(DBI)
library(RSQLite)
library(digest)
library(ggplot2)

APP_TZ <- Sys.getenv("APP_TZ", "Asia/Manila")
Sys.setenv(TZ = APP_TZ)  # makes Sys.time() follow PH time

# ---------------- LOGIN CREDENTIALS ----------------
ADMIN_USER <- "admin"
ADMIN_PASS_HASH <- digest("parkease123", algo = "sha256")

# ================== SQLITE DB HELPERS ==================
# ================== SQLITE DB HELPERS (DEPLOY-SAFE) ==================
# Seed DB lives in your app folder (tracked in GitHub)
SEED_DB <- Sys.getenv("SEED_DB", "parkease_seed.sqlite")

# Live DB is created in a writable temp folder (works on shinyapps.io)
LIVE_DB <- Sys.getenv("LIVE_DB", "parkease_live.sqlite")

get_db_path <- function() {
  live_path <- file.path(tempdir(), LIVE_DB)
  
  # First run: copy bundled seed -> temp live db
  if (!file.exists(live_path)) {
    seed_path <- file.path(getwd(), SEED_DB)
    
    # If seed doesn't exist yet, create an empty live db anyway
    if (file.exists(seed_path)) {
      file.copy(seed_path, live_path, overwrite = TRUE)
    }
  }
  
  live_path
}

get_con <- function() {
  db_path <- get_db_path()
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Safer behavior for concurrency + speed
  try(dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
  try(dbExecute(con, "PRAGMA foreign_keys = ON;"), silent = TRUE)
  try(dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
  
  con
}


db_get <- function(sql, params = NULL) {
  con <- NULL
  on.exit({ if (!is.null(con)) try(dbDisconnect(con), silent = TRUE) }, add = TRUE)
  con <- get_con()
  
  if (is.null(params)) {
    return(dbGetQuery(con, sql))
  } else {
    return(dbGetQuery(con, sql, params = params))
  }
}

db_exec <- function(sql, params = NULL) {
  con <- NULL
  on.exit({ if (!is.null(con)) try(dbDisconnect(con), silent = TRUE) }, add = TRUE)
  con <- get_con()
  
  if (is.null(params)) {
    return(dbExecute(con, sql))
  } else {
    return(dbExecute(con, sql, params = params))
  }
}

ensure_tables <- function() {
  
  # parking_slots: is_available stored as INTEGER 1/0 (SQLite style)
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
  
  # reservations: start_time/time_out stored as TEXT timestamps (from R, PH time)
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

fmt_ts <- function(x) {
  # store as "YYYY-mm-dd HH:MM:SS" string
  format(x, "%Y-%m-%d %H:%M:%S")
}

rate_for_type <- function(type) {
  switch(type,
         "Regular"  = 30,
         "Compact"  = 20,
         "Electric" = 60,
         "Disabled" = 0,
         30)
}

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(
      rel="stylesheet",
      href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
    ),
    tags$script(HTML("
      // 👁 Password toggle
      $(document).on('click', '#toggle_pass', function() {
        let input = document.getElementById('login_pass');
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

      // 🖨 Ticket print
      function printTicket() {
        var printContents = document.getElementById('ticket_area').innerHTML;
        var win = window.open('', '', 'height=700,width=420');
        win.document.write('<html><head><title>Parking Ticket</title></head><body>');
        win.document.write(printContents);
        win.document.write('</body></html>');
        win.document.close();
        win.focus();
        win.print();
        win.close();
      }
    "))
  ),
  uiOutput("app_ui")
)

# ---------------- SERVER ----------------
server <- function(input, output, session){
  
  logged_in <- reactiveVal(FALSE)
  refresh <- reactiveVal(0)
  
  tryCatch(ensure_tables(), error = function(e) {
    showNotification(paste("DB init error:", conditionMessage(e)), type="error", duration=NULL)
  })
  
  # ---------- LOGIN UI ----------
  login_ui <- function(){
    div(style="height:100vh; display:flex; justify-content:center; align-items:center;",
        div(style="
          background:#020617;
          padding:40px;
          border-radius:16px;
          width:360px;
          box-shadow:0 0 40px rgba(0,0,0,0.6);
        ",
            h2("ParkEase Login", style="text-align:center; margin-bottom:20px;"),
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
  
  # ---------- MAIN UI ----------
  main_ui <- function(){
    tagList(
      div(class="topbar",
          div(class="brand","ParkEase"),
          actionButton("logout","Logout", class="btn-end")
      ),
      
      div(class="layout",
          div(class="sidebar",
              actionButton("nav_overview","Overview",class="side-btn active"),
              hr(),
              actionButton("nav_add","➕ Add Slot",class="side-btn"),
              actionButton("nav_reserve","⏱ Reserve Slot",class="side-btn")
          ),
          
          div(class="content",
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
                             plotOutput("daily_revenue", height=250),
                             plotOutput("weekly_revenue", height=250),
                             plotOutput("monthly_revenue", height=250)
                    ),
                    tabPanel("Most Used Zones",
                             plotOutput("zone_usage", height=300)
                    ),
                    tabPanel("Peak Hours",
                             plotOutput("peak_hours", height=300)
                    )
                  )
              )
          )
      )
    )
  }
  
  output$app_ui <- renderUI({ if (logged_in()) main_ui() else login_ui() })
  
  observeEvent(input$login_btn,{
    req(input$login_user, input$login_pass)
    if (digest(input$login_pass,"sha256")==ADMIN_PASS_HASH && input$login_user==ADMIN_USER) {
      logged_in(TRUE)
      output$login_error <- renderText("")
      refresh(refresh() + 1)
    } else {
      output$login_error <- renderText("Invalid username or password")
    }
  })
  
  observeEvent(input$logout,{ logged_in(FALSE) })
  
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
    
    # Since time_out is stored as TEXT "YYYY-mm-dd HH:MM:SS",
    # we can use date(time_out) comparison safely.
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
  
  # ---------- ANALYTICS ----------
  output$daily_revenue <- renderPlot({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$time_out <- to_posix(df$time_out)
    df$date <- as.Date(df$time_out)
    agg <- aggregate(total_fee ~ date, df, sum)
    ggplot(agg, aes(date, total_fee)) +
      geom_line(color="#22c55e", linewidth=1.2) +
      geom_point(color="#22c55e", size=3) +
      labs(title="Daily Revenue", y="₱", x="Date") +
      theme_minimal()
  })
  
  output$weekly_revenue <- renderPlot({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$time_out <- to_posix(df$time_out)
    df$week <- format(as.Date(df$time_out), "%Y-%U")
    agg <- aggregate(total_fee ~ week, df, sum)
    ggplot(agg, aes(week, total_fee)) +
      geom_col(fill="#3b82f6") +
      labs(title="Weekly Revenue", y="₱", x="Week") +
      theme_minimal()
  })
  
  output$monthly_revenue <- renderPlot({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$time_out <- to_posix(df$time_out)
    df$month <- format(as.Date(df$time_out), "%Y-%m")
    agg <- aggregate(total_fee ~ month, df, sum)
    ggplot(agg, aes(month, total_fee)) +
      geom_col(fill="#a855f7") +
      labs(title="Monthly Revenue", y="₱", x="Month") +
      theme_minimal()
  })
  
  output$zone_usage <- renderPlot({
    df <- completed()
    if (nrow(df)==0 || is.null(df$zone)) return(NULL)
    agg <- aggregate(reservation_id ~ zone, df, length)
    ggplot(agg, aes(zone, reservation_id)) +
      geom_col(fill="#f97316") +
      labs(title="Most Used Zones", y="Reservations", x="Zone") +
      theme_minimal()
  })
  
  output$peak_hours <- renderPlot({
    df <- completed()
    if (nrow(df)==0) return(NULL)
    df$start_time <- to_posix(df$start_time)
    df$hour <- format(df$start_time, "%H")
    agg <- aggregate(reservation_id ~ hour, df, length)
    ggplot(agg, aes(hour, reservation_id)) +
      geom_col(fill="#22c55e") +
      labs(title="Peak Hours", y="Reservations", x="Hour") +
      theme_minimal()
  })
  
  # ---------- RESERVE CASCADE ----------
  observe({
    req(logged_in(), input$res_zone, input$res_type)
    refresh()
    
    sql <- paste(
      "SELECT slot_id, slot_no",
      "FROM parking_slots",
      "WHERE is_available=1 AND zone=? AND type=?",
      "ORDER BY slot_no",
      sep="\n"
    )
    
    avail <- tryCatch(
      db_get(sql, params=list(input$res_zone, input$res_type)),
      error=function(e) data.frame()
    )
    
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
    
    tryCatch({
      db_exec(
        "INSERT INTO parking_slots(slot_no,zone,type,rate,is_available) VALUES (?,?,?,?,1)",
        params=list(input$slot_no, input$zone, input$type, rate)
      )
      refresh(refresh()+1)
      showNotification("Slot added!", type="message")
    }, error=function(e){
      showNotification(paste("Add slot error:", conditionMessage(e)), type="error")
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
      selectInput("edit_slot","Slot (Available)",choices=c("Loading..."="")),
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
      
      showModal(modalDialog(
        easyClose = TRUE,
        footer = tagList(
          tags$button("🖨 Print Ticket", class="btn-success", onclick="printTicket()"),
          modalButton("Close")
        ),
        div(id="ticket_area", class="ticket",
            h3("🅿 ParkEase Ticket"),
            hr(),
            p(strong("Slot"), span(info$slot_no[1])),
            p(strong("Vehicle"), span(info$vehicle_no[1])),
            p(strong("Driver"), span(info$driver_name[1])),
            p(strong("Time In"), span(as.character(info$start_time[1]))),
            p(strong("Time Out"), span(as.character(time_out))),
            p(strong("Hours"), span(hours)),
            p(strong("Rate/hr"), span(paste0("₱", rate))),
            hr(),
            h3(paste0("₱", formatC(total, digits=2, format="f")))
        )
      ))
      
      refresh(refresh()+1)
    }, error=function(e){
      showNotification(paste("End reservation error:", conditionMessage(e)), type="error")
    })
  })
  
  # ---------- TABLE ----------
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
}

shinyApp(ui, server)
