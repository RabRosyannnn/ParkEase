# ==========================================
# ParkEase - SQLite Seeder (Demo Data, PER ZONE COUNTS)
# Creates parkease_seed.sqlite with:
# - Slots per zone (Zone A/B/C):
#   Regular  = 20
#   Compact  = 30
#   Electric = 3
#   Disabled = 3
# - Sample Completed reservations (for analytics)
# - Sample Active reservations (for Active table demo)
# ==========================================

library(DBI)
library(RSQLite)

APP_TZ <- "Asia/Manila"
Sys.setenv(TZ = APP_TZ)

SEED_DB <- "parkease_seed.sqlite"

get_con_seed <- function() {
  con <- dbConnect(RSQLite::SQLite(), dbname = file.path(getwd(), SEED_DB))
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  dbExecute(con, "PRAGMA journal_mode = WAL;")
  con
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

create_tables <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS parking_slots (
      slot_id INTEGER PRIMARY KEY AUTOINCREMENT,
      slot_no TEXT NOT NULL UNIQUE,
      zone    TEXT NOT NULL,
      type    TEXT NOT NULL,
      rate    REAL NOT NULL DEFAULT 0,
      is_available INTEGER NOT NULL DEFAULT 1,
      created_at TEXT DEFAULT (datetime('now'))
    );
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS reservations (
      reservation_id INTEGER PRIMARY KEY AUTOINCREMENT,
      slot_id INTEGER NOT NULL,
      vehicle_no TEXT NOT NULL,
      driver_name TEXT NOT NULL,
      start_time TEXT NOT NULL,
      time_out TEXT NULL,
      duration_hours INTEGER NULL,
      total_fee REAL NULL,
      status TEXT NOT NULL DEFAULT 'Active',
      created_at TEXT DEFAULT (datetime('now')),
      FOREIGN KEY(slot_id) REFERENCES parking_slots(slot_id)
    );
  ")
  
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_reservations_status  ON reservations(status);")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_reservations_start   ON reservations(start_time);")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_reservations_timeout ON reservations(time_out);")
}

# -------------------------------------------------------
# SLOT SEEDING (PER ZONE)
# Zone A, Zone B, Zone C each has:
#   Regular 20, Compact 30, Electric 3, Disabled 3
# Total per zone = 56, total overall = 168
# Slot number format: <ZoneCode>-<TypeLetter>-<NNN>
# Example: A-R-001, B-C-045, C-E-053
# -------------------------------------------------------
seed_slots <- function(con) {
  
  type_letter <- function(type) {
    switch(type,
           "Regular"  = "R",
           "Compact"  = "C",
           "Electric" = "E",
           "Disabled" = "D",
           "X")
  }
  
  make_slots <- function(zone_code, zone_name, type, start_no, n) {
    data.frame(
      slot_no = sprintf("%s-%s-%03d", zone_code, type_letter(type), start_no:(start_no + n - 1)),
      zone    = zone_name,
      type    = type,
      stringsAsFactors = FALSE
    )
  }
  
  zones <- list(
    list(code="A", name="Zone A"),
    list(code="B", name="Zone B"),
    list(code="C", name="Zone C")
  )
  
  all_slots <- data.frame()
  
  for (z in zones) {
    # numbering block per zone
    # Regular:  1-20
    # Compact: 21-50
    # Electric:51-53
    # Disabled:54-56
    all_slots <- rbind(
      all_slots,
      make_slots(z$code, z$name, "Regular",   1, 20),
      make_slots(z$code, z$name, "Compact",  21, 30),
      make_slots(z$code, z$name, "Electric", 51,  3),
      make_slots(z$code, z$name, "Disabled", 54,  3)
    )
  }
  
  all_slots$rate <- vapply(all_slots$type, rate_for_type, numeric(1))
  all_slots$is_available <- 1L
  
  for (i in seq_len(nrow(all_slots))) {
    dbExecute(
      con,
      "INSERT OR IGNORE INTO parking_slots
       (slot_no, zone, type, rate, is_available)
       VALUES (?, ?, ?, ?, 1)",
      params = list(
        all_slots$slot_no[i],
        all_slots$zone[i],
        all_slots$type[i],
        all_slots$rate[i]
      )
    )
  }
}

# -------------------------------------------------------
# RESERVATION SEEDING
# - Completed: spread across days/weeks for analytics
# - Active: currently running reservations
# -------------------------------------------------------
seed_reservations <- function(con) {
  
  slot_map <- dbGetQuery(con, "SELECT slot_id, slot_no, rate, zone, type FROM parking_slots")
  
  get_slot_id <- function(slot_no) slot_map$slot_id[match(slot_no, slot_map$slot_no)]
  get_rate    <- function(slot_no) slot_map$rate[match(slot_no, slot_map$slot_no)]
  
  # helper: pick a slot that exists
  pick_existing <- function(slot_no) {
    if (!is.na(get_slot_id(slot_no))) slot_no else slot_map$slot_no[1]
  }
  
  now <- Sys.time()
  
  # Completed reservations:
  # Create a good spread so daily/weekly/monthly charts are not empty
  completed_rows <- list(
    list(slot_no=pick_existing("A-R-001"), vehicle="ABC-1234", driver="Juan Dela Cruz",  start=now - 1*24*3600 - 2.2*3600, hours=3),
    list(slot_no=pick_existing("A-C-025"), vehicle="XYZ-8899", driver="Maria Santos",    start=now - 2*24*3600 - 1.5*3600, hours=2),
    list(slot_no=pick_existing("B-R-010"), vehicle="KLM-4567", driver="Peter Reyes",     start=now - 3*24*3600 - 4.1*3600, hours=5),
    list(slot_no=pick_existing("B-C-044"), vehicle="QWE-2020", driver="Angel Bautista",  start=now - 7*24*3600 - 2.0*3600, hours=2),
    list(slot_no=pick_existing("C-R-015"), vehicle="CAR-7777", driver="Nicole Garcia",   start=now - 10*24*3600 - 6.5*3600, hours=7),
    list(slot_no=pick_existing("C-E-051"), vehicle="EV-9001",  driver="Ethan Cruz",      start=now - 12*24*3600 - 1.2*3600, hours=2),
    list(slot_no=pick_existing("A-R-020"), vehicle="JKL-3333", driver="Trisha Lopez",    start=now - 14*24*3600 - 3.0*3600, hours=4),
    list(slot_no=pick_existing("B-D-054"), vehicle="PWD-111",  driver="Mark Villanueva", start=now - 20*24*3600 - 2.0*3600, hours=1)
  )
  
  for (x in completed_rows) {
    slot_id <- get_slot_id(x$slot_no)
    rate    <- get_rate(x$slot_no)
    start_t <- x$start
    out_t   <- start_t + x$hours * 3600
    total   <- x$hours * rate
    
    dbExecute(con,
              "INSERT INTO reservations(slot_id, vehicle_no, driver_name, start_time, time_out, duration_hours, total_fee, status)
       VALUES (?, ?, ?, ?, ?, ?, ?, 'Completed')",
              params = list(slot_id, x$vehicle, x$driver, fmt_ts(start_t), fmt_ts(out_t), as.integer(x$hours), as.numeric(total))
    )
  }
  
  # Active reservations (choose 3 actives across zones/types)
  active_rows <- list(
    list(slot_no=pick_existing("A-R-002"), vehicle="DEM-1111", driver="Chris Lim",   start=now - 45*60),
    list(slot_no=pick_existing("B-C-021"), vehicle="DEM-2222", driver="Jasmine Uy",  start=now - 2.2*3600),
    list(slot_no=pick_existing("C-R-003"), vehicle="DEM-3333", driver="Paolo Ramos", start=now - 1.1*3600)
  )
  
  for (x in active_rows) {
    slot_id <- get_slot_id(x$slot_no)
    
    dbExecute(con,
              "INSERT INTO reservations(slot_id, vehicle_no, driver_name, start_time, status)
       VALUES (?, ?, ?, ?, 'Active')",
              params = list(slot_id, x$vehicle, x$driver, fmt_ts(x$start))
    )
    
    dbExecute(con, "UPDATE parking_slots SET is_available=0 WHERE slot_id=?",
              params = list(slot_id))
  }
}

# ---------- RUN (WINDOWS-SAFE) ----------
# 1) Make sure no old connection is holding the DB file
try({ if (exists("con")) dbDisconnect(con) }, silent = TRUE)
rm(list = c("con"), envir = .GlobalEnv)
gc()

# 2) Remove old DB + side files (WAL/SHM/JOURNAL) with retries
files <- c(
  SEED_DB,
  paste0(SEED_DB, "-wal"),
  paste0(SEED_DB, "-shm"),
  paste0(SEED_DB, "-journal")
)

for (f in files) {
  if (file.exists(f)) {
    message("Deleting existing file: ", f)
    deleted <- FALSE
    for (i in 1:8) {
      deleted <- tryCatch(file.remove(f), error = function(e) FALSE)
      if (isTRUE(deleted)) break
      Sys.sleep(0.25)
    }
    if (!isTRUE(deleted)) {
      stop("Permission denied / file locked: ", f,
           "\nClose the Shiny app + restart RStudio then run the seeder again.")
    }
  }
}

# 3) Create fresh seed DB
con <- get_con_seed()
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

create_tables(con)
seed_slots(con)
seed_reservations(con)

message("✅ Seed DB created: ", file.path(getwd(), SEED_DB))
message("Next: run your app. You should see 168 slots, active reservations, and analytics charts.")

