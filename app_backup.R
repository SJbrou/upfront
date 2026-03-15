##############################################################################
# Supply Chain Optimization Dashboard - Shiny MVP
# Grocery retailer: supplier → warehouse → shops
# Optimizes import & delivery schedule over 30-day horizon
##############################################################################

# ---- Load Libraries --------------------------------------------------------
library(shiny)
library(leaflet)
library(data.table)
library(DT)
library(ggplot2)
library(lpSolve)

# ---- Constants -------------------------------------------------------------
PLANNING_HORIZON <- 30
DEFAULT_SERVICE_LEVEL <- 0.99
DEFAULT_ERROR_PCT <- 20

# Weekly seasonality factors (Mon-Sun)
WEEKLY_SEASONALITY <- c(1.1, 1.0, 1.0, 1.05, 1.15, 1.3, 0.8)
# Monthly seasonality factors (Jan-Dec)
MONTHLY_SEASONALITY <- c(0.90, 0.85, 0.95, 1.00, 1.05, 1.10,
                         1.10, 1.05, 1.00, 0.95, 0.90, 1.00)

# Default warehouse location (Amsterdam area)
DEFAULT_WH_LAT <- 52.3676
DEFAULT_WH_LNG <- 4.9041

# Default store locations (Dutch cities)
DEFAULT_STORES <- data.table(
  store_id   = 1:3,
  store_name = c("Store Rotterdam", "Store Utrecht", "Store Den Haag"),
  lat        = c(51.9225, 52.0907, 52.0705),
  lng        = c(4.4792, 5.1214, 4.3007),
  multiplier = c(1.0, 0.8, 1.2)
)

# ---- Helper Functions ------------------------------------------------------

read_product_csv <- function(file_path) {
  # Read file as text to detect delimiter
  lines <- readLines(file_path, n = 2, warn = FALSE)
  header <- lines[1]

  # Detect delimiter: if semicolons present in header, use ;
  if (grepl(";", header)) {
    dt <- fread(file_path, sep = ";", dec = ",", header = TRUE,
                encoding = "UTF-8")
  } else {
    # Comma delimiter with potential quoted decimal values
    dt <- fread(file_path, sep = ",", header = TRUE, encoding = "UTF-8")
    # Fix decimal commas in quoted fields
    num_cols <- c("price", "inkoop", "weight_(g)", "weight (gr)",
                  "volume_(l)", "volume (l)")
    for (col in intersect(num_cols, names(dt))) {
      if (is.character(dt[[col]])) {
        dt[[col]] <- as.numeric(gsub(",", ".", dt[[col]]))
      }
    }
  }

  # Normalize column names
  old_names <- names(dt)
  new_names <- old_names
  new_names <- gsub("weight_\\(g\\)|weight \\(gr\\)", "weight_g", new_names)
  new_names <- gsub("volume_\\(l\\)|volume \\(l\\)", "volume_l", new_names)
  setnames(dt, old_names, new_names)

  # Add missing columns with defaults
  if (!"import_order_size" %in% names(dt))
    dt[, import_order_size := 50]
  if (!"import_delivery_time" %in% names(dt))
    dt[, import_delivery_time := 2]
  if (!"shelf_life" %in% names(dt))
    dt[, shelf_life := 7]
  if (!"gekoeld" %in% names(dt))
    dt[, gekoeld := 0]
  if (!"bevroren" %in% names(dt))
    dt[, bevroren := 0]
  if (!"weight_g" %in% names(dt))
    dt[, weight_g := 0.1]
  if (!"volume_l" %in% names(dt))
    dt[, volume_l := 0.1]
  if (!"price" %in% names(dt))
    dt[, price := 1.0]
  if (!"inkoop" %in% names(dt))
    dt[, inkoop := 0.5]

  # Ensure numeric types
  num_fields <- c("price", "inkoop", "weight_g", "volume_l",
                  "shelf_life", "gekoeld", "bevroren",
                  "import_order_size", "import_delivery_time")
  for (col in intersect(num_fields, names(dt))) {
    if (is.character(dt[[col]])) {
      dt[[col]] <- as.numeric(gsub(",", ".", dt[[col]]))
    }
  }

  # Add product_idx
  dt[, product_idx := .I]

  # Add default weekly demand if not present
  if (!"weekly_demand" %in% names(dt))
    dt[, weekly_demand := 100]

  dt
}

haversine_km <- function(lat1, lng1, lat2, lng2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlng <- (lng2 - lng1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlng / 2)^2
  2 * R * asin(sqrt(a))
}

generate_demand <- function(products, stores, horizon = PLANNING_HORIZON,
                            error_pct = DEFAULT_ERROR_PCT,
                            start_date = Sys.Date(),
                            demand_overrides = NULL) {
  dates <- start_date + 0:(horizon - 1)
  wday_idx <- as.integer(format(dates, "%u"))  # 1=Mon, 7=Sun
  month_idx <- as.integer(format(dates, "%m"))

  demand_list <- list()
  for (s in seq_len(nrow(stores))) {
    for (p in seq_len(nrow(products))) {
      # Check for per-store override
      weekly_dem <- NULL
      if (!is.null(demand_overrides) &&
          nrow(demand_overrides) >= p &&
          stores$store_name[s] %in% names(demand_overrides)) {
        weekly_dem <- demand_overrides[[stores$store_name[s]]][p]
      }
      if (is.null(weekly_dem) || is.na(weekly_dem)) {
        weekly_dem <- products$weekly_demand[p] * stores$multiplier[s]
      }
      daily_base <- weekly_dem / 7

      daily_demand <- numeric(horizon)
      for (d in seq_len(horizon)) {
        seasonal <- WEEKLY_SEASONALITY[wday_idx[d]] *
          MONTHLY_SEASONALITY[month_idx[d]]
        err <- runif(1, 1 - error_pct / 100, 1 + error_pct / 100)
        daily_demand[d] <- max(0, round(daily_base * seasonal * err, 1))
      }
      demand_list[[length(demand_list) + 1]] <- data.table(
        store_id    = stores$store_id[s],
        product_idx = p,
        day         = 1:horizon,
        date        = dates,
        demand      = daily_demand
      )
    }
  }
  rbindlist(demand_list)
}

# ---- Optimization (LP per product with lpSolve) ---------------------------
run_optimization <- function(products, stores, demand_dt, vehicles,
                             distances, wh_capacity, store_capacity,
                             service_level) {
  D <- PLANNING_HORIZON
  S <- nrow(stores)
  P <- nrow(products)

  # Transport cost per unit weight to each store
  # Use cheapest vehicle, approximate cost per kg
  best_cost_per_km <- min(vehicles$cost_per_km)
  best_fixed_cost  <- min(vehicles$fixed_cost_per_trip)
  best_max_weight  <- max(vehicles$max_weight)

  cost_per_kg_to_store <- numeric(S)
  for (s in seq_len(S)) {
    trip_cost <- best_fixed_cost + 2 * distances[s] * best_cost_per_km
    cost_per_kg_to_store[s] <- trip_cost / best_max_weight
  }

  # Results containers
  all_import    <- list()
  all_delivery  <- list()
  all_wh_inv    <- list()
  all_st_inv    <- list()
  all_stockout  <- list()

  for (p in seq_len(P)) {
    product <- products[p, ]
    margin  <- product$price - product$inkoop
    weight  <- product$weight_g / 1000  # kg per unit
    lead_time <- product$import_delivery_time

    # Get demand for this product across stores
    dem_p <- demand_dt[product_idx == p]

    # Build demand matrix [S x D]
    dem_matrix <- matrix(0, nrow = S, ncol = D)
    for (s in seq_len(S)) {
      dd <- dem_p[store_id == stores$store_id[s]]
      if (nrow(dd) > 0) {
        dem_matrix[s, ] <- dd$demand[order(dd$day)]
      }
    }

    # --- Variables layout for LP ---
    # import_qty[d]         : indices 1..D
    # delivery_qty[s, d]    : indices D + (s-1)*D + d  → D+1 .. D+S*D
    # stockout[s, d]        : indices D+S*D + (s-1)*D + d → D+S*D+1 .. D+2*S*D
    n_vars <- D + 2 * S * D

    idx_import   <- function(d) d
    idx_delivery <- function(s, d) D + (s - 1) * D + d
    idx_stockout <- function(s, d) D + S * D + (s - 1) * D + d

    # --- Objective ---
    obj <- rep(0, n_vars)
    for (s in seq_len(S)) {
      for (d in seq_len(D)) {
        # Transport cost per unit delivered (approximated)
        obj[idx_delivery(s, d)] <- cost_per_kg_to_store[s] * weight
        # Lost margin for stockout
        obj[idx_stockout(s, d)] <- margin
      }
    }

    # --- Constraints ---
    con_mat  <- list()
    con_dir  <- character(0)
    con_rhs  <- numeric(0)

    # Initial inventories
    total_daily_demand <- sum(dem_matrix) / D
    wh_inv_0 <- min(
      round(total_daily_demand * 5),
      wh_capacity$max_weight / max(weight, 0.001) / P
    ) * 0.8

    store_inv_0 <- numeric(S)
    for (s in seq_len(S)) {
      avg_demand_s <- sum(dem_matrix[s, ]) / D
      store_inv_0[s] <- min(
        round(avg_demand_s * 3),
        store_capacity$max_weight / max(weight, 0.001) / P
      )
    }

    # 1) Warehouse inventory >= 0 for each day
    for (d in seq_len(D)) {
      row <- rep(0, n_vars)
      # wh_inv[d] = wh_inv_0 + sum(import[1..d]) - sum_s(sum(delivery[s,1..d]))
      for (dd in 1:d) {
        ld <- dd - lead_time
        if (ld >= 1) {
          row[idx_import(ld)] <- 1
        }
      }
      for (s in seq_len(S)) {
        for (dd in 1:d) {
          row[idx_delivery(s, dd)] <- row[idx_delivery(s, dd)] - 1
        }
      }
      con_mat[[length(con_mat) + 1]] <- row
      con_dir <- c(con_dir, ">=")
      con_rhs <- c(con_rhs, -wh_inv_0)
    }

    # 2) Store inventory >= 0 for each store, each day
    for (s in seq_len(S)) {
      cum_demand <- cumsum(dem_matrix[s, ])
      for (d in seq_len(D)) {
        row <- rep(0, n_vars)
        for (dd in 1:d) {
          row[idx_delivery(s, dd)] <- 1
          row[idx_stockout(s, dd)] <- 1
        }
        con_mat[[length(con_mat) + 1]] <- row
        con_dir <- c(con_dir, ">=")
        con_rhs <- c(con_rhs, cum_demand[d] - store_inv_0[s])
      }
    }

    # 3) Warehouse capacity constraint for each day
    wh_cap_units <- wh_capacity$max_weight / max(weight, 0.001) / P
    for (d in seq_len(D)) {
      row <- rep(0, n_vars)
      for (dd in 1:d) {
        ld <- dd - lead_time
        if (ld >= 1) {
          row[idx_import(ld)] <- 1
        }
      }
      for (s in seq_len(S)) {
        for (dd in 1:d) {
          row[idx_delivery(s, dd)] <- row[idx_delivery(s, dd)] - 1
        }
      }
      con_mat[[length(con_mat) + 1]] <- row
      con_dir <- c(con_dir, "<=")
      con_rhs <- c(con_rhs, wh_cap_units - wh_inv_0)
    }

    # 4) Store capacity constraint
    store_cap_units <- store_capacity$max_weight / max(weight, 0.001) / P
    for (s in seq_len(S)) {
      cum_demand <- cumsum(dem_matrix[s, ])
      for (d in seq_len(D)) {
        row <- rep(0, n_vars)
        for (dd in 1:d) {
          row[idx_delivery(s, dd)] <- 1
          row[idx_stockout(s, dd)] <- 1
        }
        con_mat[[length(con_mat) + 1]] <- row
        con_dir <- c(con_dir, "<=")
        con_rhs <- c(con_rhs, store_cap_units - store_inv_0[s] + cum_demand[d])
      }
    }

    # 5) Service level for this product:
    #    sum(margin * stockout) <= (1 - service_level) * total expected margin
    total_expected_margin <- margin * sum(dem_matrix)
    row <- rep(0, n_vars)
    for (s in seq_len(S)) {
      for (d in seq_len(D)) {
        row[idx_stockout(s, d)] <- margin
      }
    }
    con_mat[[length(con_mat) + 1]] <- row
    con_dir <- c(con_dir, "<=")
    con_rhs <- c(con_rhs, (1 - service_level) * total_expected_margin)

    # 6) Import quantity must be non-negative multiple of order size
    #    (relax to just non-negative for LP)

    # Solve LP
    A <- do.call(rbind, con_mat)

    sol <- tryCatch({
      lp("min", obj, A, con_dir, con_rhs)
    }, error = function(e) {
      list(status = 1, solution = rep(0, n_vars))
    })

    if (sol$status != 0) {
      # Infeasible - use fallback: deliver exactly demand, no stockout control
      sol_vec <- rep(0, n_vars)
      for (s in seq_len(S)) {
        for (d in seq_len(D)) {
          sol_vec[idx_delivery(s, d)] <- dem_matrix[s, d]
        }
      }
      # Back-calculate imports needed
      for (d in seq_len(D)) {
        total_del <- sum(sapply(seq_len(S), function(s) sol_vec[idx_delivery(s, d)]))
        import_day <- min(d + lead_time, D)
        if (import_day <= D) {
          sol_vec[idx_import(d)] <- total_del
        }
      }
    } else {
      sol_vec <- sol$solution
    }

    # Extract results
    import_qty <- sapply(seq_len(D), function(d) max(0, round(sol_vec[idx_import(d)], 1)))

    # Round import to order size multiples
    ord_size <- product$import_order_size
    if (!is.na(ord_size) && ord_size > 0) {
      import_qty <- ceiling(import_qty / ord_size) * ord_size
      import_qty[import_qty < ord_size & import_qty > 0] <- ord_size
    }

    for (d in seq_len(D)) {
      if (import_qty[d] > 0) {
        all_import[[length(all_import) + 1]] <- data.table(
          product_idx = p,
          product_name = product$name,
          day = d,
          import_qty = import_qty[d]
        )
      }
    }

    # Simulate forward with rounded values to get actual inventories
    wh_inv <- numeric(D)
    pending_imports <- rep(0, D + lead_time + 1)
    for (d in seq_len(D)) {
      pending_imports[d + lead_time] <- pending_imports[d + lead_time] + import_qty[d]
    }

    curr_wh <- wh_inv_0
    for (d in seq_len(D)) {
      curr_wh <- curr_wh + if (d <= length(pending_imports)) pending_imports[d] else 0
      total_del_today <- 0
      for (s in seq_len(S)) {
        del <- max(0, round(sol_vec[idx_delivery(s, d)], 1))
        so  <- max(0, round(sol_vec[idx_stockout(s, d)], 1))

        # Limit delivery to warehouse availability
        del <- min(del, curr_wh)
        curr_wh <- curr_wh - del

        # Store inventory update
        if (d == 1) {
          prev_si <- store_inv_0[s]
        } else {
          prev_row <- all_st_inv[sapply(all_st_inv, function(x)
            x$product_idx[1] == p & x$store_id[1] == stores$store_id[s] &
              x$day[1] == (d - 1))]
          if (length(prev_row) > 0) {
            prev_si <- prev_row[[1]]$store_inventory
          } else {
            prev_si <- store_inv_0[s]
          }
        }

        actual_inv <- prev_si + del - dem_matrix[s, d]
        actual_so <- max(0, -actual_inv)
        actual_inv <- max(0, actual_inv)

        all_delivery[[length(all_delivery) + 1]] <- data.table(
          store_id = stores$store_id[s],
          product_idx = p,
          product_name = product$name,
          day = d,
          delivery_qty = del
        )
        all_st_inv[[length(all_st_inv) + 1]] <- data.table(
          store_id = stores$store_id[s],
          product_idx = p,
          product_name = product$name,
          day = d,
          store_inventory = actual_inv,
          stockout = actual_so
        )
        if (actual_so > 0) {
          all_stockout[[length(all_stockout) + 1]] <- data.table(
            store_id = stores$store_id[s],
            product_idx = p,
            product_name = product$name,
            day = d,
            stockout_qty = actual_so,
            lost_margin = actual_so * margin
          )
        }
        total_del_today <- total_del_today + del
      }
      wh_inv[d] <- curr_wh
    }

    for (d in seq_len(D)) {
      all_wh_inv[[length(all_wh_inv) + 1]] <- data.table(
        product_idx = p,
        product_name = product$name,
        day = d,
        warehouse_inventory = wh_inv[d]
      )
    }
  }

  # Combine results
  import_schedule <- if (length(all_import) > 0) rbindlist(all_import) else
    data.table(product_idx = integer(), product_name = character(),
               day = integer(), import_qty = numeric())
  delivery_schedule <- if (length(all_delivery) > 0) rbindlist(all_delivery) else
    data.table(store_id = integer(), product_idx = integer(),
               product_name = character(), day = integer(),
               delivery_qty = numeric())
  wh_inventory <- if (length(all_wh_inv) > 0) rbindlist(all_wh_inv) else
    data.table(product_idx = integer(), product_name = character(),
               day = integer(), warehouse_inventory = numeric())
  st_inventory <- if (length(all_st_inv) > 0) rbindlist(all_st_inv) else
    data.table(store_id = integer(), product_idx = integer(),
               product_name = character(), day = integer(),
               store_inventory = numeric(), stockout = numeric())
  stockouts <- if (length(all_stockout) > 0) rbindlist(all_stockout) else
    data.table(store_id = integer(), product_idx = integer(),
               product_name = character(), day = integer(),
               stockout_qty = numeric(), lost_margin = numeric())

  # Calculate truck trips
  truck_trips <- calculate_truck_trips(delivery_schedule, products, stores,
                                       vehicles, distances)

  # Cost analysis
  transport_cost <- calculate_transport_cost(truck_trips, vehicles, distances)
  total_lost_margin <- if (nrow(stockouts) > 0) sum(stockouts$lost_margin) else 0
  total_demand_margin <- sum(demand_dt$demand *
                               products$price[demand_dt$product_idx] -
                               demand_dt$demand *
                               products$inkoop[demand_dt$product_idx])
  achieved_sl <- if (total_demand_margin > 0) {
    1 - total_lost_margin / total_demand_margin
  } else 1.0

  list(
    import_schedule    = import_schedule,
    delivery_schedule  = delivery_schedule,
    wh_inventory       = wh_inventory,
    store_inventory    = st_inventory,
    stockouts          = stockouts,
    truck_trips        = truck_trips,
    transport_cost     = transport_cost,
    total_lost_margin  = total_lost_margin,
    total_cost         = transport_cost + total_lost_margin,
    service_level      = achieved_sl,
    demand             = demand_dt
  )
}

calculate_truck_trips <- function(delivery_schedule, products, stores,
                                  vehicles, distances) {
  if (nrow(delivery_schedule) == 0 || sum(delivery_schedule$delivery_qty) == 0) {
    return(data.table(day = integer(), store_id = integer(),
                      vehicle_type = character(), trips = integer(),
                      weight_kg = numeric(), volume_l = numeric()))
  }

  # Merge product weights/volumes
  del <- copy(delivery_schedule)
  del <- merge(del, products[, .(product_idx, weight_g, volume_l)],
               by = "product_idx", all.x = TRUE)
  del[, weight_kg := delivery_qty * weight_g / 1000]
  del[, total_vol := delivery_qty * volume_l]

  # Aggregate per store per day
  daily_store <- del[, .(total_weight = sum(weight_kg, na.rm = TRUE),
                         total_volume = sum(total_vol, na.rm = TRUE)),
                     by = .(day, store_id)]
  daily_store <- daily_store[total_weight > 0 | total_volume > 0]

  if (nrow(daily_store) == 0) {
    return(data.table(day = integer(), store_id = integer(),
                      vehicle_type = character(), trips = integer(),
                      weight_kg = numeric(), volume_l = numeric()))
  }

  trips_list <- list()
  for (i in seq_len(nrow(daily_store))) {
    row <- daily_store[i]
    s_idx <- which(stores$store_id == row$store_id)
    if (length(s_idx) == 0) next
    dist <- distances[s_idx]

    remaining_weight <- row$total_weight
    remaining_volume <- row$total_volume

    # Sort vehicles: prefer truck (larger capacity)
    v_order <- order(-vehicles$max_weight)
    for (v in v_order) {
      veh <- vehicles[v, ]
      max_trips <- floor(veh$max_distance_per_day / (2 * max(dist, 1)))
      max_trips <- max(max_trips, 0)

      while (remaining_weight > 0 && remaining_volume > 0 && max_trips > 0) {
        trips_list[[length(trips_list) + 1]] <- data.table(
          day = row$day,
          store_id = row$store_id,
          vehicle_type = veh$type,
          trips = 1L,
          weight_kg = min(remaining_weight, veh$max_weight),
          volume_l = min(remaining_volume, veh$max_volume)
        )
        remaining_weight <- remaining_weight - veh$max_weight
        remaining_volume <- remaining_volume - veh$max_volume
        max_trips <- max_trips - 1
      }
      if (remaining_weight <= 0 && remaining_volume <= 0) break
    }
  }

  if (length(trips_list) > 0) rbindlist(trips_list) else
    data.table(day = integer(), store_id = integer(),
               vehicle_type = character(), trips = integer(),
               weight_kg = numeric(), volume_l = numeric())
}

calculate_transport_cost <- function(truck_trips, vehicles, distances) {
  if (nrow(truck_trips) == 0) return(0)
  total <- 0
  for (i in seq_len(nrow(truck_trips))) {
    tt <- truck_trips[i]
    v_idx <- which(vehicles$type == tt$vehicle_type)[1]
    if (is.na(v_idx)) next
    veh <- vehicles[v_idx, ]
    # Find distance for this store
    dist <- 50  # fallback
    s_idx <- which(truck_trips$store_id == tt$store_id)[1]
    if (!is.na(s_idx) && s_idx <= length(distances)) {
      dist <- distances[min(s_idx, length(distances))]
    }
    total <- total + tt$trips * (veh$fixed_cost_per_trip + 2 * dist * veh$cost_per_km)
  }
  total
}


# Simple value box since we're not using shinydashboard
valueBoxUI <- function(id) {
  column(3,
         div(style = "background:#f8f9fa; border:1px solid #dee2e6;
                      border-radius:8px; padding:15px; text-align:center;
                      margin-bottom:10px;",
             h5(textOutput(paste0(id, "_title")),
                style = "margin:0; color:#6c757d;"),
             h3(textOutput(paste0(id, "_value")),
                style = "margin:5px 0; color:#212529;")
         ))
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  titlePanel("Supply Chain Optimization Dashboard"),
  sidebarLayout(
    # ---- Sidebar ----------------------------------------------------------
    sidebarPanel(
      width = 3,
      h4("Data Input"),
      fileInput("csv_upload", "Upload Product CSV",
                accept = c(".csv", ".txt")),
      hr(),

      h4("Stores"),
      numericInput("n_stores", "Number of Stores", value = 3, min = 1, max = 10),
      actionButton("reset_stores", "Reset Store Locations"),
      hr(),

      h4("Service Level"),
      sliderInput("service_level", "OTIN Service Level (%)",
                  min = 80, max = 100, value = 99, step = 0.5),
      hr(),

      h4("Demand Settings"),
      numericInput("error_pct", "Demand Error (%)", value = 20,
                   min = 0, max = 50),
      hr(),

      h4("Warehouse Capacity"),
      numericInput("wh_max_weight", "Max Weight (kg)", value = 50000),
      numericInput("wh_max_volume", "Max Volume (L)", value = 100000),
      numericInput("wh_max_refrig_weight", "Max Refrigerated Weight (kg)",
                   value = 20000),
      numericInput("wh_max_frozen_weight", "Max Frozen Weight (kg)",
                   value = 10000),
      hr(),

      h4("Store Capacity (per store)"),
      numericInput("st_max_weight", "Max Weight (kg)", value = 5000),
      numericInput("st_max_volume", "Max Volume (L)", value = 10000),
      numericInput("st_max_refrig_weight", "Max Refrigerated Weight (kg)",
                   value = 2000),
      numericInput("st_max_frozen_weight", "Max Frozen Weight (kg)",
                   value = 1000),
      hr(),

      actionButton("run_optim", "Run Optimization",
                   class = "btn-primary btn-lg btn-block",
                   style = "width:100%; margin-top:10px;"),
      hr(),
      textOutput("optim_status")
    ),

    # ---- Main Panel -------------------------------------------------------
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # Tab 1: Map -------------------------------------------------------
        tabPanel("Map",
                 h3("Logistics Locations"),
                 p("Drag markers to reposition the warehouse and stores."),
                 leafletOutput("map", height = "600px"),
                 hr(),
                 h4("Store Settings"),
                 DTOutput("store_table")
        ),

        # Tab 2: Products --------------------------------------------------
        tabPanel("Products",
                 h3("Product Catalog"),
                 fluidRow(
                   column(6, actionButton("add_product", "Add Product")),
                   column(6, actionButton("remove_product",
                                          "Remove Selected"))
                 ),
                 br(),
                 DTOutput("product_table"),
                 hr(),
                 h4("Edit Product"),
                 uiOutput("product_edit_ui")
        ),

        # Tab 3: Demand ----------------------------------------------------
        tabPanel("Demand",
                 h3("Demand Forecast"),
                 fluidRow(
                   column(4, selectInput("demand_product", "Product",
                                         choices = NULL)),
                   column(4, selectInput("demand_store", "Store",
                                         choices = NULL))
                 ),
                 plotOutput("demand_chart", height = "350px"),
                 hr(),
                 h4("Per-Store Product Demand (weekly units)"),
                 p("Edit cells to set custom weekly demand per product per store. Values override the base weekly demand multiplied by the store multiplier."),
                 DTOutput("demand_override_table"),
                 hr(),
                 h4("Seasonality Patterns"),
                 fluidRow(
                   column(6, plotOutput("weekly_seasonality_chart",
                                        height = "250px")),
                   column(6, plotOutput("monthly_seasonality_chart",
                                        height = "250px"))
                 )
        ),

        # Tab 3b: Vehicle Fleet -----------------------------------------------
        tabPanel("Vehicle Fleet",
                 h3("Vehicle Fleet Configuration"),
                 p("Edit vehicle parameters directly in the table below."),
                 DTOutput("vehicle_fleet_table")
        ),

        # Tab 4: Optimization Results --------------------------------------
        tabPanel("Optimization Results",
                 h3("Optimization Results"),
                 conditionalPanel(
                   condition = "output.has_results",
                   fluidRow(
                     valueBoxUI("vb_cost"),
                     valueBoxUI("vb_transport"),
                     valueBoxUI("vb_lost_margin"),
                     valueBoxUI("vb_service")
                   ),
                   hr(),
                   tabsetPanel(
                     tabPanel("Import Schedule",
                              DTOutput("import_table")),
                     tabPanel("Warehouse Inventory",
                              plotOutput("wh_inv_chart", height = "400px"),
                              DTOutput("wh_inv_table")),
                     tabPanel("Store Inventory",
                              fluidRow(
                                column(4, selectInput("inv_store",
                                                      "Store", choices = NULL)),
                                column(4, selectInput("inv_product",
                                                      "Product", choices = NULL))
                              ),
                              plotOutput("st_inv_chart", height = "400px")),
                     tabPanel("Delivery Schedule",
                              DTOutput("delivery_table")),
                     tabPanel("Vehicle Usage",
                              plotOutput("vehicle_chart", height = "400px"),
                              DTOutput("vehicle_table"))
                   )
                 ),
                 conditionalPanel(
                   condition = "!output.has_results",
                   h4("Run the optimization to see results.",
                      style = "color:gray; text-align:center; margin-top:50px;")
                 )
        ),

        # Tab 5: Cost Analysis ----------------------------------------------
        tabPanel("Cost Analysis",
                 h3("Cost Analysis"),
                 conditionalPanel(
                   condition = "output.has_results",
                   fluidRow(
                     column(6, plotOutput("cost_breakdown_chart",
                                          height = "350px")),
                     column(6, plotOutput("cost_daily_chart",
                                          height = "350px"))
                   ),
                   hr(),
                   plotOutput("service_level_chart", height = "300px")
                 ),
                 conditionalPanel(
                   condition = "!output.has_results",
                   h4("Run the optimization to see cost analysis.",
                      style = "color:gray; text-align:center; margin-top:50px;")
                 )
        )
      )
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  # ---- Reactive Values ---------------------------------------------------
  rv <- reactiveValues(
    products        = NULL,
    stores          = copy(DEFAULT_STORES),
    wh_lat          = DEFAULT_WH_LAT,
    wh_lng          = DEFAULT_WH_LNG,
    map_clicks      = 0,
    results         = NULL,
    demand_cache    = NULL,
    demand_overrides = NULL,
    vehicles        = {
      vdt <- data.table(
        type = c("Truck", "Van"),
        count = c(1L, 1L),
        cost_per_km = c(1.20, 0.60),
        speed = c(60, 80),
        fixed_cost_per_trip = c(50, 25),
        max_weight = c(8000, 1500),
        max_volume = c(40000, 8000),
        refrigerated_capable = c(TRUE, TRUE),
        frozen_capable = c(TRUE, FALSE)
      )
      copy(vdt)
    }
  )

  # ---- Vehicle Data (reactive) -------------------------------------------
  vehicles_dt <- reactive({
    vdt <- copy(rv$vehicles)
    vdt[, max_distance_per_day := speed * 16]
    vdt
  })

  # ---- Load Default Products ---------------------------------------------
  observe({
    default_path <- file.path(getwd(), "store_products.csv")
    if (file.exists(default_path) && is.null(rv$products)) {
      rv$products <- read_product_csv(default_path)
    }
  })

  # ---- CSV Upload --------------------------------------------------------
  observeEvent(input$csv_upload, {
    req(input$csv_upload)
    rv$products <- read_product_csv(input$csv_upload$datapath)
    rv$results <- NULL
    rv$demand_overrides <- NULL  # Reset demand overrides on new CSV
    showNotification(paste("Loaded", nrow(rv$products), "products"),
                     type = "message")
  })

  # ---- Update Stores on n_stores Change ----------------------------------
  observeEvent(input$n_stores, {
    n <- input$n_stores
    current <- rv$stores
    if (n > nrow(current)) {
      # Add new stores with random positions near Amsterdam
      for (i in (nrow(current) + 1):n) {
        new_store <- data.table(
          store_id = i,
          store_name = paste("Store", i),
          lat = DEFAULT_WH_LAT + runif(1, -0.5, 0.5),
          lng = DEFAULT_WH_LNG + runif(1, -0.5, 0.5),
          multiplier = 1.0
        )
        current <- rbind(current, new_store)
      }
    } else if (n < nrow(current)) {
      current <- current[1:n]
    }
    rv$stores <- current
    rv$results <- NULL
    rv$demand_overrides <- NULL  # Reset demand overrides when stores change
  })

  observeEvent(input$reset_stores, {
    rv$stores <- copy(DEFAULT_STORES)[1:min(input$n_stores, 3)]
    if (input$n_stores > 3) {
      for (i in 4:input$n_stores) {
        rv$stores <- rbind(rv$stores, data.table(
          store_id = i,
          store_name = paste("Store", i),
          lat = DEFAULT_WH_LAT + runif(1, -0.5, 0.5),
          lng = DEFAULT_WH_LNG + runif(1, -0.5, 0.5),
          multiplier = 1.0
        ))
      }
    }
    rv$wh_lat <- DEFAULT_WH_LAT
    rv$wh_lng <- DEFAULT_WH_LNG
    rv$map_clicks <- 0
  })

  # ---- Distances (reactive) ---------------------------------------------
  distances <- reactive({
    stores <- rv$stores
    sapply(seq_len(nrow(stores)), function(i) {
      haversine_km(rv$wh_lat, rv$wh_lng,
                   stores$lat[i], stores$lng[i])
    })
  })

  # ---- Map ---------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = DEFAULT_WH_LNG, lat = DEFAULT_WH_LAT, zoom = 8)
  })

  observe({
    stores <- rv$stores
    dists <- distances()
    proxy <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

    # Warehouse marker (draggable)
    proxy <- proxy %>% addMarkers(
      lng = rv$wh_lng, lat = rv$wh_lat,
      layerId = "warehouse",
      popup = "Warehouse (drag to move)",
      options = markerOptions(draggable = TRUE)
    )

    # Store markers (draggable) and routes
    for (i in seq_len(nrow(stores))) {
      proxy <- proxy %>%
        addMarkers(
          lng = stores$lng[i], lat = stores$lat[i],
          layerId = paste0("store_", stores$store_id[i]),
          popup = paste0(stores$store_name[i],
                         "<br>Distance: ", round(dists[i], 1), " km",
                         "<br>Multiplier: ", stores$multiplier[i],
                         "<br><i>Drag to move</i>"),
          icon = makeIcon(
            iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png",
            iconWidth = 25, iconHeight = 41,
            iconAnchorX = 12, iconAnchorY = 41
          ),
          options = markerOptions(draggable = TRUE)
        ) %>%
        addPolylines(
          lng = c(rv$wh_lng, stores$lng[i]),
          lat = c(rv$wh_lat, stores$lat[i]),
          color = "steelblue", weight = 2, opacity = 0.6,
          popup = paste(round(dists[i], 1), "km")
        )
    }
  })

  # Drag-end: update warehouse location
  observeEvent(input$map_marker_dragend, {
    evt <- input$map_marker_dragend
    if (evt$id == "warehouse") {
      rv$wh_lat <- evt$lat
      rv$wh_lng <- evt$lng
    } else if (grepl("^store_", evt$id)) {
      sid <- as.integer(sub("^store_", "", evt$id))
      idx <- which(rv$stores$store_id == sid)
      if (length(idx) == 1) {
        rv$stores$lat[idx] <- evt$lat
        rv$stores$lng[idx] <- evt$lng
      }
    }
  })

  # ---- Store Table (editable multipliers) --------------------------------
  output$store_table <- renderDT({
    stores <- rv$stores
    dists <- distances()
    display <- data.table(
      Store = stores$store_name,
      Latitude = round(stores$lat, 4),
      Longitude = round(stores$lng, 4),
      `Distance (km)` = round(dists, 1),
      Multiplier = stores$multiplier
    )
    datatable(display, editable = list(target = "cell",
                                       disable = list(columns = c(0, 1, 2, 3, 4))),
              options = list(pageLength = 10, dom = "t"),
              rownames = FALSE)
  })

  observeEvent(input$store_table_cell_edit, {
    info <- input$store_table_cell_edit
    if (info$col == 4) {  # Multiplier column (0-indexed: 4)
      new_val <- as.numeric(info$value)
      if (!is.na(new_val) && new_val > 0) {
        rv$stores$multiplier[info$row] <- new_val
      }
    }
  })

  # ---- Product Table -----------------------------------------------------
  output$product_table <- renderDT({
    req(rv$products)
    display_cols <- intersect(
      c("product_idx", "name", "price", "inkoop", "weight_g", "volume_l",
        "shelf_life", "gekoeld", "bevroren", "import_order_size",
        "import_delivery_time", "weekly_demand"),
      names(rv$products)
    )
    datatable(rv$products[, ..display_cols],
              selection = "single",
              editable = list(
                target = "cell",
                disable = list(columns = c(0))
              ),
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE)
  })

  observeEvent(input$product_table_cell_edit, {
    info <- input$product_table_cell_edit
    display_cols <- intersect(
      c("product_idx", "name", "price", "inkoop", "weight_g", "volume_l",
        "shelf_life", "gekoeld", "bevroren", "import_order_size",
        "import_delivery_time", "weekly_demand"),
      names(rv$products)
    )
    col_name <- display_cols[info$col + 1]
    new_val <- info$value
    if (col_name != "name") new_val <- as.numeric(new_val)
    rv$products[info$row, (col_name) := new_val]
  })

  observeEvent(input$add_product, {
    req(rv$products)
    n <- nrow(rv$products) + 1
    new_row <- data.table(
      name = paste("New Product", n),
      price = 1.00,
      inkoop = 0.50,
      weight_g = 0.1,
      volume_l = 0.1,
      shelf_life = 7,
      gekoeld = 0,
      bevroren = 0,
      import_order_size = 50,
      import_delivery_time = 2,
      product_idx = n,
      weekly_demand = 100
    )
    # Add any missing columns from original
    for (col in setdiff(names(rv$products), names(new_row))) {
      new_row[, (col) := NA]
    }
    for (col in setdiff(names(new_row), names(rv$products))) {
      rv$products[, (col) := NA]
    }
    rv$products <- rbind(rv$products, new_row, fill = TRUE)
    rv$products[, product_idx := .I]
  })

  observeEvent(input$remove_product, {
    req(rv$products)
    sel <- input$product_table_rows_selected
    if (length(sel) > 0) {
      rv$products <- rv$products[-sel]
      rv$products[, product_idx := .I]
    }
  })

  output$product_edit_ui <- renderUI({
    p("Click a cell in the table above to edit product details directly.")
  })

  # ---- Vehicle Fleet Table -----------------------------------------------
  output$vehicle_fleet_table <- renderDT({
    vdt <- copy(rv$vehicles)
    datatable(vdt,
              editable = list(target = "cell",
                              disable = list(columns = c(0))),
              options = list(pageLength = 10, dom = "t", scrollX = TRUE),
              rownames = FALSE)
  })

  observeEvent(input$vehicle_fleet_table_cell_edit, {
    info <- input$vehicle_fleet_table_cell_edit
    col_name <- names(rv$vehicles)[info$col + 1]
    new_val <- info$value
    if (col_name %in% c("refrigerated_capable", "frozen_capable")) {
      new_val <- as.logical(new_val)
    } else if (col_name != "type") {
      new_val <- as.numeric(new_val)
    }
    rv$vehicles[info$row, (col_name) := new_val]
  })

  # ---- Demand Overrides Table --------------------------------------------
  observe({
    req(rv$products, rv$stores)
    if (is.null(rv$demand_overrides) ||
        nrow(rv$demand_overrides) != nrow(rv$products) ||
        ncol(rv$demand_overrides) != nrow(rv$stores) + 1) {
      # Build default demand overrides: base_weekly_demand * store_multiplier
      mat <- data.table(Product = rv$products$name)
      for (s in seq_len(nrow(rv$stores))) {
        col_name <- rv$stores$store_name[s]
        mat[, (col_name) := round(rv$products$weekly_demand *
                                    rv$stores$multiplier[s], 1)]
      }
      rv$demand_overrides <- mat
    }
  })

  output$demand_override_table <- renderDT({
    req(rv$demand_overrides)
    datatable(rv$demand_overrides,
              editable = list(target = "cell",
                              disable = list(columns = c(0))),
              options = list(pageLength = 20, scrollX = TRUE, dom = "tp"),
              rownames = FALSE)
  })

  observeEvent(input$demand_override_table_cell_edit, {
    info <- input$demand_override_table_cell_edit
    col_name <- names(rv$demand_overrides)[info$col + 1]
    new_val <- as.numeric(info$value)
    if (!is.na(new_val) && new_val >= 0) {
      rv$demand_overrides[info$row, (col_name) := new_val]
    }
  })

  # ---- Demand Selectors --------------------------------------------------
  observe({
    req(rv$products)
    choices <- setNames(rv$products$product_idx, rv$products$name)
    updateSelectInput(session, "demand_product", choices = choices)
    updateSelectInput(session, "inv_product", choices = choices)
  })

  observe({
    stores <- rv$stores
    choices <- setNames(stores$store_id, stores$store_name)
    updateSelectInput(session, "demand_store", choices = choices)
    updateSelectInput(session, "inv_store", choices = choices)
  })

  # ---- Demand Chart ------------------------------------------------------
  demand_data <- reactive({
    req(rv$products, rv$stores)
    generate_demand(rv$products, rv$stores, PLANNING_HORIZON,
                    input$error_pct,
                    demand_overrides = rv$demand_overrides)
  })

  output$demand_chart <- renderPlot({
    req(rv$products, rv$stores)
    dd <- demand_data()
    p_idx <- as.integer(input$demand_product)
    s_id  <- as.integer(input$demand_store)
    if (is.na(p_idx) || is.na(s_id)) return(NULL)

    plot_data <- dd[product_idx == p_idx & store_id == s_id]
    if (nrow(plot_data) == 0) return(NULL)

    p_name <- rv$products$name[rv$products$product_idx == p_idx][1]
    s_name <- rv$stores$store_name[rv$stores$store_id == s_id][1]

    ggplot(plot_data, aes(x = date, y = demand)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_smooth(method = "loess", formula = y ~ x,
                  se = FALSE, color = "darkred", linewidth = 1) +
      labs(title = paste("Daily Demand Forecast:", p_name, "-", s_name),
           x = "Date", y = "Units") +
      theme_minimal()
  })

  output$weekly_seasonality_chart <- renderPlot({
    df <- data.frame(
      Day = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                   levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      Factor = WEEKLY_SEASONALITY
    )
    ggplot(df, aes(x = Day, y = Factor)) +
      geom_col(fill = "coral", alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
      labs(title = "Weekly Seasonality", y = "Multiplier") +
      theme_minimal()
  })

  output$monthly_seasonality_chart <- renderPlot({
    df <- data.frame(
      Month = factor(month.abb, levels = month.abb),
      Factor = MONTHLY_SEASONALITY
    )
    ggplot(df, aes(x = Month, y = Factor)) +
      geom_col(fill = "mediumpurple", alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
      labs(title = "Monthly Seasonality", y = "Multiplier") +
      theme_minimal()
  })

  # ---- Optimization Execution -------------------------------------------
  observeEvent(input$run_optim, {
    req(rv$products, nrow(rv$products) > 0)

    showNotification("Running optimization...", id = "optim_notif",
                     duration = NULL, type = "message")

    # Build capacity lists
    wh_cap <- list(
      max_weight = input$wh_max_weight,
      max_volume = input$wh_max_volume,
      max_refrig_weight = input$wh_max_refrig_weight,
      max_frozen_weight = input$wh_max_frozen_weight
    )
    st_cap <- list(
      max_weight = input$st_max_weight,
      max_volume = input$st_max_volume,
      max_refrig_weight = input$st_max_refrig_weight,
      max_frozen_weight = input$st_max_frozen_weight
    )

    # Generate demand
    demand <- generate_demand(rv$products, rv$stores, PLANNING_HORIZON,
                              input$error_pct,
                              demand_overrides = rv$demand_overrides)
    rv$demand_cache <- demand

    # Run optimization
    tryCatch({
      results <- run_optimization(
        products      = rv$products,
        stores        = rv$stores,
        demand_dt     = demand,
        vehicles      = vehicles_dt(),
        distances     = distances(),
        wh_capacity   = wh_cap,
        store_capacity = st_cap,
        service_level = input$service_level / 100
      )
      rv$results <- results
      removeNotification("optim_notif")
      showNotification(
        paste("Optimization complete! Total cost:",
              format(round(results$total_cost, 2), big.mark = ","),
              "| Service level:",
              round(results$service_level * 100, 2), "%"),
        type = "message", duration = 10
      )
    }, error = function(e) {
      removeNotification("optim_notif")
      showNotification(paste("Optimization error:", e$message),
                       type = "error", duration = 15)
    })
  })

  output$optim_status <- renderText({
    if (is.null(rv$results)) {
      "Status: Ready - configure settings and click Run Optimization"
    } else {
      paste("Status: Complete | Cost:",
            format(round(rv$results$total_cost, 2), big.mark = ","),
            "| SL:", round(rv$results$service_level * 100, 2), "%")
    }
  })

  # ---- Has Results (for conditional panels) ------------------------------
  output$has_results <- reactive({ !is.null(rv$results) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # ---- Value Boxes -------------------------------------------------------
  output$vb_cost_title <- renderText("Total Cost")
  output$vb_cost_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$total_cost, 2), big.mark = ","))
  })
  output$vb_transport_title <- renderText("Transport Cost")
  output$vb_transport_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$transport_cost, 2), big.mark = ","))
  })
  output$vb_lost_margin_title <- renderText("Lost Margin")
  output$vb_lost_margin_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$total_lost_margin, 2),
                            big.mark = ","))
  })
  output$vb_service_title <- renderText("Service Level")
  output$vb_service_value <- renderText({
    req(rv$results)
    paste0(round(rv$results$service_level * 100, 2), "%")
  })

  # ---- Import Schedule Table ---------------------------------------------
  output$import_table <- renderDT({
    req(rv$results)
    dt <- rv$results$import_schedule
    if (nrow(dt) == 0) return(datatable(data.frame(Message = "No imports needed")))

    # Pivot: products as rows, days as columns
    wide <- dcast(dt, product_name ~ paste0("Day_", day),
                  value.var = "import_qty", fill = 0, fun.aggregate = sum)
    datatable(wide, options = list(scrollX = TRUE, pageLength = 20),
              rownames = FALSE) %>%
      formatRound(columns = setdiff(names(wide), "product_name"), digits = 0)
  })

  # ---- Warehouse Inventory Chart -----------------------------------------
  output$wh_inv_chart <- renderPlot({
    req(rv$results)
    dt <- rv$results$wh_inventory
    if (nrow(dt) == 0) return(NULL)

    agg <- dt[, .(total_inventory = sum(warehouse_inventory)), by = day]

    ggplot(agg, aes(x = day, y = total_inventory)) +
      geom_area(fill = "steelblue", alpha = 0.3) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(title = "Total Warehouse Inventory Over Time",
           x = "Day", y = "Total Units") +
      theme_minimal()
  })

  output$wh_inv_table <- renderDT({
    req(rv$results)
    dt <- rv$results$wh_inventory
    if (nrow(dt) == 0) return(datatable(data.frame()))
    wide <- dcast(dt, product_name ~ paste0("Day_", day),
                  value.var = "warehouse_inventory", fill = 0)
    datatable(wide, options = list(scrollX = TRUE, pageLength = 20),
              rownames = FALSE) %>%
      formatRound(columns = setdiff(names(wide), "product_name"), digits = 0)
  })

  # ---- Store Inventory Chart ---------------------------------------------
  output$st_inv_chart <- renderPlot({
    req(rv$results)
    dt <- rv$results$store_inventory
    s_id <- as.integer(input$inv_store)
    p_idx <- as.integer(input$inv_product)
    if (is.na(s_id) || is.na(p_idx)) return(NULL)

    plot_dt <- dt[store_id == s_id & product_idx == p_idx]
    if (nrow(plot_dt) == 0) return(NULL)

    p_name <- plot_dt$product_name[1]
    s_name <- rv$stores$store_name[rv$stores$store_id == s_id][1]

    ggplot(plot_dt, aes(x = day)) +
      geom_area(aes(y = store_inventory), fill = "forestgreen", alpha = 0.3) +
      geom_line(aes(y = store_inventory), color = "forestgreen", linewidth = 1) +
      geom_col(aes(y = -stockout), fill = "red", alpha = 0.6) +
      labs(title = paste("Store Inventory:", p_name, "-", s_name),
           x = "Day", y = "Units (negative = stockout)") +
      theme_minimal()
  })

  # ---- Delivery Schedule Table -------------------------------------------
  output$delivery_table <- renderDT({
    req(rv$results)
    dt <- rv$results$delivery_schedule
    if (nrow(dt) == 0) return(datatable(data.frame(Message = "No deliveries")))

    # Aggregate by store and day
    agg <- dt[delivery_qty > 0,
              .(total_units = sum(delivery_qty),
                products = paste(unique(product_name), collapse = ", ")),
              by = .(store_id, day)]
    agg <- merge(agg, rv$stores[, .(store_id, store_name)],
                 by = "store_id", all.x = TRUE)
    agg <- agg[order(day, store_id)]
    datatable(agg[, .(Day = day, Store = store_name,
                      `Total Units` = round(total_units),
                      Products = products)],
              options = list(scrollX = TRUE, pageLength = 30),
              rownames = FALSE)
  })

  # ---- Vehicle Usage Chart -----------------------------------------------
  output$vehicle_chart <- renderPlot({
    req(rv$results)
    tt <- rv$results$truck_trips
    if (nrow(tt) == 0) return(NULL)

    agg <- tt[, .(total_trips = sum(trips),
                  total_weight = sum(weight_kg)),
              by = .(day, vehicle_type)]

    ggplot(agg, aes(x = day, y = total_trips, fill = vehicle_type)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Daily Vehicle Trips",
           x = "Day", y = "Number of Trips", fill = "Vehicle") +
      scale_fill_manual(values = c("Truck" = "steelblue",
                                    "Van" = "coral")) +
      theme_minimal()
  })

  output$vehicle_table <- renderDT({
    req(rv$results)
    tt <- rv$results$truck_trips
    if (nrow(tt) == 0) return(datatable(data.frame(Message = "No trips")))

    agg <- tt[, .(Trips = sum(trips),
                  `Weight (kg)` = round(sum(weight_kg), 1),
                  `Volume (L)` = round(sum(volume_l), 1)),
              by = .(Day = day, Vehicle = vehicle_type,
                     Store = store_id)]
    agg <- merge(agg, rv$stores[, .(store_id, store_name)],
                 by.x = "Store", by.y = "store_id", all.x = TRUE)
    agg[, Store := store_name][, store_name := NULL]
    datatable(agg[order(Day, Store)],
              options = list(scrollX = TRUE, pageLength = 30),
              rownames = FALSE)
  })

  # ---- Cost Analysis Charts ----------------------------------------------
  output$cost_breakdown_chart <- renderPlot({
    req(rv$results)
    costs <- data.frame(
      Category = c("Transport", "Lost Margin"),
      Cost = c(rv$results$transport_cost, rv$results$total_lost_margin)
    )
    ggplot(costs, aes(x = Category, y = Cost, fill = Category)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = paste0("\u20AC", round(Cost, 0))),
                vjust = -0.5, size = 4) +
      labs(title = "Cost Breakdown", y = "Cost (\u20AC)") +
      scale_fill_manual(values = c("Transport" = "steelblue",
                                    "Lost Margin" = "coral")) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$cost_daily_chart <- renderPlot({
    req(rv$results)
    # Daily transport cost from truck trips
    tt <- rv$results$truck_trips
    veh <- vehicles_dt()
    dists <- distances()

    daily_transport <- data.table(day = 1:PLANNING_HORIZON, transport = 0)
    if (nrow(tt) > 0) {
      for (i in seq_len(nrow(tt))) {
        row <- tt[i]
        v_idx <- which(veh$type == row$vehicle_type)[1]
        if (is.na(v_idx)) next
        s_idx <- which(rv$stores$store_id == row$store_id)[1]
        dist <- if (!is.na(s_idx) && s_idx <= length(dists)) dists[s_idx] else 50
        cost <- row$trips * (veh$fixed_cost_per_trip[v_idx] +
                               2 * dist * veh$cost_per_km[v_idx])
        daily_transport[day == row$day, transport := transport + cost]
      }
    }

    # Daily lost margin from stockouts
    so <- rv$results$stockouts
    daily_lost <- data.table(day = 1:PLANNING_HORIZON, lost_margin = 0)
    if (nrow(so) > 0) {
      agg <- so[, .(lost_margin = sum(lost_margin)), by = day]
      daily_lost <- merge(daily_lost, agg, by = "day", all.x = TRUE,
                          suffixes = c("", ".y"))
      daily_lost[!is.na(lost_margin.y),
                 lost_margin := lost_margin + lost_margin.y]
      daily_lost[, lost_margin.y := NULL]
    }

    daily_costs <- merge(daily_transport, daily_lost, by = "day")
    daily_costs[, total := transport + lost_margin]

    plot_dt <- melt(daily_costs, id.vars = "day",
                    measure.vars = c("transport", "lost_margin"),
                    variable.name = "type", value.name = "cost")
    plot_dt[, type := fifelse(type == "transport", "Transport", "Lost Margin")]

    ggplot(plot_dt, aes(x = day, y = cost, fill = type)) +
      geom_area(position = "stack", alpha = 0.7) +
      labs(title = "Daily Cost Breakdown",
           x = "Day", y = "Cost (\u20AC)", fill = "Type") +
      scale_fill_manual(values = c("Transport" = "steelblue",
                                    "Lost Margin" = "coral")) +
      theme_minimal()
  })

  output$service_level_chart <- renderPlot({
    req(rv$results)
    demand <- rv$results$demand
    so <- rv$results$stockouts
    products <- rv$products

    # Calculate daily service level
    daily_sl <- data.table(day = 1:PLANNING_HORIZON)

    for (d in 1:PLANNING_HORIZON) {
      day_demand <- demand[day == d]
      total_margin_day <- sum(day_demand$demand *
                                (products$price[day_demand$product_idx] -
                                   products$inkoop[day_demand$product_idx]))
      lost_day <- if (nrow(so) > 0 && d %in% so$day) {
        sum(so[day == d, lost_margin])
      } else 0

      daily_sl[day == d, sl := ifelse(total_margin_day > 0,
                                       1 - lost_day / total_margin_day, 1)]
    }

    target_sl <- input$service_level / 100

    ggplot(daily_sl, aes(x = day, y = sl * 100)) +
      geom_line(color = "forestgreen", linewidth = 1) +
      geom_point(color = "forestgreen", size = 2) +
      geom_hline(yintercept = target_sl * 100,
                 linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = PLANNING_HORIZON, y = target_sl * 100 - 1,
               label = paste0("Target: ", target_sl * 100, "%"),
               hjust = 1, color = "red") +
      labs(title = "Daily Service Level (OTIN)",
           x = "Day", y = "Service Level (%)") +
      ylim(c(max(0, min(daily_sl$sl, na.rm = TRUE) * 100 - 5), 100)) +
      theme_minimal()
  })
}

# ============================================================================
# Run App
# ============================================================================
shinyApp(ui = ui, server = server)
