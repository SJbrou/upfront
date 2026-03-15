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
library(httr)
library(jsonlite)
library(plotly)

# ---- Constants -------------------------------------------------------------
PLANNING_HORIZON <- 30
DEFAULT_SERVICE_LEVEL <- 0.99
DEFAULT_ERROR_PCT <- 20

DEFAULT_WEEKLY_SEASONALITY <- c(1.1, 1.0, 1.0, 1.05, 1.15, 1.3, 0.8)
DEFAULT_MONTHLY_SEASONALITY <- c(0.90, 0.85, 0.95, 1.00, 1.05, 1.10,
                                  1.10, 1.05, 1.00, 0.95, 0.90, 1.00)
DAY_NAMES  <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

DEFAULT_WH_LAT <- 51.9786
DEFAULT_WH_LNG <- 5.0940

# Default store list: includes Rotterdam, Utrecht, Den Haag, plus Leiden and Amsterdam
DEFAULT_STORES <- data.table(
  store_id   = 1:5,
  store_name = c("Store Rotterdam", "Store Utrecht", "Store Den Haag", "Store Leiden", "Store Amsterdam"),
  lat        = c(51.9249, 52.0907, 52.0705, 52.1680, 52.3612),
  lng        = c(4.4320, 5.1214, 4.3007, 4.4868, 4.8979),
  multiplier = c(1.0, 0.8, 1.2, 1.0, 1.1) # how busy each store is (for demand generation)
)

# ---- Helper Functions ------------------------------------------------------

read_product_csv <- function(file_path) {
  lines <- readLines(file_path, n = 2, warn = FALSE)
  header <- lines[1]

  if (grepl(";", header)) {
    dt <- fread(file_path, sep = ";", dec = ",", header = TRUE,
                encoding = "UTF-8")
  } else {
    dt <- fread(file_path, sep = ",", header = TRUE, encoding = "UTF-8")
    num_cols <- c("price", "inkoop", "weight_(g)", "weight (gr)",
                  "volume_(l)", "volume (l)")
    for (col in intersect(num_cols, names(dt))) {
      if (is.character(dt[[col]]))
        dt[[col]] <- as.numeric(gsub(",", ".", dt[[col]]))
    }
  }

  old_names <- names(dt)
  new_names <- old_names
  new_names <- gsub("weight_\\(g\\)|weight \\(gr\\)", "weight_g", new_names)
  new_names <- gsub("volume_\\(l\\)|volume \\(l\\)", "volume_l", new_names)
  setnames(dt, old_names, new_names)

  if (!"import_order_size" %in% names(dt))     dt[, import_order_size := 50]
  if (!"import_delivery_time" %in% names(dt))  dt[, import_delivery_time := 2]
  if (!"shelf_life" %in% names(dt))            dt[, shelf_life := 7]
  if (!"gekoeld" %in% names(dt))               dt[, gekoeld := 0]
  if (!"bevroren" %in% names(dt))              dt[, bevroren := 0]
  if (!"weight_g" %in% names(dt))              dt[, weight_g := 100]
  if (!"volume_l" %in% names(dt))              dt[, volume_l := 0.1]
  if (!"price" %in% names(dt))                 dt[, price := 1.0]
  if (!"inkoop" %in% names(dt))                dt[, inkoop := 0.5]

  num_fields <- c("price", "inkoop", "weight_g", "volume_l",
                  "shelf_life", "gekoeld", "bevroren",
                  "import_order_size", "import_delivery_time")
  for (col in intersect(num_fields, names(dt))) {
    if (is.character(dt[[col]]))
      dt[[col]] <- as.numeric(gsub(",", ".", dt[[col]]))
  }

  # Remove fully empty rows (for example a trailing delimiter-only CSV row).
  non_empty <- lapply(dt, function(col) {
    !is.na(col) & trimws(as.character(col)) != ""
  })
  keep_rows <- Reduce(`|`, non_empty)
  dt <- dt[keep_rows]

  # Fill NA values in required numeric fields.
  dt[is.na(import_order_size) | import_order_size <= 0, import_order_size := 50]
  dt[is.na(import_delivery_time) | import_delivery_time < 0, import_delivery_time := 2]
  dt[, import_order_size := pmax(1, round(import_order_size))]
  dt[, import_delivery_time := pmax(0, as.integer(round(import_delivery_time)))]
  dt[is.na(shelf_life) | shelf_life <= 0, shelf_life := 7]
  dt[is.na(weight_g) | weight_g <= 0, weight_g := 100]
  dt[is.na(volume_l) | volume_l <= 0, volume_l := 0.1]
  dt[is.na(price) | price < 0, price := 1.0]
  dt[is.na(inkoop) | inkoop < 0, inkoop := 0.5]

  # If weight values are very small (e.g. 0.2), assume input was in kg, not grams.
  # Convert to grams so capacity calculations stay realistic.
  if (nrow(dt) > 0 && median(dt$weight_g, na.rm = TRUE) <= 5) {
    dt[, weight_g := weight_g * 1000]
  }

  if (!"name" %in% names(dt)) dt[, name := paste("Product", .I)]
  dt[is.na(name) | trimws(as.character(name)) == "", name := paste("Product", .I)]

  dt[, product_idx := .I]
  if (!"daily_demand" %in% names(dt)) dt[, weekly_demand := 609] # default: 87 per day -> 609 per week
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

get_osrm_route <- function(lat1, lng1, lat2, lng2) {
  tryCatch({
    url <- sprintf(
      "https://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson&steps=false",
      lng1, lat1, lng2, lat2)
    resp <- GET(url, timeout(5))
    if (status_code(resp) == 200) {
      body <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      if (body$code == "Ok" && length(body$routes) > 0) {
        coords <- body$routes$geometry$coordinates[[1]]
        route_lng <- coords[, 1]
        route_lat <- coords[, 2]
        return(list(
          distance_km  = body$routes$distance[1] / 1000,
          duration_min = body$routes$duration[1] / 60,
          route_lng = route_lng,
          route_lat = route_lat
        ))
      }
    }
    NULL
  }, error = function(e) NULL)
}

# ---- Demand Generation -----------------------------------------------------

generate_demand <- function(products, stores, horizon = PLANNING_HORIZON,
                            error_pct = DEFAULT_ERROR_PCT,
                            start_date = Sys.Date(),
                            demand_overrides = NULL,
                            weekly_seasonality = NULL,
                            monthly_seasonality = NULL) {
  dates    <- start_date + 0:(horizon - 1)
  wday_idx <- as.integer(format(dates, "%u"))
  month_idx <- as.integer(format(dates, "%m"))

  demand_list <- list()
  for (s in seq_len(nrow(stores))) {
    for (p in seq_len(nrow(products))) {
      # Per-product seasonality (columns 2:8 / 2:13 are the factors)
      w_seas <- if (!is.null(weekly_seasonality) && p <= nrow(weekly_seasonality)) {
        as.numeric(weekly_seasonality[p, 2:8])
      } else DEFAULT_WEEKLY_SEASONALITY
      m_seas <- if (!is.null(monthly_seasonality) && p <= nrow(monthly_seasonality)) {
        as.numeric(monthly_seasonality[p, 2:13])
      } else DEFAULT_MONTHLY_SEASONALITY

      # Base weekly demand (per-store override or default * multiplier)
      weekly_dem <- NULL
      if (!is.null(demand_overrides) && nrow(demand_overrides) >= p &&
          stores$store_name[s] %in% names(demand_overrides))
        weekly_dem <- demand_overrides[[stores$store_name[s]]][p]
      if (is.null(weekly_dem) || is.na(weekly_dem))
        weekly_dem <- products$weekly_demand[p] * stores$multiplier[s]
      daily_base <- weekly_dem / 7

      daily_demand <- numeric(horizon)
      for (d in seq_len(horizon)) {
        seasonal <- w_seas[wday_idx[d]] * m_seas[month_idx[d]]
        err <- runif(1, 1 - error_pct / 100, 1 + error_pct / 100)
        daily_demand[d] <- max(0, round(daily_base * seasonal * err, 1))
      }
      demand_list[[length(demand_list) + 1]] <- data.table(
        store_id = stores$store_id[s], product_idx = p,
        day = 1:horizon, date = dates, demand = daily_demand)
    }
  }
  rbindlist(demand_list)
}

# ---- Optimization (LP per product + joint simulation) ----------------------

run_optimization <- function(products, stores, demand_dt, vehicles,
                             distances, wh_capacity, store_capacity,
                             service_level,
                             planning_horizon = PLANNING_HORIZON,
                             enforce_service_constraint = TRUE,
                             transport_cost_weight = 1,
                             stockout_penalty_mult = 1,
                             target_cover_days = 5) {
  D <- planning_horizon
  S <- nrow(stores)
  P <- nrow(products)

  # Build demand array [P, S, D]
  dem_array <- array(0, dim = c(P, S, D))
  for (p in 1:P) for (s in 1:S) {
    dd <- demand_dt[product_idx == p & store_id == stores$store_id[s]]
    if (nrow(dd) > 0) dem_array[p, s, ] <- dd$demand[order(dd$day)]
  }

  # Normalize product parameters to avoid NA-driven optimization crashes.
  products <- copy(products)
  products[is.na(weight_g) | weight_g <= 0, weight_g := 100]
  products[is.na(volume_l) | volume_l <= 0, volume_l := 0.1]
  products[is.na(import_order_size) | import_order_size <= 0, import_order_size := 1]
  products[is.na(import_delivery_time) | import_delivery_time < 0, import_delivery_time := 2]
  products[, import_order_size := pmax(1, round(import_order_size))]
  products[, import_delivery_time := pmax(0, as.integer(round(import_delivery_time)))]
  products[is.na(price), price := 0]
  products[is.na(inkoop), inkoop := 0]

  weights_kg <- products$weight_g / 1000
  volumes_l  <- products$volume_l

  # Demand-weighted capacity share per product (by weight)
  dw_share <- sapply(1:P, function(p) sum(dem_array[p,,]) * weights_kg[p])
  total_dw <- sum(dw_share)
  dw_share <- if (total_dw > 0) dw_share / total_dw else rep(1 / P, P)

  # Transport cost approximation
  # For the LP cost approximation, pick the vehicle with the lowest

  # cost-per-kg for each store (considering fixed + per-km costs).
  cost_per_kg_to_store <- sapply(1:S, function(s) {
    trip_costs <- vehicles$fixed_cost_per_trip + 2 * distances[s] * vehicles$cost_per_km
    cost_per_kg <- trip_costs / pmax(vehicles$max_weight, 1e-6)
    min(cost_per_kg)
  })

  # --- Phase 1: Per-product LP ---
  lp_import   <- matrix(0, nrow = P, ncol = D)
  lp_delivery <- array(0, dim = c(P, S, D))

  for (p in 1:P) {
    product   <- products[p, ]
    margin    <- product$price - product$inkoop
    weight    <- weights_kg[p]
    lead_time <- product$import_delivery_time

    dem_matrix <- matrix(dem_array[p,,], nrow = S, ncol = D)

    n_vars <- D + 2 * S * D
    idx_imp <- function(d) d
    idx_del <- function(s, d) D + (s - 1) * D + d
    idx_so  <- function(s, d) D + S * D + (s - 1) * D + d

    # Objective: transport cost + stockout penalty
    obj <- rep(0, n_vars)
    for (s in 1:S) for (d in 1:D) {
      obj[idx_del(s, d)] <- transport_cost_weight * cost_per_kg_to_store[s] * weight
      obj[idx_so(s, d)]  <- stockout_penalty_mult * max(margin, 0.01)
    }

    con_mat <- list(); con_dir <- character(0); con_rhs <- numeric(0)

    # Capacity shares for this product
    wh_cap_units <- wh_capacity$max_weight * dw_share[p] / max(weight, 1e-6)
    st_cap_units <- store_capacity$max_weight * dw_share[p] / max(weight, 1e-6)

    total_daily_dem <- sum(dem_matrix) / D
    wh_inv_0 <- min(round(total_daily_dem * 3), wh_cap_units * 0.5)
    store_inv_0 <- numeric(S)
    for (s in 1:S) {
      avg_s <- sum(dem_matrix[s, ]) / D
      store_inv_0[s] <- min(round(avg_s * 2), st_cap_units * 0.5)
    }

    # C1: Warehouse inventory >= 0 each day
    for (d in 1:D) {
      row <- rep(0, n_vars)
      for (dd in 1:d) { ld <- dd - lead_time; if (ld >= 1) row[idx_imp(ld)] <- 1 }
      for (s in 1:S) for (dd in 1:d) row[idx_del(s, dd)] <- row[idx_del(s, dd)] - 1
      con_mat[[length(con_mat) + 1]] <- row
      con_dir <- c(con_dir, ">="); con_rhs <- c(con_rhs, -wh_inv_0)
    }
    # C2: Warehouse capacity each day
    for (d in 1:D) {
      row <- rep(0, n_vars)
      for (dd in 1:d) { ld <- dd - lead_time; if (ld >= 1) row[idx_imp(ld)] <- 1 }
      for (s in 1:S) for (dd in 1:d) row[idx_del(s, dd)] <- row[idx_del(s, dd)] - 1
      con_mat[[length(con_mat) + 1]] <- row
      con_dir <- c(con_dir, "<="); con_rhs <- c(con_rhs, wh_cap_units - wh_inv_0)
    }
    # C3: Store inventory >= 0
    for (s in 1:S) {
      cum_dem <- cumsum(dem_matrix[s, ])
      for (d in 1:D) {
        row <- rep(0, n_vars)
        for (dd in 1:d) { row[idx_del(s, dd)] <- 1; row[idx_so(s, dd)] <- 1 }
        con_mat[[length(con_mat) + 1]] <- row
        con_dir <- c(con_dir, ">="); con_rhs <- c(con_rhs, cum_dem[d] - store_inv_0[s])
      }
    }
    # C4: Store capacity
    for (s in 1:S) {
      cum_dem <- cumsum(dem_matrix[s, ])
      for (d in 1:D) {
        row <- rep(0, n_vars)
        for (dd in 1:d) { row[idx_del(s, dd)] <- 1; row[idx_so(s, dd)] <- 1 }
        con_mat[[length(con_mat) + 1]] <- row
        con_dir <- c(con_dir, "<=")
        con_rhs <- c(con_rhs, st_cap_units - store_inv_0[s] + cum_dem[d])
      }
    }
    # C5: Service level
    if (isTRUE(enforce_service_constraint)) {
      total_margin <- max(margin, 0.01) * sum(dem_matrix)
      row <- rep(0, n_vars)
      for (s in 1:S) for (d in 1:D) row[idx_so(s, d)] <- max(margin, 0.01)
      con_mat[[length(con_mat) + 1]] <- row
      con_dir <- c(con_dir, "<=")
      con_rhs <- c(con_rhs, (1 - service_level) * total_margin)
    }

    A <- do.call(rbind, con_mat)
    sol <- tryCatch(lp("min", obj, A, con_dir, con_rhs),
                    error = function(e) list(status = 1, solution = rep(0, n_vars)))

    if (sol$status != 0) {
      sol_vec <- rep(0, n_vars)
      for (s in 1:S) for (d in 1:D) sol_vec[idx_del(s, d)] <- dem_matrix[s, d]
      for (d in 1:D) sol_vec[idx_imp(d)] <- sum(sapply(1:S, function(s) sol_vec[idx_del(s, d)]))
    } else sol_vec <- sol$solution

    for (d in 1:D) {
      qty <- max(0, round(sol_vec[idx_imp(d)], 1))
      ord <- product$import_order_size
      if (!is.na(ord) && ord > 0 && qty > 0) qty <- ceiling(qty / ord) * ord
      lp_import[p, d] <- qty
    }
    for (s in 1:S) for (d in 1:D)
      lp_delivery[p, s, d] <- max(0, round(sol_vec[idx_del(s, d)], 1))
  }

  # --- Phase 2: Replenishment-based delivery simulation ---
  # Import schedule from LP Phase 1 is used as-is.
  # Delivery scheduling uses a replenishment policy instead of the LP plan:
  #   - Trigger a delivery to a store when ANY product drops below
  #     reorder_trigger_days of stock.
  #   - When triggered, top up ALL products to target_cover_days of stock.
  # This naturally consolidates all products into fewer, larger trips,
  # minimising transport cost while maintaining service.

  avg_demand_ps <- matrix(0, nrow = P, ncol = S)
  for (p in 1:P) for (s in 1:S)
    avg_demand_ps[p, s] <- max(sum(dem_array[p, s, ]) / D, 0)

  wh_inv <- numeric(P)
  st_inv <- matrix(0, nrow = S, ncol = P)

  for (p in 1:P) {
    w <- weights_kg[p]
    wh_cap_share <- wh_capacity$max_weight * dw_share[p]
    st_cap_share <- store_capacity$max_weight * dw_share[p]
    total_daily_dem <- sum(dem_array[p,,]) / D
    wh_inv[p] <- min(round(total_daily_dem * 3),
                     wh_cap_share / max(w, 1e-6) * 0.5)
    for (s in 1:S) {
      avg_s <- avg_demand_ps[p, s]
      st_inv[s, p] <- min(round(avg_s * 2),
                          st_cap_share / max(w, 1e-6) * 0.5)
    }
  }

  # Enforce total warehouse capacity
  total_wh_w <- sum(wh_inv * weights_kg)
  if (total_wh_w > wh_capacity$max_weight) {
    scale <- wh_capacity$max_weight / total_wh_w
    wh_inv <- floor(wh_inv * scale)
  }
  total_wh_v <- sum(wh_inv * volumes_l)
  if (total_wh_v > wh_capacity$max_volume) {
    scale <- wh_capacity$max_volume / total_wh_v
    wh_inv <- floor(wh_inv * scale)
  }
  for (s in 1:S) {
    total_st_w <- sum(st_inv[s, ] * weights_kg)
    if (total_st_w > store_capacity$max_weight) {
      scale <- store_capacity$max_weight / total_st_w
      st_inv[s, ] <- floor(st_inv[s, ] * scale)
    }
    total_st_v <- sum(st_inv[s, ] * volumes_l)
    if (total_st_v > store_capacity$max_volume) {
      scale <- store_capacity$max_volume / total_st_v
      st_inv[s, ] <- floor(st_inv[s, ] * scale)
    }
  }

  # Build pending imports from LP schedule
  max_lt <- max(products$import_delivery_time)
  import_pending <- matrix(0, nrow = P, ncol = D + max_lt + 1)
  for (p in 1:P) {
    lt <- products$import_delivery_time[p]
    for (d in 1:D) {
      arr <- d + lt
      if (arr <= ncol(import_pending))
        import_pending[p, arr] <- import_pending[p, arr] + lp_import[p, d]
    }
  }

  # Per-vehicle available trips to each store (for Phase 2 tracking)
  # Also compute aggregate max capacity per store for quick checks.
  veh_trips_per_store <- list()   # list of S vectors, each of length nrow(vehicles)
  max_del_weight <- numeric(S)
  max_del_volume <- numeric(S)
  for (s in 1:S) {
    vt <- integer(nrow(vehicles))
    for (v in 1:nrow(vehicles)) {
      rt <- 2 * distances[s]
      vt[v] <- as.integer(floor(vehicles$max_distance_per_day[v] / max(rt, 0.1)) *
                          vehicles$count[v])
      max_del_weight[s] <- max_del_weight[s] + vt[v] * vehicles$max_weight[v]
      max_del_volume[s] <- max_del_volume[s] + vt[v] * vehicles$max_volume[v]
    }
    veh_trips_per_store[[s]] <- vt
  }

  # Reorder trigger: deliver when any product has fewer than this many days
  # of stock. Lower value = fewer trips but potentially lower service.
  reorder_trigger <- 1.0

  res_import   <- list()
  res_delivery <- list()
  res_wh_inv   <- list()
  res_st_inv   <- list()
  res_stockout <- list()

  for (d in 1:D) {
    # Step 1: Receive imports at warehouse
    for (p in 1:P) {
      if (d <= ncol(import_pending) && import_pending[p, d] > 0) {
        current_wh_kg <- sum(wh_inv * weights_kg)
        current_wh_vol <- sum(wh_inv * volumes_l)
        space_kg <- wh_capacity$max_weight - current_wh_kg
        space_vol <- wh_capacity$max_volume - current_wh_vol
        max_units_w <- floor(space_kg / max(weights_kg[p], 1e-6))
        max_units_v <- floor(space_vol / max(volumes_l[p], 1e-6))
        actual <- min(import_pending[p, d], max(0, min(max_units_w, max_units_v)))
        wh_inv[p] <- wh_inv[p] + actual
      }
    }

    # Step 2: Replenishment-based deliveries
    for (s in 1:S) {
      days_stock <- sapply(1:P, function(pp) {
        if (avg_demand_ps[pp, s] <= 0) return(Inf)
        st_inv[s, pp] / avg_demand_ps[pp, s]
      })

      # Only deliver when at least one product is running low
      if (min(days_stock) > reorder_trigger) next

      # Execute delivery: top up ALL products to target_cover_days
      rem_veh_cap_w <- max_del_weight[s]
      rem_veh_cap_v <- max_del_volume[s]
      cur_st_kg   <- sum(st_inv[s, ] * weights_kg)
      cur_st_vol  <- sum(st_inv[s, ] * volumes_l)
      st_space_kg <- store_capacity$max_weight - cur_st_kg
      st_space_vol <- store_capacity$max_volume - cur_st_vol

      prio <- order(days_stock)
      for (pp in prio) {
        target_inv <- ceiling(avg_demand_ps[pp, s] * target_cover_days)
        desired <- max(0, target_inv - st_inv[s, pp])
        if (desired <= 0) next

        desired <- min(desired, wh_inv[pp])
        max_by_st_w <- floor(max(0, st_space_kg) / max(weights_kg[pp], 1e-6))
        max_by_st_v <- floor(max(0, st_space_vol) / max(volumes_l[pp], 1e-6))
        desired <- min(desired, max_by_st_w, max_by_st_v)
        max_by_veh_w <- floor(max(0, rem_veh_cap_w) / max(weights_kg[pp], 1e-6))
        max_by_veh_v <- floor(max(0, rem_veh_cap_v) / max(volumes_l[pp], 1e-6))
        desired <- min(desired, max_by_veh_w, max_by_veh_v)
        actual <- max(0L, as.integer(floor(desired)))

        if (actual > 0) {
          wh_inv[pp]    <- wh_inv[pp] - actual
          st_inv[s, pp] <- st_inv[s, pp] + actual
          rem_veh_cap_w <- rem_veh_cap_w - actual * weights_kg[pp]
          rem_veh_cap_v <- rem_veh_cap_v - actual * volumes_l[pp]
          st_space_kg   <- st_space_kg - actual * weights_kg[pp]
          st_space_vol  <- st_space_vol - actual * volumes_l[pp]
          res_delivery[[length(res_delivery) + 1]] <- data.table(
            store_id = stores$store_id[s], product_idx = pp,
            product_name = products$name[pp], day = d,
            delivery_qty = actual)
        }
      }
    }

    # Step 4: Fulfill demand at stores
    for (s in 1:S) for (p in 1:P) {
      demand_today <- dem_array[p, s, d]
      fulfilled <- min(st_inv[s, p], demand_today)
      stockout  <- demand_today - fulfilled
      st_inv[s, p] <- st_inv[s, p] - fulfilled

      res_st_inv[[length(res_st_inv) + 1]] <- data.table(
        store_id = stores$store_id[s], product_idx = p,
        product_name = products$name[p], day = d,
        store_inventory = st_inv[s, p], stockout = stockout)

      if (stockout > 0) {
        m <- products$price[p] - products$inkoop[p]
        res_stockout[[length(res_stockout) + 1]] <- data.table(
          store_id = stores$store_id[s], product_idx = p,
          product_name = products$name[p], day = d,
          stockout_qty = stockout, lost_margin = stockout * m)
      }
    }

    # Record warehouse inventory
    for (p in 1:P)
      res_wh_inv[[length(res_wh_inv) + 1]] <- data.table(
        product_idx = p, product_name = products$name[p],
        day = d, warehouse_inventory = wh_inv[p])
  }

  # Record imports (from LP plan)
  for (p in 1:P) for (d in 1:D) if (lp_import[p, d] > 0)
    res_import[[length(res_import) + 1]] <- data.table(
      product_idx = p, product_name = products$name[p],
      day = d, import_qty = lp_import[p, d])

  # Assemble data.tables
  empty_imp <- data.table(product_idx = integer(), product_name = character(),
                          day = integer(), import_qty = numeric())
  empty_del <- data.table(store_id = integer(), product_idx = integer(),
                          product_name = character(), day = integer(),
                          delivery_qty = numeric())
  empty_wh  <- data.table(product_idx = integer(), product_name = character(),
                          day = integer(), warehouse_inventory = numeric())
  empty_st  <- data.table(store_id = integer(), product_idx = integer(),
                          product_name = character(), day = integer(),
                          store_inventory = numeric(), stockout = numeric())
  empty_so  <- data.table(store_id = integer(), product_idx = integer(),
                          product_name = character(), day = integer(),
                          stockout_qty = numeric(), lost_margin = numeric())

  import_schedule    <- if (length(res_import) > 0)   rbindlist(res_import)   else empty_imp
  delivery_schedule  <- if (length(res_delivery) > 0) rbindlist(res_delivery) else empty_del
  wh_inventory       <- if (length(res_wh_inv) > 0)   rbindlist(res_wh_inv)   else empty_wh
  st_inventory       <- if (length(res_st_inv) > 0)   rbindlist(res_st_inv)   else empty_st
  stockouts          <- if (length(res_stockout) > 0)  rbindlist(res_stockout) else empty_so

  # --- Financial metrics ---
  import_cost <- if (nrow(import_schedule) > 0) {
    imp_m <- merge(import_schedule, products[, .(product_idx, inkoop)],
                   by = "product_idx")
    sum(imp_m$import_qty * imp_m$inkoop)
  } else 0

  demand_m <- merge(demand_dt, products[, .(product_idx, price)],
                    by = "product_idx")
  total_possible_revenue <- sum(demand_m$demand * demand_m$price)
  lost_revenue <- if (nrow(stockouts) > 0) {
    so_m <- merge(stockouts, products[, .(product_idx, price)],
                  by = "product_idx")
    sum(so_m$stockout_qty * so_m$price)
  } else 0
  total_revenue <- total_possible_revenue - lost_revenue

  # Truck trips & transport cost
  truck_trips    <- calculate_truck_trips(delivery_schedule, products,
                                          stores, vehicles, distances)
  transport_cost <- calculate_transport_cost(truck_trips, vehicles,
                                             distances, stores)

  total_lost_margin <- if (nrow(stockouts) > 0) sum(stockouts$lost_margin) else 0
  total_demand_margin <- sum(demand_dt$demand *
                               (products$price[demand_dt$product_idx] -
                                  products$inkoop[demand_dt$product_idx]))
  achieved_sl <- if (total_demand_margin > 0) {
    1 - total_lost_margin / total_demand_margin
  } else 1.0

  netto_profit <- total_revenue - import_cost - transport_cost

  list(
    import_schedule   = import_schedule,
    delivery_schedule = delivery_schedule,
    wh_inventory      = wh_inventory,
    store_inventory   = st_inventory,
    stockouts         = stockouts,
    truck_trips       = truck_trips,
    transport_cost    = transport_cost,
    import_cost       = import_cost,
    total_revenue     = total_revenue,
    netto_profit      = netto_profit,
    total_lost_margin = total_lost_margin,
    total_cost        = transport_cost + import_cost,
    service_level     = achieved_sl,
    demand            = demand_dt
  )
}

run_optimization_with_service_guard <- function(products, stores, demand_dt, vehicles,
                                                distances, wh_capacity, store_capacity,
                                                requested_service_level,
                                                planning_horizon = PLANNING_HORIZON) {
  primary <- run_optimization(
    products = products,
    stores = stores,
    demand_dt = demand_dt,
    vehicles = vehicles,
    distances = distances,
    wh_capacity = wh_capacity,
    store_capacity = store_capacity,
    service_level = requested_service_level,
    planning_horizon = planning_horizon,
    enforce_service_constraint = TRUE,
    transport_cost_weight = 1,
    stockout_penalty_mult = 1
  )

  if (primary$service_level + 1e-6 >= requested_service_level) {
    primary$service_warning <- NULL
    return(primary)
  }

  # If target OTIN is infeasible, maximize service level first and return best feasible plan.
  best_service <- run_optimization(
    products = products,
    stores = stores,
    demand_dt = demand_dt,
    vehicles = vehicles,
    distances = distances,
    wh_capacity = wh_capacity,
    store_capacity = store_capacity,
    service_level = 0,
    planning_horizon = planning_horizon,
    enforce_service_constraint = FALSE,
    transport_cost_weight = 0.05,
    stockout_penalty_mult = 1000,
    target_cover_days = 5
  )

  best_service$service_warning <- paste0(
    "Requested service level of ", round(requested_service_level * 100, 2),
    "% is infeasible under current constraints. Returning best achievable service level of ",
    round(best_service$service_level * 100, 2), "% instead."
  )
  best_service
}

# ---- Transport Calculations ------------------------------------------------

calculate_truck_trips <- function(delivery_schedule, products, stores,
                                  vehicles, distances) {
  if (nrow(delivery_schedule) == 0 || sum(delivery_schedule$delivery_qty) == 0)
    return(data.table(day = integer(), store_id = integer(),
                      vehicle_type = character(), trips = integer(),
                      weight_kg = numeric(), volume_l = numeric()))

  del <- copy(delivery_schedule)
  del <- merge(del, products[, .(product_idx, weight_g, volume_l)],
               by = "product_idx", all.x = TRUE)
  del[, weight_kg := delivery_qty * weight_g / 1000]
  del[, total_vol := delivery_qty * volume_l]

  daily_store <- del[, .(total_weight = sum(weight_kg, na.rm = TRUE),
                         total_volume = sum(total_vol, na.rm = TRUE)),
                     by = .(day, store_id)]
  daily_store <- daily_store[total_weight > 0 | total_volume > 0]
  if (nrow(daily_store) == 0)
    return(data.table(day = integer(), store_id = integer(),
                      vehicle_type = character(), trips = integer(),
                      weight_kg = numeric(), volume_l = numeric()))

  trips_list <- list()
  for (i in seq_len(nrow(daily_store))) {
    row <- daily_store[i]
    s_idx <- which(stores$store_id == row$store_id)
    if (length(s_idx) == 0) next
    dist <- distances[s_idx]

    remaining_weight <- row$total_weight
    remaining_volume <- row$total_volume

    # Pre-compute available trips and cost-per-kg for each vehicle to this store
    v_avail <- data.table(
      v_idx = seq_len(nrow(vehicles)),
      type  = vehicles$type,
      max_w = vehicles$max_weight,
      max_v = vehicles$max_volume,
      trip_cost = vehicles$fixed_cost_per_trip + 2 * dist * vehicles$cost_per_km,
      avail_trips = as.integer(floor(vehicles$max_distance_per_day / (2 * max(dist, 1))) *
                               vehicles$count)
    )
    v_avail <- v_avail[avail_trips > 0]
    # Cost efficiency: cost per kg of capacity (lower = better)
    v_avail[, cost_per_kg := trip_cost / pmax(max_w, 1e-6)]

    while ((remaining_weight > 0 || remaining_volume > 0) && nrow(v_avail) > 0) {
      # Choose cheapest cost-per-kg vehicle that can still make a trip
      # Among equally cheap, prefer larger capacity
      best_idx <- v_avail[, .I[which.min(cost_per_kg)]]
      veh_row <- v_avail[best_idx]

      load_w <- min(max(0, remaining_weight), veh_row$max_w)
      load_v <- min(max(0, remaining_volume), veh_row$max_v)

      trips_list[[length(trips_list) + 1]] <- data.table(
        day = row$day, store_id = row$store_id,
        vehicle_type = veh_row$type, trips = 1L,
        weight_kg = load_w,
        volume_l  = load_v)
      remaining_weight <- remaining_weight - veh_row$max_w
      remaining_volume <- remaining_volume - veh_row$max_v

      # Decrement available trips for this vehicle
      v_avail[best_idx, avail_trips := avail_trips - 1L]
      v_avail <- v_avail[avail_trips > 0]
    }
  }
  if (length(trips_list) > 0) rbindlist(trips_list) else
    data.table(day = integer(), store_id = integer(),
               vehicle_type = character(), trips = integer(),
               weight_kg = numeric(), volume_l = numeric())
}

calculate_transport_cost <- function(truck_trips, vehicles, distances, stores) {
  if (nrow(truck_trips) == 0) return(0)
  total <- 0
  for (i in seq_len(nrow(truck_trips))) {
    tt <- truck_trips[i]
    v_idx <- which(vehicles$type == tt$vehicle_type)[1]
    if (is.na(v_idx)) next
    veh <- vehicles[v_idx, ]
    s_idx <- which(stores$store_id == tt$store_id)[1]
    if (is.na(s_idx) || s_idx > length(distances)) next
    dist <- distances[s_idx]
    total <- total + tt$trips * (veh$fixed_cost_per_trip +
                                   2 * dist * veh$cost_per_km)
  }
  total
}

# ---- UI Helpers ------------------------------------------------------------

valueBoxUI <- function(id, width = 3) {
  column(width,
    div(class = "value-box",
      h5(textOutput(paste0(id, "_title")), style = "margin:0; color:#6c757d;"),
      h3(textOutput(paste0(id, "_value")), style = "margin:5px 0; color:#212529;")
    ))
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .value-box {
      background: #f8f9fa; border: 1px solid #dee2e6;
      border-radius: 8px; padding: 15px; text-align: center;
      margin-bottom: 10px;
    }
    .editable-hint {
      background: #fffde7; border: 1px solid #fff9c4;
      border-radius: 4px; padding: 8px 12px; margin-bottom: 10px;
      font-size: 13px; color: #f57f17;
    }
    .editable-hint::before { content: '\u270F\uFE0F '; }
    .description-box {
      background: #e3f2fd; border-left: 4px solid #1976d2;
      padding: 12px 16px; margin-bottom: 16px; border-radius: 0 4px 4px 0;
      color: #0d47a1; font-size: 14px;
    }
    .tab-pane { padding-top: 10px; }
  "))),

  titlePanel("Supply Chain Optimization Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Input"),
      fileInput("csv_upload", "Upload Product CSV",
                accept = c(".csv", ".txt")),
      hr(),

      h4("Stores"),
      numericInput("n_stores", "Number of Stores", value = 5, min = 1, max = 10),
      actionButton("reset_stores", "Reset Store Locations"),
      numericInput("planning_horizon", "Planning Horizon (days)",
           value = PLANNING_HORIZON, min = 1, max = 365),
      hr(),

      h4("Service Level"),
      sliderInput("service_level", "OTIN Service Level (%)",
                  min = 80, max = 100, value = 99, step = 0.5),
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

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # ---- Tab 1: Map ---------------------------------------------------
        tabPanel("Map",
          h3("Logistics Locations"),
          div(class = "description-box",
            "Configure the warehouse and store locations on the map. ",
            tags$b("Drag markers"), " to reposition the warehouse (blue) ",
            "and stores (green). Road distances and travel times are ",
            "calculated automatically using OSRM routing. The table below ",
            "shows store settings. You can add stores from this tab, and ",
            "edit store multipliers in the Demand tab."
          ),
          fluidRow(
            column(4, actionButton("add_store_btn", "Add Store At Center", class = "btn-success")),
            column(4, actionButton("remove_last_store_btn", "Remove Last Store", class = "btn-danger"))
          ),
          br(),
          leafletOutput("map", height = "600px"),
          hr(),
          h4("Store Settings"),
          div(class = "editable-hint",
              "Drag markers to move stores. Multipliers are configured in the Demand tab."),
          DTOutput("store_table")
        ),

        # ---- Tab 2: Products ----------------------------------------------
        tabPanel("Products",
          h3("Product Catalog"),
          div(class = "description-box",
            "Manage the product catalog. Click any cell to edit product ",
            "properties directly. Key fields: ", tags$b("price"), " (retail ",
            "price per unit), ", tags$b("inkoop"), " (purchase/import price), ",
            tags$b("weight_g"), " (weight in grams per unit), ",
            tags$b("import_order_size"), " (minimum import batch size), ",
            tags$b("import_delivery_time"), " (days from order to warehouse ",
            "arrival), ", tags$b("weekly_demand"), " (base weekly demand)."
          ),
          fluidRow(
            column(6, actionButton("add_product", "Add Product")),
            column(6, actionButton("remove_product", "Remove Selected"))
          ),
          br(),
          div(class = "editable-hint",
              "Click any cell (except product_idx) to edit values directly."),
          DTOutput("product_table")
        ),

        # ---- Tab 3: Demand ------------------------------------------------
        tabPanel("Demand",
          h3("Demand Forecast"),
          div(class = "description-box",
            "View projected daily demand and configure demand parameters. ",
            "The chart shows the demand forecast for a selected product/store ",
            "combination. Edit the ", tags$b("Per-Store Product Demand"),
            " table to set custom weekly demand per product per store. ",
            "Edit the ", tags$b("Seasonality Patterns"), " tables to customise ",
            "weekly and monthly demand variation per product."
          ),
          fluidRow(
            column(4, selectInput("demand_product", "Product", choices = NULL)),
            column(4, selectInput("demand_store", "Store", choices = NULL)),
            column(4, numericInput("error_pct", "Demand Error (%)", value = 20,
                                   min = 0, max = 50))
          ),
          plotlyOutput("demand_chart", height = "350px"),
          hr(),
          h4("Per-Store Product Demand (weekly units)"),
          div(class = "editable-hint",
              "Click any store column cell to override weekly demand for that product/store."),
          DTOutput("demand_override_table"),
            hr(),
            h4("Store Multipliers"),
            div(class = "editable-hint",
              "Edit each store multiplier here. Demand = product weekly demand × store multiplier."),
            DTOutput("demand_multiplier_table"),
          hr(),
          h4("Seasonality Patterns (per product)"),
          p("Adjust the multipliers to model how demand varies by day-of-week ",
            "and month. Values > 1 mean higher demand, < 1 mean lower demand."),
          fluidRow(
            column(6,
              h5("Weekly Seasonality"),
              div(class = "editable-hint",
                  "Click day columns to edit the weekly demand multiplier per product."),
              DTOutput("weekly_seas_table", height = "auto")
            ),
            column(6,
              h5("Monthly Seasonality"),
              div(class = "editable-hint",
                  "Click month columns to edit the monthly demand multiplier per product."),
              DTOutput("monthly_seas_table", height = "auto")
            )
          ),
          hr(),
          h4("Seasonality Preview"),
          fluidRow(
            column(6, plotlyOutput("weekly_seasonality_chart", height = "250px")),
            column(6, plotlyOutput("monthly_seasonality_chart", height = "250px"))
          )
        ),

        # ---- Tab 4: Vehicle Fleet -----------------------------------------
        tabPanel("Vehicle Fleet",
          h3("Vehicle Fleet Configuration"),
          div(class = "description-box",
            "Configure the transport fleet used for warehouse-to-store ",
            "deliveries. Edit vehicle properties in the table below. ",
            tags$b("speed"), " (km/h) determines how far a vehicle can travel ",
            "per day (speed \u00D7 12 hours = max km/day). ",
            tags$b("count"), " is the number of vehicles of each type. ",
            tags$b("max_weight"), " (kg) and ", tags$b("max_volume"), " (L) ",
            "set the per-trip load capacity."
          ),
          div(class = "editable-hint",
              "Click any cell (except row numbers) to edit vehicle parameters."),
          fluidRow(
            column(4, actionButton("add_vehicle", "Add Vehicle", class = "btn-success")),
            column(4, actionButton("remove_vehicle", "Remove Selected", class = "btn-danger"))
          ),
          br(),
          DTOutput("vehicle_fleet_table")
        ),

        # ---- Tab 5: Optimization Results ----------------------------------
        tabPanel("Optimization Results",
          h3("Optimization Results"),
          div(class = "description-box",
            "Results of the supply chain optimization over the selected planning horizon. ",
            "The LP solver computes an initial plan per product, ",
            "and a joint simulation then enforces all physical constraints: ",
            "total warehouse weight, total store weight, vehicle trip limits, ",
            "and import lead times. The metrics below reflect the ",
            tags$b("simulated (realistic)"), " outcome."
          ),
          conditionalPanel(
            condition = "output.has_results",
            fluidRow(
              valueBoxUI("vb_revenue", 3),
              valueBoxUI("vb_import_cost", 3),
              valueBoxUI("vb_transport", 3),
              valueBoxUI("vb_profit", 3)
            ),
            fluidRow(
              valueBoxUI("vb_lost_margin", 4),
              valueBoxUI("vb_cost", 4),
              valueBoxUI("vb_service", 4)
            ),
            hr(),
            tabsetPanel(
              tabPanel("Import Schedule",   DTOutput("import_table")),
              tabPanel("Warehouse Inventory",
                       plotlyOutput("wh_inv_chart", height = "400px"),
                       DTOutput("wh_inv_table")),
              tabPanel("Store Inventory",
                fluidRow(
                  column(4, selectInput("inv_store", "Store", choices = NULL)),
                  column(4, selectInput("inv_product", "Product", choices = NULL))
                ),
                plotlyOutput("st_inv_chart", height = "400px")),
              tabPanel("Delivery Schedule", DTOutput("delivery_table")),
              tabPanel("Vehicle Usage",
                       plotlyOutput("vehicle_chart", height = "400px"),
                       DTOutput("vehicle_table"))
            )
          ),
          conditionalPanel(
            condition = "!output.has_results",
            h4("Run the optimization to see results.",
               style = "color:gray; text-align:center; margin-top:50px;")
          )
        ),

        # ---- Tab 6: Cost Analysis -----------------------------------------
        tabPanel("Cost Analysis",
          h3("Cost Analysis"),
          div(class = "description-box",
            "Detailed financial analysis of the optimized plan. Includes ",
            "transport costs, lost margin from stock-outs, import/purchasing ",
            "costs, total revenue, and net profitability. The daily charts ",
            "show how costs evolve over the planning horizon."
          ),
          conditionalPanel(
            condition = "output.has_results",
            fluidRow(
              column(6, plotlyOutput("cost_breakdown_chart", height = "350px")),
              column(6, plotlyOutput("cost_daily_chart", height = "350px"))
            ),
            hr(),
            plotlyOutput("service_level_chart", height = "300px")
          ),
          conditionalPanel(
            condition = "!output.has_results",
            h4("Run the optimization to see cost analysis.",
               style = "color:gray; text-align:center; margin-top:50px;")
          )
        ),

        # ---- Tab 7: Assumptions -----------------------------------------
        tabPanel("Assumptions",
          h3("Model Assumptions And Formulas"),
          div(class = "description-box",
            "Assumptions used in this optimizer."
          ),
          h4("Planning Scope"),
          tags$ul(
            tags$li("One warehouse and 1-10 stores."),
            tags$li("Daily time-step planning over a user-defined horizon."),
            tags$li("Objective: minimize import cost + transport cost + lost margin.")
          ),
          h4("Demand Model"),
          tags$p("For each product, store and day:"),
          tags$pre("Demand = (WeeklyDemand / 7) * WeeklySeasonality * MonthlySeasonality * RandomError"),
          tags$ul(
            tags$li("RandomError is sampled uniformly between (1 - error%) and (1 + error%)."),
            tags$li("Store-level demand uses either overrides or weekly demand scaled by store multiplier.")
          ),
          h4("Inventory And Flow Assumptions"),
          tags$ul(
            tags$li("Supplier imports arrive after import_delivery_time days."),
            tags$li("Import quantities are rounded to import_order_size multiples."),
            tags$li("Initial inventory starts at a demand-based level for warehouse and stores."),
            tags$li("Warehouse and store constraints enforce both weight and volume capacities."),
            tags$li("Stockouts are allowed and valued as lost margin.")
          ),
          h4("Delivery Schedule (Replenishment Policy)"),
          tags$p("Deliveries use a replenishment policy that minimises transport cost:"),
          tags$ul(
            tags$li("A delivery trip to a store is triggered when ANY product drops below the reorder threshold (1 day of stock)."),
            tags$li("When triggered, ALL products at that store are topped up to the target coverage level (5 days of demand)."),
            tags$li("This consolidates multiple products into fewer, larger trips, reducing fixed trip costs."),
            tags$li("The LP solver plans import orders, while the replenishment policy determines delivery timing.")
          ),
          h4("Transport And Routing"),
          tags$ul(
            tags$li("Road routes are retrieved from OSRM; otherwise uses linear distance approximation when unavailable."),
            tags$li("Per vehicle max distance per day = speed * 12 hours. (assuming 12 hours in a workday)"),
            tags$li("Trips are constrained by round-trip distance and vehicle count."),
            tags$li("Delivery feasibility is constrained by vehicle weight and volume capacities.")
          ),
          h4("Service Level"),
          tags$p("The achieved OTIN style service level is computed as:"),
          tags$pre("ServiceLevel = 1 - (TotalLostMargin / TotalDemandMargin)"),
          tags$p("The optimizer targets the user-specified service level while balancing operating costs.")
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    products         = NULL,
    stores           = copy(DEFAULT_STORES),
    wh_lat           = DEFAULT_WH_LAT,
    wh_lng           = DEFAULT_WH_LNG,
    results          = NULL,
    demand_cache     = NULL,
    demand_overrides = NULL,
    weekly_seasonality  = NULL,
    monthly_seasonality = NULL,
    vehicles = {
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

  # ---- Vehicle data with computed max_distance_per_day -------------------
  vehicles_dt <- reactive({
    vdt <- copy(rv$vehicles)
    vdt[, max_distance_per_day := speed * 12]
    vdt
  })

  planning_horizon <- reactive({
    horizon <- suppressWarnings(as.integer(input$planning_horizon))
    if (is.na(horizon)) return(PLANNING_HORIZON)
    max(1L, min(365L, horizon))
  })

  # ---- OSRM Road Routing ------------------------------------------------
  route_info <- reactive({
    stores <- rv$stores
    routes <- list()
    for (i in seq_len(nrow(stores))) {
      route <- get_osrm_route(rv$wh_lat, rv$wh_lng,
                              stores$lat[i], stores$lng[i])
      if (is.null(route)) {
        d <- haversine_km(rv$wh_lat, rv$wh_lng,
                          stores$lat[i], stores$lng[i])
        route <- list(distance_km = d * 1.3, duration_min = d * 1.3,
                      route_lng = c(rv$wh_lng, stores$lng[i]),
                      route_lat = c(rv$wh_lat, stores$lat[i]),
                      fallback = TRUE)
      } else {
        route$fallback <- FALSE
      }
      routes[[i]] <- route
    }
    routes
  })

  distances <- reactive({
    sapply(route_info(), function(r) r$distance_km)
  })

  # ---- Load Default Products ---------------------------------------------
  observe({
    default_path <- file.path(getwd(), "store_products.csv")
    if (file.exists(default_path) && is.null(rv$products))
      rv$products <- read_product_csv(default_path)
  })

  observeEvent(input$csv_upload, {
    req(input$csv_upload)
    rv$products <- read_product_csv(input$csv_upload$datapath)
    rv$results <- NULL
    rv$demand_overrides <- NULL
    rv$weekly_seasonality <- NULL
    rv$monthly_seasonality <- NULL
    showNotification(paste("Loaded", nrow(rv$products), "products"),
                     type = "message")
  })

  # ---- Store Management --------------------------------------------------
  observeEvent(input$n_stores, {
    n <- input$n_stores
    cur <- rv$stores
    if (n > nrow(cur)) {
      for (i in (nrow(cur) + 1):n)
        cur <- rbind(cur, data.table(
          store_id = i, store_name = paste("Store", i),
          lat = DEFAULT_WH_LAT + runif(1, -0.5, 0.5),
          lng = DEFAULT_WH_LNG + runif(1, -0.5, 0.5),
          multiplier = 1.0))
    } else if (n < nrow(cur)) cur <- cur[1:n]
    rv$stores <- cur
    rv$results <- NULL
    rv$demand_overrides <- NULL
  })

  observeEvent(input$add_store_btn, {
    cur <- rv$stores
    if (nrow(cur) >= 10) {
      showNotification("Maximum of 10 stores reached.", type = "warning")
      return()
    }
    n <- nrow(cur) + 1
    new_store <- data.table(
      store_id = n,
      store_name = paste("Store", n),
      lat = rv$wh_lat,
      lng = rv$wh_lng,
      multiplier = 1.0
    )
    rv$stores <- rbind(cur, new_store)
    rv$results <- NULL
    rv$demand_overrides <- NULL
    updateNumericInput(session, "n_stores", value = n)
  })

  observeEvent(input$remove_last_store_btn, {
    cur <- rv$stores
    if (nrow(cur) <= 1) {
      showNotification("At least one store is required.", type = "warning")
      return()
    }
    rv$stores <- cur[1:(nrow(cur) - 1)]
    rv$results <- NULL
    rv$demand_overrides <- NULL
    updateNumericInput(session, "n_stores", value = nrow(rv$stores))
  })

  observeEvent(input$reset_stores, {
    rv$stores <- copy(DEFAULT_STORES)[1:min(input$n_stores, 3)]
    if (input$n_stores > 3) {
      for (i in 4:input$n_stores)
        rv$stores <- rbind(rv$stores, data.table(
          store_id = i, store_name = paste("Store", i),
          lat = DEFAULT_WH_LAT + runif(1, -0.5, 0.5),
          lng = DEFAULT_WH_LNG + runif(1, -0.5, 0.5),
          multiplier = 1.0))
    }
    rv$wh_lat <- DEFAULT_WH_LAT
    rv$wh_lng <- DEFAULT_WH_LNG
  })

  # ---- Map ---------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = DEFAULT_WH_LNG, lat = DEFAULT_WH_LAT, zoom = 8)
  })

  observe({
    stores <- rv$stores
    dists  <- distances()
    routes <- route_info()
    proxy  <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

    proxy <- proxy %>% addMarkers(
      lng = rv$wh_lng, lat = rv$wh_lat, layerId = "warehouse",
      popup = "Warehouse (drag to move)",
      options = markerOptions(draggable = TRUE))

    for (i in seq_len(nrow(stores))) {
      proxy <- proxy %>%
        addMarkers(
          lng = stores$lng[i], lat = stores$lat[i],
          layerId = paste0("store_", stores$store_id[i]),
          popup = paste0(stores$store_name[i],
                         "<br>Road: ", round(dists[i], 1), " km",
                         "<br><i>Drag to move</i>"),
          icon = makeIcon(
            iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png",
            iconWidth = 25, iconHeight = 41,
            iconAnchorX = 12, iconAnchorY = 41),
          options = markerOptions(draggable = TRUE)
        ) %>%
        addPolylines(
          lng = routes[[i]]$route_lng,
          lat = routes[[i]]$route_lat,
          color = "steelblue", weight = 5, opacity = 0.85,
          popup = paste(round(dists[i], 1), "km"))
    }
  })

  observeEvent(input$map_marker_dragend, {
    evt <- input$map_marker_dragend
    if (evt$id == "warehouse") {
      rv$wh_lat <- evt$lat; rv$wh_lng <- evt$lng
    } else if (grepl("^store_", evt$id)) {
      sid <- as.integer(sub("^store_", "", evt$id))
      idx <- which(rv$stores$store_id == sid)
      if (length(idx) == 1) {
        rv$stores$lat[idx] <- evt$lat; rv$stores$lng[idx] <- evt$lng
      }
    }
  })

  # ---- Store Table -------------------------------------------------------
  output$store_table <- renderDT({
    stores <- rv$stores
    dists  <- distances()
    display <- data.table(
      Store = stores$store_name,
      Latitude = round(stores$lat, 4),
      Longitude = round(stores$lng, 4),
      `Road Distance (km)` = round(dists, 1)
    )
    datatable(display,
              options = list(pageLength = 10, dom = "t"),
              rownames = FALSE)
  })

  # ---- Product Table -----------------------------------------------------
  output$product_table <- renderDT({
    req(rv$products)
    display_cols <- intersect(
      c("product_idx", "name", "price", "inkoop", "weight_g", "volume_l",
        "shelf_life", "gekoeld", "bevroren", "import_order_size",
        "import_delivery_time", "weekly_demand", "image_url"),
      names(rv$products))

    display_dt <- copy(rv$products[, ..display_cols])

    # If an image URL is present, create a small inline HTML image column
    if ("image_url" %in% names(display_dt)) {
      display_dt[, Image := paste0(
        '<img src="', image_url, '" style="height:36px; width:36px; object-fit:cover; border-radius:4px;"/>'
      )]
      # Put Image first for nicer display, and drop raw image_url from view
      cols <- c("Image", setdiff(names(display_dt), c("Image", "image_url")))
      setcolorder(display_dt, cols)
      display_dt[, image_url := NULL]
    }

    # Disable editing for the identifier and the image column (zero-based indices)
    disable_cols <- 0
    if ("Image" %in% names(display_dt)) disable_cols <- c(0, 1)

    datatable(display_dt,
              selection = "single",
              editable = list(target = "cell", disable = list(columns = disable_cols)),
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE,
              escape = FALSE)
  })

  observeEvent(input$product_table_cell_edit, {
    info <- input$product_table_cell_edit
    display_cols <- intersect(
      c("product_idx", "name", "price", "inkoop", "weight_g", "volume_l",
        "shelf_life", "gekoeld", "bevroren", "import_order_size",
        "import_delivery_time", "weekly_demand"),
      names(rv$products))
    col_name <- display_cols[info$col + 1]
    new_val <- info$value
    if (col_name != "name") new_val <- as.numeric(new_val)
    rv$products[info$row, (col_name) := new_val]
  })

  observeEvent(input$add_product, {
    req(rv$products)
    n <- nrow(rv$products) + 1
    new_row <- data.table(
      name = paste("New Product", n), price = 1.00, inkoop = 0.50,
      weight_g = 100, volume_l = 0.1, shelf_life = 7, gekoeld = 0,
      bevroren = 0, import_order_size = 50, import_delivery_time = 2,
      product_idx = n, weekly_demand = 609)
    for (col in setdiff(names(rv$products), names(new_row)))
      new_row[, (col) := NA]
    for (col in setdiff(names(new_row), names(rv$products)))
      rv$products[, (col) := NA]
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

  # ---- Vehicle Fleet Table -----------------------------------------------
  output$vehicle_fleet_table <- renderDT({
    vdt <- copy(rv$vehicles)
    datatable(vdt,
              selection = "single",
              editable = list(target = "cell", disable = list(columns = c(0))),
              options = list(pageLength = 10, dom = "t", scrollX = TRUE),
              rownames = FALSE)
  })

  observeEvent(input$add_vehicle, {
    vdt <- copy(rv$vehicles)
    n <- nrow(vdt) + 1
    vdt <- rbind(vdt, data.table(
      type = paste("Vehicle", n),
      count = 1L,
      cost_per_km = 0.8,
      speed = 60,
      fixed_cost_per_trip = 30,
      max_weight = 2500,
      max_volume = 12000,
      refrigerated_capable = TRUE,
      frozen_capable = FALSE
    ), fill = TRUE)
    rv$vehicles <- vdt
  })

  observeEvent(input$remove_vehicle, {
    sel <- input$vehicle_fleet_table_rows_selected
    if (length(sel) == 1 && nrow(rv$vehicles) > 1) {
      rv$vehicles <- rv$vehicles[-sel]
    }
  })

  observeEvent(input$vehicle_fleet_table_cell_edit, {
    info <- input$vehicle_fleet_table_cell_edit
    col_name <- names(rv$vehicles)[info$col + 1]
    new_val <- info$value
    if (col_name %in% c("refrigerated_capable", "frozen_capable"))
      new_val <- as.logical(new_val)
    else if (col_name != "type") new_val <- as.numeric(new_val)
    rv$vehicles[info$row, (col_name) := new_val]
  })

  # ---- Demand Overrides Table --------------------------------------------
  observe({
    req(rv$products, rv$stores)
    if (is.null(rv$demand_overrides) ||
        nrow(rv$demand_overrides) != nrow(rv$products) ||
        ncol(rv$demand_overrides) != nrow(rv$stores) + 1) {
      mat <- data.table(Product = rv$products$name)
      for (s in seq_len(nrow(rv$stores)))
        mat[, (rv$stores$store_name[s]) :=
              round(rv$products$weekly_demand * rv$stores$multiplier[s], 1)]
      rv$demand_overrides <- mat
    }
  })

  output$demand_override_table <- renderDT({
    req(rv$demand_overrides)
    datatable(rv$demand_overrides,
              editable = list(target = "cell", disable = list(columns = c(0))),
              options = list(pageLength = 20, scrollX = TRUE, dom = "tp"),
              rownames = FALSE)
  })

  observeEvent(input$demand_override_table_cell_edit, {
    info <- input$demand_override_table_cell_edit
    col_name <- names(rv$demand_overrides)[info$col + 1]
    new_val <- as.numeric(info$value)
    if (!is.na(new_val) && new_val >= 0)
      rv$demand_overrides[info$row, (col_name) := new_val]
  })

  output$demand_multiplier_table <- renderDT({
    stores <- copy(rv$stores)
    display <- stores[, .(
      Store = store_name,
      Multiplier = round(multiplier, 3)
    )]
    datatable(display,
              editable = list(target = "cell", disable = list(columns = c(0))),
              options = list(pageLength = 10, dom = "t"),
              rownames = FALSE)
  })

  observeEvent(input$demand_multiplier_table_cell_edit, {
    info <- input$demand_multiplier_table_cell_edit
    if (info$col == 1) {
      new_val <- as.numeric(info$value)
      if (!is.na(new_val) && new_val > 0) {
        rv$stores$multiplier[info$row] <- new_val
      }
    }
  })

  # ---- Seasonality Tables ------------------------------------------------
  observe({
    req(rv$products)
    if (is.null(rv$weekly_seasonality) ||
        nrow(rv$weekly_seasonality) != nrow(rv$products)) {
      dt <- data.table(Product = rv$products$name)
      for (i in seq_along(DAY_NAMES))
        dt[, (DAY_NAMES[i]) := DEFAULT_WEEKLY_SEASONALITY[i]]
      rv$weekly_seasonality <- dt
    }
    if (is.null(rv$monthly_seasonality) ||
        nrow(rv$monthly_seasonality) != nrow(rv$products)) {
      dt <- data.table(Product = rv$products$name)
      for (i in seq_along(month.abb))
        dt[, (month.abb[i]) := DEFAULT_MONTHLY_SEASONALITY[i]]
      rv$monthly_seasonality <- dt
    }
  })

  output$weekly_seas_table <- renderDT({
    req(rv$weekly_seasonality)
    datatable(rv$weekly_seasonality,
              editable = list(target = "cell", disable = list(columns = c(0))),
              options = list(pageLength = 20, dom = "t", scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = DAY_NAMES, digits = 2)
  })

  observeEvent(input$weekly_seas_table_cell_edit, {
    info <- input$weekly_seas_table_cell_edit
    col_name <- names(rv$weekly_seasonality)[info$col + 1]
    new_val <- as.numeric(info$value)
    if (!is.na(new_val) && new_val >= 0)
      rv$weekly_seasonality[info$row, (col_name) := new_val]
  })

  output$monthly_seas_table <- renderDT({
    req(rv$monthly_seasonality)
    datatable(rv$monthly_seasonality,
              editable = list(target = "cell", disable = list(columns = c(0))),
              options = list(pageLength = 20, dom = "t", scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = month.abb, digits = 2)
  })

  observeEvent(input$monthly_seas_table_cell_edit, {
    info <- input$monthly_seas_table_cell_edit
    col_name <- names(rv$monthly_seasonality)[info$col + 1]
    new_val <- as.numeric(info$value)
    if (!is.na(new_val) && new_val >= 0)
      rv$monthly_seasonality[info$row, (col_name) := new_val]
  })

  # ---- Demand Selectors --------------------------------------------------
  observe({
    req(rv$products)
    choices <- setNames(rv$products$product_idx, rv$products$name)
    updateSelectInput(session, "demand_product", choices = choices)
    choices_with_all <- c("All Products" = "all", choices)
    updateSelectInput(session, "inv_product", choices = choices_with_all)
  })

  observe({
    choices <- setNames(rv$stores$store_id, rv$stores$store_name)
    updateSelectInput(session, "demand_store", choices = choices)
    updateSelectInput(session, "inv_store", choices = choices)
  })

  # ---- Demand Chart ------------------------------------------------------
  demand_data <- reactive({
    req(rv$products, rv$stores)
    generate_demand(rv$products, rv$stores, planning_horizon(), input$error_pct,
                    demand_overrides = rv$demand_overrides,
                    weekly_seasonality = rv$weekly_seasonality,
                    monthly_seasonality = rv$monthly_seasonality)
  })

  output$demand_chart <- renderPlotly({
    req(rv$products, rv$stores)
    dd    <- demand_data()
    p_idx <- as.integer(input$demand_product)
    s_id  <- as.integer(input$demand_store)
    if (is.na(p_idx) || is.na(s_id)) return(NULL)
    plot_data <- dd[product_idx == p_idx & store_id == s_id]
    if (nrow(plot_data) == 0) return(NULL)
    p_name <- rv$products$name[rv$products$product_idx == p_idx][1]
    s_name <- rv$stores$store_name[rv$stores$store_id == s_id][1]
    p <- ggplot(plot_data, aes(x = date, y = demand)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_smooth(method = "loess", formula = y ~ x,
                  se = FALSE, color = "darkred", linewidth = 1) +
      labs(title = paste("Daily Demand Forecast:", p_name, "-", s_name),
           x = "Date", y = "Units") +
      theme_minimal()
    ggplotly(p)
  })

  # ---- Seasonality Charts ------------------------------------------------
  output$weekly_seasonality_chart <- renderPlotly({
    req(rv$weekly_seasonality)
    avg <- colMeans(rv$weekly_seasonality[, 2:8])
    df <- data.frame(
      Day = factor(DAY_NAMES, levels = DAY_NAMES),
      Factor = avg)
    p <- ggplot(df, aes(x = Day, y = Factor)) +
      geom_col(fill = "coral", alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
      labs(title = "Weekly Seasonality (avg across products)", y = "Multiplier") +
      theme_minimal()
    ggplotly(p)
  })

  output$monthly_seasonality_chart <- renderPlotly({
    req(rv$monthly_seasonality)
    avg <- colMeans(rv$monthly_seasonality[, 2:13])
    df <- data.frame(
      Month = factor(month.abb, levels = month.abb),
      Factor = avg)
    p <- ggplot(df, aes(x = Month, y = Factor)) +
      geom_col(fill = "mediumpurple", alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
      labs(title = "Monthly Seasonality (avg across products)", y="Multiplier") +
      theme_minimal()
    ggplotly(p)
  })

  # ---- Run Optimization --------------------------------------------------
  observeEvent(input$run_optim, {
    req(rv$products, nrow(rv$products) > 0)
    showNotification("Running optimization...", id = "optim_notif",
                     duration = NULL, type = "message")

    wh_cap <- list(max_weight = input$wh_max_weight,
                   max_volume = input$wh_max_volume,
                   max_refrig_weight = input$wh_max_refrig_weight,
                   max_frozen_weight = input$wh_max_frozen_weight)
    st_cap <- list(max_weight = input$st_max_weight,
                   max_volume = input$st_max_volume,
                   max_refrig_weight = input$st_max_refrig_weight,
                   max_frozen_weight = input$st_max_frozen_weight)

    demand <- generate_demand(rv$products, rv$stores, planning_horizon(),
                              input$error_pct,
                              demand_overrides = rv$demand_overrides,
                              weekly_seasonality = rv$weekly_seasonality,
                              monthly_seasonality = rv$monthly_seasonality)
    rv$demand_cache <- demand

    tryCatch({
      results <- run_optimization_with_service_guard(
        products       = rv$products,
        stores         = rv$stores,
        demand_dt      = demand,
        vehicles       = vehicles_dt(),
        distances      = distances(),
        wh_capacity    = wh_cap,
        store_capacity = st_cap,
        requested_service_level = input$service_level / 100,
        planning_horizon = planning_horizon())
      rv$results <- results
      removeNotification("optim_notif")
      if (!is.null(results$service_warning)) {
        showNotification(results$service_warning, type = "warning", duration = 12)
      }
      showNotification(
        paste0("Optimization complete! Service level: ",
               round(results$service_level * 100, 2), "% | Netto profit: \u20AC",
               format(round(results$netto_profit, 2), big.mark = ",")),
        type = "message", duration = 10)
    }, error = function(e) {
      removeNotification("optim_notif")
      showNotification(paste("Optimization error:", e$message),
                       type = "error", duration = 15)
    })
  })

  output$optim_status <- renderText({
    if (is.null(rv$results))
      "Status: Ready \u2014 configure settings and click Run Optimization"
    else
      paste0("Status: Complete | SL: ",
             round(rv$results$service_level * 100, 2), "% | Profit: \u20AC",
             format(round(rv$results$netto_profit, 2), big.mark = ","))
  })

  output$has_results <- reactive({ !is.null(rv$results) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # ---- Value Boxes -------------------------------------------------------
  output$vb_revenue_title <- renderText("Total Revenue")
  output$vb_revenue_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$total_revenue, 2), big.mark = ","))
  })
  output$vb_import_cost_title <- renderText("Import Cost")
  output$vb_import_cost_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$import_cost, 2), big.mark = ","))
  })
  output$vb_transport_title <- renderText("Transport Cost")
  output$vb_transport_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$transport_cost, 2), big.mark = ","))
  })
  output$vb_profit_title <- renderText("Netto Profit")
  output$vb_profit_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$netto_profit, 2), big.mark = ","))
  })
  output$vb_lost_margin_title <- renderText("Lost Margin")
  output$vb_lost_margin_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$total_lost_margin, 2),
                            big.mark = ","))
  })
  output$vb_cost_title <- renderText("Total Cost")
  output$vb_cost_value <- renderText({
    req(rv$results)
    paste0("\u20AC ", format(round(rv$results$total_cost, 2), big.mark = ","))
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
    if (nrow(dt) == 0)
      return(datatable(data.frame(Message = "No imports needed")))
    wide <- dcast(dt, product_name ~ paste0("Day_", day),
                  value.var = "import_qty", fill = 0, fun.aggregate = sum)
    datatable(wide, options = list(scrollX = TRUE, pageLength = 20),
              rownames = FALSE) %>%
      formatRound(columns = setdiff(names(wide), "product_name"), digits = 0)
  })

  # ---- Warehouse Inventory Chart -----------------------------------------
  output$wh_inv_chart <- renderPlotly({
    req(rv$results)
    dt <- rv$results$wh_inventory
    if (nrow(dt) == 0) return(NULL)
    p <- ggplot(dt, aes(x = day, y = warehouse_inventory,
                        fill = product_name)) +
      geom_area(alpha = 0.7, position = "stack") +
      labs(title = "Warehouse Inventory Over Time (Stacked by Product)",
           x = "Day", y = "Units", fill = "Product") +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggplotly(p)
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
  output$st_inv_chart <- renderPlotly({
    req(rv$results)
    dt <- rv$results$store_inventory
    s_id  <- as.integer(input$inv_store)
    p_idx <- input$inv_product
    if (is.na(s_id)) return(NULL)
    s_name <- rv$stores$store_name[rv$stores$store_id == s_id][1]

    if (p_idx == "all") {
      plot_dt <- dt[store_id == s_id]
      if (nrow(plot_dt) == 0) return(NULL)
      p <- ggplot(plot_dt, aes(x = day, y = store_inventory,
                               fill = product_name)) +
        geom_area(alpha = 0.7, position = "stack") +
        labs(title = paste("Store Inventory (All Products) -", s_name),
             x = "Day", y = "Units", fill = "Product") +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else {
      p_idx_int <- as.integer(p_idx)
      if (is.na(p_idx_int)) return(NULL)
      plot_dt <- dt[store_id == s_id & product_idx == p_idx_int]
      if (nrow(plot_dt) == 0) return(NULL)
      p_name <- plot_dt$product_name[1]
      p <- ggplot(plot_dt, aes(x = day)) +
        geom_area(aes(y = store_inventory), fill = "forestgreen", alpha = 0.3) +
        geom_line(aes(y = store_inventory), color = "forestgreen", linewidth = 1) +
        geom_col(aes(y = -stockout), fill = "red", alpha = 0.6) +
        labs(title = paste("Store Inventory:", p_name, "-", s_name),
             x = "Day", y = "Units (negative = stockout)") +
        theme_minimal()
    }
    ggplotly(p)
  })

  # ---- Delivery Schedule Table -------------------------------------------
  output$delivery_table <- renderDT({
    req(rv$results)
    dt <- rv$results$delivery_schedule
    if (nrow(dt) == 0)
      return(datatable(data.frame(Message = "No deliveries")))
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
  output$vehicle_chart <- renderPlotly({
    req(rv$results)
    tt <- rv$results$truck_trips
    if (nrow(tt) == 0) return(NULL)
    agg <- tt[, .(total_trips = sum(trips), total_weight = sum(weight_kg)),
              by = .(day, vehicle_type)]
    p <- ggplot(agg, aes(x = day, y = total_trips, fill = vehicle_type)) +
      geom_col(position = "stack", alpha = 0.8) +
      labs(title = "Daily Vehicle Trips",
           x = "Day", y = "Number of Trips", fill = "Vehicle") +
      scale_fill_manual(values = c("Truck" = "steelblue", "Van" = "coral")) +
      theme_minimal()
    ggplotly(p)
  })

  output$vehicle_table <- renderDT({
    req(rv$results)
    tt <- rv$results$truck_trips
    if (nrow(tt) == 0) return(datatable(data.frame(Message = "No trips")))
    agg <- tt[, .(Trips = sum(trips),
                  `Weight (kg)` = round(sum(weight_kg), 1),
                  `Volume (L)` = round(sum(volume_l), 1)),
              by = .(Day = day, Vehicle = vehicle_type, Store = store_id)]
    agg <- merge(agg, rv$stores[, .(store_id, store_name)],
                 by.x = "Store", by.y = "store_id", all.x = TRUE)
    agg[, Store := store_name][, store_name := NULL]
    datatable(agg[order(Day, Store)],
              options = list(scrollX = TRUE, pageLength = 30),
              rownames = FALSE)
  })

  # ---- Cost Analysis Charts ----------------------------------------------
  output$cost_breakdown_chart <- renderPlotly({
    req(rv$results)
    costs <- data.frame(
      Category = c("Import Cost", "Transport", "Lost Margin"),
      Cost = c(rv$results$import_cost, rv$results$transport_cost,
               rv$results$total_lost_margin))
    p <- ggplot(costs, aes(x = Category, y = Cost, fill = Category)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = paste0("\u20AC", round(Cost, 0))),
                vjust = -0.5, size = 4) +
      labs(title = "Cost Breakdown", y = "Cost (\u20AC)") +
      scale_fill_manual(values = c("Import Cost" = "goldenrod",
                                    "Transport" = "steelblue",
                                    "Lost Margin" = "coral")) +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$cost_daily_chart <- renderPlotly({
    req(rv$results)
    tt  <- rv$results$truck_trips
    veh <- vehicles_dt()
    dists <- distances()

    horizon <- max(rv$results$demand$day)
    daily_transport <- data.table(day = 1:horizon, transport = 0)
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

    so <- rv$results$stockouts
    daily_lost <- data.table(day = 1:horizon, lost_margin = 0)
    if (nrow(so) > 0) {
      agg <- so[, .(lm = sum(lost_margin)), by = day]
      for (r in seq_len(nrow(agg)))
        daily_lost[day == agg$day[r], lost_margin := agg$lm[r]]
    }

    daily_costs <- merge(daily_transport, daily_lost, by = "day")
    plot_dt <- melt(daily_costs, id.vars = "day",
                    measure.vars = c("transport", "lost_margin"),
                    variable.name = "type", value.name = "cost")
    plot_dt[, type := fifelse(type == "transport", "Transport", "Lost Margin")]

    p <- ggplot(plot_dt, aes(x = day, y = cost, fill = type)) +
      geom_area(position = "stack", alpha = 0.7) +
      labs(title = "Daily Cost Breakdown",
           x = "Day", y = "Cost (\u20AC)", fill = "Type") +
      scale_fill_manual(values = c("Transport" = "steelblue",
                                    "Lost Margin" = "coral")) +
      theme_minimal()
    ggplotly(p)
  })

  output$service_level_chart <- renderPlotly({
    req(rv$results)
    demand   <- rv$results$demand
    so       <- rv$results$stockouts
    products <- rv$products

    horizon <- max(rv$results$demand$day)
    daily_sl <- data.table(day = 1:horizon, sl = 1.0)
    for (d in 1:horizon) {
      day_demand <- demand[day == d]
      total_margin_day <- sum(day_demand$demand *
                                (products$price[day_demand$product_idx] -
                                   products$inkoop[day_demand$product_idx]))
      lost_day <- if (nrow(so) > 0 && d %in% so$day) sum(so[day == d, lost_margin]) else 0
      daily_sl[day == d, sl := ifelse(total_margin_day > 0,
                                       1 - lost_day / total_margin_day, 1)]
    }
    target_sl <- input$service_level / 100
    p <- ggplot(daily_sl, aes(x = day, y = sl * 100)) +
      geom_line(color = "forestgreen", linewidth = 1) +
      geom_point(color = "forestgreen", size = 2) +
      geom_hline(yintercept = target_sl * 100,
                 linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = horizon, y = target_sl * 100 - 1,
               label = paste0("Target: ", target_sl * 100, "%"),
               hjust = 1, color = "red") +
      labs(title = "Daily Service Level",
           x = "Day", y = "Service Level (%)") +
      ylim(c(max(0, min(daily_sl$sl, na.rm = TRUE) * 100 - 5), 100)) +
      theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui, server)
