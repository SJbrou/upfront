library(data.table); library(lpSolve); library(httr); library(jsonlite)

lines <- readLines("app.R")
last_line <- which(grepl("^shinyApp", lines))
if (length(last_line) > 0) lines <- lines[1:(last_line[1]-1)]
eval(parse(text = paste(lines, collapse = "\n")))

products <- read_product_csv("store_products.csv")
stores <- copy(DEFAULT_STORES)
cat("Products:", nrow(products), "\nStores:", nrow(stores), "\n")

# Test OSRM routing
route <- get_osrm_route(DEFAULT_WH_LAT, DEFAULT_WH_LNG, stores$lat[1], stores$lng[1])
if (!is.null(route))
  cat("OSRM OK: Amsterdam->Rotterdam =", round(route$distance_km,1), "km,",
      round(route$duration_min,0), "min\n")
if (is.null(route)) cat("OSRM fallback\n")

# Test demand generation with per-product seasonality
w_seas <- data.table(Product = products$name)
for (d in DAY_NAMES) w_seas[, (d) := 1.0]
m_seas <- data.table(Product = products$name)
for (m in month.abb) m_seas[, (m) := 1.0]
demand <- generate_demand(products, stores, 30, 20,
                          weekly_seasonality = w_seas,
                          monthly_seasonality = m_seas)
cat("Demand rows:", nrow(demand), "\n")

# Test optimization (normal capacity)
vehicles <- data.table(
  type = c("Truck","Van"), count = c(1L,1L),
  cost_per_km = c(1.2,0.6), speed = c(60,80),
  fixed_cost_per_trip = c(50,25),
  max_weight = c(8000,1500), max_volume = c(40000,8000),
  refrigerated_capable = c(TRUE,TRUE),
  frozen_capable = c(TRUE,FALSE),
  max_distance_per_day = c(720,960))
distances <- c(60, 40, 55)
wh_cap <- list(max_weight=50000, max_volume=100000,
               max_refrig_weight=20000, max_frozen_weight=10000)
st_cap <- list(max_weight=5000, max_volume=10000,
               max_refrig_weight=2000, max_frozen_weight=1000)

res <- run_optimization(products, stores, demand, vehicles,
                        distances, wh_cap, st_cap, 0.99)
cat("\n=== Normal capacity results ===\n")
cat("Service Level:", round(res$service_level*100,2), "%\n")
cat("Import Cost: EUR", round(res$import_cost,2), "\n")
cat("Transport Cost: EUR", round(res$transport_cost,2), "\n")
cat("Total Revenue: EUR", round(res$total_revenue,2), "\n")
cat("Netto Profit: EUR", round(res$netto_profit,2), "\n")
cat("Lost Margin: EUR", round(res$total_lost_margin,2), "\n")
cat("Imports:", nrow(res$import_schedule), "records\n")
cat("Deliveries:", nrow(res$delivery_schedule), "records\n")

# Test with 5kg store capacity
cat("\n=== 5kg store capacity ===\n")
st_cap_low <- list(max_weight=5, max_volume=10000,
                   max_refrig_weight=2000, max_frozen_weight=1000)
res2 <- run_optimization(products, stores, demand, vehicles,
                         distances, wh_cap, st_cap_low, 0.99)
cat("Service Level:", round(res2$service_level*100,2), "%\n")
cat("Lost Margin: EUR", round(res2$total_lost_margin,2), "\n")
cat("Netto Profit: EUR", round(res2$netto_profit,2), "\n")

cat("\nAll tests passed!\n")
