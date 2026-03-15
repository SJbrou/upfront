You are a supply chain analyst tasked with creating a Shiny dashboard MVP in R that optimizes the import schedule (supplier → warehouse) and delivery schedule (warehouse → shops) for a grocery retailer. The model should calculate the optimal inventory flows for the next 30 days (daily time steps) while minimizing operating costs and respecting logistics and storage constraints.

Goal
Create a Shiny dashboard MVP in R that optimizes the import schedule (supplier → warehouse) and delivery schedule (warehouse → shops) for a grocery retailer.
The model should calculate the optimal inventory flows for the next 30 days (daily time steps) while minimizing operating costs and respecting logistics and storage constraints.

The tool should support:
16–160 products
1–10 stores
1 warehouse

The optimization objective is to minimize total cost, defined as:
transport_cost
+ lost_margin_due_to_stockouts

while satisfying a service level constraint (OTIN).
Default service level:
99% OTIN
but this must be editable by the user.

Technical Requirements
Create the application using:

R
Shiny
leaflet (map)
data.table or dplyr
ompr or lpSolve for optimization
DT for tables
ggplot2 for charts

Structure the app as:
app.R
  UI
  server
  optimization_model()
  
Time Horizon
planning_horizon = 30 days
time_step = 1 day
Optimization Variables

The model should determine:

import_qty[product, day]
warehouse_inventory[product, day]
store_inventory[store, product, day]
delivery_qty[store, product, day]
truck_trips[vehicle, store, day]
Objective Function

Minimize:

Total cost =
  transport_cost
  + lost_margin_due_to_stockouts

Where:

lost_margin = price - inkoop

Constraint:

lost_margin_total <= (1 - service_level) * total_margin
Inventory Initialization

At the start of the planning horizon:

store_inventory = 100% of capacity based on expected demand
warehouse_inventory = 80% of capacity based on expected demand
Logistics Locations

Assume:

1 warehouse
1–10 stores

The user can place locations on a map using Leaflet.

The system should compute:

distance_warehouse_store
travel_time_warehouse_store

These values should be used in transportation calculations.

Storage Constraints

Each store has the following capacity:

store_max_volume
store_max_weight
store_max_refrigerated_volume
store_max_refrigerated_weight
store_max_frozen_volume
store_max_frozen_weight

Warehouse has equivalent constraints:

warehouse_max_volume
warehouse_max_weight
warehouse_max_refrigerated_volume
warehouse_max_refrigerated_weight
warehouse_max_frozen_volume
warehouse_max_frozen_weight

For simplicity:

all stores have identical capacity
Product Data

Products are imported from:

store_products.csv

Delimiter:

;

Decimal separator:

,

Columns:

name
price
link
image_url
vendor
handle
product_id
shelf_life
gekoeld
bevroren
weight (gr)
volume (l)
inkoop
import_order_size
import_delivery_time

Features:

upload CSV

edit products

add products

remove products

Demand Model

Demand is defined per product.

Demand formula:

Demand = BaseDemand
         * WeeklySeasonality
         * MonthlySeasonality
         * RandomError

Random error:

uniform(-error%, +error%)

Example:

error = 20%

Demand is entered as weekly demand and converted to daily demand.

Store demand:

store_demand = base_demand * store_multiplier
Transportation Model

Vehicle fleet:

1 standard truck
1 van

User can edit:

number_of_trucks
number_of_vans

Each vehicle has:

cost_per_km
speed
fixed_cost_per_trip
max_weight
max_volume
refrigerated_capable
frozen_capable
max_distance_per_day

Transportation rules:

vehicles operate 16 hours per day
max_distance_per_day = speed * 16

Trips per day are calculated from:

warehouse_store_distance
travel_time
Simplifications

Assume:

no lead time variability
no loading time
no unloading time
no holding costs
no supplier transport costs

Stockouts are allowed.

Cost of stockout:

lost_margin = price - inkoop

Constraint:

total_lost_margin <= 1% of total margin

(default OTIN = 99%)

Required Dashboard UI

Create the following layout.

Sidebar

Inputs:

Upload product CSV
Edit vehicle parameters
Set service level
Set store demand multipliers
Set storage capacities
Run optimization button
Main Panel Tabs
1 Map

Interactive map:

warehouse location
store locations
routes
2 Products

Table with:

product info
edit/delete
3 Demand

Charts:

daily demand forecast
seasonality visualization
4 Optimization Results

Tables:

supplier order schedule
warehouse inventory
store inventory
delivery schedule
vehicle usage
5 Cost Analysis

Charts:

transport cost
lost margin
total cost
Performance Requirements

The model should run within:

< 10 seconds

for:

160 products
10 stores
30 days
Expected Output

The dashboard should display:

Optimal supplier order schedule
Optimal delivery schedule
Inventory levels over time
Vehicle utilization
Cost breakdown
Service level achieved
Implementation Instructions

Create Shiny UI layout

Add CSV upload and product table

Add map input using Leaflet

Implement demand generation function

Implement optimization model using ompr or lpSolve

Create reactive outputs for results

Display tables and charts

Add optimization trigger button

Output Format

Provide the full working Shiny app code in a single file:

app.R