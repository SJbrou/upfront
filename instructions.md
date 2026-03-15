# Assignment
You are a professional supply chain analyst in the grocery retail sector in the netherlands. Your task is to set up a MVP of an online tool where the optimal import to warehouse, and transportation to shops are calculated for the upcomming month. The tool should automatically determine the optimal way to start (fill all shops to 100%, fill warehouse to 80%, based on demand), and then calculate the optimal orders to be placed with suppliers, and the optimal transportation from the warehouse to the shops. The tool should be able to handle a large number of products (between 16 and 160) and between 1 and 10 shops. The tool should be able to optimize the inventory levels and orders based on the demand for each product, the lead time for each product, the storage capacity of the warehouse and shops, and the transportation costs. The tool should also be able to handle different types of products, such as refrigerated and frozen products, and take into account their specific storage and transportation requirements. The tool should be user-friendly and allow the user to easily input data and view the results. Time granularity should be daily. 

The tool should handle at least the following:
- logistics locations
Assume there is going to be one central warehouse and between 1-10 shops. The tool should use a (google) maps where the user can pin the locations of the single warehouse and the shops. The tool should then calculate the distances & travel times between the warehouse and the shops, and use this information to optimize the inventory levels and orders. The operating costs should be minimized, while assuming a certain service level, which is 99% OTIN by default, but should be editable. For each store, there is a store_max_volume
store_max_weight, store_max_refrigerated_volume, store_max_refrigerated_weight, store_max_frozen_volume, store_max_frozen_weight. For the warehouse, there is a warehouse_max_volume, warehouse_max_weight, warehouse_max_refrigerated_volume, warehouse_max_refrigerated_weight, warehouse_max_frozen_volume, warehouse_max_frozen_weight.

For each shop, assume it has the same inventory and storage space

- products
There are going to be 16-160 products in the foodstore. product information can be found in store_products.csv. The final online store should have a option to add, edit or remove products. The tool should also have an option to import product information from a csv file. 
in the csv file are the following columns
- name --> product name
- price --> price per unit
- link --> link to the product page on the website
- image_url --> link to the product image
- vendor --> the supplier of the product
- handle --> the unique identifier of the product on the website
- product_id --> the unique identifier of the product in the database
- shelf_life --> the number of days until the product expires
- gekoeld --> whether the product needs to be stored in a refrigerated environment (1 for yes, 0 for no)
- bevroren --> whether the product needs to be stored in a frozen environment (1 for yes, 0 for no)
- weight (gr) --> the weight of the product in grams
- volume (l) --> the volume of the product in liters
- inkoop --> the cost price of the product per unit (use this to calculate margin)
- import_order_size --> minimum order quantity for the product when ordering from the supplier 
- import_delivery_time --> the number of days it takes for the supplier to deliver the product after an order is placed
note: csv has ";" as delimiter, "," as decimal separator. 


- Actual demand
the tool should have an option to input demand for each product. For now, assume that the demand for each product is the same in all shops, except for a multiplier that can be set for each shop. For example, if the demand for a product is 100 units per week, and the multiplier for shop A is 1.5, then the demand for that product in shop A is 150 units per week. Per item, allow the user to fill in the formula for the demand for that product. Allow a term for base demand, a term for weekly seasonality, a term for monthly seasonality, and a random error term (you decide on what formula is best used in demand forecasting, the random error term should be the maximum random deviation that can occur in the demand, for example 20% of the base demand).

- Logistics, storage and transportation
For storage and transportation, assume only weight, volume, refrigeration and freezing requirements are relevant. Assume each store to have the same storage capacity (default and toggelable). Same for the central warehouse. Adittionally, assume that transportation can be done by a different means which should be editable. Assume a fleet of 1 standard truck and a 1 smaller van, with different costs and capacities. For each vehicle, there should be a cost per kilometer, speed, a fixed cost per trip, a maximum capacity in terms of weight and volume, option for refrigerated and frozen transport (toggelable). Prefill the tool with industry standards, but allow the user to edit these values and add new transportation means, or edit the number of a certain truck. Also, per truck, there should be a maximum distance it can travel each day. Calculate the maximum number of trips a truck can make in a day based on the distance between the warehouse and the shops, and the travel time. Assume that transportation can be done 16 hours per day, and the transportation is thus limited by 16h * speed per truck.

# simplifications
For now, we simplify the following:
- we ignore lead time variability
- we ignore loading and unloading times
- we ignore order processng times
- we ignore holding costs
- we allow items to be out of stock, loss is in that case equal to the margin lost on that item multiplied by the demand for that item. This is used in the optimization process, where the tool will try to minimize the total cost, but must adhere to the 99% OTIN service level, which means that the total lost margin due to out of stock should be less than 1% of the total margin (in number of producs!)
- assume no supplier transportation costs