import time
import csv
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options

URL = "https://upfront.nl/collections/alles?layouts=sm"

# Setup browser
options = Options()
options.add_argument("--headless=new")
options.add_argument("--window-size=1920,1080")

driver = webdriver.Chrome(options=options)
driver.get(URL)

time.sleep(3)

# Scroll to load all products
last_height = driver.execute_script("return document.body.scrollHeight")

while True:
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
    time.sleep(2)

    new_height = driver.execute_script("return document.body.scrollHeight")
    if new_height == last_height:
        break

    last_height = new_height

# Get all product cards
products = driver.find_elements(By.CSS_SELECTOR, "xo-product-card")

data = []

# helper to remove euro sign and trim whitespace
def clean_price(value):
    if value is None:
        return None
    return value.replace('€', '').strip()

for p in products:

    product_id = p.get_attribute("xo-product-id")
    handle = p.get_attribute("xo-product-handle")
    vendor = p.get_attribute("xo-product-vendor")
    raw_price = p.get_attribute("xo-price")
    image_attr = p.get_attribute("xo-product-image")

    try:
        name = p.find_element(By.CSS_SELECTOR, "h3").text.strip()
    except:
        name = None

    try:
        price = p.find_element(By.CSS_SELECTOR, ".xo-price__item").text.strip()
    except:
        price = raw_price

    price = clean_price(price)

    try:
        link = p.find_element(By.CSS_SELECTOR, "a.xo-product-card__heading").get_attribute("href")
    except:
        link = f"https://upfront.nl/products/{handle}"

    try:
        image = p.find_element(By.CSS_SELECTOR, "img").get_attribute("src")
    except:
        image = f"https:{image_attr}" if image_attr else None

    try:
        rating = p.find_element(By.CSS_SELECTOR, ".star-rating-2__text").text
    except:
        rating = None

    data.append({
        "product_id": product_id,
        "name": name,
        "price": price,
        "vendor": vendor,
        "handle": handle,
        "product_url": link,
        "image_url": image,
        "rating": rating
    })

driver.quit()

# Save to CSV
with open("upfront_online_store_products.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=data[0].keys(), delimiter=';')
    writer.writeheader()
    writer.writerows(data)

print(f"Saved {len(data)} products")