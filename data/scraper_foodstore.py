import time
import csv
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options

URL = "https://upfront.nl/collections/foodstore?sort_by=price-ascending"

# Setup headless browser
options = Options()
options.add_argument("--headless=new")
options.add_argument("--window-size=1920,1080")

driver = webdriver.Chrome(options=options)
driver.get(URL)

time.sleep(3)

# Scroll until all products load
last_height = driver.execute_script("return document.body.scrollHeight")

while True:
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
    time.sleep(2)

    new_height = driver.execute_script("return document.body.scrollHeight")
    if new_height == last_height:
        break
    last_height = new_height

# Find all product cards
products = driver.find_elements(By.CSS_SELECTOR, "xo-product-card")

data = []

for p in products:
    try:
        name = p.find_element(By.CSS_SELECTOR, "h3").text.strip()
    except:
        name = None

    try:
        price = p.find_element(By.CSS_SELECTOR, ".xo-price__item").text.strip()
    except:
        price = None

    try:
        link = p.find_element(By.CSS_SELECTOR, "a.xo-product-card__heading").get_attribute("href")
    except:
        link = None

    try:
        img = p.find_element(By.CSS_SELECTOR, "img").get_attribute("src")
    except:
        img = None

    # attributes stored directly on the tag
    product_id = p.get_attribute("xo-product-id")
    handle = p.get_attribute("xo-product-handle")
    vendor = p.get_attribute("xo-product-vendor")

    data.append({
        "name": name,
        "price": price,
        "link": link,
        "image_url": img,
        "vendor": vendor,
        "handle": handle,
        "product_id": product_id
    })

driver.quit()

# Save CSV
keys = data[0].keys()

with open("upfront_products.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=keys)
    writer.writeheader()
    writer.writerows(data)

print(f"Saved {len(data)} products to upfront_products.csv")