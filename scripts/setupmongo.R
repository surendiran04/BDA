# Install if not already installed
#install.packages("mongolite")

library(mongolite)

# ---- Load CSVs ----
#ratings <- read.csv("data/interactions_generated.csv", stringsAsFactors = FALSE)
items_data <- read.csv("data/walmart_dataset.csv", stringsAsFactors = FALSE)
items_data$Product_Rating <- as.numeric(items_data$Product_Rating)
#users_data <- read.csv("data/users.csv", stringsAsFactors = FALSE)

# ---- MongoDB Connection ----
# Replace with your MongoDB Atlas or local URI
mongo_url <- "mongodb://localhost:27017"

# Connect to each collection
#ratings_collection <- mongo(collection = "ratings", db = "ecommerce", url = mongo_url)
items_collection <- mongo(collection = "items_data", db = "ecommerce", url = mongo_url)
#users_collection <- mongo(collection = "users_data", db = "ecommerce", url = mongo_url)

# ---- Write Tables to MongoDB (overwrite) ----
#ratings_collection$drop()
items_collection$drop()
#users_collection$drop()

#ratings_collection$insert(ratings)
items_collection$insert(items_data)
#users_collection$insert(users_data)

# ---- Confirmation and Sample Preview ----
cat("✅ All datasets loaded into MongoDB database 'ecommerce'\n")

cat("📋 Preview from users_data collection:\n")
#print(users_collection$find(limit = 5))

#result <- users_collection$find('{"user_id": 200}')
#print(result)
