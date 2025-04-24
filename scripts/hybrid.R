library(recommenderlab)
library(tm)
library(dplyr)
library(proxy)

hybrid <- function(ratings, items_data,user_input, search_term, reco_num) {

    sink("output.txt", append = TRUE)

    ##NEW USER INPUT

    if (!(user_input %in% ratings$user_id)) {
        cat("\nNew user detected. Returning content-based recommendations.\n")
    
        # Fallback to content-based filtering
        item_metadata <- items_data
        item_metadata$text_data <- paste(item_metadata$Product_Name, item_metadata$Product_Tags, item_metadata$Product_Description, 
                                      item_metadata$Product_Brand, item_metadata$Product_Category)
    
        # Filter items based on the search term
        product_input <- tolower(search_term)
        filtered_items <- item_metadata %>%
        filter(grepl(product_input, tolower(Product_Tags)))
    
        # Return top N filtered items
        print(head(filtered_items, reco_num))
        return(head(filtered_items, reco_num))
    }

    # =========================
    # COLLABORATIVE FILTERING
    # =========================

    # Clean NA-heavy columns
    ratings <- ratings[, colSums(is.na(ratings)) < nrow(ratings) * 0.5]

    ratings <- ratings %>% mutate(rating = ifelse(is.na(rating), 0, rating))
    # Select necessary columns
    rating_data <- ratings %>% select(user_id, product_id, rating)

    # Create utility matrix
    utility_matrix <- as(rating_data, "realRatingMatrix")

    # Build model
    ibcf_model <- Recommender(utility_matrix, method = "IBCF")

    # Predict top n products
    predicted_ibcf <- predict(ibcf_model, utility_matrix[user_input, ], n = reco_num)
    predicted <- as(predicted_ibcf, "list")
    #print(predicted)

    # Map product IDs to names
    product_mapping <- setNames(items_data$Product_Name, items_data$Product_Id)

    # Show collaborative filtering recommendations
    user_products <- predicted[[1]]
    user_product_names <- product_mapping[user_products]
    #cat("\nCollaborative Filtering Recommendations:\n")
    #print(user_product_names)


    # =========================
    # CONTENT-BASED FILTERING
    # =========================

    cbf_scores <- NULL

    item_metadata <- items_data
    item_metadata$text_data <- paste(item_metadata$Product_Name, item_metadata$Product_Tags, item_metadata$Product_Description, 
                                item_metadata$Product_Brand, item_metadata$Product_Category)
    #print(item_metadata$text_data[2])

    item_corpus <- VCorpus(VectorSource(item_metadata$text_data))
    item_corpus <- tm_map(item_corpus, content_transformer(tolower))
    item_corpus <- tm_map(item_corpus, removePunctuation)
    item_corpus <- tm_map(item_corpus, removeWords, stopwords("en"))
    item_corpus <- tm_map(item_corpus, stripWhitespace)

    #inspect(item_corpus[[1]])  

    tdm <- TermDocumentMatrix(item_corpus)
    tdm_matrix <- as.matrix(tdm)
    cat("\nTerm Document Matrix:\n")
    print(head(tdm_matrix[1:10, 1:10]))

    product_input <- tolower(search_term)

    liked_items <- which(as(utility_matrix[user_input, ], "matrix") > 3)
    liked_item_ids <- items_data$Product_Id[liked_items]


    # Filter items based on input keyword
    filtered_items <- item_metadata %>%
        filter(grepl(product_input, tolower(Product_Tags)))
    filtered_ids <- filtered_items$Product_Id
    #print(filtered_ids)

    valid_filtered_ids <- intersect(filtered_ids, colnames(utility_matrix))
    valid_liked_ids <- intersect(liked_item_ids, colnames(utility_matrix))

    #print(length(valid_liked_ids))

    #print(valid_filtered_ids)

    # If we have valid filtered and liked items, calculate item similarity between them
    if (length(valid_filtered_ids) > 0 && length(valid_liked_ids) > 0) {

        all_req_items <- union(valid_liked_ids, valid_filtered_ids)
        all_req_items <- intersect(all_req_items, colnames(utility_matrix))

        #print(all_req_items)

        sub_matrix <- utility_matrix[, all_req_items, drop = FALSE]
        sub_matrix <- as(sub_matrix,"matrix")
        sub_matrix_t <- t(sub_matrix)  

        #print("hi")
        item_similarity <- proxy::simil(sub_matrix_t, method = "cosine")
        #print("hi")
        item_similarity <- as.matrix(item_similarity)

        #print(dim(item_similarity))

        cat("\nItem Similarity Matrix based on content filtering:\n")
        print(head(item_similarity[1:10, 1:10]))

    
        # Map product IDs to numeric row indices in item_similarity
        valid_filtered_indices <- match(valid_filtered_ids, rownames(item_similarity))
        valid_liked_indices <- match(valid_liked_ids, rownames(item_similarity))

        #print(valid_filtered_indices)
        #print("-----")
        #print(valid_liked_indices)

        # Now you can subset the item similarity matrix correctly
        similarity_scores <- item_similarity[valid_filtered_indices, valid_liked_indices, drop = FALSE]
        similarity_scores <- as(similarity_scores,"matrix")
        #print(head(similarity_scores))

        similarity_scores[is.na(similarity_scores)] <- 0
        # Compute content-based filtering scores
        cbf_scores <- rowMeans(similarity_scores)

        cat("\nContent-Based Filtering Scores:\n")
        print(cbf_scores[1:10])
    
        # Sort the scores to get the top N recommendations
        top_n <- reco_num
        top_scores <- sort(cbf_scores, decreasing = TRUE)

        # Print the top N scores
        #print(head(top_scores, top_n))

        cbf_recommendations <- names(head(sort(cbf_scores, decreasing = TRUE), top_n))
    
        # Map the recommendation ids back to product names
        recommended_names <- product_mapping[cbf_recommendations]

        #cat("\nContent-Based Recommendations (filtered by keyword):\n")
        #print(recommended_names)
    }

    # =========================
    # HYBRID
    # =========================

    # Get CF predicted ratings for all items
    cf_prediction <- predict(ibcf_model, utility_matrix[user_input, ], type = "ratings")
    cf_scores <- as(cf_prediction, "matrix")[1, ]

    # Normalize both score vectors (0 to 1 scale)
    normalize <- function(x) {
        x <- as.numeric(x)
        if (all(is.na(x))) return(rep(0, length(x)))  # All NA check
        if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
            return(rep(0, length(x)))
        }
        return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    }


    # Normalize both score vectors
    norm_cf_scores <- normalize(cf_scores)
    names(norm_cf_scores) <- names(cf_scores)

    norm_cbf_scores <- normalize(cbf_scores)
    names(norm_cbf_scores) <- names(cbf_scores)

    # Common items in both models   
    common_items <- intersect(names(norm_cf_scores), names(norm_cbf_scores))

    if (length(common_items) > 0) {
        hybrid_scores <- 0.5 * norm_cf_scores[common_items] + 0.5 * norm_cbf_scores[common_items]
    
        top_n <- reco_num
        top_hybrid <- names(head(sort(hybrid_scores, decreasing = TRUE), top_n))
    
        hybrid_recommendations <- product_mapping[top_hybrid]
        #print(is.vector(hybrid_recommendations))
        cat("\nHybrid Recommendations (Combined CF + CBF):\n")
        print(hybrid_recommendations)

        hybrid_recommendations <- data.frame(Product_Name = hybrid_recommendations)
        sink()
        return(hybrid_recommendations)
    } else {
        cat("\nHybrid Recommendation not available (no common items in CF and CBF).\n")
        sink()
        return(NULL)
    }
}
