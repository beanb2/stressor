#' Cluster-based partitions for cross validation
#'
#' This function creates cluster-based partitions of a sample space based on
#'   k-means clustering. Included in the function are algorithms that attempt
#'   to produce clusters of roughly equal size.
#' @param features A scaled matrix of features to be used in the clustering.
#'   Scaling usually done with [base::scale()] and should not include the
#'   predictor variable.
#' @param k The number of partitions for k-fold cross validation.
#' @param k_mult k*k_mult determines the number of subgroups that will be
#'   created as part of the balancing algorithm.
#' @param ... Additional arguments passed to [stats::kmeans()] as needed.
#' @export
cluster_part <- function(features, k, k_mult = 5, ...){

  if(k_mult %% 1 != 0 || k %% 1 != 0){
    stop("k and k_mult must be integers")
  }

  if(k_mult < 2){
    stop("k_mult must be 2 or greater (and preferably at least 5")
  }

  n <- nrow(features)

  # Article on spatial cross validation using k-means clustering:
  # - https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=6352393&casa_token=iEKQT6SqZNoAAAAA:YpAKRz2-9gAOjkz0AX62VpXuypSK3MtOontxm0-lQJZADRhyahxlxwFt9_sdwHl-T6P-Jw1RFA&tag=1
  # Book on spatial cross validation
  # - https://geocompr.robinlovelace.net/
  kmeans_results <- kmeans(features, centers = k*k_mult, ...)

  # Extract the important stuff and eliminate kmeans vector
  features_cut <- kmeans_results$cluster
  features_sum <- kmeans_results$size
  features_order <- order(features_sum, decreasing = TRUE)
  features_center <- kmeans_results$centers
  remove(kmeans_results)

  # stores new group memberships
  member_assign <- vector("integer", k*k_mult)

  # keeps track of group counts
  kvec <- vector("integer", k)

  # generates sample vector for group assignment
  ksamp <- seq_len(k)

  # determines how big groups "should" be
  klimit <- ceiling(sum(features_sum)/k)

  # Iterate from largest to smallest group size
  for(i in features_order){
    # Randomly assign the smaller groups to one of k larger groups.
    tsamp <- sample(ksamp, 1)

    # If there is room in the group, make the assignment and increase the
    # cumulative sample limit.
    no_find <- TRUE
    for(j in seq_len(k)){
      if(kvec[tsamp] + features_sum[i] < klimit){
        member_assign[i] <- tsamp
        kvec[tsamp] <- kvec[tsamp] + features_sum[i]
        no_find <- FALSE
        break
      }else{
        # If there is no room, then try the subsequent groups (with wrap).
        tsamp <- (tsamp %% k) + 1
      }
    }

    # If all put us over the edge, randomly fill one and remove
    # from conversation.
    if(no_find){
      tsamp <- sample(ksamp, 1)
      member_assign[i] <- tsamp
      kvec[tsamp] <- kvec[tsamp] + features_sum[i]
      ksamp <- ksamp[ksamp != tsamp]
    }

  }

  member_assign[features_cut]
}
