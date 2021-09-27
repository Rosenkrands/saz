#' @import tidyverse ggvoronoi
#' @importFrom magrittr "%>%"
library(tidyverse)
library(ggvoronoi)

euclid_norm <- function(x) sqrt(sum(x^2))

#' Generate 2d instance
#'
#' @export
generate_2d_instance <- function(
  no_of_points = 50,
  interval = c("min" = -10, "max" = 10)
) {
  id <- 1:no_of_points
  x <- runif(no_of_points, min = interval["min"], max = interval["max"])
  y <- runif(no_of_points, min = interval["min"], max = interval["max"])
  arrival_rate <- round(runif(no_of_points, min = 1, max = 3))
  data <- tibble::tibble(
    "Demand point id" = id,
    "x" = x,
    "y" = y,
    "Arrival rate" = arrival_rate
  )
  results <- list("data" = data, "interval" = interval)
  return(results)
}

#' @export
solve_centroid <- function(instance, no_of_centers = 4) {
  # place centroid randomly
  id <- 1:no_of_centers
  x <- runif(
    no_of_centers,
    min = instance$interval["min"],
    max = instance$interval["max"]
  )
  y <- runif(
    no_of_centers,
    min = instance$interval["min"],
    max = instance$interval["max"]
  )
  centers <- tibble::tibble("Center id" = id, "x" = x, "y" = y)

  # for each demand point find the closest center
  no_of_rows <- nrow(centers)*nrow(instance$data)
  center_point <- integer(no_of_rows)
  demand_point <- integer(no_of_rows)
  distance_to_center <- double(no_of_rows)
  iter <- 0
  for (i in 1:nrow(centers)) {
    for (j in 1:nrow(instance$data)) {
      iter <- iter + 1
      center_point[iter] <- dplyr::pull(centers[i, "Center id"])
      demand_point[iter] <- dplyr::pull(instance$data[j, "Demand point id"])
      distance_to_center[iter] <- euclid_norm(
        c(
          dplyr::pull(instance$data[j, "x"] - centers[i, "x"]),
          dplyr::pull(instance$data[j, "y"] - centers[i, "y"])
        )
      )
    }
  }
  distances <- tibble::tibble(
    "Center id" = center_point,
    "Demand point id" = demand_point,
    "Distance to center" = distance_to_center
  )
  assignment <- distances %>%
    dplyr::group_by(`Demand point id`) %>%
    dplyr::filter(`Distance to center` == min(`Distance to center`)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(centers, by="Center id") %>%
    dplyr::rename(x.center = x, y.center = y)
  objective <- instance$data %>%
    dplyr::select(`Demand point id`, `Arrival rate`) %>%
    dplyr::inner_join(
      dplyr::select(assignment, `Center id`, `Demand point id`),
      by = "Demand point id"
    ) %>%
    dplyr::group_by(`Center id`) %>%
    dplyr::summarise(`Arrival rate variance` = var(`Arrival rate`))
  return(list("assignment" = assignment, "objective" = objective))
}

#' @export
plot_2d_instance <- function(instance, closest) {
  vor_data <- closest$assignment %>%
    dplyr::select(`Center id`, x.center, y.center) %>%
    dplyr::distinct() %>%
    dplyr::mutate(`Center id` = as.character(`Center id`))

  ggplot2::ggplot(
    instance$data %>%
      dplyr::inner_join(closest$assignment, by="Demand point id") %>%
      dplyr::mutate(`Center id` = as.character(`Center id`),
             `Arrival rate` = as.character(`Arrival rate`))
  ) +
    ggvoronoi::geom_voronoi(
      data = vor_data,
      aes(x.center, y.center, fill = `Center id`),
      alpha = .25,
      # geom="path",
      outline = data.frame(
        x = 1.1*c(-10,-10,10,10),
        y = 1.1*c(-10,10,10,-10)
      )
    ) +
    ggvoronoi::stat_voronoi(
      data = vor_data,
      aes(x.center, y.center),
      geom="path",
      outline = data.frame(
        x = 1.1*c(-10,-10,10,10),
        y = 1.1*c(-10,10,10,-10)
      )
    ) +
    ggplot2::geom_point(aes(
      x, y,
      # label = `Arrival rate`,
      color = `Center id`,
      # shape = `Arrival rate`
    )) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "top")
    # scale_x_continuous(limits = instance$interval) +
    # scale_y_continuous(limits = instance$interval)
}

# instance = generate_2d_instance(
#   no_of_points = 100,
#   interval = c("min" = -10, "max" = 10)
# )
# closest = solve_centroid(instance, no_of_centers = 8)
# plot_2d_instance(instance, closest)
