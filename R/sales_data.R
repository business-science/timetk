#' Fictional sales data for bike shops purchasing Cannondale bikes
#'
#' A dataset containing the fictional bicycle orders spanning 2011 through 2015.
#' Hypothetically, the `bike_sales` data are similar to sales data mainatained
#' in a business' sales data base. The unit price and model names come from
#' data provided by model for the bicycle manufacturer, Cannondale (2016).
#' The customers (bicycle shops) including name, location, etc and
#' the orders including quantity purchased and order dates are fictional.
#' The data is intended for implementing business analytics techniques
#' (e.g. forecast, clustering, etc) to identify underlying trends.
#'
#' @format A data frame with 15644 rows and 17 variables:
#' \describe{
#'   \item{order.date}{Date the order was placed}
#'   \item{order.id}{A unique order identification number}
#'   \item{order.line}{The sequential identification number for products on and order}
#'   \item{quantity}{Number of units purchased}
#'   \item{price}{The unit price of the bicycle}
#'   \item{price.ext}{The extended price = price x quantity}
#'   \item{customer.id}{A unique customer identification number}
#'   \item{bikeshop.name}{The customer name}
#'   \item{bikeshop.city}{The city that the bike shop is located}
#'   \item{bikeshop.state}{The state that the bike shop is located}
#'   \item{latitude}{The geograhpic latitude of the customer location}
#'   \item{longitude}{The geograhpic longitude of the customer location}
#'   \item{product.id}{A unique product identification number}
#'   \item{model}{The model name of the bicycle}
#'   \item{category.primary}{The main bicycle category, either "Mountain" or "Road"}
#'   \item{category.secondary}{One of nine more specific bicycle categories}
#'   \item{frame}{The bicycle frame material, either "Carbon" or "Aluminum"}
#' }
#' @source
#' The 2016 bicycle model names and prices originated from \url{http://www.cannondale.com/en/USA}
"bike_sales"
