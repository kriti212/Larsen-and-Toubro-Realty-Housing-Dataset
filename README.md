# Interactive Housing Data Dashboard

This project contains an interactive dashboard built using the Larsen & Toubro Realty Housing Dataset. The dashboard allows users to explore various housing attributes and understand how factors such as location, property type, amenities, furnishing level, and metro accessibility influence pricing. It provides a visual and data-driven view of real estate patterns across major Indian cities.

## Live Dashboard

[https://kriti02.shinyapps.io/app1/](https://kriti02.shinyapps.io/app1/)

## Overview

The dashboard includes visualizations for:

* Distribution of property types and furnishing levels
* Relationships between size, bedroom count, and bathrooms
* Price and log-price distributions
* City-wise price variations
* Price per square foot
* Distance from metro stations and its impact on pricing

Users can apply filters and compare different categories to gain insights into affordability, property features, and market trends.

## Dataset

The dashboard uses a subset of the L&T Realty dataset for performance.
It includes variables such as:

* Property type
* Size (sqft)
* Bedrooms and bathrooms
* Furnishing
* Amenities
* Location and metro distance
* Price and price per square foot

## Technical Setup

### Clone the repository

```
git clone https://github.com/your-username/your-repo-name.git
cd your-repo-name
```

### Install required R packages

```
install.packages(c("shiny", "tidyverse", "ggplot2", "shinythemes"))
```

### Run the dashboard locally

```
library(shiny)
runApp("app")
```

### Deploying to shinyapps.io

```
library(rsconnect)
rsconnect::setAccountInfo(name="yourname",
                          token="your_token",
                          secret="your_secret")
deployApp("path_to_your_app")
```


