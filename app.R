# app.R
# Modern Top Header + Tabs Below — with new histogram color and line-only trends

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)
library(forcats)

# ---- Load Data ----
data_path <- "sampled_data.csv"
df <- read.csv(data_path, stringsAsFactors = FALSE)

df <- df %>%
  rename_all(~str_trim(.)) %>%
  mutate(
    furnishing = ifelse(is.na(furnishing) | furnishing == "", "Unknown", furnishing),
    property_type = ifelse(is.na(property_type) | property_type == "", "Unknown", property_type),
    location = ifelse(is.na(location) | location == "", "Unknown", location),
    price = as.numeric(price),
    size_sqft = as.numeric(size_sqft),
    price_per_sqft = ifelse(size_sqft > 0, price / size_sqft, NA_real_),
    log_price = ifelse(price > 0, log10(price), NA_real_)
  )

df_amen <- df %>%
  mutate(amenities_clean = amenities %>%
           str_replace_all("\\[|\\]|\\'|\\\"", "") %>%
           str_trim()) %>%
  mutate(amenities_clean = ifelse(amenities_clean == "", NA, amenities_clean)) %>%
  separate_rows(amenities_clean, sep = ",\\s*") %>%
  mutate(amenities_clean = str_trim(amenities_clean)) %>%
  filter(!is.na(amenities_clean) & amenities_clean != "")

# ---- Theme ----
modern_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#004b8d",
  secondary = "#17a2b8",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins")
)

# ---- UI ----
ui <- fluidPage(
  theme = modern_theme,
  tags$style(HTML("
    body { background-color: #f8fafc; }
    .header-bar {
      background: linear-gradient(135deg, #004b8d, #007bff);
      color: white;
      text-align: center;
      padding: 25px 10px 20px 10px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    }
    .header-title {
      font-size: 34px;
      font-weight: 700;
      letter-spacing: 0.3px;
    }
    .header-sub {
      font-size: 16px;
      color: #e0e0e0;
      margin-top: 4px;
    }
    .nav-tabs {
      margin-top: 0;
      border-bottom: 3px solid #004b8d !important;
      background-color: #ffffff;
      font-weight: 500;
      font-size: 15px;
    }
    .nav-tabs > li > a {
      color: #004b8d !important;
      border: none !important;
      padding: 12px 20px;
    }
    .nav-tabs > li.active > a {
      background: linear-gradient(90deg, #004b8d, #17a2b8) !important;
      color: #fff !important;
      border-radius: 0;
    }
    .tab-content {
      margin-top: 20px;
    }
  ")),
  
  # ---- HEADER BAR ----
  div(class = "header-bar",
      div(class = "header-title", "Larsen & Toubro Realty Housing Dataset"),
      div(class = "header-sub", "An Interactive Dashboard for Exploring Pricing, Furnishing, and Property Insights")
  ),
  
  # ---- TABS BELOW HEADER ----
  tabsetPanel(
    id = "tabs",
    type = "tabs",
    
    # ---------------- INTRO ----------------
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      wellPanel(
                        h4("🌇 Introduction", style = "color:#004b8d; font-weight:700;"),
                        p("The Visual Analytics of Larsen & Toubro Realty Dataset is more than just a data dashboard — it’s a window into the dynamics of modern urban housing. It transforms structured property data into meaningful visuals that highlight relationships between pricing, furnishing levels, and urban accessibility. By combining analytical depth with an intuitive interface, the dashboard empowers users to identify pricing patterns, furnishing preferences, and locality-driven market variations — all through interactive, data-driven storytelling.",
                          style = "font-size:15px; color:#333333; line-height:1.6;")
                      )
               )
             ),
             br(),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("📊 Data Description", style = "color:#004b8d; font-weight:700;"),
                        HTML("<p style='font-size:15px; color:#333333; line-height:1.6;'>
              The dataset encapsulates detailed information about residential properties developed or listed under Larsen & Toubro Realty. 
              It includes property attributes such as price, size (in square feet), furnishing status, number of bedrooms and bathrooms, 
              available amenities, and distance from the nearest metro station. Each observation represents a single property listing — 
              reflecting the diversity of housing options across regions and price segments. The dataset serves as a foundation for exploring 
              how lifestyle features, spatial accessibility, and furnishing levels collectively shape the real estate value spectrum.
              </p>
              <p style='font-size:15px; color:#333333;'>
              🔗 <b>Original Dataset:</b> 
              <a class='dataset-link' href='https://www.kaggle.com/datasets/pixelphantom12/larsen-and-toubro-realty-housing-dataset' target='_blank'>
              Kaggle – Larsen & Toubro Realty Housing Dataset</a><br>
              📁 <b>Sampled Subset (5,000 records):</b> 
              <a class='dataset-link' href='https://drive.google.com/file/d/1vL7yx2PSSIikejOMCMU-mRyylUX4RiFX/view?usp=drive_link' target='_blank'>
              Google Drive – Sampled Dataset (5,000 properties)</a>
              </p>")
                      )
               )
             ),
             br(),
             fluidRow(column(12, wellPanel(
               h3("🙏 Acknowledgement", style = "color:#004b8d; font-weight:700;"),
               p("This project was completed under the valuable guidance of ",
                 tags$b("Professor Sourish Das"), 
                 " and with insightful contributions from ",
                 tags$b("Anish Rai"), ". Their constant support and feedback played a key role in shaping the analytical and visual framework of this dashboard.",
                 style = "font-size:15px; color:#333333; line-height:1.6;")
             ))
             )
    ),
    
    # ---------------- OVERVIEW ----------------
    tabPanel("Overview",
             fluidRow(
               column(4, wellPanel(
                 h4("Average Price (₹)"),
                 h3(format(round(mean(df$price, na.rm = TRUE)), big.mark = ","), style = "color:#004b8d; font-weight:600;")
               )),
               column(4, wellPanel(
                 h4("Average Size (sqft)"),
                 h3(round(mean(df$size_sqft, na.rm = TRUE), 1), style = "color:#17a2b8; font-weight:600;")
               )),
               column(4, wellPanel(
                 h4("Total Listings"),
                 h3(nrow(df), style = "color:#28a745; font-weight:600;")
               ))
             ),
             fluidRow(
               column(6, plotlyOutput("hist_log_price", height = "350px")),
               column(6,
                      selectInput("box_prop_filter", "Property Type:", choices = c("All", sort(unique(df$property_type))), selected = "All"),
                      plotlyOutput("box_price_psq", height = "350px"))
             )
    ),
    
    # ---------------- FURNISHING ----------------
    tabPanel("Furnishing & Types",
             fluidRow(
               column(6,
                      selectInput("bar_prop_type", "Select property type:", choices = sort(unique(df$property_type))),
                      plotlyOutput("bar_furn_counts", height = "350px")),
               column(6,
                      selectInput("dens_size_prop", "Select property type:", choices = sort(unique(df$property_type))),
                      plotOutput("density_size_separate", height = "400px"))
             )
    ),
    
    # ---------------- BEDROOMS / BATHROOMS ----------------
    tabPanel("Bedrooms / Bathrooms",
             fluidRow(
               column(12,
                      selectInput("bed_prop", "Select property type:", choices = sort(unique(df$property_type))),
                      fluidRow(
                        column(4, plotlyOutput("donut_bed_unf", height = "300px")),
                        column(4, plotlyOutput("donut_bed_semi", height = "300px")),
                        column(4, plotlyOutput("donut_bed_full", height = "300px"))
                      )
               )
             ),
             fluidRow(
               column(12,
                      selectInput("bath_prop", "Select property type:", choices = sort(unique(df$property_type))),
                      fluidRow(
                        column(4, plotlyOutput("donut_bath_unf", height = "300px")),
                        column(4, plotlyOutput("donut_bath_semi", height = "300px")),
                        column(4, plotlyOutput("donut_bath_full", height = "300px"))
                      )
               )
             )
    ),
    
    # ---------------- SIZE & AMENITIES ----------------
    tabPanel("Size & Amenities",
             fluidRow(
               column(12,
                      selectInput("amen_prop", "Select property type:", choices = sort(unique(df$property_type))),
                      plotlyOutput("donut_amenities", height = "400px"))
             )
    ),
    
    # ---------------- COMPARATIVE TRENDS ----------------
    tabPanel("Comparative Trends",
             fluidRow(
               column(6,
                      selectInput("trend_locations", "Select locations:", choices = sort(unique(df$location)), selected = unique(df$location)[1:5], multiple = TRUE),
                      plotlyOutput("line_median_price", height = "350px")),
               column(6,
                      selectInput("trend_locations2", "Select locations:", choices = sort(unique(df$location)), selected = unique(df$location)[1:5], multiple = TRUE),
                      plotlyOutput("line_median_dist", height = "350px"))
             )
    ),
    
    # ---------------- CONCLUSION ----------------
    tabPanel("Results",
             fluidRow(
               column(12, wellPanel(
                 h3("📈 Results & Insights", style = "color:#004b8d; font-weight:700;"),
                 tags$ul(
                   tags$li("Fully furnished properties generally exhibit the highest price per square foot across all property types."),
                   tags$li("Apartments closer to metro stations tend to have higher market values, indicating strong urban accessibility influence."),
                   tags$li("The majority of listings are semi-furnished, suggesting mid-range affordability and balanced buyer preference."),
                   tags$li("Amenities such as gyms, swimming pools, and gardens are more frequently associated with premium-priced apartments."),
                   tags$li("Median property prices vary notably across locations, with certain high-demand zones showing clear upward trends.")
                 )
               ))),
             br(),
             # --- About the Creator ---
             fluidRow(column(12, wellPanel(
               h3("👩‍💻 About the Creator", style = "color:#17a2b8; font-weight:700;"),
               p("This dashboard was designed and developed by ",
                 tags$b("Swikriti Paul"), 
                 ", an Msc. data science student with a focus on visual storytelling and quantitative insight generation. 
    The project was created as part of an academic initiative in data visualization and applied analytics, 
    showcasing the intersection of real estate data and interactive exploration.",
                 style = "font-size:15px; color:#333333; line-height:1.6;"),
               tags$hr(style = "border-top:1px solid #17a2b8; width:80%; margin:15px auto;"),
               p("📧 ", tags$b("Email:"), " paulswikriti2004@gmail.com",
                 br(),
                 "🏫 ", tags$b("Affiliation:"), " Chennai Mathematical Institute (CMI)",
                 br(),
                 "📍 ", tags$b("Location:"), " Chennai, India",
                 style = "font-size:14px; color:#333333; line-height:1.6; text-align:center;")
             )))
             
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # 1️⃣ New Color for Histogram
  output$hist_log_price <- renderPlotly({
    p <- ggplot(df, aes(x = log_price)) +
      geom_histogram(bins = 40, fill = "#17a2b8", color = "white", alpha = 0.85) +  # Changed color here
      theme_minimal() +
      labs(x = "log10(Price)", y = "Count", title = "Distribution of Property Prices (log10 Scale)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#004b8d"))
    ggplotly(p)
  })
  
  # Boxplot
  output$box_price_psq <- renderPlotly({
    tmp <- df
    if (input$box_prop_filter != "All") tmp <- tmp %>% filter(property_type == input$box_prop_filter)
    p <- ggplot(tmp, aes(x = furnishing, y = price_per_sqft, fill = furnishing)) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Furnishing", y = "Price per sqft (₹)")
    ggplotly(p)
  })
  
  # Furnishing Bar
  output$bar_furn_counts <- renderPlotly({
    sel <- input$bar_prop_type
    d <- df %>% filter(property_type == sel) %>% group_by(furnishing) %>% summarise(n = n())
    ggplotly(ggplot(d, aes(furnishing, n, fill = furnishing)) + geom_col() + coord_flip() + theme_minimal())
  })
  
  # Density Plots
  output$density_size_separate <- renderPlot({
    tmp <- df %>% filter(property_type == input$dens_size_prop)
    ggplot(tmp, aes(size_sqft, fill = furnishing)) + geom_density(alpha = 0.6) +
      facet_wrap(~furnishing, scales = "free_y") + theme_minimal()
  })
  
  # Donuts
  # Donuts (ordered labels)
  make_donut <- function(tmp, var, furn) {
    t <- tmp %>% filter(furnishing == furn)
    agg <- t %>%
      group_by(!!sym(var)) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(!!sym(var) := as.numeric(!!sym(var))) %>%           # Convert to numeric
      arrange(!!sym(var))                                        # Order by number
    
    if (nrow(agg) == 0) return(NULL)
    
    plot_ly(agg, labels = ~as.factor(get(var)), values = ~n, 
            type = "pie", hole = 0.45,
            sort = FALSE) %>%                                    # Prevent auto-sorting by Plotly
      layout(title = furn)
  }
  
  output$donut_bed_unf <- renderPlotly({ make_donut(df %>% filter(property_type == input$bed_prop), "num_bedroom", "Unfurnished") })
  output$donut_bed_semi <- renderPlotly({ make_donut(df %>% filter(property_type == input$bed_prop), "num_bedroom", "Semi Furnished") })
  output$donut_bed_full <- renderPlotly({ make_donut(df %>% filter(property_type == input$bed_prop), "num_bedroom", "Fully Furnished") })
  output$donut_bath_unf <- renderPlotly({ make_donut(df %>% filter(property_type == input$bath_prop), "num_bathroom", "Unfurnished") })
  output$donut_bath_semi <- renderPlotly({ make_donut(df %>% filter(property_type == input$bath_prop), "num_bathroom", "Semi Furnished") })
  output$donut_bath_full <- renderPlotly({ make_donut(df %>% filter(property_type == input$bath_prop), "num_bathroom", "Fully Furnished") })
  
  # Amenities
  output$donut_amenities <- renderPlotly({
    tmp <- df_amen %>% filter(property_type == input$amen_prop)
    agg <- tmp %>% group_by(amenities_clean) %>% summarise(n = n())
    plot_ly(agg, labels = ~amenities_clean, values = ~n, type = "pie", hole = 0.5)
  })
  
  # 2️⃣ Comparative Trends (Line Graphs only)
  output$line_median_price <- renderPlotly({
    locs <- input$trend_locations
    tmp <- df %>% filter(location %in% locs) %>%
      group_by(location, property_type) %>%
      summarise(med_price = median(price, na.rm = TRUE), .groups = "drop")
    p <- ggplot(tmp, aes(x = location, y = med_price, color = property_type, group = property_type)) +
      geom_line() + geom_point() +
      theme_minimal() +
      labs(x = "Location", y = "Median Price (₹)")
    ggplotly(p)
  })
  
  output$line_median_dist <- renderPlotly({
    locs <- input$trend_locations2
    tmp <- df %>% filter(location %in% locs) %>%
      group_by(location, property_type) %>%
      summarise(med_dist = median(distance_to_metro_km, na.rm = TRUE), .groups = "drop")
    p <- ggplot(tmp, aes(x = location, y = med_dist, color = property_type, group = property_type)) +
      geom_line() + geom_point() +
      theme_minimal() +
      labs(x = "Location", y = "Median Distance (km)")
    ggplotly(p)
  })
}

# ---- Run App ----
shinyApp(ui, server)
