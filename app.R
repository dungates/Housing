library(bslib)
library(DGThemes)
library(dplyr)
library(glue)
library(ggmap)
library(ggplot2)
library(ggpmisc)
library(gt)
library(leaflet)
library(scales)
library(thematic)
library(shiny)

Sys.setenv(http_proxy = "AIzaSyCkia4ftgZ_v_9IdowbWpgedgoq9vsKyM8")

housing_df <- readr::read_rds("Data/housing_full.rds")
map_df <- readr::read_rds("Data/housing_full_map.rds")

duncan_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#2A9FD6",
  secondary = "#3D3E3D",
  success = "#77B300",
  info = "#9933CC",
  warning = "#FF8800",
  danger = "#CC0000",
  base_font = "Open Sans",
  heading_font = "Fira Sans",
  code_font = "Chalkduster"
)

vars <- c(
    "Is SuperZIP?" = "superzip",
    "Centile score" = "centile",
    "College education" = "college",
    "Median income" = "income",
    "Population" = "adultpop"
)

ui <- navbarPage(
  title = h3("Housing Data"),
  tabPanel(
    title = "Home", icon = shiny::icon("home"),
    fluidRow(
      column(
        6,
        gt_output("house_table")
      ),
      column(
        6,
        p("Click on the map to see the corresponding information in the table", style = "text-align:center;color:#1f6632;font-size:x-large;padding-top:4px;padding-bottom:4px;font-weight:bold;font-family:'Fira Sans';"),
        leafletOutput("mapPlot", height = "800px"),
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
          width = 330, height = "auto",
          h2("Data explorer"),
          selectInput("beds", "Beds", c("1+" = 1, "2+" = 2, "3+" = 3, "4+" = 4, "5+" = 5)),
          sliderInput("price", "Price", 
                      min = min(housing_df$price, na.rm = T), max = max(housing_df$price, na.rm = T),
                      value = c(min(housing_df$price, na.rm = T), max = max(housing_df$price, na.rm = T)),
                      pre = "$")
        )
      )
    )
  ),
  tabPanel(
    title = "Analysis", icon = shiny::icon("chart-area"),
    tabsetPanel(
      tabPanel("Linear Regression",
               plotOutput("price_plot", height = "900px")),
      tabPanel("Mapping",
               plotOutput("loc_plot", height = "900px"))
    )
  ),
  tabPanel(
    title = "About", icon = shiny::icon("info"),
    uiOutput("about_text")
  ),
  theme = duncan_theme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$footer(
    actionLink("show_help_text", "Help"),
    span(" | "),
    actionLink("show_data_protection_policy", "Data protection policy"),
    span(" | "),
    actionLink("show_legal_notice", "© Duncan Gates, 2021"),
    align = "center",
    style = "bottom:0;
              right:0;
              left:0;
              background:transparent;
              color: white;
              padding:10px;
              box-sizing:border-box;
              text-align: center"
  ),
  includeCSS("www/styles.css")
)

server <- function(input, output) {
  thematic::thematic_shiny()

  
  map_reactive <- reactive({
    map_df %>%
      dplyr::filter(between(price, input$price[1], input$price[2])) %>%
      dplyr::filter(beds >= input$beds)
  })
  
  # MAP on right
  output$mapPlot <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "Detailed") %>%
      addProviderTiles("CartoDB.Positron", group = "Simple") %>%
      addAwesomeMarkers(
        lng = map_reactive()$longitude, lat = map_reactive()$latitude,
        popup = glue::glue("Address: {map_reactive()$address}<br> Price: ${map_reactive()$price}<br> Beds: {map_reactive()$beds}<br> Type: {map_reactive()$type}"),
        icon = awesomeIcons(icon = "home", markerColor = map_reactive()$color),
        group = "Subject Property"
      ) %>%
      addLegend("bottomright",
        labels = c("House for rent", "Apartment for rent", "Townhouse for rent"),
        colors = c("red", "blue", "green")
      )
  })
  
  # Observe event map clicker
  map_address <- reactive({
    if (is.null(input$mapPlot_marker_click)) {
      address = NA
    } else {
      click_id <- input$mapPlot_marker_click$lat
      print(click_id)
      data_id <- map_df %>% 
        dplyr::filter(latitude == click_id) %>%
        pull(address)
    }
  })

  # GT Table on left
  output$house_table <- render_gt(
    housing_df %>%
      dplyr::filter(between(price, input$price[1], input$price[2])) %>%
      dplyr::filter(beds >= input$beds) %>%
      select(-1) %>%
      mutate(is_selected = if_else(address == map_address(), T, F)) %>%
      # mutate(`Price per Month` = price / 3) %>%
      # relocate(`Price per Month`, .after = price) %>%
      # Move the selected variable to the top of dataframe
      arrange(desc(is_selected), price) %>%
      gt::gt() %>%
      tab_header(title = html("<b><span style='color:#1f6632'>Best Portland Rentals")) %>%
      cols_label(
        address = "Address",
        price = "Monthly Rent",
        beds = "Beds",
        baths = "Baths",
        house_area = "House Area",
        type = "Type",
        zillow_link = "Zillow Link",
        image = "Image"
      ) %>%
      data_color(
        columns = c(
          # `Price per Month`,
          `price`),
        colors = scales::col_numeric(c("#1B8366", "#57C478", "#FFDD0E", "#E9AE0B"), domain = NULL)
      ) %>%
      data_color(
        columns = house_area,
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(
            palette = "ggsci::red_material"
          ) %>% as.character(),
          domain = NULL
        )
      ) %>%
      # fmt_currency(columns = c(`Price per Month`)) %>%
      fmt_number(
        columns = c(price),
        pattern = "${x}",
        decimals = 0
      ) %>%
      fmt_number(
        columns = house_area,
        pattern = "{x} sq ft.",
        decimals = 0
      ) %>%
      gt::text_transform(
        locations = cells_body(columns = image),
        fn = function(x) {
          gt::web_image(
            url = x,
            height = 200
          )
        }
      ) %>%
      gt::text_transform(
        locations = cells_body(columns = type),
        fn = function(x) {
          str_remove_all(x, " for rent")
        }
      ) %>%
      gt::opt_table_font(
        font = list(
          gt::google_font("Open Sans"),
          gt::default_fonts()
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(
            size = "medium",
            align = "center"
          )
        ),
        locations = gt::cells_body(
          columns = !c(address),
          rows = everything()
        )
      ) %>%
      cols_align(align = "center") %>%
      tab_style(
        style = cell_borders(
          sides = c("right"),
          color = "white",
          weight = px(1.5),
          style = "solid"
        ),
        locations = cells_body(
          columns = price
        )
      ) %>%
      tab_style(
        style = cell_fill(
          color = "#E1D4B2"
        ),
        locations = cells_body(
          columns = !price,
          rows = address == map_address()
        )
      ) %>%
      DGThemes::gt_theme_duncan() %>%
      gt::tab_options(
        heading.border.bottom.color = "white",
        table.border.top.color = "white",
        column_labels.border.bottom.color = "white",
        column_labels.border.top.color = "white",
        table.border.left.color = "white",
        table.border.right.color = "white",
        table_body.hlines.color = "pink"
      ),
    height = px(900)
  )
  
  output$price_plot <- renderPlot({
    housing_df %>%
      ggplot(aes(price, house_area, color = factor(beds, levels = c(1, 2, 3, 4, 5)))) +
      geom_point() +
      geom_smooth(method = "lm", se = F) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      scale_x_continuous(labels = scales::label_dollar()) +
      scale_y_continuous(labels = scales::label_comma()) +
      labs(x = "Price", y = "House Area (sqft)", title = "Price vs. Area", color = "# of Beds") +
      DGThemes::theme_duncan()
  })
  
  output$loc_plot <- renderPlot({
    
    ggmap(get_map(location = c(-122.6970879, 45.5000969), zoom = 12)) +
      geom_point(data = map_df, aes(x = longitude, y = latitude, size = price), 
                 color = "#8466B9", alpha = 0.7) +
      scale_size_continuous(labels = scales::label_dollar(), range = c(5, 12)) +
      labs(size = "Price") +
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  
  output$about_text <- renderUI({
    url <- tags$a('here', href = 'duncangates.me')
    fluidRow(
      column(12,
             div(style="display:inline-block;vertical-align:top;text-align:center;line-height:1.7;",
                 tagList("See my write-up on this Shiny App on my website ", 
                         url, 
                         "or click", 
                         actionButton("show_html", label = "Here"),
                         "to see it within the app (not as pretty)")
             ),
             conditionalPanel(
               condition = "input.show_html != 0",
               htmltools::includeHTML("post.html")
             )
             )
    )
  })
}

shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui = ui, server = server))
