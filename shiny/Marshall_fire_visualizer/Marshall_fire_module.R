### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

Marshall_fire_UI <- function(id) {
  
  tabItem(
    tabName = "Marshall fire",
    
    # Style tags
    module_style,
    
    # Main map
    mapdeckOutput(NS(id, "map"), height = "91vh"),
    
    # Title bar
    title_UI(NS(id, "title")),
    
    # Right panel
    absolutePanel(
      id = NS(id, "right_panel"), style =
        "z-index:500; max-height: 90vh; overflow-y: auto; overflow-x:hidden; padding: 5px; border-width: 0px;",
      class = "panel panel-default", top = 70, right = 20, width = 300,
      
      # 3D switch
      shinyWidgets::materialSwitch(inputId = NS(id, "extrude"), 
                                   label = i18n$t("View in 3D"),
                                   status = "danger", value = FALSE),
      
      hr(),
      
      # Compare panel
      fluidRow(
        column(width = 7, h4(i18n$t("Compare"))),
        column(width = 5, align = "right", 
               actionLink(inputId = NS(id, "hide_compare"),
                          label = i18n$t("Hide")))),
      
      conditionalPanel(
        condition = "output.hide_compare_status == 1", ns = NS(id),
        selectInput(NS(id, "var_right"), label = NULL, choices = var_right_list),
        plotOutput(NS(id, "map_right"), height = 200)),
      
      conditionalPanel(
        condition = "input.extrude == 0", ns = NS(id), hr(),
        
        # Explore panel
        explore_UI(NS(id, "canale")),
        
        hr(),
        
        # DYK panel
        dyk_UI(NS(id, "canale"))
      )
    ),
    
    # Floating legend
    absolutePanel(
      id = NS(id, "legend_container"), class = "panel panel-default",
      style = "z-index:500; background-color: rgba(0,0,255,0); border-width: 0px; margin:0px", 
      bottom = 20, fixed = TRUE,
      conditionalPanel(
        condition = 'input.var_right != " "', ns = NS(id),
        id = NS(id, "legend"),
        img(src = "bivariate_legend_2.png", width = 200, height = 177))
    )
  )
}


# Server ------------------------------------------------------------------

Marshall_fire_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Data 
    # TKTK need to change this to not hard code canale as left_variable,
    # and maybe make its own module
    data_canale <- reactive({
      
      if (input$extrude) {
        data <- data_canale_DA_1
      } else data <- switch(rv_canale$zoom, "OUT" = data_canale_borough, 
                            "IN" = data_canale_CT, "ISO" = data_canale_DA_1, 
                            "ISO_2" = data_canale_DA_2)
      
      if (input$var_right == " ") {
        data <-
          data %>%
          dplyr::select(ID, name, name_2, population,
                        left_variable_full = ale_index,
                        left_variable = ale_index_quant3, ale_class, width,
                        group, fill, elevation, fill_opacity)
        
      } else {
        data <-
          data %>%
          dplyr::select(
            ID, name, name_2, population,
            left_variable_full = ale_index, left_variable = ale_index_quant3, 
            ale_class, right_variable_full = input$var_right, 
            right_variable = paste0(input$var_right, "_quant3"), 
            width, group = paste0(input$var_right, "_quant3_group"),
            fill = paste0(input$var_right, "_quant3_fill"),
            elevation = paste0(input$var_right, "_quant3_elevation"),
            fill_opacity = paste0(input$var_right, 
                                  "_quant3_fill_opacity"))
      }
      
      return(data)
    })
    
    # Title bar
    title_server("title", "canale")
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_canale$zoom <- case_when(
        input$map_view_change$zoom >= 14 ~ "ISO_2",
        input$map_view_change$zoom >= 12 ~ "ISO",
        input$map_view_change$zoom >= 10.5 ~ "IN",
        TRUE ~ "OUT")
    })
    
    # Translate drop-down list
    observe({
      updateSelectInput(session = session, inputId = "var_right",
                        choices = sus_translate(var_right_list))
    })
    
    
    ## Observe and change click status -------------------------------------------
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_canale$poly_selected <- NA
      } else rv_canale$poly_selected <- lst$object$properties$id
    })
    
    # Clear click status if prompted
    observeEvent(input$clear_selection, {rv_canale$poly_selected <- NA})
    
    # Output polygon select status
    output$poly_selected <- reactive({
      if (is.na(rv_canale$poly_selected)) FALSE else TRUE
    })
    outputOptions(output, "poly_selected", suspendWhenHidden = FALSE)
    
    # Clear polygon select on zoom change
    observeEvent(rv_canale$zoom, {rv_canale$poly_selected <- NA},
                 ignoreInit = TRUE)
    
    # Clear polygon select on tab change
    observeEvent(input$tabs, {rv_canale$poly_selected <- NA}, ignoreInit = TRUE)
    
    # Observe and react to change in extrude status
    observeEvent(input$extrude, {rv_canale$poly_selected <- NA})
    
    
    ## Render the map ------------------------------------------------------------
    
    output$map <- renderMapdeck({
      mapdeck(
        style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
        token = paste0(
          "pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
          "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
        zoom = 10.1, location = c(-73.58, 45.53), pitch = 0) %>% 
        add_polygon(
          data = data_canale(),
          stroke_width = "width", stroke_colour = "#FFFFFF",
          fill_colour = "fill_opacity", update_view = FALSE,
          layer_id = "polylayer", id = "ID", auto_highlight = TRUE,
          highlight_colour = "#FFFFFF90", legend = FALSE)
    })
    
    # Render explore panel
    explore_server("canale", data_canale, reactive(input$var_right),
                   reactive(rv_canale$poly_selected), 
                   reactive(rv_canale$zoom), "CanALE index")
    
    # Render did-you-know panel
    dyk_server("canale", reactive("ale_index"), reactive(input$var_right))
    
    
    ## Update map in response to variable changes, zooming, or options -----------
    
    observeEvent({
      input$var_right
      rv_canale$zoom
      input$extrude}, {
        if (!input$extrude) {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = data_canale(),
              stroke_width = "width", stroke_colour = "#FFFFFF",
              fill_colour = "fill_opacity", update_view = FALSE,
              layer_id = "polylayer", id = "ID", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90", legend = FALSE)
        } else {
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "polylayer") %>%
            add_polygon(
              data = data_canale(),
              fill_colour = "fill", elevation = "elevation",
              update_view = FALSE, layer_id = "extrude", id = "ID",
              auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(lightsPosition = c(0, 0, 5000),
                                    numberOfLights = 1, ambientRatio = 1))
        }
      }
    )
    
    
    ## Update map on click -------------------------------------------------------
    
    observeEvent(rv_canale$poly_selected, {
      
      # Mode if not in 3D
      if (!input$extrude) {
        if (!is.na(rv_canale$poly_selected)) {
          
          # print(paste0("Selecting polygon ", rv_canale$poly_selected))
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            add_polygon(
              data = filter(data_canale(), ID == rv_canale$poly_selected),
              stroke_width = "width", stroke_colour = "#000000",
              fill_colour = "fill", update_view = FALSE, 
              layer_id = "poly_highlight", auto_highlight = TRUE,
              highlight_colour = "#FFFFFF90", legend = FALSE)
        }
        
        if (is.na(rv_canale$poly_selected)) {
          
          # print("Removing selection")
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_highlight")
        }
        
        # Mode if in 3D
      } else if (input$extrude) {
        if (!is.na(rv_canale$poly_selected)) {
          
          # print(paste0("Selecting 3D polygon ", rv_canale$poly_selected))
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "polylayer") %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = {
                data_canale() %>%
                  mutate(elevation = if_else(
                    group == group[ID == rv_canale$poly_selected], 4000, 0))},
              fill_colour = "fill", elevation = "elevation",
              update_view = FALSE, layer_id = "extrude", id = "ID",
              auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
              legend = FALSE,
              light_settings = list(
                lightsPosition = c(0, 0, 5000),
                numberOfLights = 1,
                ambientRatio = 1))
        }
        
        if (is.na(rv_canale$poly_selected)) {
          
          # print("Removing 3D selection")
          
          mapdeck_update(map_id = NS(id, "map")) %>%
            clear_polygon(layer_id = "poly_highlight") %>%
            clear_polygon(layer_id = "extrude") %>%
            add_polygon(
              data = data_canale(), fill_colour = "fill", 
              elevation = "elevation", update_view = FALSE, layer_id = "extrude",
              id = "ID", auto_highlight = TRUE, highlight_colour = "#FFFFFF90",
              legend = FALSE)
        }
      }
    })
    
    # Hide compare status
    output$hide_compare_status <- reactive(input$hide_compare %% 2 == 0)
    outputOptions(output, "hide_compare_status", suspendWhenHidden = FALSE)
    
    observeEvent(input$hide_compare, {
      if (input$hide_compare %% 2 == 0) {
        txt <- sus_translate("Hide")
      } else txt <- sus_translate("Show")
      updateActionButton(session, "hide_compare", label = txt)
    })
    
    # Left map
    left_map_server("canale", data_canale, reactive(rv_canale$zoom))
    
    # Right map
    output$map_right <- renderPlot({
      
      if (input$var_right == " ") {
        p <- 
          ggplot(data_canale()) +
          geom_sf(fill = "#CABED0", color = "white", size = 0.01) +
          theme_map()
        
        cowplot::ggdraw() +
          cowplot::draw_image(dropshadow_right, scale = 1.17) +
          cowplot::draw_plot(p)
      } else {
        p <-
          ggplot(data_canale()) +
          geom_sf(aes(fill = as.factor(right_variable)),
                  color = "white", size = 0.01) +
          scale_fill_manual(values = rev(colors[c(4:6)])) +
          theme_map()
        
        cowplot::ggdraw() +
          cowplot::draw_image(dropshadow_right, scale = 1.17) +
          cowplot::draw_plot(p) +
          cowplot::draw_image(uni_legend_right, scale = .45, vjust = 0.25, 
                              hjust = -0.25)
      }
    }, bg = "transparent") %>% bindCache(input$var_right, 
                                         rv_canale$zoom)
  })
}
