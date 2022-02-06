# Shiny options -----------------------------------------------------------

shinyOptions(cache = diskCache("./app-cache")) # Remove once modules are updated
shinyOptions(cache = cachem::cache_disk("./app-cache"))


# Packages ----------------------------------------------------------------

#getDependencies("mapboxapi", installed=TRUE, available=FALSE)

library(shiny)
library(shinydashboard)
# library(shinybusy) # :: or remove
# library(shinyjqui) # :: or remove
library(shinyWidgets)
# library(shinythemes) # ::
# library(extrafont) # :: or remove
library(shiny.i18n)
library(waiter)

library(dplyr)
library(ggplot2)
# library(tidyr) # :: or remove
# library(purrr) # ::
library(readr) # Eventually get rid of in favour of qs
# library(tibble) # ::
# library(stringr) # ::
# library(ggthemes) # :: or remove

library(sf)
library(mapdeck) 
library(mapboxapi)
# library(geojsonsf) # ::
# library(jsonify) # ::
# library(raster) # :: or remove
# library(RColorBrewer) # ::

# library(markdown) # :: or remove
# library(png) # ::
# library(cowplot) # ::, long-term replace with patchwork
# library(classInt) # :: or remove
# library(scales) # ::

library(DT)
library(qs)
library(glue)
# library(aniview) # ::
# library(data.table) # remove

#library(shinipsum)
# library(fakir)
#library(googleLanguageR)
#library(shinyanimate)
# library(shinycssloaders)



# When debugging, switch to TRUE and get detailed error log on server
options(shiny.trace = FALSE)


# Functions ---------------------------------------------------------------

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

theme_map <- function(...) {
  default_background_color <- "transparent"
  default_font_color <- "black"
  default_font_family <- "Helvetica"
  
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family, color = default_font_color),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = default_background_color, color = NA),
      panel.background = element_rect(fill = default_background_color, color = NA),
      legend.background = element_rect(fill = default_background_color, color = NA),
      legend.position = "none",
      plot.margin = unit(c(0, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 22, hjust = 0, color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5, color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = default_font_color,
                                   margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm"),
                                   debug = F),
      plot.caption = element_text(size = 7, hjust = .5, 
                                  margin = margin(t = 0.2, b = 0, unit = "cm"),
                                  color = "#939184"),
      ...)
}


# Colours -----------------------------------------------------------------

bivariate_color_scale <- tibble(
  "3 - 3" = "#2A5A5B", "2 - 3" = "#567994", "1 - 3" = "#6C83B5", 
  "3 - 2" = "#5A9178", "2 - 2" = "#90B2B3", "1 - 2" = "#B5C0DA",
  "3 - 1" = "#73AE80", "2 - 1" = "#B8D6BE", "1 - 1" = "#E8E8E8") %>%
  tidyr::pivot_longer(everything(), "group",values_to = "fill")

color_scale <- tibble(
  "6" = "#73AE80", "5" = "#B8D6BE", "4" = "#E8E8E8", "3" = "#6C83B5",
  "2" = "#B5C0DA", "1" = "#E8E8E8") %>%
  tidyr::pivot_longer(everything(), "group", values_to = "fill")

color_scale_2 <- tibble(
  "3" = "#73AE80", "2" = "#B8D6BE", "1" = "#E8E8E8") %>%
  tidyr::pivot_longer(everything(), "group", "fill")

colors <- as.character(color_scale$fill)

default_background_color <- "transparent"
default_font_color <- "black"
default_font_family <- "Helvetica"


# Animation ---------------------------------------------------------------

animateCSS <- function(effect, delay = 0, duration = 500, then = NULL) {
  
  effect <- match.arg(effect, c(
    "bounce", "flash", "pulse", "rubberBand", "shakeX", "shakeY", "headShake",
    "swing", "tada", "wobble", "jello", "heartBeat", "backInDown",
    "backInLeft", "backInRight", "backInUp", "backOutDown", "backOutLeft",
    "backOutRight", "backOutUp", "bounceIn", "bounceInDown", "bounceInLeft",
    "bounceInRight", "bounceInUp", "bounceOut", "bounceOutDown",
    "bounceOutLeft", "bounceOutRight", "bounceOutUp", "fadeIn", "fadeInDown",
    "fadeInDownBig", "fadeInLeft", "fadeInLeftBig", "fadeInRight",
    "fadeInRightBig", "fadeInUp", "fadeInUpBig", "fadeInTopLeft",
    "fadeInTopRight", "fadeInBottomLeft", "fadeInBottomRight", "fadeOut",
    "fadeOutDown", "fadeOutDownBig", "fadeOutLeft", "fadeOutLeftBig",
    "fadeOutRight", "fadeOutRightBig", "fadeOutUp", "fadeOutUpBig",
    "fadeOutTopLeft", "fadeOutTopRight", "fadeOutBottomRight",
    "fadeOutBottomLeft", "flip", "flipInX", "flipInY", "flipOutX", "flipOutY",
    "lightSpeedInRight", "lightSpeedInLeft", "lightSpeedOutRight",
    "lightSpeedOutLeft", "rotateIn", "rotateInDownLeft", "rotateInDownRight",
    "rotateInUpLeft", "rotateInUpRight", "rotateOut", "rotateOutDownLeft",
    "rotateOutDownRight", "rotateOutUpLeft", "rotateOutUpRight", "hinge",
    "jackInTheBox", "rollIn", "rollOut", "zoomIn", "zoomInDown", "zoomInLeft",
    "zoomInRight", "zoomInUp", "zoomOut", "zoomOutDown", "zoomOutLeft",
    "zoomOutRight", "zoomOutUp", "slideInDown", "slideInLeft", "slideInRight",
    "slideInUp", "slideOutDown", "slideOutLeft", "slideOutRight",
    "slideOutUp"))
  
  js <- paste(
    "    $this.animateCSS('%s', {",
    "      delay: %d,",
    "      duration: %d,",
    "      callback: function(){",
    "        %s",
    "      }",
    "    });",
    sep = "\n"
  )
  
  sprintf(js, effect, delay, duration, ifelse(is.null(then), "", then))
}

onShowJS <- function(animation, fadeDuration){
  sprintf(paste(
    "$('#%%s>div').on('show', function(){",
    "  var $this = $(this);",
    "  $this.css('opacity', 0).animate({opacity: 1}, %d, function(){",
    animation,
    "  });",
    "});",
    sep = "\n"
  ), fadeDuration)
}

onHideJS <- function(animation, fadeDuration){
  paste(
    "$('#%s>div').on('hide', function(){",
    "  var $this = $(this);",
    "  setTimeout(function(){",
    sub(
      "^(\\s.*?\\$this\\.animateCSS)",
      "$this.show().animateCSS",
      sub(
        "\\{\n        \n      \\}",
        sprintf("{$this.hide(%d);}", fadeDuration),
        animation
      )
    ),
    "  }, 0);",
    "});",
    sep = "\n"
  )
}

animatedConditionalPanel <-
  function(condition, ..., onShow = NULL, fadeIn = 600, onHide = NULL, 
           fadeOut = 400) {
    id <- paste0("animateCSS-", stringi::stri_rand_strings(1, 15))
    jsShow <- ifelse(!is.null(onShow), sprintf(onShowJS(onShow, fadeIn), id), "")
    jsHide <- ifelse(!is.null(onHide), sprintf(onHideJS(onHide, fadeOut), id), "")
    script <- tags$script(HTML(paste(jsShow,jsHide,sep = "\n")))
    condPanel <- conditionalPanel(condition, ...)
    tags$div(id = id, tagList(condPanel, script))
  }




loadingLogo <-
  function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
    tagList(
      tags$head(
        tags$script(
          "setInterval(function() {
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }
        },100)")
      ),
      tags$a(href = href,
             div(class = "busy",
                 img(src = loadingsrc, height = height, width = width, alt = alt)),
             div(class = 'notbusy',
                 img(src = src, height = height, width = width, alt = alt))
      )
    )
  }


# Load data ---------------------------------------------------------------
## THESE ALL NEED TO BE TURNED INTO QS BINARIES

#title_text <- qread("data/title_text.qs")

#variable_explanations <- read_csv("data/variable_explanations.csv")

# Load data for pedestrian realm 
#load(file = "data/sidewalks_WSG.Rdata")


#census_analysis_quantile_WSG <- census_analysis_quantile %>% 
#  st_transform(4326)


# Other prep --------------------------------------------------------------

# This doesn't work with module namespacing TKTK
module_style <- 
  tags$head(tags$style(HTML("
          #title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_overlay {border-width: 10px; 
          border-color: rgba(255,255,255,1);}
          #input_control_left {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #input_control_left2 {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}")))

js_ped_1 <- "$(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped <- "$(document).ready(function(){
  $('#plotContainer_ped').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped_control <- "$(document).ready(function(){
  $('#plotContainer_ped_control').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js <- "
  $(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js2 <- "
$(document).ready(function(){
  $('#menuContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"


js3 <- "
$(document).ready(function(){
  $('#plotContainer2').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"

styler <- '
      /* logo */
      .skin-black .main-header .logo {
      background-color: #FFFFFF;
      }
      
      /* logo when hovered */
      .skin-black .main-header .logo:hover {
      background-color: #FFFFFF;
      }
      
      /* navbar (rest of the header) */
      .skin-black .main-header .navbar {
      background-color: #FFFFFF;
      }
      
      /* main sidebar */
      .skin-black .main-sidebar {
      background-color: #FFFFFF;
      
      }
      
      /* active selected tab in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #0096C9;
      color: #FFFFFF;
      
      }
      
      /* other links in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu a{
      background-color: #FFFFFF50;
      color: #3C3C3B;
      height: 60px;
      }
      
      /* other links in the sidebarmenu when hovered */
      .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
      background-color: #0096C910;
      }
      
      /* toggle button when hovered  */
      .skin-black .main-header .navbar .sidebar-toggle:hover{
      background-color: #FFFFFF;
      }
      
      /* body */
      .content-wrapper, .right-side {
      background-color: #FFFFFF;
      }
                                '


# Establish reactiveValues ------------------------------------------------

#qz <- reactiveValues(zoom_level = 'OUT')

rz_pedestrian <- reactiveValues(zoom = 'OUT',
                                poly_selected = NA)



# Commute mode change globals ---------------------------------------------

# Set access token  
set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')


spinner <- tagList(
  spin_chasing_dots(),
  span("Sus is currently down for maintenance", style="color:white;")
)
