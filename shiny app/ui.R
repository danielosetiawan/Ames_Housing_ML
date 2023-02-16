# Load packages
library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)

thematic_shiny()

dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    dark = TRUE,
    help = TRUE,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = sprintf("bs4Dash v%s", as.character(utils::packageVersion("bs4Dash"))),
        color = "primary",
        href = "https://divadnojnarg.github.io/outstanding-shiny-ui/",
        image = "https://adminlte.io/themes/v3/dist/img/user2-160x160.jpg",
        opacity = 0.8
      ),
      fixed = TRUE,
      tooltip(
        title = "This toggles the right sidebar",
        placement = "bottom",
        actionButton(inputId = "controlbarToggle", label = "Toggle Controlbar", class = "mx-2")
      ),
      popover(
        title = "Toggle button",
        content = "This toggle the left sidebar",
        placement = "bottom",
        # `data-trigger` = "hover",
        actionButton(inputId = "sidebarToggle", label = "Toggle left sidebar", class = "mx-2")
      ),
      rightUi = tagList(
        dropdownMenu(
          badgeStatus = "danger",
          type = "messages",
          messageItem(
            inputId = "triggerAction1",
            message = "message 1",
            from = "Divad Nojnarg",
            image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
            time = "today",
            color = "lime"
          )
        ),
        userOutput("user")
      ),
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "info",
          type = "notifications",
          notificationItem(
            inputId = "triggerAction2",
            text = "Error!",
            status = "danger"
          )
        ),
        dropdownMenu(
          badgeStatus = "info",
          type = "tasks",
          taskItem(
            inputId = "triggerAction3",
            text = "My progress",
            color = "orange",
            value = 10
          )
        )
      )
    ),
    sidebar = dashboardSidebar(
      
      collapsed = TRUE,
      # fixed = TRUE,
      skin = "light",
      status = "primary",
      id = "sidebar",
      customArea = fluidRow(
        actionButton(
          inputId = "myAppButton",
          label = NULL,
          icon = icon("users"),
          width = NULL,
          status = "primary",
          style = "margin: auto",
          dashboardBadge(textOutput("btnVal"), color = "danger")
        )
      ),
      sidebarUserPanel(
        image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png",
        name = "Welcome Onboard!"
      ),
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        sidebarHeader("Cards"),
        menuItem(
          "Basic cards",
          tabName = "cards",
          icon = icon("sliders")
        ),
        menuItem(
          "Cards API",
          badgeLabel = "New",
          badgeColor = "success",
          tabName = "cardsAPI",
          icon = icon("laptop-code")
        ),
        menuItem(
          "Social cards",
          tabName = "socialcards",
          icon = icon("id-card")
        ),
        menuItem(
          "Tab cards",
          tabName = "tabcards",
          icon = icon("layer-group")
        ),
        menuItem(
          "Sortable cards",
          tabName = "sortablecards",
          icon = icon("object-ungroup")
        ),
        menuItem(
          "Stats elements",
          tabName = "statsboxes",
          icon = icon("chart-area")
        ),
        sidebarHeader("Other boxes"),
        menuItem(
          "Value/Info boxes",
          tabName = "valueboxes",
          icon = icon("suitcase")
        ),
        
        sidebarHeader("Colors"),
        
        menuItem(
          "Colors",
          tabName = "colors",
          icon = icon("droplet")
        ),
        
        sidebarHeader("BS4 gallery"),
        menuItem(
          text = "Galleries",
          icon = icon("cubes"),
          startExpanded = FALSE,
          menuSubItem(
            text = HTML(
              paste(
                "Gallery 1",
                dashboardBadge(
                  "new",
                  position = "right",
                  color = "danger"
                )
              )
            ),
            tabName = "gallery1",
            icon = icon("circle")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "Gallery 2",
                dashboardBadge(
                  "!",
                  position = "right",
                  color = "success"
                )
              )
            ),
            tabName = "gallery2"
          )
        )
      )
    ),
    body = dashboardBody(
      tabItems(
        basic_cards_tab,
        cards_api_tab,
        social_cards_tab,
        tab_cards_tab,
        sortable_cards_tab,
        statsboxes_tab,
        value_boxes_tab,
        colors_tab,
        gallery_1_tab,
        gallery_2_tab
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "pills",
        controlbarItem(
          "Inputs",
          column(
            width = 12,
            align = "center",
            radioButtons(
              inputId = "dist",
              label = "Distribution type:",
              c(
                "Normal" = "norm",
                "Uniform" = "unif",
                "Log-normal" = "lnorm",
                "Exponential" = "exp"
              )
            )
          )
        ),
        controlbarItem(
          "Skin",
          skinSelector()
        )
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      left = a(
        href = "https://twitter.com/divadnojnarg",
        target = "_blank", "@DivadNojnarg"
      ),
      right = "2022"
    ),
    title = "bs4Dash Showcase"
  )