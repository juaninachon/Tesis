library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)
library(psych)
library(gvlma)
library(janitor)
library(performance)
library(magrittr)
library(cowplot)
datos <- read.csv("data/shiny.csv", check.names = FALSE)
# UI
ui <- navbarPage(
  fluid = TRUE,
  theme = shinytheme("paper"),
  title = "Explore los datos",
  
  # Tab 1
  tabPanel(
    "Distribución de las variables",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "variable_datos", "Seleccione una variable:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        checkboxGroupInput(
          "group_filter_1", "Muestras:",
          choices = unique(datos$Estudio),
          selected = unique(datos$Estudio)
        ),
        numericInput(
          "binwidth", "Ancho del intervalo:",
          min = 0, max = 10, value = 1, step = .25
        )
      ),

      mainPanel(
        plotOutput("combined_plot_1"),
        tags$head(
          tags$style(HTML("
            #describe_1 {
              display: table;
              margin-left: auto;
              margin-right: auto;
            }
          "))
        ),
        tableOutput("describe_1")
      )
    )
  ),
  # Tab 2
  tabPanel(
    "Correlaciones",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "covariable_1", "Seleccione una variable:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        selectInput(
          "covariable_2", "Seleccione otra variable:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = "Woodcock-Muñoz – Comprensión de Oraciones"
        ),
        selectInput(
          "covariable_3", "Seleccione un factor de agrupamiento:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = "Edad años"
        ),
        checkboxGroupInput(
          "group_filter_2", "Muestras:",
          choices = unique(datos$Estudio),
          selected = unique(datos$Estudio)
        ),
        selectInput(
          "cormet", "Método:",
          choices = list("Pearson", "Spearman"),
          multiple = FALSE,
          selected = "Pearson"
        ),
        checkboxInput(
          "corpar", "Correlación parcial por el factor de agrupamiento", FALSE
        )
      ),
      mainPanel(
        plotOutput("corplot_2"),
        verbatimTextOutput("cortest_2")
      )
    )
  ),
  # Tab 3
  tabPanel(
    "Modelos multivariados",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "lm_1", "Seleccione una variable dependiente:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        selectInput(
          "lm_2", "Seleccione los predictores:",
          multiple = TRUE,
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        checkboxGroupInput(
          "group_filter_3", "Muestras:",
          choices = unique(datos$Estudio),
          selected = unique(datos$Estudio)
        )
      ),
      mainPanel(
        p("Ajuste del modelo"),
        verbatimTextOutput("lm_fit"),
        p("Prueba de supuestos"),
        verbatimTextOutput("lm_assumptions")
      )
    )
  ),
  # Tab 4
  tabPanel(
    "Modelos anidados",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "nlm_1", "Seleccione una variable dependiente:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        selectInput(
          "nlm_2", "Seleccione los predictores del modelo base:",
          multiple = TRUE,
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        selectInput(
          "nlm_3", "Seleccione los predictores del modelo extendido:",
          multiple = TRUE,
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        checkboxGroupInput(
          "group_filter_4", "Muestras:",
          choices = unique(datos$Estudio),
          selected = unique(datos$Estudio)
        )
      ),
      mainPanel(
        p("Índices de ajuste"),
        verbatimTextOutput("nlm_fit"),
        p("Anova"),
        verbatimTextOutput("nlm_anova")
      )
    )
  ),
  # Tab 5
  tabPanel(
    "Moderación",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "mlm_1", "Seleccione una variable dependiente:",
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = NULL
        ),
        selectInput(
          "mlm_2", "Seleccione un predictor:",
          multiple = FALSE,
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = "NeuroUX - Toca botón - Puntaje"
        ),
        selectInput(
          "mlm_3", "Seleccione un moderador (interacción simple):",
          multiple = FALSE,
          choices = colnames(datos) %>% setdiff(., c("Estudio")),
          selected = "Edad años"
        ),
        checkboxGroupInput(
          "group_filter_5", "Muestras:",
          choices = unique(datos$Estudio),
          selected = unique(datos$Estudio)
        )
      ),
      mainPanel(
        p("Ajuste del modelo"),
        verbatimTextOutput("mlm_fit"),
        p("Prueba de supuestos"),
        verbatimTextOutput("mlm_assumptions")
      )
    )
  ),
  #git
  tabPanel(
    title = tags$a(
      href = "https://github.com/juaninachon/Tesis",
      target = "_blank",
      icon("github"),
      class = "nav-link github-tab"
    )
  ),
  tabPanel(NULL),
  # Add CSS to fix baseline offset
  header = tags$head(
  tags$style(HTML("
    /* Default (desktop / landscape) */
    .github-tab > i {
      position: relative;
      top: -40px;  /* aligns icon nicely with tabs */
    }
    .github-tab {
      display: inline-flex;
      align-items: center;
      justify-content: flex-start;
      height: 0px;
      font-size: 20px;
      padding-top: 0;
      padding-bottom: 0;
    }

    /* Portrait / mobile fix */
    @media (max-width: 768px) {
      .github-tab > i {
        top: 0 !important;   /* reset vertical offset */
      }
      .github-tab {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        height: auto;
        padding: 8px 0;      /* restore normal spacing */
      }
    }
  "))
)
)
# Server
server <- function(input, output, session) {
  # Tab 1
  filtered_data_1 <- reactive({
    datos %>%
      filter(
        Estudio %in% input$group_filter_1
      )
  })
  cols <- reactive({
    if_else(
      input$group_filter_1 == c("jardines", "museo"),
      c("#F8766D", "#00BFC4"),
      if_else(
        input$group_filter_1 == c("jardines"),
        c("#F8766D"),
        c("#00BFC4")
      )
    ) 
  })
  output$combined_plot_1 <- renderPlot({
    histo_1 <- filtered_data_1() %>%
      ggplot() +
      geom_histogram(
        aes(x = .data[[input$variable_datos]], fill = Estudio),
        binwidth = input$binwidth,
        alpha = 0.65, position = "identity",
        na.rm = TRUE
      ) +
      scale_fill_manual(values = cols()) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),

      ) +
      labs(x = input$variable_datos, y = "n")

    caja_1 <- filtered_data_1() %>%
      ggplot() +
      geom_boxplot(
        aes(
          x = .data[[input$variable_datos]],
          fill = Estudio
        ),
        alpha = 0.65,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = cols()) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )

    plot_grid(
      histo_1, caja_1,
      ncol = 1, rel_heights = c(4, 1),
      align = "hv", axis = "b"
    )
  })
  output$describe_1 <- renderTable({
    psych::describe(
      filtered_data_1() %>%
        select(input$variable_datos) %>%
        na.omit()
    ) %>%
      select(-c(vars, mad, trimmed, se))
  }, options = list(pageLength = 10))

  # Tab 2
  filtered_data_2 <- reactive({
    datos %>%
      filter(
        Estudio %in% input$group_filter_2
      ) %>%
      select_if(is.numeric)
  })
  output$corplot_2 <- renderPlot({
    df <- filtered_data_2()
    xvar <- input$covariable_1
    yvar <- input$covariable_2
    cvar <- input$covariable_3

    if (!isTRUE(input$corpar)) {
      return(
        df |>
          ggplot(aes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            colour = .data[[cvar]]
          )) +
          geom_point() +
          scale_colour_viridis_c() +
          geom_smooth(method = "lm", se = FALSE) +
          theme_minimal() +
          labs(x = xvar, y = yvar)
      )
    }

    df2 <- df |>
      dplyr::select(all_of(c(xvar, yvar, cvar))) |>
      tidyr::drop_na()

    rx <- resid(lm(df2[[xvar]] ~ df2[[cvar]]))
    ry <- resid(lm(df2[[yvar]] ~ df2[[cvar]]))

    part_df <- data.frame(rx = rx, ry = ry)

    ggplot(part_df, aes(x = rx, y = ry)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(
        x = paste0("Residuos de ", xvar, " (controlando por ", cvar, ")"),
        y = paste0("Residuos de ", yvar)
      )
  })

  output$cortest_2 <- renderPrint({
    req(input$cormet)

    df <- filtered_data_2()
    xvar <- input$covariable_1
    yvar <- input$covariable_2
    cvar <- input$covariable_3
 
    if (!isTRUE(input$corpar)) {
      df2 <- df |>
        dplyr::select(all_of(c(xvar, yvar))) |>
        tidyr::drop_na()

      method_used <- if (input$cormet == "Pearson") "pearson" else "spearman"

      return(cor.test(
        df2[[xvar]],
        df2[[yvar]],
        method = method_used
      ))
    }

    df2 <- df |>
      dplyr::select(all_of(c(xvar, yvar, cvar))) |>
      tidyr::drop_na()

    rx <- resid(lm(df2[[xvar]] ~ df2[[cvar]]))
    ry <- resid(lm(df2[[yvar]] ~ df2[[cvar]]))

    method_used <- if (input$cormet == "Pearson") "pearson" else "spearman"
    cor.test(rx, ry, method = method_used)
  })

  # Tab 3
  filtered_data_3 <- reactive({
    datos %>%
      filter(
        Estudio %in% input$group_filter_3
      ) %>%
      select_if(is.numeric)
  })
  output$lm_fit <- renderPrint({
    req(input$lm_1, input$lm_2)
    data <- clean_names(filtered_data_3())
    vd <- make_clean_names(input$lm_1)
    vis <- make_clean_names(input$lm_2)
    visa <- paste(vis, collapse = " + ")
    fit <- lm(as.formula(paste(vd, "~", paste(visa, collapse = "+"))), data = data)
    print(
      summary(fit)
    )
  })
  output$lm_assumptions <- renderPrint({
    req(input$lm_1, input$lm_2)
    data <- clean_names(filtered_data_3())
    vd <- make_clean_names(input$lm_1)
    vis <- make_clean_names(input$lm_2)
    visa <- paste(vis, collapse = " + ")
    fit <- lm(as.formula(paste(vd, "~", visa)), data = data)
    print(
      gvlma(fit)
    )
  })
  # Tab 4
  filtered_data_4 <- reactive({
    req(input$nlm_1, input$nlm_2, input$nlm_3)
    vars <- c(input$nlm_1, input$nlm_2, input$nlm_3)
    datos %>%
      filter(Estudio %in% input$group_filter_4) %>%
      select(all_of(vars)) %>%
      select_if(is.numeric) %>%
      filter(complete.cases(.))
  })
  output$nlm_fit <- renderPrint({
    req(input$nlm_1, input$nlm_2, input$nlm_3)
    data <- clean_names(filtered_data_4())
    vd <- make_clean_names(input$nlm_1)
    vis <- make_clean_names(input$nlm_2)
    visb <- make_clean_names(input$nlm_3)
    fit_base <- lm(as.formula(paste(vd, "~", paste(vis, collapse = "+"))), data = data)
    fit_ext  <- lm(as.formula(paste(vd, "~", paste(c(vis, visb), collapse = "+"))), data = data)
    print(compare_performance(fit_base, fit_ext, rank = FALSE, metrics = "common"))
  })

  output$nlm_anova <- renderPrint({
    req(input$nlm_1, input$nlm_2, input$nlm_3)
    data <- clean_names(filtered_data_4())
    vd <- make_clean_names(input$nlm_1)
    vis <- make_clean_names(input$nlm_2)
    visb <- make_clean_names(input$nlm_3)
    fit_base <- lm(as.formula(paste(vd, "~", paste(vis, collapse = "+"))), data = data)
    fit_ext  <- lm(as.formula(paste(vd, "~", paste(c(vis, visb), collapse = "+"))), data = data)
    print(anova(fit_base, fit_ext))
  })
  # Tab 5
  filtered_data_5 <- reactive({
    sapply(
      datos %>%
        filter(
          Estudio %in% input$group_filter_5
        ) %>%
        select_if(is.numeric)
      ,
      function(x) scale(x, scale = FALSE)
    ) %>%
    as.data.frame
  })
  output$mlm_fit <- renderPrint({
    req(input$mlm_1, input$mlm_2, input$mlm_3)
    data <- clean_names(filtered_data_5())
    vd <- make_clean_names(input$mlm_1)
    vis <- make_clean_names(input$mlm_2)
    vim <- make_clean_names(input$mlm_3)
    fit <- lm(as.formula(paste(vd, "~", vis, "+", vim, "+", vis, "*", vim)), data = data)
    print(summary(fit))
  })
  output$mlm_assumptions <- renderPrint({
    req(input$mlm_1, input$mlm_2, input$mlm_3)
    data <- clean_names(filtered_data_5())
    vd <- make_clean_names(input$mlm_1)
    vis <- make_clean_names(input$mlm_2)
    vim <- make_clean_names(input$mlm_3)
    fit <- lm(as.formula(paste(vd, "~", vis, "+", vim, "+", vis, "*", vim)), data = data)
    print(gvlma(fit))
  })
}

shinyApp(ui = ui, server = server)