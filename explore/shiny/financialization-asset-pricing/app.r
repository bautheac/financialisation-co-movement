
library(shinydashboard)
library(shiny)
library(magrittr)

linebreaks <- function(n){HTML(strrep(br(), n))}

source("datasets.r")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Globals", tabName = "globals", icon = icon("tools")
    ),
    menuItem(
      "Statistics", tabName = "statistics", icon = icon("table"),
      menuItem(
        "Descriptives", tabName = "stats-descriptives",
        menuItem(
          "Commodity futures", tabName = "stats-commodities",
          menuItem(
            "Market variables", tabName = "stats-commodities-market",
            menuSubItem(
              "Individuals", tabName = "stats-commodities-market-individuals"
            ),
            menuSubItem(
              "groups", tabName = "stats-commodities-market-groups"
            )
          ),
          menuItem(
            "Positions (pressure)", tabName = "stats-commodities-positions",
            menuSubItem(
              "Individuals", tabName = "stats-commodities-positions-individuals"
            ),
            menuSubItem(
              "groups", tabName = "stats-commodities-positions-groups"
            )
          )
        ),
        menuItem(
          "Factors", tabName = "stats-factors",
          menuSubItem(
            "Asset pool: US commodities", tabName = "stats-factors-assetpool-UScommos"
          ),
          menuSubItem(
            "Asset pool: factor picks", tabName = "stats-factors-assetpool-picks"
          )
        )
      ),
      menuItem(
        "Differences", tabName = "differences",
        menuItem(
          "Between periods", tabName = "differences-periods",
          menuItem(
            "Commodity futures", tabName = "differences-periods-commodities",
            menuItem(
              "Market variables", tabName = "differences-periods-commodities-market",
              menuSubItem(
                "Individuals", tabName = "differences-periods-commodities-market-individuals"
              ),
              menuSubItem(
                "groups", tabName = "differences-periods-commodities-market-groups"
              )
            ),
            menuItem(
              "Positions (pressure)", tabName = "differences-periods-commodities-positions",
              menuSubItem(
                "Individuals", tabName = "differences-periods-commodities-positions-individuals"
              ),
              menuSubItem(
                "groups", tabName = "differences-periods-commodities-positions-groups"
              )
            )
          ),
          menuItem(
            "Factors", tabName = "differences-periods-factors",
            menuSubItem(
              "Asset pool: US commodities", tabName = "differences-periods-factors-assetpool-UScommos"
            )
          )
        ),
        menuItem(
          "Between regimes", tabName = "differences-regimes",
          menuItem(
            "Commodity futures", tabName = "differences-periods-commodities",
            menuItem(
              "Market variables", tabName = "differences-regimes-commodities-market",
              menuSubItem(
                "Individuals", tabName = "differences-regimes-commodities-market-individuals"
              ),
              menuSubItem(
                "groups", tabName = "differences-regimes-commodities-market-groups"
              )
            )
          ),
          menuItem(
            "Factors", tabName = "differences-regimes-factors",
            menuSubItem(
              "Asset pool: US commodities", tabName = "differences-regimes-factors-assetpool-UScommos"
            ),
            menuSubItem(
              "Asset pool: factor picks", tabName = "differences-regimes-factors-assetpool-picks"
            )
          )
        )
      )
    ),
    menuItem("Factor picks", tabName = "factor-picks", icon = icon("table")),
    menuItem(
      "Regressions", tabName = "regressions", icon = icon("table"),
      menuSubItem("Time series", tabName = "regressions-time-series"),
      menuSubItem("Cross section", tabName = "regressions-cross-section")
    ),
    menuItem(
      "Correlations", tabName = "correlations", icon = icon("table"),
      menuSubItem("US commodities", tabName = "correlations-UScommos"),
      menuSubItem(
        "US commodities (no metals)", tabName = "correlations-UScommos-nometals"
      ),
      menuSubItem("US metals", tabName = "correlations-USmetals"),
      menuSubItem("UK metals", tabName = "correlations-UKmetals"),
      # menuSubItem("US energy", tabName = "correlations-USenergy"),
      menuSubItem("Factor picks", tabName = "correlations-picks")
    ),
    menuItem("Proportions", tabName = "proportions", icon = icon("table"))
  )
)



body <- dashboardBody(
  tabItems(
    
    
    # tabItem(
    #   tabName = "globals",
    #   
    #   fluidRow(
    #     column(
    #       3, align = "left",
    #       h3("Periods:"),
    #       p(linebreaks(2L)),
    #       tableOutput("periods")
    #     ),
    #     column(
    #       9, align = "left",
    #       h3("Factors construction parameters:"),
    #       p(linebreaks(2L)),
    #       tableOutput("factor_parameters")
    #     )
    #   )
    # ),
    
    tabItem(
      tabName = "globals",
      
      fluidRow(
        column(
          3, align = "left",
          h3("Periods:"),
          p(linebreaks(2L)),
          tableOutput("periods")
        ),
        column(
          9, align = "left",
          h3("Factors construction parameters:"),
          p(linebreaks(2L)),
          fluidRow(
            column(
              3L, align = "left",
              selectInput(
                "factors_param_update_freq", "update frequency",
                choices = unique(factor_parameters$`update frequency`),
                selected = "week", multiple = F
              )
            ),
            column(
              3L, align = "left",
              selectInput(
                "factors_param_return_freq", "return frequency",
                choices = unique(factor_parameters$`return frequency`),
                selected = "day", multiple = F
              )
            ),
            column(
              3L, align = "left",
              selectInput(
                "factors_param_ranking_period", "ranking period",
                choices = unique(factor_parameters$`ranking period`),
                selected = 26L, multiple = F
              )
            )
          ),
          fluidRow(
            column(
              3L, align = "left",
              selectInput(
                "factors_param_long_threshold", "long threshold",
                choices = unique(factor_parameters$`long threshold`),
                selected = "", multiple = F
              )
            ),
            column(
              3L, align = "left",
              selectInput(
                "factors_param_short_threshold", "short threshold",
                choices = unique(factor_parameters$`short threshold`),
                selected = "", multiple = F
              )
            ),
            column(
              3L, align = "left",
              selectInput(
                "factors_param_short_threshold", "weighted",
                choices = unique(factor_parameters$weighted),
                selected = "", multiple = F
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6, align = "left",
          h3("Others:"),
          p(linebreaks(1L)),
          h4("Carry = (first/second) - 1L"),
          h4("Regressions: excess returns")
        )
      )
    ),
    
    # `update frequency` == input$factors_param_update_freq,
    # `return frequency` == input$factors_param_return_freq,
    # `ranking period` == input$factors_param_ranking_period,
    # `long threshold` == input$factors_param_long_threshold,
    # `short threshold` == input$factors_param_short_threshold
    
    
    
    
    # descriptive statistics
    
    ## commodity futures
    
    ### market variables
    
    #### individual commodities
    
    
    tabItem(
      tabName = "stats-commodities-market-individuals",
      
      
      ##### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - market variables - individuals - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_levels_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_periods_levels$commodity
              ),
              "all"
            ),
            selected = "all",
            multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_levels_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_individuals_periods_levels$field
            ),
            selected = "close price",
            multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_levels_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_periods_levels$period
              ),
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_levels_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_individuals_periods_levels$regime
            ),
            selected = "",
            multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_individuals_periods_levels_download',
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_individuals_periods_levels"),
      p(linebreaks(2L)),
      
      
      
      ##### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - individuals - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_returns_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_periods_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_returns_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_individuals_periods_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_market_individuals_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_returns_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_periods_returns$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_returns_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_individuals_periods_returns$regime
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_periods_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_individuals_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_individuals_periods_returns"),
      p(linebreaks(7L)),
      
      
      ##### by year
      h2("By year"),
      ###### levels
      h3("Levels"),
      
      # Commodity futures - market variables - individuals - by year - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_levels_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_years_levels$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_levels_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_individuals_years_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_levels_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_levels_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_individuals_years_levels$regime
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_individuals_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_individuals_years_levels"),
      p(linebreaks(2L)),
      
      
      
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - individuals - by year - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_returns_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_years_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_returns_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_individuals_years_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_market_individuals_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_returns_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_market_individuals_years_returns$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_returns_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_individuals_years_returns$regime
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_individuals_years_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_individuals_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_individuals_years_returns")
    ),
    
    
    
    #### Commodity groups
    
    tabItem(
      tabName = "stats-commodities-market-groups",
      
      
      #### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - market variables - groups - by period - levels
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_periods_levels$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_levels$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_levels$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_periods_levels$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_levels$regime
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        ) 
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_groups_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_groups_periods_levels"),
      p(linebreaks(2L)),
      
      
      ##### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - groups - by period - returns
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_periods_returns$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_returns$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_periods_returns$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_groups_periods_returns$regime
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_periods_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_groups_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_groups_periods_returns"),
      p(linebreaks(7L)),
      
      
      
      ##### by year
      h2("By year"),
      
      ###### levels
      h3("Levels"),
      
      
      # Commodity futures - market variables - groups - by year - levels
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_years_levels$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_market_groups_years_levels$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_market_groups_years_levels$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_groups_years_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_groups_years_levels$regime
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_groups_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_groups_years_levels"),
      p(linebreaks(2L)),
      
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - groups - by year - returns
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_years_returns$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_market_groups_years_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_market_groups_years_returns$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_field",
            "field",
            choices = unique(
              statistics_commodity_futures_market_groups_years_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_market_groups_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_market_groups_years_returns$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_regime",
            "regime",
            choices = unique(
              statistics_commodity_futures_market_groups_years_returns$regime
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_market_groups_years_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_market_groups_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_market_groups_years_returns")
    ),
    
    
    
    
    ### position variables
    
    #### individual commodities
    
    tabItem(
      tabName = "stats-commodities-positions-individuals",
      
      
      ##### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - position variables - individuals - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_levels_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_periods_levels$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_levels_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_individuals_periods_levels$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_levels_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_periods_levels$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_individuals_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_individuals_periods_levels"),
      p(linebreaks(2L)),
      
      
      
      ##### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - position variables - individuals - by period - returns
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_returns_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_periods_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_returns_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_individuals_periods_returns$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_cftc_individuals_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_returns_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_periods_returns$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_periods_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_individuals_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_individuals_periods_returns"),
      p(linebreaks(7L)),
      
      
      ##### by year
      h2("By year"),
      
      ###### levels
      h3("Levels"),
      
      # Commodity futures - position variables - individuals - by year - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_levels_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_years_levels$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_levels_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_individuals_years_levels$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_levels_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_individuals_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_individuals_years_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - position variables - individuals - by year - returns
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_returns_commodity",
            "commodity",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_years_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_returns_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_individuals_years_returns$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_cftc_individuals_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_returns_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_individuals_years_returns$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_individuals_years_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_individuals_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_individuals_years_returns")
    ),
    
    
    
    
    
    #### Commodity groups
    
    tabItem(
      tabName = "stats-commodities-positions-groups",
      
      
      #### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - position variables - groups - by period - levels
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_levels_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_periods_levels$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_levels_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_levels$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_levels_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_levels$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_levels_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_levels$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_levels_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_periods_levels$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_groups_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_groups_periods_levels"),
      p(linebreaks(2L)),
      
      
      ##### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - position variables - groups - by period - returns
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_periods_returns$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_returns$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_returns$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_cftc_groups_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_period",
            "period",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_periods_returns$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_periods_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_groups_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_groups_periods_returns"),
      p(linebreaks(7L)),
      
      
      
      ##### by year
      h2("By year"),
      
      ###### levels
      h3("Levels"),
      
      
      # Commodity futures - position variables - groups - by year - levels
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_levels_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_years_levels$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_levels_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_levels$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_levels_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_levels$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_levels_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_levels$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_levels_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_levels_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_groups_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_groups_years_levels"),
      p(linebreaks(2L)),
      
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - position variables - groups - by year - returns
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_country",
            "country",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_years_returns$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_sector",
            "sector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_subsector",
            "subsector",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_returns$subsector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_participant",
            "participant",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_returns$participant
            ),
            selected = "commercial", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_frequency",
            "frequency",
            choices = unique(
              statistics_commodity_futures_cftc_groups_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_year",
            "year",
            choices = c(
              unique(
                statistics_commodity_futures_cftc_groups_years_returns$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_commodity_futures_cftc_groups_years_returns_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_commodity_futures_cftc_groups_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_commodity_futures_cftc_groups_years_returns")
    ),
    
    
    
    
    ## factors
    
    ### asset pool: US commodities
    
    tabItem(
      tabName = "stats-factors-assetpool-UScommos",
      
      
      #### by period
      h2("By period"),
      
      ##### returns
      h3("Returns"),
      
      # Factors - asset pool: US commodities - by period
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_periods_factor", 
            "factor",
            choices = c(
              unique(
                statistics_factors_US_commos_periods$name
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_periods_leg", 
            "leg",
            choices = c(
              unique(
                statistics_factors_US_commos_periods$leg
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_periods_period", 
            "period",
            choices = c(
              unique(
                statistics_factors_US_commos_periods$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_periods_regime", 
            "regime",
            choices = unique(
              statistics_factors_US_commos_periods$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_periods_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_factors_US_commos_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_factors_US_commos_periods_returns"),
      p(linebreaks(7L)),
      
      
      
      #### by year
      h2("By year"),
      
      ##### returns
      h3("Returns"),
      
      # Factors - asset pool: US commodities - by year
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_years_factor", 
            "factor",
            choices = c(
              unique(
                statistics_factors_US_commos_years$name
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_years_leg", 
            "leg",
            choices = c(unique( statistics_factors_US_commos_years$leg ), "all"),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_years_year", 
            "year",
            choices = c(
              unique(
                statistics_factors_US_commos_years$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_years_regime", 
            "regime",
            choices = unique(
              statistics_factors_US_commos_years$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_US_commos_years_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_factors_US_commos_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_factors_US_commos_years_returns")
    ),
    
    
    
    
    ### asset pool: factor picks
    
    tabItem(
      tabName = "stats-factors-assetpool-picks",
      
      #### by Period
      h2("By period"),
      
      ##### returns
      h3("Returns"),
      
      # Factors - asset pool: factor picks - by period
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_picking_factor", 
            "picking factor",
            choices = unique(
              statistics_factors_factor_picks_periods$`picking factor name`
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              statistics_factors_factor_picks_periods$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              statistics_factors_factor_picks_periods$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        ), 
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_factor", 
            "factor",
            choices = c(
              unique(
                statistics_factors_factor_picks_periods$name
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_leg", 
            "leg",
            choices = c(
              unique(
                statistics_factors_factor_picks_periods$leg
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_period", 
            "period",
            choices = c(
              unique(
                statistics_factors_factor_picks_periods$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_regime", 
            "regime",
            choices = unique(
              statistics_factors_factor_picks_periods$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_factor_picks_periods_summary",
            "summary",
            choices = c("average", "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_factors_factor_picks_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_factors_factor_picks_periods_returns"),
      p(linebreaks(7L)),
      
      
      
      #### by year
      h2("By year"),
      
      ##### returns
      h3("Returns"),
      
      # Factors - asset pool: factor picks - by year
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_picking_factor", 
            "picking factor",
            choices = unique(
              statistics_factors_factor_picks_years$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              statistics_factors_factor_picks_years$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              statistics_factors_factor_picks_years$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        ), 
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_factor", 
            "factor",
            choices = c(
              unique(
                statistics_factors_factor_picks_years$name
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_leg", 
            "leg",
            choices = c(
              unique(
                statistics_factors_factor_picks_years$leg
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_year", 
            "year",
            choices = c(
              unique(
                statistics_factors_factor_picks_years$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_regime", 
            "regime",
            choices = unique(
              statistics_factors_factor_picks_years$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "statistics_factors_factor_picks_years_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'statistics_factors_factor_picks_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("statistics_factors_factor_picks_years_returns")
    ),
    
    
    
    
    
    
    
    
    
    # tests of difference
    
    ## periods
    
    ### commodity futures
    
    #### market variables
    
    ##### individual commodities
    tabItem(
      tabName = "differences-periods-commodities-market-individuals",
      
      ##### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - market variables - individuals - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_levels_commodity",
            "commodity",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_individuals_levels$commodity
              ),
              "all"
            ), selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_levels_field",
            "field",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_levels_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_individuals_levels$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_levels_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_levels$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_levels_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_levels$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_levels_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_levels$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_market_individuals_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_market_individuals_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - individuals - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_commodity",
            "commodity",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_individuals_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_field",
            "field",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_frequency",
            "frequency",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_individuals_returns$moment
              ), "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_returns$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_returns$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_individuals_returns_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_market_individuals_returns$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_market_individuals_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_market_individuals_returns")
    ),
    
    
    
    ##### groups
    tabItem(
      tabName = "differences-periods-commodities-market-groups",
      
      
      ##### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - market variables - groups - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_country",
            "country",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_groups_levels$country
              ),
              "all"
            ),
            selected = "all",multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_sector",
            "sector",
            choices = unique(
              differences_periods_commodity_futures_market_groups_levels$sector
            ),
            selected = "all",multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_subsector",
            "subsector",
            choices = unique(
              differences_periods_commodity_futures_market_groups_levels$subsector
            ),
            selected = "all",multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_field",
            "field",
            choices = unique(
              differences_periods_commodity_futures_market_groups_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_groups_levels$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_market_groups_levels$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_market_groups_levels$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_levels_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_market_groups_levels$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_market_groups_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_market_groups_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - groups - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_country",
            "country",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_groups_returns$country
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_sector",
            "sector",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_subsector",
            "subsector",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$subsector
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_field",
            "field",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_frequency",
            "frequency",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_market_groups_returns$moment
              ), "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_market_groups_returns_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_market_groups_returns$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_market_groups_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_market_groups_returns")
      
    ),
    
    
    
    #### positions
    
    ##### individual commodities
    tabItem(
      tabName = "differences-periods-commodities-positions-individuals",
      
      
      ###### by period
      h2("By period"),
      
      ###### levels
      h3("Levels"),
      
      # Commodity futures - positions - individuals - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_levels_commodity",
            "commodity",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_individuals_levels$commodity
              ),
              "all"
            ),
            selected = "all",multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_levels_participant",
            "participant",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_levels$participant
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_levels_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_individuals_levels$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_levels_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_levels$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_levels_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_levels$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_levels_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_levels$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_positions_individuals_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_positions_individuals_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - positions - individuals - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_commodity",
            "commodity",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_individuals_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_participant",
            "participant",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_returns$participant
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_frequency",
            "frequency",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_individuals_returns$moment
              ), "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_returns$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_returns$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_individuals_returns_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_positions_individuals_returns$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_positions_individuals_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_positions_individuals_returns")
    ),
    
    
    
    ##### groups
    tabItem(
      tabName = "differences-periods-commodities-positions-groups",
      
      
      ###### by period
      h2("By period"),
      
      ###### levels
      h3("Levels"),
      
      # Commodity futures - positions - groups - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_country",
            "country",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_groups_levels$country
              ),
              "all"
            ),
            selected = "all",multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_sector",
            "sector",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_levels$sector
            ),
            selected = "all",multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_subsector",
            "subsector",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_levels$subsector
            ),
            selected = "all",multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_participant",
            "participant",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_levels$participant
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_groups_levels$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_levels$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_levels$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_levels_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_levels$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_positions_groups_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_positions_groups_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - positions - groups - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_country",
            "country",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_groups_returns$country
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_sector",
            "sector",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_subsector",
            "subsector",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$subsector
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_participant",
            "participant",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$participant
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_frequency",
            "frequency",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_moment",
            "moment",
            choices = c(
              unique(
                differences_periods_commodity_futures_positions_groups_returns$moment
              ), "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_period1",
            "period 1",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_period2",
            "period 2",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_commodity_futures_positions_groups_returns_regime",
            "regime",
            choices = unique(
              differences_periods_commodity_futures_positions_groups_returns$regime
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_commodity_futures_positions_groups_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_commodity_futures_positions_groups_returns")
    ),
    
    
    ### factors
    
    #### asset pool: US commodities
    
    tabItem(
      tabName = "differences-periods-factors-assetpool-UScommos",
      
      
      ##### by period
      h2("By period"),
      
      ###### returns
      h3("Returns"),
      
      # Factors - asset pool: US commodities - by period
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_factors_US_commos_factor", 
            "factor",
            choices = c(
              unique(
                differences_periods_factors_US_commos$name
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_factors_US_commos_leg", 
            "leg",
            choices = c(
              unique(
                differences_periods_factors_US_commos$leg
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_factors_US_commos_period1", 
            "period 1",
            choices = unique(
              differences_periods_factors_US_commos$`period 1`
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_factors_US_commos_period2", 
            "period 2",
            choices = unique(
              differences_periods_factors_US_commos$`period 2`
            ),
            selected = "financialization", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_periods_factors_US_commos_regime", 
            "regime",
            choices = unique(
              differences_periods_factors_US_commos$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_periods_factors_US_commos_moment", 
            "moment",
            choices = unique(
              differences_periods_factors_US_commos$moment
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_periods_factors_US_commos_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_periods_factors_US_commos_returns")
    ),
    
    
    
    
    # regimes
    
    ### commodity futures
    
    #### market variables
    
    ##### individual commodities
    tabItem(
      tabName = "differences-regimes-commodities-market-individuals",
      
      
      ##### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - market variables - individuals - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_levels_commodity",
            "commodity",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_individuals_levels$commodity
              ),
              "all"
            ), selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_levels_field",
            "field",
            choices = unique(
              differences_regimes_commodity_futures_market_individuals_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_levels_moment",
            "moment",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_individuals_levels$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_levels_period",
            "period",
            choices = unique(
              differences_regimes_commodity_futures_market_individuals_levels$period
            ),
            selected = "past", multiple = F
          )
        )
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_regimes_commodity_futures_market_individuals_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_regimes_commodity_futures_market_individuals_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - individuals - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_returns_commodity",
            "commodity",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_individuals_returns$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_returns_field",
            "field",
            choices = unique(
              differences_regimes_commodity_futures_market_individuals_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_returns_frequency",
            "frequency",
            choices = unique(
              differences_regimes_commodity_futures_market_individuals_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_returns_moment",
            "moment",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_individuals_returns$moment
              ), "all"
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_individuals_returns_period",
            "period",
            choices = unique(
              differences_regimes_commodity_futures_market_individuals_returns$period
            ),
            selected = "past", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_regimes_commodity_futures_market_individuals_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_regimes_commodity_futures_market_individuals_returns")
      
    ),
    
    
    
    ##### groups
    tabItem(
      tabName = "differences-regimes-commodities-market-groups",
      
      
      ##### by period
      h2("By period"),
      
      ##### levels
      h3("Levels"),
      
      # Commodity futures - market variables - groups - by period - levels
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_levels_country",
            "country",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_groups_levels$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_levels_sector",
            "sector",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_levels$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_levels_subsector",
            "subsector",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_levels$subsector
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_levels_field",
            "field",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_levels_period",
            "period",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_levels$period
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_levels_moment",
            "moment",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_groups_levels$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_regimes_commodity_futures_market_groups_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_regimes_commodity_futures_market_groups_levels"),
      p(linebreaks(2L)),
      
      
      
      ###### returns
      h3("Relative changes (returns - annualised)"),
      
      # Commodity futures - market variables - groups - by period - returns
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_country",
            "country",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_groups_returns$country
              ),
              "all"
            ),
            selected = "US", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_sector",
            "sector",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_returns$sector
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_subsector",
            "subsector",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_returns$subsector
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_field",
            "field",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_frequency",
            "frequency",
            choices = unique(
              differences_regimes_commodity_futures_market_groups_returns$frequency
            ),
            selected = "day", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_period",
            "period",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_groups_returns$period
              ), "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_commodity_futures_market_groups_returns_moment",
            "moment",
            choices = c(
              unique(
                differences_regimes_commodity_futures_market_groups_returns$moment
              ), "all"
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_regimes_commodity_futures_market_groups_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_regimes_commodity_futures_market_groups_returns")
    ),
    
    
    ### factors
    
    #### asset pool: US commodities
    
    tabItem(
      tabName = "differences-regimes-factors-assetpool-UScommos",
      
      
      ##### by period
      h2("By period"),
      
      ###### returns
      h3("Returns"),
      
      # Factors - asset pool: US commodities - by period
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_US_commos_factor", 
            "factor",
            choices = c(
              unique(
                differences_regimes_factors_US_commos$name
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_US_commos_leg", 
            "leg",
            choices = c(
              unique(
                differences_regimes_factors_US_commos$leg
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_US_commos_period", 
            "period",
            choices = unique(
              differences_regimes_factors_US_commos$period
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_US_commos_moment", 
            "moment",
            choices = unique(
              differences_regimes_factors_US_commos$moment
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_regimes_factors_US_commos_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_regimes_factors_US_commos_returns")
    ),
    
    
    ### asset pool: factor picks
    tabItem(
      tabName = "differences-regimes-factors-assetpool-picks",
      
      #### by Period
      h2("By period"),
      
      ##### returns
      h3("Returns"),
      
      # Factors - asset pool: factor picks - by period
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_picking_factor", 
            "picking factor",
            choices = unique(
              differences_regimes_factors_from_picks_periods$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              differences_regimes_factors_from_picks_periods$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              differences_regimes_factors_from_picks_periods$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        ), 
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_factor", 
            "factor",
            choices = c(
              unique(
                differences_regimes_factors_from_picks_periods$`factor name`
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_leg", 
            "leg",
            choices = c(
              unique(
                differences_regimes_factors_from_picks_periods$leg
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_period", 
            "period",
            choices = c(
              unique(
                differences_regimes_factors_from_picks_periods$period
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "differences_regimes_factors_from_picks_periods_moment", 
            "moment",
            choices = unique(
              differences_regimes_factors_from_picks_periods$moment
            ),
            selected = "mean", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          downloadButton(
            'differences_regimes_factors_from_picks_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("differences_regimes_factors_from_picks_periods_returns"),
      
    ),
    
    
    
    
    
    
    
    # factor picks
    
    tabItem(
      tabName = "factor-picks",
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "picks_factor", 
            "factor",
            choices = c(unique(picks$factor), "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "picks_factor_asset_pool", 
            "asset pool",
            choices = unique(picks$`asset pool`),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "picks_factor_leg", 
            "leg",
            choices = unique(picks$leg),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "picks_period", 
            "period",
            choices = unique(picks$period),
            selected = "past", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", downloadButton('factor_picks_download', 'Download .csv')
        )
      ),
      
      tableOutput("picks")
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # regressions
    
    ## time series
    
    tabItem(
      tabName = "regressions-time-series",
      
      
      
      
      
      ## US commodity returns ~ US commodity individual CHP
      
      h2("US commodities ~ US commodity individual CHP"),
      
      
      p(linebreaks(1L)),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_sector", 
            "sector",
            choices = c("all", unique(time_series_US_commos_vs_US_individual_CHP$sector)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_subsector", 
            "subsector",
            choices = c("all", unique(time_series_US_commos_vs_US_individual_CHP$subsector)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_regime_CHP_type", 
            "CHP type (for regimes)",
            choices = c("all", unique(time_series_US_commos_vs_US_individual_CHP$regime.CHP.type)),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_regime", 
            "regime",
            choices = unique(time_series_US_commos_vs_US_individual_CHP$CHP.regime),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_regressor", 
            "regressor",
            choices = c("all", unique(time_series_US_commos_vs_US_individual_CHP$regressor)),
            selected = "pressure change contemporaneous", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_commodity", 
            "regressand (commodity)",
            choices = c("all", unique(time_series_US_commos_vs_US_individual_CHP$commodity)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_period", 
            "period",
            choices = c(
              "all", 
              as.character(unique(time_series_US_commos_vs_US_individual_CHP$period))
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "rsquared", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_individual_CHP_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_US_commos_vs_US_individual_CHP_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("time_series_US_commos_vs_US_individual_CHP"),
      p(linebreaks(2L)),
      
      

      
      ## US commodity returns ~ US commodity aggregate CHP
      
      h2("US commodities ~ US commodity aggregate CHP"),
      
      
      p(linebreaks(1L)),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_sector", 
            "sector",
            choices = c("all", unique(time_series_US_commos_vs_US_aggregate_CHP$sector)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_subsector", 
            "subsector",
            choices = c("all", unique(time_series_US_commos_vs_US_aggregate_CHP$subsector)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_regime_CHP_type", 
            "CHP type (for regimes)",
            choices = c("all", unique(time_series_US_commos_vs_US_aggregate_CHP$regime.CHP.type)),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_regime", 
            "regime",
            choices = unique(time_series_US_commos_vs_US_aggregate_CHP$CHP.regime),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_regressor", 
            "regressor",
            choices = c("all", unique(time_series_US_commos_vs_US_aggregate_CHP$regressor)),
            selected = "pressure change contemporaneous", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_commodity", 
            "regressand (commodity)",
            choices = c("all", unique(time_series_US_commos_vs_US_aggregate_CHP$commodity)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_period", 
            "period",
            choices = c(
              "all", 
              as.character(unique(time_series_US_commos_vs_US_aggregate_CHP$period))
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "rsquared", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_US_aggregate_CHP_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_US_commos_vs_US_aggregate_CHP_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("time_series_US_commos_vs_US_aggregate_CHP"),
      p(linebreaks(2L)),


      
      
      ## UK commodity returns ~ US commodity aggregate CHP
      
      h2("UK commodities ~ US commodity aggregate CHP"),
      
      
      p(linebreaks(1L)),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_sector", 
            "sector",
            choices = c("all", unique(time_series_UK_commos_vs_US_aggregate_CHP$sector)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_subsector", 
            "subsector",
            choices = c("all", unique(time_series_UK_commos_vs_US_aggregate_CHP$subsector)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_regime_CHP_type", 
            "CHP type (for regimes)",
            choices = c("all", unique(time_series_UK_commos_vs_US_aggregate_CHP$regime.CHP.type)),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_regime", 
            "regime",
            choices = unique(time_series_UK_commos_vs_US_aggregate_CHP$CHP.regime),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_regressor", 
            "regressor",
            choices = c("all", unique(time_series_UK_commos_vs_US_aggregate_CHP$regressor)),
            selected = "pressure change contemporaneous", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_commodity", 
            "regressand (commodity)",
            choices = c("all", unique(time_series_UK_commos_vs_US_aggregate_CHP$commodity)),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_period", 
            "period",
            choices = c(
              "all", 
              as.character(unique(time_series_UK_commos_vs_US_aggregate_CHP$period))
            ),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "rsquared", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_commos_vs_US_aggregate_CHP_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_UK_commos_vs_US_aggregate_CHP_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("time_series_UK_commos_vs_US_aggregate_CHP"),
      p(linebreaks(2L)),

            
            
      
      
      ## US commodities ~ factors from US commodities
      
      h2("US commodities ~ factors from US commodities"),
      
      strong(
        "Access full model by selecting market, CHP, OI nearby, OI aggregate and
      term structure; in that order."
      ),
      
      p(linebreaks(1L)),
      
      # Factors - asset pool: US commodities - by period
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"), 
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_asset_pool", 
            "factor asset pool",
            choices = unique(time_series_US_commos_vs_factors_from_US_commos$`asset pool`),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_factor_leg", 
            "leg",
            choices = c(unique(time_series_US_commos_vs_factors_from_US_commos$leg), "all"),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_period", 
            "period",
            choices = c(unique(time_series_US_commos_vs_factors_from_US_commos$period), "all"),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_regime", 
            "regime",
            choices = unique(time_series_US_commos_vs_factors_from_US_commos$regime),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_commo", 
            "regressand (commodity)",
            choices = c(
              unique(
                time_series_US_commos_vs_factors_from_US_commos$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "coefficients", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commos_vs_factors_from_US_commos_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_US_commos_vs_factors_from_US_commos_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("time_series_US_commos_vs_factors_from_US_commos"),
      p(linebreaks(2L)),
      
      
      
      
      ## factor picks ~ factors from picks
      
      h2("factor picks ~ factors from picks"),
      
      # factor picks ~ factors from picks
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_picking_factor", 
            "picking factor",
            choices = unique(
              time_series_factor_picks_vs_factors_from_picks$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              time_series_factor_picks_vs_factors_from_picks$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              time_series_factor_picks_vs_factors_from_picks$`picking factor leg`
            ),
            selected = "factor", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_leg", 
            "leg",
            choices = c("long", "short", "factor"),
            selected = "factor", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_period", 
            "period",
            choices = c(
              unique(
                time_series_factor_picks_vs_factors_from_picks$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_regime", 
            "regime",
            choices = unique(
              time_series_factor_picks_vs_factors_from_picks$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_pick", 
            "regressand (factor pick)",
            choices = c(unique(time_series_factor_picks_vs_factors_from_picks$pick), "all"),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "coefficients", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_factor_picks_vs_factors_from_picks_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_factor_picks_vs_factors_from_picks_download', 
            'Download .csv'
          )
        )
      ),
      
      
      tableOutput("time_series_factor_picks_vs_factors_from_picks"),
      p(linebreaks(2L)),    
      
      
      
      
      h2("US commodities ~ factors from picks"),
      
      # US commodities ~ factors from picks
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_picking_factor", 
            "picking factor",
            choices = unique(
              time_series_US_commodities_vs_factors_from_picks$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              time_series_US_commodities_vs_factors_from_picks$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              time_series_US_commodities_vs_factors_from_picks$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_leg", 
            "leg",
            choices = c(
              unique(
                time_series_US_commodities_vs_factors_from_picks$`factor leg`
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        )
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_period", 
            "period",
            choices = c(
              unique(
                time_series_US_commodities_vs_factors_from_picks$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_regime", 
            "regime",
            choices = unique(
              time_series_US_commodities_vs_factors_from_picks$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_commodity", 
            "regressand (commodity)",
            choices = c(
              unique(
                time_series_US_commodities_vs_factors_from_picks$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_US_commodities_vs_factors_from_picks_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_US_commodities_vs_factors_from_picks_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("time_series_US_commodities_vs_factors_from_picks"),
      p(linebreaks(2L)),
      
      
      
      ## UK metals ~ factors from picks 
      
      h2("UK metals ~ factors from picks"),
      
      # UK metals ~ factors from picks
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_picking_factor", 
            "picking factor",
            choices = unique(
              time_series_UK_metals_vs_factors_from_picks$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              time_series_UK_metals_vs_factors_from_picks$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              time_series_UK_metals_vs_factors_from_picks$`picking factor leg`
            ),
            selected = "factor", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        )
      ),
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_leg", 
            "leg",
            choices = c(
              unique(
                time_series_UK_metals_vs_factors_from_picks$`factor leg`
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_period", 
            "period",
            choices = c(
              unique(
                time_series_UK_metals_vs_factors_from_picks$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_regime", 
            "regime",
            choices = unique(
              time_series_UK_metals_vs_factors_from_picks$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_commodity", 
            "regressand (commodity)",
            choices = c(
              unique(
                time_series_UK_metals_vs_factors_from_picks$commodity
              ),
              "all"
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_show", 
            "show",
            choices = c("coefficients", "rsquared"),
            selected = "coefficients", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "time_series_UK_metals_vs_factors_from_picks_summary", 
            "summary",
            choices = c("none", "average"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'time_series_UK_metals_vs_factors_from_picks_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("time_series_UK_metals_vs_factors_from_picks"),
      p(linebreaks(2L))
      
      
      
      # ## UK metals ~ factors from picks  (regressor: factor leg)
      # 
      # h2("UK metals ~ factors from picks (regressor: factor leg)"),
      # 
      # # UK metals ~ factors from picks (regressor: factor leg)
      # fluidRow(
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_picking_factor", 
      #       "picking factor",
      #       choices = unique(
      #         time_series_UK_metals_vs_factors_from_picks_legs$`picking factor name`
      #       ),
      #       selected = "market", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_picking_factor_asset_pool", 
      #       "picking factor asset pool",
      #       choices = unique(
      #         time_series_UK_metals_vs_factors_from_picks_legs$`picking factor asset pool`
      #       ),
      #       selected = "", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_picking_factor_leg", 
      #       "picking factor leg",
      #       choices = unique(
      #         time_series_UK_metals_vs_factors_from_picks_legs$`picking factor leg`
      #       ),
      #       selected = "", multiple = F
      #     )
      #   )
      # ),
      # 
      # fluidRow(
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_factor_1", 
      #       "regressor 1 (factor)",
      #       choices = c(regression_factors, "all"),
      #       selected = "CHP", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_factor_2", 
      #       "regressor 2 (factor)",
      #       choices = c(regression_factors, "none"),
      #       selected = "none", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_factor_3", 
      #       "regressor 3 (factor)",
      #       choices = c(regression_factors, "none"),
      #       selected = "none", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_factor_4", 
      #       "regressor 4 (factor)",
      #       choices = c(regression_factors, "none"),
      #       selected = "none", multiple = F
      #     )
      #   )
      # ),
      # 
      # fluidRow(
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_factor_5", 
      #       "regressor 5 (factor)",
      #       choices = c(regression_factors, "none"),
      #       selected = "none", multiple = F
      #     )
      #   ),
      #   column(
      #     2, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_leg", 
      #       "leg",
      #       choices = c(
      #         unique(
      #           time_series_UK_metals_vs_factors_from_picks_legs$leg
      #         ),
      #         "all"
      #       ),
      #       selected = "factor", multiple = F
      #     )
      #   )
      # ),
      # 
      # fluidRow(
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_period", 
      #       "period",
      #       choices = c(
      #         unique(
      #           time_series_UK_metals_vs_factors_from_picks_legs$period
      #         ),
      #         "all"
      #       ),
      #       selected = "past", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_regime", 
      #       "regime",
      #       choices = unique(
      #         time_series_UK_metals_vs_factors_from_picks_legs$regime
      #       ),
      #       selected = "all", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_commodity", 
      #       "regressand (commodity)",
      #       choices = c(
      #         unique(
      #           time_series_UK_metals_vs_factors_from_picks_legs$commodity
      #         ),
      #         "all"
      #       ),
      #       selected = "all", multiple = F
      #     )
      #   ),
      #   column(
      #     3, align = "left",
      #     selectInput(
      #       "time_series_UK_metals_vs_factors_from_picks_legs_show", 
      #       "show",
      #       choices = c("coefficients", "rsquared"),
      #       selected = "coefficients", multiple = F
      #     )
      #   )
      # ),
      # 
      # tableOutput("time_series_UK_metals_vs_factors_from_picks_legs"),
      # p(linebreaks(10L)),
      
    ),
    
    
    
    
    
    ## cross-section
    
    tabItem(
      tabName = "regressions-cross-section",
      
      ## US commodities ~ factors from US commodities - multiple
      
      h2("US commodities ~ factors from US commodities"),
      
      strong(
        "Access full model by selecting market, CHP, OI nearby, OI aggregate and
      term structure; in that order."
      ),
      
      p(linebreaks(1L)),
      
      # Factors - asset pool: US commodities - by period
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_asset_pool", 
            "factor asset pool",
            choices = unique(cross_section_US_commos_vs_factors_from_US_commos$`asset pool`),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_factor_leg", 
            "leg",
            choices = c(unique(cross_section_US_commos_vs_factors_from_US_commos$leg), "all"),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_period", 
            "period",
            choices = c(unique(cross_section_US_commos_vs_factors_from_US_commos$period), "all"),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_regime", 
            "regime",
            choices = unique(cross_section_US_commos_vs_factors_from_US_commos$regime),
            selected = "all", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commos_vs_factors_from_US_commos_show", 
            "show",
            choices = c("lambdas", "betas"),
            selected = "lambdas", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'cross_section_US_commos_vs_factors_from_US_commos_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("cross_section_US_commos_vs_factors_from_US_commos"),
      p(linebreaks(2L)),
      
      
      
      
      ## factor picks ~ factors from picks
      
      h2("factor picks ~ factors from picks"),
      
      # factor picks ~ factors from picks
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_picking_factor", 
            "picking factor",
            choices = unique(
              cross_section_factor_picks_vs_factors_from_picks$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              cross_section_factor_picks_vs_factors_from_picks$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              cross_section_factor_picks_vs_factors_from_picks$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_leg", 
            "leg",
            choices = c("long", "short", "factor"),
            selected = "factor", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_period", 
            "period",
            choices = c(
              unique(
                cross_section_factor_picks_vs_factors_from_picks$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_regime", 
            "regime",
            choices = unique(
              cross_section_factor_picks_vs_factors_from_picks$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_factor_picks_vs_factors_from_picks_show", 
            "show",
            choices = c("lambdas", "betas"),
            selected = "lambdas", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'cross_section_factor_picks_vs_factors_from_picks_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("cross_section_factor_picks_vs_factors_from_picks"),
      p(linebreaks(2L)),    
      
      
      
      
      h2("US commodities ~ factors from picks"),
      
      # US commodities ~ factors from picks
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_picking_factor", 
            "picking factor",
            choices = unique(
              cross_section_US_commodities_vs_factors_from_picks$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              cross_section_US_commodities_vs_factors_from_picks$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              cross_section_US_commodities_vs_factors_from_picks$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_leg", 
            "leg",
            choices = c(
              unique(
                cross_section_US_commodities_vs_factors_from_picks$`factor leg`
              ),
              "all"
            ),
            selected = "factor", multiple = F
          )
        )
      ),
      
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_period", 
            "period",
            choices = c(
              unique(
                cross_section_US_commodities_vs_factors_from_picks$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_regime", 
            "regime",
            choices = unique(
              cross_section_US_commodities_vs_factors_from_picks$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_US_commodities_vs_factors_from_picks_show", 
            "show",
            choices = c("lambdas", "betas"),
            selected = "lambdas", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'cross_section_US_commodities_vs_factors_from_picks_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("cross_section_US_commodities_vs_factors_from_picks"),
      p(linebreaks(2L)),
      
      
      
      
      ## UK metals ~ factors from picks
      
      h2("UK metals ~ factors from picks"),
      
      # UK metals ~ factors from picks
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_picking_factor", 
            "picking factor",
            choices = unique(
              cross_section_UK_metals_vs_factors_from_picks$`picking factor name`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_picking_factor_asset_pool", 
            "picking factor asset pool",
            choices = unique(
              cross_section_UK_metals_vs_factors_from_picks$`picking factor asset pool`
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_picking_factor_leg", 
            "picking factor leg",
            choices = unique(
              cross_section_UK_metals_vs_factors_from_picks$`picking factor leg`
            ),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_factor_1", 
            "regressor 1 (factor)",
            choices = c(regression_factors, "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_factor_2", 
            "regressor 2 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_factor_3", 
            "regressor 3 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_factor_4", 
            "regressor 4 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        )
      ),
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_factor_5", 
            "regressor 5 (factor)",
            choices = c(regression_factors, "none"),
            selected = "none", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_leg", 
            "leg",
            choices = c(
              unique(
                cross_section_UK_metals_vs_factors_from_picks$`factor leg`
              ),
              "all"
            ),
            selected = "none", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_period", 
            "period",
            choices = c(
              unique(
                cross_section_UK_metals_vs_factors_from_picks$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_regime", 
            "regime",
            choices = unique(
              cross_section_UK_metals_vs_factors_from_picks$regime
            ),
            selected = "all", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "cross_section_UK_metals_vs_factors_from_picks_show", 
            "show",
            choices = c("lambdas", "betas"),
            selected = "lambdas", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'cross_section_UK_metals_vs_factors_from_picks_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("cross_section_UK_metals_vs_factors_from_picks"),
      p(linebreaks(2L))
      
      
      
      #   ## UK metals ~ factors from picks  (regressor: factor leg)
      #   
      #   h2("UK metals ~ factors from picks (regressor: factor leg)"),
      #   
      #   # UK metals ~ factors from picks (regressor: factor leg)
      #   fluidRow(
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_picking_factor", 
      #         "picking factor",
      #         choices = unique(
      #           cross_section_UK_metals_vs_factors_from_picks_legs$`picking factor name`
      #         ),
      #         selected = "market", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_picking_factor_asset_pool", 
      #         "picking factor asset pool",
      #         choices = unique(
      #           cross_section_UK_metals_vs_factors_from_picks_legs$`picking factor asset pool`
      #         ),
      #         selected = "", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_picking_factor_leg", 
      #         "picking factor leg",
      #         choices = unique(
      #           cross_section_UK_metals_vs_factors_from_picks_legs$`picking factor leg`
      #         ),
      #         selected = "", multiple = F
      #       )
      #     )
      #   ),
      #   
      #   fluidRow(
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_factor_1", 
      #         "regressor 1 (factor)",
      #         choices = c(regression_factors, "all"),
      #         selected = "CHP", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_factor_2", 
      #         "regressor 2 (factor)",
      #         choices = c(regression_factors, "none"),
      #         selected = "none", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_factor_3", 
      #         "regressor 3 (factor)",
      #         choices = c(regression_factors, "none"),
      #         selected = "none", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_factor_4", 
      #         "regressor 4 (factor)",
      #         choices = c(regression_factors, "none"),
      #         selected = "none", multiple = F
      #       )
      #     )
      #   ),
      #   
      #   fluidRow(
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_factor_5", 
      #         "regressor 5 (factor)",
      #         choices = c(regression_factors, "none"),
      #         selected = "none", multiple = F
      #       )
      #     )
      #   ),
      #   
      #   fluidRow(
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_leg", 
      #         "leg",
      #         choices = c(
      #           unique(
      #             cross_section_UK_metals_vs_factors_from_picks_legs$leg
      #           ),
      #           "all"
      #         ),
      #         selected = "factor", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_period", 
      #         "period",
      #         choices = c(
      #           unique(
      #             cross_section_UK_metals_vs_factors_from_picks_legs$period
      #           ),
      #           "all"
      #         ),
      #         selected = "past", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_regime", 
      #         "regime",
      #         choices = unique(
      #           cross_section_UK_metals_vs_factors_from_picks_legs$regime
      #         ),
      #         selected = "all", multiple = F
      #       )
      #     ),
      #     column(
      #       3, align = "left",
      #       selectInput(
      #         "cross_section_UK_metals_vs_factors_from_picks_legs_show", 
      #         "show",
      #         choices = c("lambdas", "betas"),
      #         selected = "lambdas", multiple = F
      #       )
      #     )
      #   ),
      #   
      #   tableOutput("cross_section_UK_metals_vs_factors_from_picks_legs"),
      #   p(linebreaks(10L))
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # correlations
    
    ## amongst US commodities
    
    tabItem(
      tabName = "correlations-UScommos",
      
      
      ### by period
      h3("By period"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_levels_field",
            "field",
            choices = unique(
              correlations_US_commodities_periods_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_levels_frequency",
            "frequency",
            choices = unique(
              correlations_US_commodities_periods_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_levels_period",
            "period",
            choices = c(
              unique(
                correlations_US_commodities_periods_levels$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_periods_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_periods_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_returns_field", 
            "field",
            choices = unique(
              correlations_US_commodities_periods_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_US_commodities_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_returns_period", 
            "period",
            choices = c(
              unique(
                correlations_US_commodities_periods_returns$period
              ), 
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_periods_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_periods_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_periods_returns"),
      p(linebreaks(5L)),
      
      
      ### by year
      h3("By year"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_levels_field",
            "field",
            choices = unique(
              correlations_US_commodities_years_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_levels_frequency",
            "frequency",
            choices = unique(
              correlations_US_commodities_years_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_levels_year",
            "year",
            choices = c(
              unique(
                correlations_US_commodities_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_years_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_years_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_returns_field", 
            "field",
            choices = unique(
              correlations_US_commodities_years_returns$field
            ),
            selected = "close price",  multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_US_commodities_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_returns_year", 
            "year",
            choices = c(
              unique(
                correlations_US_commodities_years_returns$year
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_years_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_years_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_years_returns"),
      p(linebreaks(2L))
    ),
    
    
    
    
    
    
    ## amongst US commodities (no metals)
    
    tabItem(
      tabName = "correlations-UScommos-nometals",
      
      
      ### by period
      h3("By period"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_levels_field",
            "field",
            choices = unique(
              correlations_US_commodities_no_metals_periods_levels$field
            ),
            selected = "close price",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_levels_frequency",
            "frequency",
            choices = unique(
              correlations_US_commodities_no_metals_periods_levels$frequency
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_levels_period",
            "period",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_periods_levels$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_periods_levels$regime
              ),
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average",
            multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_no_metals_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_no_metals_periods_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_returns_field", 
            "field",
            choices = unique(
              correlations_US_commodities_no_metals_periods_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_US_commodities_no_metals_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_returns_period", 
            "period",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_periods_returns$period
              ), 
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_periods_returns$regime
              ), 
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_periods_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average",
            multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_no_metals_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_no_metals_periods_returns"),
      p(linebreaks(5L)),
      
      
      ### by year
      h3("By year"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_levels_field",
            "field",
            choices = unique(
              correlations_US_commodities_no_metals_years_levels$field
            ),
            selected = "close price",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_levels_frequency",
            "frequency",
            choices = unique(
              correlations_US_commodities_no_metals_years_levels$frequency
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_levels_year",
            "year",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_years_levels$year
              ),
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_years_levels$regime
              ),
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average",
            multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_no_metals_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_no_metals_years_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_returns_field", 
            "field",
            choices = unique(
              correlations_US_commodities_no_metals_years_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_US_commodities_no_metals_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_returns_year", 
            "year",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_years_returns$year
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_US_commodities_no_metals_years_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_commodities_no_metals_years_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_commodities_no_metals_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_commodities_no_metals_years_returns"),
      p(linebreaks(2L))
      
    ),
    
    
    
    
    
    
    
    
    ## amongst US metals
    
    tabItem(
      tabName = "correlations-USmetals",
      
      ### by period
      h3("By period"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_levels_field",
            "field",
            choices = unique(
              correlations_US_metals_periods_levels$field
            ),
            selected = "close price",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_levels_frequency",
            "frequency",
            choices = unique(
              correlations_US_metals_periods_levels$frequency
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_levels_period",
            "period",
            choices = c(
              unique(
                correlations_US_metals_periods_levels$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_US_metals_periods_levels$regime
              ),
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average",
            multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_metals_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_metals_periods_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_returns_field", 
            "field",
            choices = unique(
              correlations_US_metals_periods_returns$field
            ),
            selected = "close price",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_US_metals_periods_returns$frequency
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_returns_period", 
            "period",
            choices = c(
              unique(
                correlations_US_metals_periods_returns$period
              ), 
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_US_metals_periods_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_periods_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_metals_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_metals_periods_returns"),
      p(linebreaks(5L)),
      
      
      ### by year
      h3("By year"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_levels_field",
            "field",
            choices = unique(
              correlations_US_metals_years_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_levels_frequency",
            "frequency",
            choices = unique(
              correlations_US_metals_years_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_levels_year",
            "year",
            choices = c(
              unique(
                correlations_US_metals_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_US_metals_years_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_metals_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_metals_years_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_returns_field", 
            "field",
            choices = unique(
              correlations_US_metals_years_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_US_metals_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_returns_year", 
            "year",
            choices = c(
              unique(
                correlations_US_metals_years_returns$year
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_US_metals_years_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_US_metals_years_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_US_metals_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_US_metals_years_returns"),
      p(linebreaks(2L))
    ),
    
    
    
    
    
    
    
    
    
    ## amongst UK metals
    
    tabItem(
      tabName = "correlations-UKmetals",
      
      
      ### by period
      h3("By period"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_levels_field",
            "field",
            choices = unique(
              correlations_UK_metals_periods_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_levels_frequency",
            "frequency",
            choices = unique(
              correlations_UK_metals_periods_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_levels_period",
            "period",
            choices = c(
              unique(
                correlations_UK_metals_periods_levels$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_UK_metals_periods_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_UK_metals_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      
      tableOutput("correlations_UK_metals_periods_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_returns_field", 
            "field",
            choices = unique(
              correlations_UK_metals_periods_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_UK_metals_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_returns_period", 
            "period",
            choices = c(
              unique(
                correlations_UK_metals_periods_returns$period
              ), 
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_UK_metals_periods_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_periods_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_UK_metals_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_UK_metals_periods_returns"),
      p(linebreaks(5L)),
      
      
      ### by year
      h3("By year"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_levels_field",
            "field",
            choices = unique(
              correlations_UK_metals_years_levels$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_levels_frequency",
            "frequency",
            choices = unique(
              correlations_UK_metals_years_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_levels_year",
            "year",
            choices = c(
              unique(
                correlations_UK_metals_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_UK_metals_years_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_UK_metals_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_UK_metals_years_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_returns_field", 
            "field",
            choices = unique(
              correlations_UK_metals_years_returns$field
            ),
            selected = "close price", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_returns_frequency", 
            "frequency",
            choices = unique(
              correlations_UK_metals_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_returns_year", 
            "year",
            choices = c(
              unique(
                correlations_UK_metals_years_returns$year
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_returns_regime", 
            "regime",
            choices = c(
              unique(
                correlations_UK_metals_years_returns$regime
              ), 
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          2, align = "left",
          selectInput(
            "correlations_UK_metals_years_returns_correlations", 
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_UK_metals_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_UK_metals_years_returns"),
      p(linebreaks(5L))
    ),
    
    
    
    
    
    # ## amongst US energy
    # 
    # tabItem(
    #   tabName = "correlations-USenergy",
    #   
    #   
    #   ### by period
    #   h3("By period"),
    #   
    #   #### levels
    #   h4("Levels"),
    #   
    #   
    #   fluidRow(
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_levels_field",
    #         "field",
    #         choices = unique(
    #           correlations_US_energy_periods_levels$field
    #         ),
    #         selected = "close price", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_levels_frequency",
    #         "frequency",
    #         choices = unique(
    #           correlations_US_energy_periods_levels$frequency
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_levels_period",
    #         "period",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_periods_levels$period
    #           ),
    #           "all"
    #         ),
    #         selected = "past", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_levels_regime",
    #         "regime",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_periods_levels$regime
    #           ),
    #           "all"
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_levels_correlations",
    #         "correlations",
    #         choices = c("all", "average"),
    #         selected = "average", multiple = F
    #       )
    #     )
    #   ),
    #   
    #   fluidRow(
    #     column(
    #       3, align = "left", 
    #       downloadButton(
    #         'correlations_US_energy_periods_levels_download', 
    #         'Download .csv'
    #       )
    #     )
    #   ),
    #   
    #   
    #   tableOutput("correlations_US_energy_periods_levels"),
    #   p(linebreaks(2L)),
    #   
    #   
    #   #### returns
    #   h4("Returns"),
    #   
    #   fluidRow(
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_returns_field", 
    #         "field",
    #         choices = unique(
    #           correlations_US_energy_periods_returns$field
    #         ),
    #         selected = "close price", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_returns_frequency", 
    #         "frequency",
    #         choices = unique(
    #           correlations_US_energy_periods_returns$frequency
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_returns_period", 
    #         "period",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_periods_returns$period
    #           ), 
    #           "all"
    #         ),
    #         selected = "past", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_returns_regime", 
    #         "regime",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_periods_returns$regime
    #           ), 
    #           "all"
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_periods_returns_correlations", 
    #         "correlations",
    #         choices = c("all", "average"),
    #         selected = "average", multiple = F
    #       )
    #     )
    #   ),
    #   
    #   fluidRow(
    #     column(
    #       3, align = "left", 
    #       downloadButton(
    #         'correlations_US_energy_periods_returns_download', 
    #         'Download .csv'
    #       )
    #     )
    #   ),
    #   
    #   tableOutput("correlations_US_energy_periods_returns"),
    #   p(linebreaks(5L)),
    #   
    #   
    #   ### by year
    #   h3("By year"),
    #   
    #   #### levels
    #   h4("Levels"),
    #   
    #   
    #   fluidRow(
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_levels_field",
    #         "field",
    #         choices = unique(
    #           correlations_US_energy_years_levels$field
    #         ),
    #         selected = "close price", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_levels_frequency",
    #         "frequency",
    #         choices = unique(
    #           correlations_US_energy_years_levels$frequency
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_levels_year",
    #         "year",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_years_levels$year
    #           ),
    #           "all"
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_levels_regime",
    #         "regime",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_years_levels$regime
    #           ),
    #           "all"
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_levels_correlations",
    #         "correlations",
    #         choices = c("all", "average"),
    #         selected = "average", multiple = F
    #       )
    #     )
    #   ),
    #   
    #   fluidRow(
    #     column(
    #       3, align = "left", 
    #       downloadButton(
    #         'correlations_US_energy_years_levels_download', 
    #         'Download .csv'
    #       )
    #     )
    #   ),
    #   
    #   tableOutput("correlations_US_energy_years_levels"),
    #   p(linebreaks(2L)),
    #   
    #   
    #   #### returns
    #   h4("Returns"),
    #   
    #   fluidRow(
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_returns_field", 
    #         "field",
    #         choices = unique(
    #           correlations_US_energy_years_returns$field
    #         ),
    #         selected = "close price", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_returns_frequency", 
    #         "frequency",
    #         choices = unique(
    #           correlations_US_energy_years_returns$frequency
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_returns_year", 
    #         "year",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_years_returns$year
    #           ), 
    #           "all"
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_returns_regime", 
    #         "regime",
    #         choices = c(
    #           unique(
    #             correlations_US_energy_years_returns$regime
    #           ), 
    #           "all"
    #         ),
    #         selected = "", multiple = F
    #       )
    #     ),
    #     column(
    #       2, align = "left",
    #       selectInput(
    #         "correlations_US_energy_years_returns_correlations", 
    #         "correlations",
    #         choices = c("all", "average"),
    #         selected = "average", multiple = F
    #       )
    #     )
    #   ),
    #   
    #   fluidRow(
    #     column(
    #       3, align = "left", 
    #       downloadButton(
    #         'correlations_US_energy_years_returns_download', 
    #         'Download .csv'
    #       )
    #     )
    #   ),
    #   
    #   tableOutput("correlations_US_energy_years_returns"),
    #   p(linebreaks(5L))
    # ),
    
    
    
    
    
    
    
    
    
    
    ## amongst factor picks
    
    tabItem(
      tabName = "correlations-picks",
      
      
      ### by period
      h3("By period"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_factor",
            "picking factor",
            choices = unique(
              correlations_factor_picks_periods_levels$factor
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_asset_pool",
            "picking factor asset pool",
            choices = unique(
              correlations_factor_picks_periods_levels$`asset pool`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_leg",
            "picking factor leg",
            choices = unique(
              correlations_factor_picks_periods_levels$leg
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_field",
            "field",
            choices = unique(
              correlations_factor_picks_periods_levels$field
            ),
            selected = "close price", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_frequency",
            "frequency",
            choices = unique(
              correlations_factor_picks_periods_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_period",
            "period",
            choices = c(
              unique(
                correlations_factor_picks_periods_levels$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_factor_picks_periods_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_factor_picks_periods_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_factor_picks_periods_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_factor",
            "picking factor",
            choices = unique(
              correlations_factor_picks_periods_returns$factor
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_asset_pool",
            "picking factor asset pool",
            choices = unique(
              correlations_factor_picks_periods_returns$`asset pool`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_leg",
            "picking factor leg",
            choices = unique(
              correlations_factor_picks_periods_returns$leg
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_field",
            "field",
            choices = unique(
              correlations_factor_picks_periods_returns$field
            ),
            selected = "close price", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_frequency",
            "frequency",
            choices = unique(
              correlations_factor_picks_periods_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_period",
            "period",
            choices = c(
              unique(
                correlations_factor_picks_periods_returns$period
              ),
              "all"
            ),
            selected = "past", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_regime",
            "regime",
            choices = c(
              unique(
                correlations_factor_picks_periods_returns$regime
              ),
              "all"
            ),
            selected = "",
            multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_periods_returns_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average",
            multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_factor_picks_periods_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_factor_picks_periods_returns"),
      p(linebreaks(5L)),
      
      
      
      ### by year
      h3("By year"),
      
      #### levels
      h4("Levels"),
      
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_factor",
            "picking factor",
            choices = unique(
              correlations_factor_picks_years_levels$factor
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_asset_pool",
            "picking factor asset pool",
            choices = unique(
              correlations_factor_picks_years_levels$`asset pool`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_leg",
            "picking factor leg",
            choices = unique(
              correlations_factor_picks_years_levels$leg
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_field",
            "field",
            choices = unique(
              correlations_factor_picks_years_levels$field
            ),
            selected = "close price", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_frequency",
            "frequency",
            choices = unique(
              correlations_factor_picks_years_levels$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_year",
            "year",
            choices = c(
              unique(
                correlations_factor_picks_years_levels$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_regime",
            "regime",
            choices = c(
              unique(
                correlations_factor_picks_years_levels$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_levels_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_factor_picks_years_levels_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_factor_picks_years_levels"),
      p(linebreaks(2L)),
      
      
      #### returns
      h4("Returns"),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_factor",
            "picking factor",
            choices = unique(
              correlations_factor_picks_years_returns$factor
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_asset_pool",
            "picking factor asset pool",
            choices = unique(
              correlations_factor_picks_years_returns$`asset pool`
            ),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_leg",
            "picking factor leg",
            choices = unique(
              correlations_factor_picks_years_returns$leg
            ),
            selected = "factor", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_field",
            "field",
            choices = unique(
              correlations_factor_picks_years_returns$field
            ),
            selected = "close price", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_frequency",
            "frequency",
            choices = unique(
              correlations_factor_picks_years_returns$frequency
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_year",
            "year",
            choices = c(
              unique(
                correlations_factor_picks_years_returns$year
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_regime",
            "regime",
            choices = c(
              unique(
                correlations_factor_picks_years_returns$regime
              ),
              "all"
            ),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "correlations_factor_picks_years_returns_correlations",
            "correlations",
            choices = c("all", "average"),
            selected = "average", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton(
            'correlations_factor_picks_years_returns_download', 
            'Download .csv'
          )
        )
      ),
      
      tableOutput("correlations_factor_picks_years_returns"),
      p(linebreaks(2L))
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # proportions
    tabItem(
      tabName = "proportions",
      
      h3("Proportion of time factor picks are on their respective factor"),
      
      # proportion of time factor picks are on their respective factor
      fluidRow(
        column(
          3, align = "left",
          selectInput(
            "proportions_factor", 
            "factor",
            choices = c(unique(proportions$factor), "all"),
            selected = "market", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "proportions_asset_pool", 
            "factor asset pool",
            choices = unique(proportions$`asset pool`),
            selected = "", multiple = F
          )
        ),
        column(
          3, align = "left",
          selectInput(
            "proportions_period", 
            "period",
            choices = c(unique(proportions$period), "all"),
            selected = "", multiple = F
          )
        )
      ),
      
      fluidRow(
        column(
          3, align = "left", 
          downloadButton('proportions_download', 'Download .csv')
        )
      ),
      
      tableOutput("proportions")
    )
  )
)




ui <- dashboardPage(
  dashboardHeader(title = "Co-movement"), sidebar, body, skin = "black"
)


















































server <- function(input, output) {
  
  # globals ####
  
  ## factor parameters ####
  
  output$factor_parameters <- renderTable({factor_parameters}, digits = 4L)
  
  ## factor parameters ####
  
  output$periods <- renderTable({periods}, digits = 4L)
  
  
  # descriptive statistics
  
  ## commodity futures
  
  ### market variables
  
  #### individuals
  
  ##### by year
  
  ###### levels
  # Commodity futures - market variables - individuals - by year - levels
  statistics_commodity_futures_market_individuals_years_levels_results <-
    reactive({
      
      # Filter field & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_individuals_years_levels,
        field ==
          input$statistics_commodity_futures_market_individuals_years_levels_field,
        regime ==
          input$statistics_commodity_futures_market_individuals_years_levels_regime
      ) %>% dplyr::select(-c(field, regime))
      
      # Filter ticker
      results <- if (
        input$statistics_commodity_futures_market_individuals_years_levels_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$statistics_commodity_futures_market_individuals_years_levels_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_market_individuals_years_levels_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_market_individuals_years_levels_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_individuals_years_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_individuals_years_levels <-
    renderTable({
      statistics_commodity_futures_market_individuals_years_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_individuals_years_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_individuals_years_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_individuals_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ###### returns
  # Commodity futures - market variables - individuals - by year - returns
  statistics_commodity_futures_market_individuals_years_returns_results <-
    reactive({
      
      # Filter field, frequency, regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_individuals_years_returns,
        field ==
          input$statistics_commodity_futures_market_individuals_years_returns_field,
        frequency ==
          input$statistics_commodity_futures_market_individuals_years_returns_frequency,
        regime ==
          input$statistics_commodity_futures_market_individuals_years_returns_regime
      ) %>% dplyr::select(-c(field, frequency, regime))
      
      # Filter ticker
      results <- if (
        input$statistics_commodity_futures_market_individuals_years_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$statistics_commodity_futures_market_individuals_years_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_market_individuals_years_returns_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_market_individuals_years_returns_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_individuals_years_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_individuals_years_returns <-
    renderTable({
      statistics_commodity_futures_market_individuals_years_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_individuals_years_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_individuals_years_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_individuals_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ##### by period
  
  ###### levels
  # Commodity futures - market variables - individuals - by period - levels
  
  statistics_commodity_futures_market_individuals_periods_levels_results <-
    reactive({
      
      # Filter field & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_individuals_periods_levels,
        field ==
          input$statistics_commodity_futures_market_individuals_periods_levels_field,
        regime ==
          input$statistics_commodity_futures_market_individuals_periods_levels_regime
      ) %>%
        dplyr::select(-c(field, regime)) %>%
        # Filter ticker
        {
          if (
            input$statistics_commodity_futures_market_individuals_periods_levels_commodity ==
            "all"
          ) .
          else
            dplyr::filter(
              .,
              commodity ==
                input$statistics_commodity_futures_market_individuals_periods_levels_commodity
            ) %>%
            dplyr::select(-commodity)
          
        } %>%
        # Filter period
        {
          if (
            input$statistics_commodity_futures_market_individuals_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              .,
              period ==
                input$statistics_commodity_futures_market_individuals_periods_levels_period
            ) %>%
            dplyr::select(-period)
        }
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_individuals_periods_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_individuals_periods_levels <-
    renderTable({
      statistics_commodity_futures_market_individuals_periods_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_individuals_periods_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_individuals_periods_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_individuals_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ###### returns
  # Commodity futures - market variables - individuals - by period - returns
  statistics_commodity_futures_market_individuals_periods_returns_results <-
    reactive({
      
      # Filter field, frequency & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_individuals_periods_returns,
        field ==
          input$statistics_commodity_futures_market_individuals_periods_returns_field,
        frequency ==
          input$statistics_commodity_futures_market_individuals_periods_returns_frequency,
        regime ==
          input$statistics_commodity_futures_market_individuals_periods_returns_regime
      ) %>% dplyr::select(-c(field, frequency, regime))
      
      # Filter ticker
      results <- if (
        input$statistics_commodity_futures_market_individuals_periods_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$statistics_commodity_futures_market_individuals_periods_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter period
      results <- if (
        input$statistics_commodity_futures_market_individuals_periods_returns_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$statistics_commodity_futures_market_individuals_periods_returns_period
        ) %>% dplyr::select(-period)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_individuals_periods_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_individuals_periods_returns <-
    renderTable({
      statistics_commodity_futures_market_individuals_periods_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_individuals_periods_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_individuals_periods_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_individuals_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  #### groups
  
  ##### by year
  
  ###### levels
  # Commodity futures - market variables - groups - by year - levels
  statistics_commodity_futures_market_groups_years_levels_results <-
    reactive({
      
      # browser()
      # Filter sector, subsector, field & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_groups_years_levels,
        sector == input$statistics_commodity_futures_market_groups_years_levels_sector,
        subsector == input$statistics_commodity_futures_market_groups_years_levels_subsector,
        field == input$statistics_commodity_futures_market_groups_years_levels_field,
        regime == input$statistics_commodity_futures_market_groups_years_levels_regime
      ) %>% dplyr::select(-c(sector, subsector, field, regime))
      
      # Filter country
      results <- if (
        input$statistics_commodity_futures_market_groups_years_levels_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$statistics_commodity_futures_market_groups_years_levels_country
        ) %>% dplyr::select(-country)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_market_groups_years_levels_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_market_groups_years_levels_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_groups_years_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_groups_years_levels <-
    renderTable({
      statistics_commodity_futures_market_groups_years_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_groups_years_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_groups_years_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_groups_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ###### returns
  # Commodity futures - market variables - groups - by year - returns
  statistics_commodity_futures_market_groups_years_returns_results <-
    reactive({
      
      # Filter sector, subsector, field, frequency & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_groups_years_returns,
        sector ==
          input$statistics_commodity_futures_market_groups_years_returns_sector,
        subsector ==
          input$statistics_commodity_futures_market_groups_years_returns_subsector,
        field ==
          input$statistics_commodity_futures_market_groups_years_returns_field,
        frequency ==
          input$statistics_commodity_futures_market_groups_years_returns_frequency,
        regime ==
          input$statistics_commodity_futures_market_groups_years_returns_regime
      ) %>% dplyr::select(-c(sector, subsector, field, frequency, regime))
      
      # Filter country
      results <- if (
        input$statistics_commodity_futures_market_groups_years_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$statistics_commodity_futures_market_groups_years_returns_country
        ) %>% dplyr::select(-country)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_market_groups_years_returns_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_market_groups_years_returns_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_groups_years_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_groups_years_returns <-
    renderTable({
      statistics_commodity_futures_market_groups_years_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_groups_years_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_groups_years_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_groups_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ##### by period
  
  ###### levels
  # Commodity futures - market variables - groups - by period - levels
  statistics_commodity_futures_market_groups_periods_levels_results <-
    reactive({
      
      # Filter by sector, subsector, field & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_groups_periods_levels,
        sector ==
          input$statistics_commodity_futures_market_groups_periods_levels_sector,
        subsector ==
          input$statistics_commodity_futures_market_groups_periods_levels_subsector,
        field ==
          input$statistics_commodity_futures_market_groups_periods_levels_field,
        regime ==
          input$statistics_commodity_futures_market_groups_periods_levels_regime
      ) %>% dplyr::select(-c(sector, subsector, field, regime)) %>%
        # Filter country
        {
          if (
            input$statistics_commodity_futures_market_groups_periods_levels_country ==
            "all"
          ) .
          else
            dplyr::filter(
              ., country ==
                input$statistics_commodity_futures_market_groups_periods_levels_country
            ) %>% dplyr::select(-country)
        } %>%
        # Filter period
        {
          if (
            input$statistics_commodity_futures_market_groups_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$statistics_commodity_futures_market_groups_periods_levels_period
            ) %>% dplyr::select(-period)
        }
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_groups_periods_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_groups_periods_levels <-
    renderTable({
      statistics_commodity_futures_market_groups_periods_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_groups_periods_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_groups_periods_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_groups_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ###### returns
  # Commodity futures - market variables - groups - by period - returns
  statistics_commodity_futures_market_groups_periods_returns_results <-
    reactive({
      
      # Filter sector, subsector, field, frequency & regime
      results <- dplyr::filter(
        statistics_commodity_futures_market_groups_periods_returns,
        sector ==
          input$statistics_commodity_futures_market_groups_periods_returns_sector,
        subsector ==
          input$statistics_commodity_futures_market_groups_periods_returns_subsector,
        field ==
          input$statistics_commodity_futures_market_groups_periods_returns_field,
        frequency ==
          input$statistics_commodity_futures_market_groups_periods_returns_frequency,
        regime ==
          input$statistics_commodity_futures_market_groups_periods_returns_regime
      ) %>% dplyr::select(-c(sector, subsector, field, frequency, regime))
      
      # Filter country
      results <- if (
        input$statistics_commodity_futures_market_groups_periods_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$statistics_commodity_futures_market_groups_periods_returns_country
        ) %>% dplyr::select(-country)
      
      
      # Filter period
      results <- if (
        input$statistics_commodity_futures_market_groups_periods_returns_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$statistics_commodity_futures_market_groups_periods_returns_period
        ) %>% dplyr::select(-period)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_market_groups_periods_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_market_groups_periods_returns <-
    renderTable({
      statistics_commodity_futures_market_groups_periods_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_market_groups_periods_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_market_groups_periods_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_market_groups_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ### position variables
  
  #### individuals
  
  ##### by year
  
  ###### levels
  # Commodity futures - position variables - individuals - by year - levels
  statistics_commodity_futures_cftc_individuals_years_levels_results <-
    reactive({
      
      # Filter field & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_individuals_years_levels,
        participant ==
          input$statistics_commodity_futures_cftc_individuals_years_levels_participant
      ) %>% dplyr::select(-participant)
      
      # Filter ticker
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_years_levels_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$statistics_commodity_futures_cftc_individuals_years_levels_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_years_levels_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_cftc_individuals_years_levels_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_years_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_individuals_years_levels <-
    renderTable({
      statistics_commodity_futures_cftc_individuals_years_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_individuals_years_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_individuals_years_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_individuals_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ###### returns
  # Commodity futures - position variables - individuals - by year - returns
  statistics_commodity_futures_cftc_individuals_years_returns_results <-
    reactive({
      
      # Filter participant, frequency, regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_individuals_years_returns,
        participant ==
          input$statistics_commodity_futures_cftc_individuals_years_returns_participant,
        frequency ==
          input$statistics_commodity_futures_cftc_individuals_years_returns_frequency
      ) %>% dplyr::select(-c(participant, frequency))
      
      # Filter ticker
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_years_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$statistics_commodity_futures_cftc_individuals_years_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_years_returns_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_cftc_individuals_years_returns_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_years_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_individuals_years_returns <-
    renderTable({
      statistics_commodity_futures_cftc_individuals_years_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_individuals_years_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_individuals_years_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_individuals_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ##### by period
  
  ###### levels
  # Commodity futures - position variables - individuals - by period - levels
  statistics_commodity_futures_cftc_individuals_periods_levels_results <-
    reactive({
      
      # Filter participant & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_individuals_periods_levels,
        participant ==
          input$statistics_commodity_futures_cftc_individuals_periods_levels_participant
      ) %>% dplyr::select(-participant) %>%
        # Filter ticker
        {
          if (
            input$statistics_commodity_futures_cftc_individuals_periods_levels_commodity ==
            "all"
          ) .
          else
            dplyr::filter(
              .,
              commodity ==
                input$statistics_commodity_futures_cftc_individuals_periods_levels_commodity
            ) %>% dplyr::select(-commodity)
          
        } %>%
        # Filter period
        {
          if (
            input$statistics_commodity_futures_cftc_individuals_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              .,
              period ==
                input$statistics_commodity_futures_cftc_individuals_periods_levels_period
            ) %>% dplyr::select(-period)
        }
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_periods_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_individuals_periods_levels <-
    renderTable({
      statistics_commodity_futures_cftc_individuals_periods_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_individuals_periods_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_individuals_periods_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_individuals_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ###### returns
  # Commodity futures - position variables - individuals - by period - returns
  statistics_commodity_futures_cftc_individuals_periods_returns_results <-
    reactive({
      
      # Filter participant, frequency & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_individuals_periods_returns,
        participant ==
          input$statistics_commodity_futures_cftc_individuals_periods_returns_participant,
        frequency ==
          input$statistics_commodity_futures_cftc_individuals_periods_returns_frequency
      ) %>% dplyr::select(-c(participant, frequency))
      
      # Filter ticker
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_periods_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$statistics_commodity_futures_cftc_individuals_periods_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter period
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_periods_returns_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$statistics_commodity_futures_cftc_individuals_periods_returns_period
        ) %>% dplyr::select(-period)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_individuals_periods_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_individuals_periods_returns <-
    renderTable({
      statistics_commodity_futures_cftc_individuals_periods_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_individuals_periods_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_individuals_periods_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_individuals_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  #### groups
  
  ##### by year
  
  ###### levels
  # Commodity futures - position variables - groups - by year - levels
  statistics_commodity_futures_cftc_groups_years_levels_results <-
    reactive({
      
      # Filter sector, subsector, participant & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_groups_years_levels,
        sector == input$statistics_commodity_futures_cftc_groups_years_levels_sector,
        subsector ==
          input$statistics_commodity_futures_cftc_groups_years_levels_subsector,
        participant ==
          input$statistics_commodity_futures_cftc_groups_years_levels_participant
      ) %>% dplyr::select(-c(sector, subsector, participant))
      
      # Filter country
      results <- if (
        input$statistics_commodity_futures_cftc_groups_years_levels_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$statistics_commodity_futures_cftc_groups_years_levels_country
        ) %>%  dplyr::select(-country)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_cftc_groups_years_levels_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_cftc_groups_years_levels_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_groups_years_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_groups_years_levels <-
    renderTable({
      statistics_commodity_futures_cftc_groups_years_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_groups_years_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_groups_years_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_groups_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ###### returns
  # Commodity futures - position variables - groups - by year - returns
  statistics_commodity_futures_cftc_groups_years_returns_results <-
    reactive({
      
      # Filter sector, subsector, participant, frequency & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_groups_years_returns,
        sector == input$statistics_commodity_futures_cftc_groups_years_returns_sector,
        subsector == input$statistics_commodity_futures_cftc_groups_years_returns_subsector,
        participant == input$statistics_commodity_futures_cftc_groups_years_returns_participant,
        frequency == input$statistics_commodity_futures_cftc_groups_years_returns_frequency
      ) %>% dplyr::select(-c(sector, subsector, participant, frequency))
      
      # Filter country
      results <- if (
        input$statistics_commodity_futures_cftc_groups_years_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$statistics_commodity_futures_cftc_groups_years_returns_country
        ) %>% dplyr::select(-country)
      
      # Filter year
      results <- if (
        input$statistics_commodity_futures_cftc_groups_years_returns_year ==
        "all"
      ) results
      else
        dplyr::filter(
          results, year ==
            input$statistics_commodity_futures_cftc_groups_years_returns_year
        ) %>% dplyr::select(-year)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_groups_years_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_groups_years_returns <-
    renderTable({
      statistics_commodity_futures_cftc_groups_years_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_groups_years_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_groups_years_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_groups_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ##### by period
  
  ###### levels
  # Commodity futures - position variables - groups - by period - levels
  statistics_commodity_futures_cftc_groups_periods_levels_results <-
    reactive({
      
      # Filter by sector, subsector, participant & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_groups_periods_levels,
        sector == input$statistics_commodity_futures_cftc_groups_periods_levels_sector,
        subsector == input$statistics_commodity_futures_cftc_groups_periods_levels_subsector,
        participant == input$statistics_commodity_futures_cftc_groups_periods_levels_participant
      ) %>% dplyr::select(-c(sector, subsector, participant)) %>%
        # Filter country
        {
          if (
            input$statistics_commodity_futures_cftc_groups_periods_levels_country ==
            "all"
          ) .
          else
            dplyr::filter(
              ., country ==
                input$statistics_commodity_futures_cftc_groups_periods_levels_country
            ) %>% dplyr::select(-country)
        } %>%
        # Filter period
        {
          if (
            input$statistics_commodity_futures_cftc_groups_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$statistics_commodity_futures_cftc_groups_periods_levels_period
            ) %>% dplyr::select(-period)
        }
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_groups_periods_levels_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_groups_periods_levels <-
    renderTable({
      statistics_commodity_futures_cftc_groups_periods_levels_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_groups_periods_levels_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_groups_periods_levels.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_groups_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  ###### returns
  # Commodity futures - position variables - groups - by period - returns
  statistics_commodity_futures_cftc_groups_periods_returns_results <-
    reactive({
      
      # Filter sector, subsector, participant, frequency & regime
      results <- dplyr::filter(
        statistics_commodity_futures_cftc_groups_periods_returns,
        sector ==
          input$statistics_commodity_futures_cftc_groups_periods_returns_sector,
        subsector ==
          input$statistics_commodity_futures_cftc_groups_periods_returns_subsector,
        participant ==
          input$statistics_commodity_futures_cftc_groups_periods_returns_participant,
        frequency ==
          input$statistics_commodity_futures_cftc_groups_periods_returns_frequency
      ) %>% dplyr::select(-c(sector, subsector, participant, frequency))
      
      # Filter country
      results <- if (
        input$statistics_commodity_futures_cftc_groups_periods_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$statistics_commodity_futures_cftc_groups_periods_returns_country
        ) %>% dplyr::select(-country)
      
      
      # Filter period
      results <- if (
        input$statistics_commodity_futures_cftc_groups_periods_returns_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$statistics_commodity_futures_cftc_groups_periods_returns_period
        ) %>% dplyr::select(-period)
      
      # Filter summary
      results <- if (
        input$statistics_commodity_futures_cftc_groups_periods_returns_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_commodity_futures_cftc_groups_periods_returns <-
    renderTable({
      statistics_commodity_futures_cftc_groups_periods_returns_results()
    }, digits = 4L)
  output$statistics_commodity_futures_cftc_groups_periods_returns_download <- 
    downloadHandler(
      filename = "statistics_commodity_futures_cftc_groups_periods_returns.csv",
      content = function(file) {
        data <- statistics_commodity_futures_cftc_groups_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  ## factors
  
  ### asset pool: US commodities
  
  #### by year
  
  ##### returns
  
  # factors - asset pool: US commodities- by year - returns
  statistics_factors_US_commos_years_returns_results <- 
    reactive({
      
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        statistics_factors_US_commos_years, 
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$statistics_factors_US_commos_years_regime
      ) %>%
        dplyr::select(
          -c(
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter factor
        {
          if (input$statistics_factors_US_commos_years_factor == "all") .
          else 
            dplyr::filter(
              ., name == input$statistics_factors_US_commos_years_factor
            ) %>% dplyr::select(-name)
        } %>%
        # Filter leg
        {
          if (
            input$statistics_factors_US_commos_years_leg == "all"
          ) .
          else 
            dplyr::filter(
              ., leg == input$statistics_factors_US_commos_years_leg
            ) %>%
            dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$statistics_factors_US_commos_years_year == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., year == 
                input$statistics_factors_US_commos_years_year
            ) %>%
            dplyr::select(-year)
        }
      
      # Filter summary
      results <- if (
        input$statistics_factors_US_commos_years_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_factors_US_commos_years_returns <- 
    renderTable({
      statistics_factors_US_commos_years_returns_results()
    }, digits = 4L)
  output$statistics_factors_US_commos_years_returns_download <- 
    downloadHandler(
      filename = "statistics_factors_US_commos_years_returns.csv",
      content = function(file) {
        data <- statistics_factors_US_commos_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  #### by period
  
  ##### returns
  
  # factors - asset pool: US commodities- by year - returns
  statistics_factors_US_commos_periods_returns_results <- 
    reactive({
      
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        statistics_factors_US_commos_periods, 
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime ==
          input$statistics_factors_US_commos_periods_regime
      ) %>%
        dplyr::select(
          -c(
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter factor
        {
          if (
            input$statistics_factors_US_commos_periods_factor == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., name == 
                input$statistics_factors_US_commos_periods_factor
            ) %>%
            dplyr::select(-name)
        } %>%
        # Filter leg
        {
          if (
            input$statistics_factors_US_commos_periods_leg == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., leg == 
                input$statistics_factors_US_commos_periods_leg
            ) %>%
            dplyr::select(-leg)
        } %>%
        # Filter period
        {
          if (
            input$statistics_factors_US_commos_periods_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$statistics_factors_US_commos_periods_period
            ) %>%
            dplyr::select(-period)
        }
      
      # Filter summary
      results <- if (
        input$statistics_factors_US_commos_periods_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_factors_US_commos_periods_returns <- 
    renderTable({
      statistics_factors_US_commos_periods_returns_results()
    }, digits = 4L)
  output$statistics_factors_US_commos_periods_returns_download <- 
    downloadHandler(
      filename = "statistics_factors_US_commos_periods_returns.csv",
      content = function(file) {
        data <- statistics_factors_US_commos_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ### asset pool: picking factors
  
  #### by year
  
  ##### returns
  
  # factors - asset pool: picking factors - by year - returns
  statistics_factors_factor_picks_years_returns_results <- 
    reactive({
      
      # browser()
      
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        statistics_factors_factor_picks_years, 
        `picking factor name` ==
          input$statistics_factors_factor_picks_years_picking_factor,
        `picking factor asset pool` ==
          input$statistics_factors_factor_picks_years_picking_factor_asset_pool,
        `picking factor leg` ==
          input$statistics_factors_factor_picks_years_picking_factor_leg,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$statistics_factors_factor_picks_years_regime
      ) %>%
        dplyr::select(
          -c(
            `picking factor name`, `picking factor asset pool`, `picking factor leg`,
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter factor
        {
          if (
            input$statistics_factors_factor_picks_years_factor == "all"
          ) .
          else 
            dplyr::filter(
              ., name == input$statistics_factors_factor_picks_years_factor
            ) %>% dplyr::select(-name)
        } %>%
        # Filter leg
        {
          if (
            input$statistics_factors_factor_picks_years_leg == "all"
          ) .
          else 
            dplyr::filter(
              ., leg == input$statistics_factors_factor_picks_years_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$statistics_factors_factor_picks_years_year == "all"
          ) .
          else 
            dplyr::filter(
              ., year == input$statistics_factors_factor_picks_years_year
            ) %>% dplyr::select(-year)
        }
      
      # Filter summary
      results <- if (
        input$statistics_factors_factor_picks_years_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_factors_factor_picks_years_returns <- 
    renderTable({
      statistics_factors_factor_picks_years_returns_results()
    }, digits = 4L)
  output$statistics_factors_factor_picks_years_returns_download <- 
    downloadHandler(
      filename = "statistics_factors_factor_picks_years_returns.csv",
      content = function(file) {
        data <- statistics_factors_factor_picks_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### by period
  
  ##### returns
  
  # factors - asset pool: picking factors - by period - returns
  statistics_factors_factor_picks_periods_returns_results <- 
    reactive({
      
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        statistics_factors_factor_picks_periods, 
        `picking factor name` ==
          input$statistics_factors_factor_picks_periods_picking_factor,
        `picking factor asset pool` ==
          input$statistics_factors_factor_picks_periods_picking_factor_asset_pool,
        `picking factor leg` ==
          input$statistics_factors_factor_picks_periods_picking_factor_leg,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$statistics_factors_factor_picks_periods_regime
      ) %>%
        dplyr::select(
          -c(
            `picking factor name`, `picking factor asset pool`, `picking factor leg`,
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter factor
        {
          if (
            input$statistics_factors_factor_picks_periods_factor == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., name == 
                input$statistics_factors_factor_picks_periods_factor
            ) %>%
            dplyr::select(-name)
        } %>%
        # Filter leg
        {
          if (
            input$statistics_factors_factor_picks_periods_leg == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., leg == 
                input$statistics_factors_factor_picks_periods_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$statistics_factors_factor_picks_periods_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$statistics_factors_factor_picks_periods_period
            ) %>% dplyr::select(-period)
        }
      
      # Filter summary
      results <- if (
        input$statistics_factors_factor_picks_periods_summary == "average"
      ) {
        dplyr::summarise_if(results, .predicate = is.numeric, .funs = mean, na.rm = TRUE) 
      }
      else results
      
      results
    })
  
  output$statistics_factors_factor_picks_periods_returns <- 
    renderTable({
      statistics_factors_factor_picks_periods_returns_results()
    }, digits = 4L)
  output$statistics_factors_factor_picks_periods_returns_download <- 
    downloadHandler(
      filename = "statistics_factors_factor_picks_periods_returns.csv",
      content = function(file) {
        data <- statistics_factors_factor_picks_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  
  
  # tests of difference
  
  ## periods
  
  ### commodity futures
  
  #### market variables
  
  ##### individuals
  
  ###### levels
  # Commodity futures - market variables - individuals - levels
  differences_periods_commodity_futures_market_individuals_levels_results <-
    reactive({
      
      # Filter field, regime and periods
      results <- dplyr::filter(
        differences_periods_commodity_futures_market_individuals_levels,
        field == input$differences_periods_commodity_futures_market_individuals_levels_field,
        regime == input$differences_periods_commodity_futures_market_individuals_levels_regime,
        `period 1` == input$differences_periods_commodity_futures_market_individuals_levels_period1,
        `period 2` == input$differences_periods_commodity_futures_market_individuals_levels_period2,
      ) %>% dplyr::select(-c(field, regime))
      
      # Filter ticker
      results <- if (
        input$differences_periods_commodity_futures_market_individuals_levels_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$differences_periods_commodity_futures_market_individuals_levels_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_market_individuals_levels_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_market_individuals_levels_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_market_individuals_levels <-
    renderTable({
      differences_periods_commodity_futures_market_individuals_levels_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_market_individuals_levels_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_market_individuals_levels.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_market_individuals_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ####### returns
  # Commodity futures - market variables - individuals - by year - returns
  differences_periods_commodity_futures_market_individuals_returns_results <-
    reactive({
      
      # Filter field, frequency, regime
      results <- dplyr::filter(
        differences_periods_commodity_futures_market_individuals_returns,
        field == input$differences_periods_commodity_futures_market_individuals_returns_field,
        frequency == input$differences_periods_commodity_futures_market_individuals_returns_frequency,
        regime == input$differences_periods_commodity_futures_market_individuals_returns_regime,
        `period 1` == input$differences_periods_commodity_futures_market_individuals_returns_period1,
        `period 2` == input$differences_periods_commodity_futures_market_individuals_returns_period2,
      ) %>% dplyr::select(-c(field, frequency, regime))
      
      # Filter ticker
      results <- if (
        input$differences_periods_commodity_futures_market_individuals_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$differences_periods_commodity_futures_market_individuals_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_market_individuals_returns_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_market_individuals_returns_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_market_individuals_returns <-
    renderTable({
      differences_periods_commodity_futures_market_individuals_returns_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_market_individuals_returns_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_market_individuals_returns.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_market_individuals_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ##### groups
  
  ###### levels
  # Commodity futures - market variables - groups - levels
  differences_periods_commodity_futures_market_groups_levels_results <-
    reactive({
      
      # Filter field, regime and periods
      results <- dplyr::filter(
        differences_periods_commodity_futures_market_groups_levels,
        field == input$differences_periods_commodity_futures_market_groups_levels_field,
        regime == input$differences_periods_commodity_futures_market_groups_levels_regime,
        `period 1` == input$differences_periods_commodity_futures_market_groups_levels_period1,
        `period 2` == input$differences_periods_commodity_futures_market_groups_levels_period2,
      ) %>% dplyr::select(-c(field, regime))
      
      # Filter country
      results <- if (
        input$differences_periods_commodity_futures_market_groups_levels_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$differences_periods_commodity_futures_market_groups_levels_country
        ) %>% dplyr::select(-country)
      
      # Filter sector
      results <- dplyr::filter(
        results, sector ==
          input$differences_periods_commodity_futures_market_groups_levels_sector
      ) %>% dplyr::select(-sector)
      
      
      # Filter subsector
      results <- dplyr::filter(
        results, subsector ==
          input$differences_periods_commodity_futures_market_groups_levels_subsector
      ) %>% dplyr::select(-subsector)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_market_groups_levels_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_market_groups_levels_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_market_groups_levels <-
    renderTable({
      differences_periods_commodity_futures_market_groups_levels_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_market_groups_levels_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_market_groups_levels.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_market_groups_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ####### returns
  # Commodity futures - market variables - groups - by year - returns
  differences_periods_commodity_futures_market_groups_returns_results <-
    reactive({
      
      # Filter field, frequency, regime
      results <- dplyr::filter(
        differences_periods_commodity_futures_market_groups_returns,
        field == input$differences_periods_commodity_futures_market_groups_returns_field,
        frequency == input$differences_periods_commodity_futures_market_groups_returns_frequency,
        regime == input$differences_periods_commodity_futures_market_groups_returns_regime,
        `period 1` == input$differences_periods_commodity_futures_market_groups_returns_period1,
        `period 2` == input$differences_periods_commodity_futures_market_groups_returns_period2,
      ) %>% dplyr::select(-c(field, frequency, regime))
      
      # Filter country
      results <- if (
        input$differences_periods_commodity_futures_market_groups_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$differences_periods_commodity_futures_market_groups_returns_country
        ) %>% dplyr::select(-country)
      
      # Filter sector
      results <- dplyr::filter(
        results, sector ==
          input$differences_periods_commodity_futures_market_groups_returns_sector
      ) %>% dplyr::select(-sector)
      
      # Filter subsector
      results <- dplyr::filter(
        results, subsector ==
          input$differences_periods_commodity_futures_market_groups_returns_subsector
      ) %>% dplyr::select(-subsector)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_market_groups_returns_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_market_groups_returns_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_market_groups_returns <-
    renderTable({
      differences_periods_commodity_futures_market_groups_returns_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_market_groups_returns_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_market_groups_returns.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_market_groups_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  #### positions
  
  ##### individuals
  
  ###### levels
  # Commodity futures - positions - individuals - levels
  differences_periods_commodity_futures_positions_individuals_levels_results <-
    reactive({
      
      # Filter participant, regime and periods
      results <- dplyr::filter(
        differences_periods_commodity_futures_positions_individuals_levels,
        participant == input$differences_periods_commodity_futures_positions_individuals_levels_participant,
        regime == input$differences_periods_commodity_futures_positions_individuals_levels_regime,
        `period 1` == input$differences_periods_commodity_futures_positions_individuals_levels_period1,
        `period 2` == input$differences_periods_commodity_futures_positions_individuals_levels_period2,
      ) %>% dplyr::select(-c(participant, regime))
      
      # Filter ticker
      results <- if (
        input$differences_periods_commodity_futures_positions_individuals_levels_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$differences_periods_commodity_futures_positions_individuals_levels_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_positions_individuals_levels_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_positions_individuals_levels_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_positions_individuals_levels <-
    renderTable({
      differences_periods_commodity_futures_positions_individuals_levels_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_positions_individuals_levels_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_positions_individuals_levels.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_positions_individuals_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ####### returns
  # Commodity futures - positions - individuals - by year - returns
  differences_periods_commodity_futures_positions_individuals_returns_results <-
    reactive({
      
      # Filter participant, frequency, regime
      results <- dplyr::filter(
        differences_periods_commodity_futures_positions_individuals_returns,
        participant == input$differences_periods_commodity_futures_positions_individuals_returns_participant,
        frequency == input$differences_periods_commodity_futures_positions_individuals_returns_frequency,
        regime == input$differences_periods_commodity_futures_positions_individuals_returns_regime,
        `period 1` == input$differences_periods_commodity_futures_positions_individuals_returns_period1,
        `period 2` == input$differences_periods_commodity_futures_positions_individuals_returns_period2,
      ) %>% dplyr::select(-c(participant, frequency, regime))
      
      # Filter ticker
      results <- if (
        input$differences_periods_commodity_futures_positions_individuals_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$differences_periods_commodity_futures_positions_individuals_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_positions_individuals_returns_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_positions_individuals_returns_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_positions_individuals_returns <-
    renderTable({
      differences_periods_commodity_futures_positions_individuals_returns_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_positions_individuals_returns_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_positions_individuals_returns.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_positions_individuals_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ##### groups
  
  ###### levels
  # Commodity futures - positions - groups - levels
  differences_periods_commodity_futures_positions_groups_levels_results <-
    reactive({
      
      # Filter participant, regime and periods
      results <- dplyr::filter(
        differences_periods_commodity_futures_positions_groups_levels,
        participant == input$differences_periods_commodity_futures_positions_groups_levels_participant,
        regime == input$differences_periods_commodity_futures_positions_groups_levels_regime,
        `period 1` == input$differences_periods_commodity_futures_positions_groups_levels_period1,
        `period 2` == input$differences_periods_commodity_futures_positions_groups_levels_period2,
      ) %>% dplyr::select(-c(participant, regime))
      
      # Filter country
      results <- if (
        input$differences_periods_commodity_futures_positions_groups_levels_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$differences_periods_commodity_futures_positions_groups_levels_country
        ) %>% dplyr::select(-country)
      
      # Filter sector
      results <- dplyr::filter(
        results, sector ==
          input$differences_periods_commodity_futures_positions_groups_levels_sector
      ) %>% dplyr::select(-sector)
      
      
      # Filter subsector
      results <- dplyr::filter(
        results, subsector ==
          input$differences_periods_commodity_futures_positions_groups_levels_subsector
      ) %>% dplyr::select(-subsector)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_positions_groups_levels_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_positions_groups_levels_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_positions_groups_levels <-
    renderTable({
      differences_periods_commodity_futures_positions_groups_levels_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_positions_groups_levels_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_positions_groups_levels.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_positions_groups_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ####### returns
  # Commodity futures - positions - groups - by year - returns
  differences_periods_commodity_futures_positions_groups_returns_results <-
    reactive({
      
      # Filter participant, frequency, regime
      results <- dplyr::filter(
        differences_periods_commodity_futures_positions_groups_returns,
        participant == input$differences_periods_commodity_futures_positions_groups_returns_participant,
        frequency == input$differences_periods_commodity_futures_positions_groups_returns_frequency,
        regime == input$differences_periods_commodity_futures_positions_groups_returns_regime,
        `period 1` == input$differences_periods_commodity_futures_positions_groups_returns_period1,
        `period 2` == input$differences_periods_commodity_futures_positions_groups_returns_period2,
      ) %>% dplyr::select(-c(participant, frequency, regime))
      
      # Filter country
      results <- if (
        input$differences_periods_commodity_futures_positions_groups_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$differences_periods_commodity_futures_positions_groups_returns_country
        ) %>% dplyr::select(-country)
      
      # Filter sector
      results <- dplyr::filter(
        results, sector ==
          input$differences_periods_commodity_futures_positions_groups_returns_sector
      ) %>% dplyr::select(-sector)
      
      # Filter subsector
      results <- dplyr::filter(
        results, subsector ==
          input$differences_periods_commodity_futures_positions_groups_returns_subsector
      ) %>% dplyr::select(-subsector)
      
      # Filter moment
      results <- if (
        input$differences_periods_commodity_futures_positions_groups_returns_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_periods_commodity_futures_positions_groups_returns_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_periods_commodity_futures_positions_groups_returns <-
    renderTable({
      differences_periods_commodity_futures_positions_groups_returns_results()
    }, digits = 4L)
  output$differences_periods_commodity_futures_positions_groups_returns_download <- 
    downloadHandler(
      filename = "differences_periods_commodity_futures_positions_groups_returns.csv",
      content = function(file) {
        data <- differences_periods_commodity_futures_positions_groups_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ### factors
  
  #### asset pool: US commodities
  
  ##### returns
  
  # factors - asset pool: US commodities- returns
  differences_periods_factors_US_commos_returns_results <- 
    reactive({
      # browser()
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        differences_periods_factors_US_commos, 
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$differences_periods_factors_US_commos_regime
      ) %>%
        dplyr::select(
          -c(
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter factor
        {
          if (input$differences_periods_factors_US_commos_factor == "all") .
          else 
            dplyr::filter(
              ., name == 
                input$differences_periods_factors_US_commos_factor
            ) %>% dplyr::select(-name)
        } %>%
        # Filter leg
        {
          if (input$differences_periods_factors_US_commos_leg == "all") .
          else 
            dplyr::filter(
              ., leg == 
                input$differences_periods_factors_US_commos_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter period 1
        {
          dplyr::filter(
            ., `period 1` == input$differences_periods_factors_US_commos_period1
          )
        } %>%
        # Filter period 2
        {
          dplyr::filter(
            ., `period 2` == input$differences_periods_factors_US_commos_period2
          )
        }  %>%
        # Filter moment
        {
          if (input$differences_periods_factors_US_commos_moment == "all") .
          else 
            dplyr::filter(
              ., moment == 
                input$differences_periods_factors_US_commos_moment
            ) %>% dplyr::select(-moment)
        }
      
      results
    })
  
  output$differences_periods_factors_US_commos_returns <- 
    renderTable({differences_periods_factors_US_commos_returns_results()
    }, digits = 4L)
  output$differences_periods_factors_US_commos_returns_download <- 
    downloadHandler(
      filename = "differences_periods_factors_US_commos_returns.csv",
      content = function(file) {
        data <- differences_periods_factors_US_commos_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ## regimes
  
  ### commodity futures
  
  #### market variables
  
  ##### individuals
  
  ###### levels
  # Commodity futures - market variables - individuals - levels
  differences_regimes_commodity_futures_market_individuals_levels_results <-
    reactive({
      
      # Filter ticker
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_levels_commodity ==
        "all"
      ) differences_regimes_commodity_futures_market_individuals_levels
      else
        dplyr::filter(
          differences_regimes_commodity_futures_market_individuals_levels, 
          commodity ==
            input$differences_regimes_commodity_futures_market_individuals_levels_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter field
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_levels_field ==
        "all"
      ) results
      else
        dplyr::filter(
          results, 
          field ==
            input$differences_regimes_commodity_futures_market_individuals_levels_field
        ) %>% dplyr::select(-field)
      
      # Filter period
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_levels_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, 
          period ==
            input$differences_regimes_commodity_futures_market_individuals_levels_period
        ) %>% dplyr::select(-period)
      
      # Filter moment
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_levels_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, 
          moment ==
            input$differences_regimes_commodity_futures_market_individuals_levels_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_regimes_commodity_futures_market_individuals_levels <-
    renderTable({
      differences_regimes_commodity_futures_market_individuals_levels_results()
    }, digits = 4L)
  output$differences_regimes_commodity_futures_market_individuals_levels_download <- 
    downloadHandler(
      filename = "differences_regimes_commodity_futures_market_individuals_levels.csv",
      content = function(file) {
        data <- differences_regimes_commodity_futures_market_individuals_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ####### returns
  # Commodity futures - market variables - individuals - by year - returns
  differences_regimes_commodity_futures_market_individuals_returns_results <-
    reactive({
      
      # Filter field, frequency, regime
      results <- dplyr::filter(
        differences_regimes_commodity_futures_market_individuals_returns,
        field == input$differences_regimes_commodity_futures_market_individuals_returns_field,
        frequency == input$differences_regimes_commodity_futures_market_individuals_returns_frequency
      ) %>% dplyr::select(-c(field, frequency))
      
      # Filter ticker
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_returns_commodity ==
        "all"
      ) results
      else
        dplyr::filter(
          results, commodity ==
            input$differences_regimes_commodity_futures_market_individuals_returns_commodity
        ) %>% dplyr::select(-commodity)
      
      # Filter period
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_returns_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$differences_regimes_commodity_futures_market_individuals_returns_period
        ) %>% dplyr::select(-period)
      
      # Filter moment
      results <- if (
        input$differences_regimes_commodity_futures_market_individuals_returns_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_regimes_commodity_futures_market_individuals_returns_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_regimes_commodity_futures_market_individuals_returns <-
    renderTable({
      differences_regimes_commodity_futures_market_individuals_returns_results()
    }, digits = 4L)
  output$differences_regimes_commodity_futures_market_individuals_returns_download <- 
    downloadHandler(
      filename = "differences_regimes_commodity_futures_market_individuals_returns.csv",
      content = function(file) {
        data <- differences_regimes_commodity_futures_market_individuals_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ##### groups
  
  ###### levels
  # Commodity futures - market variables - groups - levels
  differences_regimes_commodity_futures_market_groups_levels_results <-
    reactive({
      
      # browser()
      
      # Filter field
      results <- dplyr::filter(
        differences_regimes_commodity_futures_market_groups_levels,
        field == input$differences_regimes_commodity_futures_market_groups_levels_field,
      ) %>% dplyr::select(-c(field))
      
      # Filter country
      results <- if (
        input$differences_regimes_commodity_futures_market_groups_levels_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$differences_regimes_commodity_futures_market_groups_levels_country
        ) 
      # %>% dplyr::select(-country)
      
      # Filter sector
      results <- dplyr::filter(
        results, sector ==
          input$differences_regimes_commodity_futures_market_groups_levels_sector
      ) 
      # %>% dplyr::select(-sector)
      
      
      # Filter subsector
      results <- dplyr::filter(
        results, subsector ==
          input$differences_regimes_commodity_futures_market_groups_levels_subsector
      ) 
      # %>% dplyr::select(-subsector)
      
      # Filter period
      results <- if (
        input$differences_regimes_commodity_futures_market_groups_levels_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$differences_regimes_commodity_futures_market_groups_levels_period
        ) 
      # %>% dplyr::select(-period)
      
      # Filter moment
      results <- if (
        input$differences_regimes_commodity_futures_market_groups_levels_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_regimes_commodity_futures_market_groups_levels_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_regimes_commodity_futures_market_groups_levels <-
    renderTable({
      differences_regimes_commodity_futures_market_groups_levels_results()
    }, digits = 4L)
  output$differences_regimes_commodity_futures_market_groups_levels_download <- 
    downloadHandler(
      filename = "differences_regimes_commodity_futures_market_groups_levels.csv",
      content = function(file) {
        data <- differences_regimes_commodity_futures_market_groups_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ####### returns
  # Commodity futures - market variables - groups - by year - returns
  differences_regimes_commodity_futures_market_groups_returns_results <-
    reactive({
      
      # browser()
      
      # Filter field, frequency
      results <- dplyr::filter(
        differences_regimes_commodity_futures_market_groups_returns,
        field == input$differences_regimes_commodity_futures_market_groups_returns_field,
        frequency == input$differences_regimes_commodity_futures_market_groups_returns_frequency,
      ) %>% dplyr::select(-c(field, frequency))
      
      # Filter country
      results <- if (
        input$differences_regimes_commodity_futures_market_groups_returns_country ==
        "all"
      ) results
      else
        dplyr::filter(
          results, country ==
            input$differences_regimes_commodity_futures_market_groups_returns_country
        ) 
      # %>% dplyr::select(-country)
      
      # Filter sector
      results <- dplyr::filter(
        results, sector ==
          input$differences_regimes_commodity_futures_market_groups_returns_sector
      ) 
      # %>% dplyr::select(-sector)
      
      # Filter subsector
      results <- dplyr::filter(
        results, subsector ==
          input$differences_regimes_commodity_futures_market_groups_returns_subsector
      ) 
      # %>% dplyr::select(-subsector)
      
      # Filter period
      results <- if (
        input$differences_regimes_commodity_futures_market_groups_returns_period ==
        "all"
      ) results
      else
        dplyr::filter(
          results, period ==
            input$differences_regimes_commodity_futures_market_groups_returns_period
        ) 
      # %>% dplyr::select(-period)
      
      # Filter moment
      results <- if (
        input$differences_regimes_commodity_futures_market_groups_returns_moment ==
        "all"
      ) results
      else
        dplyr::filter(
          results, moment ==
            input$differences_regimes_commodity_futures_market_groups_returns_moment
        ) %>% dplyr::select(-moment)
      
      results
    })
  
  output$differences_regimes_commodity_futures_market_groups_returns <-
    renderTable({
      differences_regimes_commodity_futures_market_groups_returns_results()
    }, digits = 4L)
  output$differences_regimes_commodity_futures_market_groups_returns_download <- 
    downloadHandler(
      filename = "differences_regimes_commodity_futures_market_groups_returns.csv",
      content = function(file) {
        data <- differences_regimes_commodity_futures_market_groups_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### factors
  
  #### asset pool: US commodities
  
  ##### returns
  
  # factors - asset pool: US commodities- returns
  differences_regimes_factors_US_commos_returns_results <- 
    reactive({
      # browser()
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        differences_regimes_factors_US_commos, 
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold
      ) %>%
        dplyr::select(
          -c(
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`
          )
        ) %>%
        # Filter factor
        {
          if (input$differences_regimes_factors_US_commos_factor == "all") .
          else 
            dplyr::filter(
              ., name == input$differences_regimes_factors_US_commos_factor
            ) %>% dplyr::select(-name)
        } %>%
        # Filter leg
        {
          if (input$differences_regimes_factors_US_commos_leg == "all") .
          else 
            dplyr::filter(
              ., leg == input$differences_regimes_factors_US_commos_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter period
        {
          dplyr::filter(
            ., period == input$differences_regimes_factors_US_commos_period
          )
        } %>%
        # Filter moment
        {
          if (input$differences_regimes_factors_US_commos_moment == "all") .
          else 
            dplyr::filter(
              ., moment == input$differences_regimes_factors_US_commos_moment
            ) %>% dplyr::select(-moment)
        }
      
      results
    })
  
  output$differences_regimes_factors_US_commos_returns <- 
    renderTable({differences_regimes_factors_US_commos_returns_results()
    }, digits = 4L)
  output$differences_regimes_factors_US_commos_returns_download <- 
    downloadHandler(
      filename = "differences_regimes_factors_US_commos_returns.csv",
      content = function(file) {
        data <- differences_regimes_factors_US_commos_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### asset pool: picking factors
  
  #### by period
  
  ##### returns
  
  # factors - asset pool: picking factors - by period - returns
  differences_regimes_factors_from_picks_periods_returns_results <- 
    reactive({
      # browser()
      # Filter update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        differences_regimes_factors_from_picks_periods, 
        `picking factor name` ==
          input$differences_regimes_factors_from_picks_periods_picking_factor,
        `picking factor asset pool` ==
          input$differences_regimes_factors_from_picks_periods_picking_factor_asset_pool,
        `picking factor leg` ==
          input$differences_regimes_factors_from_picks_periods_picking_factor_leg,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold
      ) %>%
        dplyr::select(
          -c(
            `picking factor name`, `picking factor asset pool`, `picking factor leg`,
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`
          )
        ) %>%
        # Filter factor
        {
          if (
            input$differences_regimes_factors_from_picks_periods_factor == "all"
          ) .
          else 
            dplyr::filter(
              ., `factor name` == input$differences_regimes_factors_from_picks_periods_factor
            ) %>% dplyr::select(-`factor name`)
        } %>%
        # Filter leg
        {
          if (
            input$differences_regimes_factors_from_picks_periods_leg == "all"
          ) .
          else 
            dplyr::filter(
              ., leg == input$differences_regimes_factors_from_picks_periods_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$differences_regimes_factors_from_picks_periods_period == "all"
          ) .
          else 
            dplyr::filter(
              ., period == input$differences_regimes_factors_from_picks_periods_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter moment
        {
          if (input$differences_regimes_factors_from_picks_periods_moment == "all") .
          else 
            dplyr::filter(
              ., moment == input$differences_regimes_factors_from_picks_periods_moment
            ) %>% dplyr::select(-moment)
        }
      
      results
    })
  
  output$differences_regimes_factors_from_picks_periods_returns <- 
    renderTable({
      differences_regimes_factors_from_picks_periods_returns_results()
    }, digits = 4L)
  output$differences_regimes_factors_from_picks_periods_returns_download <- 
    downloadHandler(
      filename = "differences_regimes_factors_from_picks_periods_returns.csv",
      content = function(file) {
        data <- differences_regimes_factors_from_picks_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  # factor picks
  factor_picks_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold
      results <- dplyr::filter(
        picks, 
        `asset pool` == input$picks_factor_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold
      ) %>%
        dplyr::select(
          -c(
            `asset pool`,
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`
          )
        ) %>%
        # Filter factor
        {
          if (input$picks_factor == "all") .
          else dplyr::filter(., factor == input$picks_factor ) %>%
            dplyr::select(-factor)
        } %>%
        # Filter leg
        {
          if (input$picks_factor_leg == "all") .
          else dplyr::filter(., leg == input$picks_factor_leg) %>%
            dplyr::select(-leg)
        } %>%
        # Filter period
        {
          if (input$picks_period == "all") .
          else dplyr::filter(., period == input$picks_period) %>%
            dplyr::select(-period)
        }
      
      results
    })
  
  output$picks <- renderTable({factor_picks_results()}, digits = 4L)
  output$factor_picks_download <- 
    downloadHandler(
      filename = "factor_picks.csv",
      content = function(file) {
        data <- factor_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Regressions
  
  ## Time series
  
  ### US commodities ~ US commodity individual CHP
  
  time_series_US_commos_vs_US_individual_CHP_results <-  
    reactive({
      
      # browser()
      results <- dplyr::select(time_series_US_commos_vs_US_individual_CHP, -country) %>%
        # Filter sector
        {
          if ( input$time_series_US_commos_vs_US_individual_CHP_sector == "all") .
          else
            dplyr::filter(
              ., sector == input$time_series_US_commos_vs_US_individual_CHP_sector
            )
        } %>%
        # Filter subsector
        {
          if ( input$time_series_US_commos_vs_US_individual_CHP_subsector == "all") .
          else
            dplyr::filter(
              ., subsector == input$time_series_US_commos_vs_US_individual_CHP_subsector
            )
        } %>%
        # Filter regime CHP type
        {
          if ( input$time_series_US_commos_vs_US_individual_CHP_regime_CHP_type == "all") .
          else
            dplyr::filter(
              ., 
              regime.CHP.type == input$time_series_US_commos_vs_US_individual_CHP_regime_CHP_type
            )
        } %>%
        dplyr::rename(`CHP type (regimes)` = regime.CHP.type) %>%
        # Filter CHP regime
        {
          if (input$time_series_US_commos_vs_US_individual_CHP_regime == "all") .
          else
            dplyr::filter(
              ., 
              CHP.regime == input$time_series_US_commos_vs_US_individual_CHP_regime
            )
        } %>%
        dplyr::rename(regime = CHP.regime) %>%
        # Filter regressor
        {
          if (input$time_series_US_commos_vs_US_individual_CHP_regressor == "all") .
          else
            dplyr::filter(
              ., 
              regressor == input$time_series_US_commos_vs_US_individual_CHP_regressor
            )
        } %>%
        # Filter commodity
        {
          if (input$time_series_US_commos_vs_US_individual_CHP_commodity == "all") .
          else
            dplyr::filter(
              ., 
              commodity == input$time_series_US_commos_vs_US_individual_CHP_commodity
            )
        } %>%
        # Filter period
        {
          if (input$time_series_US_commos_vs_US_individual_CHP_period == "all") .
          else
            dplyr::filter(
              ., 
              period == input$time_series_US_commos_vs_US_individual_CHP_period
            )
        } %>%
        # Filter show
        {
          if (input$time_series_US_commos_vs_US_individual_CHP_show == "coefficients") 
            dplyr::select(., -c(rsquared)) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_US_commos_vs_US_individual_CHP_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      results
    })
  
  output$time_series_US_commos_vs_US_individual_CHP <- 
    renderTable({
      time_series_US_commos_vs_US_individual_CHP_results()
    }, digits = 4L)
  output$time_series_US_commos_vs_US_individual_CHP_download <- 
    downloadHandler(
      filename = "time_series_US_commos_vs_US_individual_CHP.csv",
      content = function(file) {
        data <- time_series_US_commos_vs_US_individual_CHP_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### US commodities ~ US commodity aggregate CHP
  
  time_series_US_commos_vs_US_aggregate_CHP_results <-  
    reactive({
      
      # browser()
      results <- dplyr::select(time_series_US_commos_vs_US_aggregate_CHP, -country) %>%
        # Filter sector
        {
          if ( input$time_series_US_commos_vs_US_aggregate_CHP_sector == "all") .
          else
            dplyr::filter(
              ., sector == input$time_series_US_commos_vs_US_aggregate_CHP_sector
            )
        } %>%
        # Filter subsector
        {
          if ( input$time_series_US_commos_vs_US_aggregate_CHP_subsector == "all") .
          else
            dplyr::filter(
              ., subsector == input$time_series_US_commos_vs_US_aggregate_CHP_subsector
            )
        } %>%
        # Filter regime CHP type
        {
          if ( input$time_series_US_commos_vs_US_aggregate_CHP_regime_CHP_type == "all") .
          else
            dplyr::filter(
              ., 
              regime.CHP.type == input$time_series_US_commos_vs_US_aggregate_CHP_regime_CHP_type
            )
        } %>%
        dplyr::rename(`CHP type (regimes)` = regime.CHP.type) %>%
        # Filter CHP regime
        {
          if (input$time_series_US_commos_vs_US_aggregate_CHP_regime == "all") .
          else
            dplyr::filter(
              ., 
              CHP.regime == input$time_series_US_commos_vs_US_aggregate_CHP_regime
            )
        } %>%
        dplyr::rename(regime = CHP.regime) %>%
        # Filter regressor
        {
          if (input$time_series_US_commos_vs_US_aggregate_CHP_regressor == "all") .
          else
            dplyr::filter(
              ., 
              regressor == input$time_series_US_commos_vs_US_aggregate_CHP_regressor
            )
        } %>%
        # Filter commodity
        {
          if (input$time_series_US_commos_vs_US_aggregate_CHP_commodity == "all") .
          else
            dplyr::filter(
              ., 
              commodity == input$time_series_US_commos_vs_US_aggregate_CHP_commodity
            )
        } %>%
        # Filter period
        {
          if (input$time_series_US_commos_vs_US_aggregate_CHP_period == "all") .
          else
            dplyr::filter(
              ., 
              period == input$time_series_US_commos_vs_US_aggregate_CHP_period
            )
        } %>%
        # Filter show
        {
          if (input$time_series_US_commos_vs_US_aggregate_CHP_show == "coefficients") 
            dplyr::select(., -c(rsquared)) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_US_commos_vs_US_aggregate_CHP_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      results
    })
  
  output$time_series_US_commos_vs_US_aggregate_CHP <- 
    renderTable({
      time_series_US_commos_vs_US_aggregate_CHP_results()
    }, digits = 4L)
  output$time_series_US_commos_vs_US_aggregate_CHP_download <- 
    downloadHandler(
      filename = "time_series_US_commos_vs_US_aggregate_CHP.csv",
      content = function(file) {
        data <- time_series_US_commos_vs_US_aggregate_CHP_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  

  
  ### UK commodities ~ US commodity aggregate CHP
  
  time_series_UK_commos_vs_US_aggregate_CHP_results <-  
    reactive({
      
      # browser()
      results <- dplyr::select(time_series_UK_commos_vs_US_aggregate_CHP, -country) %>%
        # Filter sector
        {
          if ( input$time_series_UK_commos_vs_US_aggregate_CHP_sector == "all") .
          else
            dplyr::filter(
              ., sector == input$time_series_UK_commos_vs_US_aggregate_CHP_sector
            )
        } %>%
        # Filter subsector
        {
          if ( input$time_series_UK_commos_vs_US_aggregate_CHP_subsector == "all") .
          else
            dplyr::filter(
              ., subsector == input$time_series_UK_commos_vs_US_aggregate_CHP_subsector
            )
        } %>%
        # Filter regime CHP type
        {
          if ( input$time_series_UK_commos_vs_US_aggregate_CHP_regime_CHP_type == "all") .
          else
            dplyr::filter(
              ., 
              regime.CHP.type == input$time_series_UK_commos_vs_US_aggregate_CHP_regime_CHP_type
            )
        } %>%
        dplyr::rename(`CHP type (regimes)` = regime.CHP.type) %>%
        # Filter CHP regime
        {
          if (input$time_series_UK_commos_vs_US_aggregate_CHP_regime == "all") .
          else
            dplyr::filter(
              ., 
              CHP.regime == input$time_series_UK_commos_vs_US_aggregate_CHP_regime
            )
        } %>%
        dplyr::rename(regime = CHP.regime) %>%
        # Filter regressor
        {
          if (input$time_series_UK_commos_vs_US_aggregate_CHP_regressor == "all") .
          else
            dplyr::filter(
              ., 
              regressor == input$time_series_UK_commos_vs_US_aggregate_CHP_regressor
            )
        } %>%
        # Filter commodity
        {
          if (input$time_series_UK_commos_vs_US_aggregate_CHP_commodity == "all") .
          else
            dplyr::filter(
              ., 
              commodity == input$time_series_UK_commos_vs_US_aggregate_CHP_commodity
            )
        } %>%
        # Filter period
        {
          if (input$time_series_UK_commos_vs_US_aggregate_CHP_period == "all") .
          else
            dplyr::filter(
              ., 
              period == input$time_series_UK_commos_vs_US_aggregate_CHP_period
            )
        } %>%
        # Filter show
        {
          if (input$time_series_UK_commos_vs_US_aggregate_CHP_show == "coefficients") 
            dplyr::select(., -c(rsquared)) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_UK_commos_vs_US_aggregate_CHP_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      results
    })
  
  output$time_series_UK_commos_vs_US_aggregate_CHP <- 
    renderTable({
      time_series_UK_commos_vs_US_aggregate_CHP_results()
    }, digits = 4L)
  output$time_series_UK_commos_vs_US_aggregate_CHP_download <- 
    downloadHandler(
      filename = "time_series_UK_commos_vs_US_aggregate_CHP.csv",
      content = function(file) {
        data <- time_series_UK_commos_vs_US_aggregate_CHP_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )

  
    
  ### US commodities ~ factors from US commodities
  
  time_series_US_commos_vs_factors_from_US_commos_results <-  
    reactive({
      
      factor_1 <- input$time_series_US_commos_vs_factors_from_US_commos_factor_1
      if (
        input$time_series_US_commos_vs_factors_from_US_commos_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$time_series_US_commos_vs_factors_from_US_commos_factor_2
      }
      if (
        input$time_series_US_commos_vs_factors_from_US_commos_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$time_series_US_commos_vs_factors_from_US_commos_factor_3
      }
      if (
        input$time_series_US_commos_vs_factors_from_US_commos_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$time_series_US_commos_vs_factors_from_US_commos_factor_4
      }
      if (
        input$time_series_US_commos_vs_factors_from_US_commos_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$time_series_US_commos_vs_factors_from_US_commos_factor_5
      }
      
      
      if (
        input$time_series_US_commos_vs_factors_from_US_commos_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime, and model (regressors)
      results <- dplyr::filter(
        time_series_US_commos_vs_factors_from_US_commos,
        `asset pool` ==
          input$time_series_US_commos_vs_factors_from_US_commos_factor_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$time_series_US_commos_vs_factors_from_US_commos_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `asset pool`, `update frequency`, `return frequency`, `ranking period`,
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter commodity
        {
          if ( input$time_series_US_commos_vs_factors_from_US_commos_commo == "all") .
          else
            dplyr::filter(
              ., 
              commodity == input$time_series_US_commos_vs_factors_from_US_commos_commo
            )
        } %>%
        # Filter leg
        {
          if (
            input$time_series_US_commos_vs_factors_from_US_commos_factor_leg == "all"
          ) .
          else
            dplyr::filter(
              ., 
              leg == input$time_series_US_commos_vs_factors_from_US_commos_factor_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter period
        {
          if (
            input$time_series_US_commos_vs_factors_from_US_commos_period == "all"
          ) .
          else
            dplyr::filter(
              ., 
              period == input$time_series_US_commos_vs_factors_from_US_commos_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter show
        {
          if (
            input$time_series_US_commos_vs_factors_from_US_commos_show == "coefficients"
          ) dplyr::select(., -c(rsquared, regressors)) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_US_commos_vs_factors_from_US_commos_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      results
    })
  
  output$time_series_US_commos_vs_factors_from_US_commos <- 
    renderTable({
      time_series_US_commos_vs_factors_from_US_commos_results()
    }, digits = 4L)
  output$time_series_US_commos_vs_factors_from_US_commos_download <- 
    downloadHandler(
      filename = "time_series_US_commos_vs_factors_from_US_commos.csv",
      content = function(file) {
        data <- time_series_US_commos_vs_factors_from_US_commos_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  ### factor picks ~ factors from picks
  time_series_factor_picks_vs_factors_from_picks_results <-  
    reactive({
      
      factor_1 <- input$time_series_factor_picks_vs_factors_from_picks_factor_1
      if (
        input$time_series_factor_picks_vs_factors_from_picks_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$time_series_factor_picks_vs_factors_from_picks_factor_2
      }
      if (
        input$time_series_factor_picks_vs_factors_from_picks_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$time_series_factor_picks_vs_factors_from_picks_factor_3
      }
      if (
        input$time_series_factor_picks_vs_factors_from_picks_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$time_series_factor_picks_vs_factors_from_picks_factor_4
      }
      if (
        input$time_series_factor_picks_vs_factors_from_picks_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$time_series_factor_picks_vs_factors_from_picks_factor_5
      }
      
      
      if (
        input$time_series_factor_picks_vs_factors_from_picks_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # browser()
      
      # Filter picking factor asset pool, picking factor leg, picking factor name, 
      # update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime, and model (regressors)
      results <- dplyr::filter(
        time_series_factor_picks_vs_factors_from_picks, 
        `picking factor asset pool` ==
          input$time_series_factor_picks_vs_factors_from_picks_picking_factor_asset_pool,
        `picking factor leg` ==
          input$time_series_factor_picks_vs_factors_from_picks_picking_factor_leg,
        `picking factor name` ==
          input$time_series_factor_picks_vs_factors_from_picks_picking_factor,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$time_series_factor_picks_vs_factors_from_picks_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `picking factor asset pool`, `picking factor leg`, 
            `picking factor name`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, regime, 
            regressors
          )
        ) %>%
        # Filter pick
        {
          if (
            input$time_series_factor_picks_vs_factors_from_picks_pick == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., pick == 
                input$time_series_factor_picks_vs_factors_from_picks_pick
            )
        } %>%
        # Filter period
        {
          if (
            input$time_series_factor_picks_vs_factors_from_picks_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$time_series_factor_picks_vs_factors_from_picks_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter leg
        {
          dplyr::filter(
            ., `factor leg` == 
              input$time_series_factor_picks_vs_factors_from_picks_leg
          )
        } %>%
        # Filter show
        {
          if (
            input$time_series_factor_picks_vs_factors_from_picks_show == "coefficients"
          ) dplyr::select(., -rsquared) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_factor_picks_vs_factors_from_picks_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      
      results
    })
  
  output$time_series_factor_picks_vs_factors_from_picks <- 
    renderTable({
      time_series_factor_picks_vs_factors_from_picks_results()
    }, digits = 4L)
  output$time_series_factor_picks_vs_factors_from_picks_download <- 
    downloadHandler(
      filename = "time_series_factor_picks_vs_factors_from_picks.csv",
      content = function(file) {
        data <- time_series_factor_picks_vs_factors_from_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  ### US commodities ~ factors from picks
  
  time_series_US_commodities_vs_factors_from_picks_results <-  
    reactive({
      
      factor_1 <- input$time_series_US_commodities_vs_factors_from_picks_factor_1
      if (
        input$time_series_US_commodities_vs_factors_from_picks_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$time_series_US_commodities_vs_factors_from_picks_factor_2
      }
      if (
        input$time_series_US_commodities_vs_factors_from_picks_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$time_series_US_commodities_vs_factors_from_picks_factor_3
      }
      if (
        input$time_series_US_commodities_vs_factors_from_picks_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$time_series_US_commodities_vs_factors_from_picks_factor_4
      }
      if (
        input$time_series_US_commodities_vs_factors_from_picks_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$time_series_US_commodities_vs_factors_from_picks_factor_5
      }
      
      if (
        input$time_series_US_commodities_vs_factors_from_picks_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # browser()
      
      # Filter picking factor asset pool, picking factor leg, picking factor name, 
      # update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime and model (regressors)
      results <- dplyr::filter(
        time_series_US_commodities_vs_factors_from_picks, 
        `picking factor asset pool` ==
          input$time_series_US_commodities_vs_factors_from_picks_picking_factor_asset_pool,
        `picking factor leg` ==
          input$time_series_US_commodities_vs_factors_from_picks_picking_factor_leg,
        `picking factor name` ==
          input$time_series_US_commodities_vs_factors_from_picks_picking_factor,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$time_series_US_commodities_vs_factors_from_picks_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `picking factor asset pool`, `picking factor leg`, 
            `picking factor name`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, regime, 
            regressors
          )
        ) %>%
        # Filter commodity
        {
          if (
            input$time_series_US_commodities_vs_factors_from_picks_commodity == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., commodity == 
                input$time_series_US_commodities_vs_factors_from_picks_commodity
            )
        } %>%
        # Filter period
        {
          if (
            input$time_series_US_commodities_vs_factors_from_picks_period == "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$time_series_US_commodities_vs_factors_from_picks_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter leg
        {
          if (
            input$time_series_US_commodities_vs_factors_from_picks_leg == "all"
          ) .
          else 
            dplyr::filter(
              ., `factor leg` == 
                input$time_series_US_commodities_vs_factors_from_picks_leg
            ) %>% dplyr::select(-`factor leg`)
        } %>%
        # Filter show
        {
          if (
            input$time_series_US_commodities_vs_factors_from_picks_show == "coefficients"
          ) dplyr::select(., -rsquared) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_US_commodities_vs_factors_from_picks_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      
      results
    })
  
  output$time_series_US_commodities_vs_factors_from_picks <- 
    renderTable({
      time_series_US_commodities_vs_factors_from_picks_results()
    }, digits = 4L)
  output$time_series_US_commodities_vs_factors_from_picks_download <- 
    downloadHandler(
      filename = "time_series_US_commodities_vs_factors_from_picks.csv",
      content = function(file) {
        data <- time_series_US_commodities_vs_factors_from_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  ### UK metals ~ factors from picks (regressor: factor)
  time_series_UK_metals_vs_factors_from_picks_results <-  
    reactive({
      
      factor_1 <- input$time_series_UK_metals_vs_factors_from_picks_factor_1
      if (
        input$time_series_UK_metals_vs_factors_from_picks_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$time_series_UK_metals_vs_factors_from_picks_factor_2
      }
      if (
        input$time_series_UK_metals_vs_factors_from_picks_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$time_series_UK_metals_vs_factors_from_picks_factor_3
      }
      if (
        input$time_series_UK_metals_vs_factors_from_picks_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$time_series_UK_metals_vs_factors_from_picks_factor_4
      }
      if (
        input$time_series_UK_metals_vs_factors_from_picks_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$time_series_UK_metals_vs_factors_from_picks_factor_5
      }
      
      
      if (
        input$time_series_UK_metals_vs_factors_from_picks_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # browser()
      # Filter picking factor asset pool, picking factor leg, picking factor name, 
      # update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime and regressors
      results <- dplyr::filter(
        time_series_UK_metals_vs_factors_from_picks, 
        `picking factor asset pool` ==
          input$time_series_UK_metals_vs_factors_from_picks_picking_factor_asset_pool,
        `picking factor leg` ==
          input$time_series_UK_metals_vs_factors_from_picks_picking_factor_leg,
        `picking factor name` ==
          input$time_series_UK_metals_vs_factors_from_picks_picking_factor,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$time_series_UK_metals_vs_factors_from_picks_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `picking factor asset pool`, `picking factor leg`, 
            `picking factor name`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, regime, 
            regressors
          )
        ) %>%
        # Filter commodity
        {
          if (
            input$time_series_UK_metals_vs_factors_from_picks_commodity == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., commodity == 
                input$time_series_UK_metals_vs_factors_from_picks_commodity
            )
        } %>%
        # Filter period
        {
          if (
            input$time_series_UK_metals_vs_factors_from_picks_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$time_series_UK_metals_vs_factors_from_picks_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter leg
        {
          if (
            input$time_series_UK_metals_vs_factors_from_picks_leg == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., `factor leg` == 
                input$time_series_UK_metals_vs_factors_from_picks_leg
            ) %>%
            dplyr::select(-`factor leg`)
        } %>%
        # Filter show
        {
          if (
            input$time_series_UK_metals_vs_factors_from_picks_show == "coefficients"
          ) dplyr::select(., -rsquared) %>% tidyr::unnest(coefficients)
          else dplyr::select(., -coefficients)
        } %>%
        # Filter summary
        {
          if (
            input$time_series_UK_metals_vs_factors_from_picks_summary == "average"
          ) {
            if ("term" %in% names(.)){
              dplyr::group_by(., term) %>%
                dplyr::summarise_if(.predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } else {
              dplyr::summarise_if(., .predicate = is.numeric, .funs = mean, na.rm = TRUE)
            } 
            
          }
          else .
        }
      
      results
    })
  
  output$time_series_UK_metals_vs_factors_from_picks <- 
    renderTable({
      time_series_UK_metals_vs_factors_from_picks_results()
    }, digits = 4L)
  output$time_series_UK_metals_vs_factors_from_picks_download <- 
    downloadHandler(
      filename = "time_series_UK_metals_vs_factors_from_picks.csv",
      content = function(file) {
        data <- time_series_UK_metals_vs_factors_from_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  # ### UK metals ~ factors from picks (regressor: factor leg)
  # 
  # time_series_UK_metals_vs_factors_from_picks_legs_results <-  
  #   reactive({
  #     # browser()
  #     factor_1 <- input$time_series_UK_metals_vs_factors_from_picks_legs_factor_1
  #     if (
  #       input$time_series_UK_metals_vs_factors_from_picks_legs_factor_2 == "none"
  #     ) {
  #       factor_2 <- NA_character_
  #     } else {
  #       factor_2 <- input$time_series_UK_metals_vs_factors_from_picks_legs_factor_2
  #     }
  #     if (
  #       input$time_series_UK_metals_vs_factors_from_picks_legs_factor_3 == "none"
  #     ) {
  #       factor_3 <- NA_character_
  #     } else {
  #       factor_3 <- input$time_series_UK_metals_vs_factors_from_picks_legs_factor_3
  #     }
  #     if (
  #       input$time_series_UK_metals_vs_factors_from_picks_legs_factor_4 == "none"
  #     ) {
  #       factor_4 <- NA_character_
  #     } else {
  #       factor_4 <- input$time_series_UK_metals_vs_factors_from_picks_legs_factor_4
  #     }
  #     if (
  #       input$time_series_UK_metals_vs_factors_from_picks_legs_factor_5 == "none"
  #     ) {
  #       factor_5 <- NA_character_
  #     } else {
  #       factor_5 <- input$time_series_UK_metals_vs_factors_from_picks_legs_factor_5
  #     }
  #     
  #     
  #     if (
  #       input$time_series_UK_metals_vs_factors_from_picks_legs_factor_1 == "all"
  #     ) {model <- regression_factors} else {
  #       model <- paste(
  #         c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
  #         , collapse = " + "
  #       )
  #     }
  #     
  #     # Filter picking factor asset pool, picking factor leg, picking factor name, 
  #     # update frequency, return frequency, ranking period,
  #     # long threshold, short threshold, regime and model (regressors)
  #     results <- dplyr::filter(
  #       time_series_UK_metals_vs_factors_from_picks_legs, 
  #       `picking factor asset pool` ==
  #         input$time_series_UK_metals_vs_factors_from_picks_legs_picking_factor_asset_pool,
  #       `picking factor leg` ==
  #         input$time_series_UK_metals_vs_factors_from_picks_legs_picking_factor_leg,
  #       `picking factor name` ==
  #         input$time_series_UK_metals_vs_factors_from_picks_legs_picking_factor,
  #       `update frequency` == input$factors_param_update_freq,
  #       `return frequency` == input$factors_param_return_freq,
  #       `ranking period` == input$factors_param_ranking_period,
  #       `long threshold` == input$factors_param_long_threshold,
  #       `short threshold` == input$factors_param_short_threshold,
  #       regime == input$time_series_UK_metals_vs_factors_from_picks_legs_regime,
  #       regressors %in% model
  #     ) %>%
  #       dplyr::select(
  #         -c(
  #           `picking factor asset pool`, `picking factor leg`, 
  #           `picking factor name`, `asset pool`,
  #           `update frequency`, `return frequency`, `ranking period`, 
  #           `long threshold`, `short threshold`, regime, regressors
  #         )
  #       ) %>%
  #       # Filter commodity
  #       {
  #         if (
  #           input$time_series_UK_metals_vs_factors_from_picks_legs_commodity == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., commodity == 
  #               input$time_series_UK_metals_vs_factors_from_picks_legs_commodity
  #           ) 
  #       } %>%
  #       # Filter factor leg
  #       {
  #         if (
  #           input$time_series_UK_metals_vs_factors_from_picks_legs_leg == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., leg == 
  #               input$time_series_UK_metals_vs_factors_from_picks_legs_leg
  #           ) %>%
  #           dplyr::select(-leg)
  #       } %>%
  #       # Filter period
  #       {
  #         if (
  #           input$time_series_UK_metals_vs_factors_from_picks_legs_period == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., period == 
  #               input$time_series_UK_metals_vs_factors_from_picks_legs_period
  #           ) %>%
  #           dplyr::select(-period)
  #       } %>%
  #       # Filter show
  #       {
  #         if (
  #           input$time_series_UK_metals_vs_factors_from_picks_legs_show == "coefficients"
  #         ) dplyr::select(., -rsquared) %>% tidyr::unnest(coefficients)
  #         else dplyr::select(., -coefficients)
  #       }
  #     
  #     results
  #   })
  # 
  # output$time_series_UK_metals_vs_factors_from_picks_legs <- 
  #   renderTable({
  #     time_series_UK_metals_vs_factors_from_picks_legs_results()
  #   }, digits = 4L)
  
  
  
  
  
  
  
  
  
  ## Cross-section
  
  ### US commodities ~ factors from US commodities
  
  cross_section_US_commos_vs_factors_from_US_commos_results <-  
    reactive({
      
      factor_1 <- input$cross_section_US_commos_vs_factors_from_US_commos_factor_1
      if (
        input$cross_section_US_commos_vs_factors_from_US_commos_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$cross_section_US_commos_vs_factors_from_US_commos_factor_2
      }
      if (
        input$cross_section_US_commos_vs_factors_from_US_commos_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$cross_section_US_commos_vs_factors_from_US_commos_factor_3
      }
      if (
        input$cross_section_US_commos_vs_factors_from_US_commos_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$cross_section_US_commos_vs_factors_from_US_commos_factor_4
      }
      if (
        input$cross_section_US_commos_vs_factors_from_US_commos_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$cross_section_US_commos_vs_factors_from_US_commos_factor_5
      }
      
      
      if (
        input$cross_section_US_commos_vs_factors_from_US_commos_factor_1 == "all"
      ) {model <-regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime, and model (regressors)
      results <- dplyr::filter(
        cross_section_US_commos_vs_factors_from_US_commos,
        `asset pool` ==
          input$cross_section_US_commos_vs_factors_from_US_commos_factor_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$cross_section_US_commos_vs_factors_from_US_commos_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `asset pool`, `update frequency`, `return frequency`, `ranking period`,
            `long threshold`, `short threshold`, regime
          )
        ) %>%
        # Filter leg
        {
          if (
            input$cross_section_US_commos_vs_factors_from_US_commos_factor_leg == "all"
          ) .
          else
            dplyr::filter(
              ., 
              leg == input$cross_section_US_commos_vs_factors_from_US_commos_factor_leg
            ) %>% dplyr::select(-leg)
        } %>%
        # Filter period
        {
          if (
            input$cross_section_US_commos_vs_factors_from_US_commos_period == "all"
          ) .
          else
            dplyr::filter(
              ., 
              period == input$cross_section_US_commos_vs_factors_from_US_commos_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter show
        {
          if (
            input$cross_section_US_commos_vs_factors_from_US_commos_show == "lambdas"
          ) dplyr::select(., -betas) %>% tidyr::unnest(lambdas)
          else dplyr::select(., -lambdas) %>% tidyr::unnest(betas)
        }
      
      results
    })
  
  output$cross_section_US_commos_vs_factors_from_US_commos <- 
    renderTable({
      cross_section_US_commos_vs_factors_from_US_commos_results()
    }, digits = 4L)
  output$cross_section_US_commos_vs_factors_from_US_commos_download <- 
    downloadHandler(
      filename = "cross_section_US_commos_vs_factors_from_US_commos.csv",
      content = function(file) {
        data <- cross_section_US_commos_vs_factors_from_US_commos_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ### factor picks ~ factors from picks
  cross_section_factor_picks_vs_factors_from_picks_results <-  
    reactive({
      
      factor_1 <- input$cross_section_factor_picks_vs_factors_from_picks_factor_1
      if (
        input$cross_section_factor_picks_vs_factors_from_picks_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$cross_section_factor_picks_vs_factors_from_picks_factor_2
      }
      if (
        input$cross_section_factor_picks_vs_factors_from_picks_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$cross_section_factor_picks_vs_factors_from_picks_factor_3
      }
      if (
        input$cross_section_factor_picks_vs_factors_from_picks_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$cross_section_factor_picks_vs_factors_from_picks_factor_4
      }
      if (
        input$cross_section_factor_picks_vs_factors_from_picks_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$cross_section_factor_picks_vs_factors_from_picks_factor_5
      }
      
      
      if (
        input$cross_section_factor_picks_vs_factors_from_picks_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # browser()
      # Filter picking factor asset pool, picking factor leg, picking factor name, 
      # update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime, and model (regressors)
      results <- dplyr::filter(
        cross_section_factor_picks_vs_factors_from_picks, 
        `picking factor asset pool` ==
          input$cross_section_factor_picks_vs_factors_from_picks_picking_factor_asset_pool,
        `picking factor leg` ==
          input$cross_section_factor_picks_vs_factors_from_picks_picking_factor_leg,
        `picking factor name` ==
          input$cross_section_factor_picks_vs_factors_from_picks_picking_factor,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$cross_section_factor_picks_vs_factors_from_picks_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `picking factor asset pool`, `picking factor leg`, 
            `picking factor name`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, regime, 
            regressors
          )
        ) %>%
        # Filter period
        {
          if (
            input$cross_section_factor_picks_vs_factors_from_picks_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$cross_section_factor_picks_vs_factors_from_picks_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter leg
        {
          dplyr::filter(
            ., `factor leg` == 
              input$cross_section_factor_picks_vs_factors_from_picks_leg
          ) %>% dplyr::select(-`factor leg`)
        } %>%
        # Filter show
        {
          if (
            input$cross_section_factor_picks_vs_factors_from_picks_show == "lambdas"
          ) dplyr::select(., -betas) %>% tidyr::unnest(lambdas)
          else dplyr::select(., -lambdas) %>% tidyr::unnest(betas)
        }
      
      results
    })
  
  output$cross_section_factor_picks_vs_factors_from_picks <- 
    renderTable({
      cross_section_factor_picks_vs_factors_from_picks_results()
    }, digits = 4L)
  output$cross_section_factor_picks_vs_factors_from_picks_download <- 
    downloadHandler(
      filename = "cross_section_factor_picks_vs_factors_from_picks.csv",
      content = function(file) {
        data <- cross_section_factor_picks_vs_factors_from_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ### US commodities ~ factors from picks
  
  cross_section_US_commodities_vs_factors_from_picks_results <-  
    reactive({
      
      factor_1 <- input$cross_section_US_commodities_vs_factors_from_picks_factor_1
      if (
        input$cross_section_US_commodities_vs_factors_from_picks_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$cross_section_US_commodities_vs_factors_from_picks_factor_2
      }
      if (
        input$cross_section_US_commodities_vs_factors_from_picks_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$cross_section_US_commodities_vs_factors_from_picks_factor_3
      }
      if (
        input$cross_section_US_commodities_vs_factors_from_picks_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$cross_section_US_commodities_vs_factors_from_picks_factor_4
      }
      if (
        input$cross_section_US_commodities_vs_factors_from_picks_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$cross_section_US_commodities_vs_factors_from_picks_factor_5
      }
      
      if (
        input$cross_section_US_commodities_vs_factors_from_picks_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # Filter picking factor asset pool, picking factor leg, picking factor name, 
      # update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime and model (regressors)
      results <- dplyr::filter(
        cross_section_US_commodities_vs_factors_from_picks, 
        `picking factor asset pool` ==
          input$cross_section_US_commodities_vs_factors_from_picks_picking_factor_asset_pool,
        `picking factor leg` ==
          input$cross_section_US_commodities_vs_factors_from_picks_picking_factor_leg,
        `picking factor name` ==
          input$cross_section_US_commodities_vs_factors_from_picks_picking_factor,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$cross_section_US_commodities_vs_factors_from_picks_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `picking factor asset pool`, `picking factor leg`, 
            `picking factor name`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, regime,
            regressors
          )
        ) %>%
        # Filter period
        {
          if (
            input$cross_section_US_commodities_vs_factors_from_picks_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$cross_section_US_commodities_vs_factors_from_picks_period
            ) %>% dplyr::select(-period)
        } %>%
        # Filter leg
        {
          if (
            input$cross_section_US_commodities_vs_factors_from_picks_leg == "all"
          ) .
          else 
            dplyr::filter(
              ., `factor leg` == 
                input$cross_section_US_commodities_vs_factors_from_picks_leg
            ) %>% dplyr::select(-`factor leg`)
        } %>%
        # Filter show
        {
          if (
            input$cross_section_US_commodities_vs_factors_from_picks_show == "lambdas"
          ) dplyr::select(., -betas) %>% tidyr::unnest(lambdas)
          else dplyr::select(., -lambdas) %>% tidyr::unnest(betas)
        }
      
      results
    })
  
  output$cross_section_US_commodities_vs_factors_from_picks <- 
    renderTable({
      cross_section_US_commodities_vs_factors_from_picks_results()
    }, digits = 4L)
  output$cross_section_US_commodities_vs_factors_from_picks_download <- 
    downloadHandler(
      filename = "cross_section_US_commodities_vs_factors_from_picks.csv",
      content = function(file) {
        data <- cross_section_US_commodities_vs_factors_from_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  ### UK metals ~ factors from picks
  
  cross_section_UK_metals_vs_factors_from_picks_results <-  
    reactive({
      
      factor_1 <- input$cross_section_UK_metals_vs_factors_from_picks_factor_1
      if (
        input$cross_section_UK_metals_vs_factors_from_picks_factor_2 == "none"
      ) {
        factor_2 <- NA_character_
      } else {
        factor_2 <- input$cross_section_UK_metals_vs_factors_from_picks_factor_2
      }
      if (
        input$cross_section_UK_metals_vs_factors_from_picks_factor_3 == "none"
      ) {
        factor_3 <- NA_character_
      } else {
        factor_3 <- input$cross_section_UK_metals_vs_factors_from_picks_factor_3
      }
      if (
        input$cross_section_UK_metals_vs_factors_from_picks_factor_4 == "none"
      ) {
        factor_4 <- NA_character_
      } else {
        factor_4 <- input$cross_section_UK_metals_vs_factors_from_picks_factor_4
      }
      if (
        input$cross_section_UK_metals_vs_factors_from_picks_factor_5 == "none"
      ) {
        factor_5 <- NA_character_
      } else {
        factor_5 <- input$cross_section_UK_metals_vs_factors_from_picks_factor_5
      }
      
      
      if (
        input$cross_section_UK_metals_vs_factors_from_picks_factor_1 == "all"
      ) {model <- regression_factors} else {
        model <- paste(
          c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
          , collapse = " + "
        )
      }
      
      # Filter picking factor asset pool, picking factor leg, picking factor name, 
      # update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime and regressors
      results <- dplyr::filter(
        cross_section_UK_metals_vs_factors_from_picks, 
        `picking factor asset pool` ==
          input$cross_section_UK_metals_vs_factors_from_picks_picking_factor_asset_pool,
        `picking factor leg` ==
          input$cross_section_UK_metals_vs_factors_from_picks_picking_factor_leg,
        `picking factor name` ==
          input$cross_section_UK_metals_vs_factors_from_picks_picking_factor,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        regime == input$cross_section_UK_metals_vs_factors_from_picks_regime,
        regressors %in% model
      ) %>%
        dplyr::select(
          -c(
            `picking factor asset pool`, `picking factor leg`, 
            `picking factor name`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, regime, 
            regressors
          )
        ) %>%
        # Filter period
        {
          if (
            input$cross_section_UK_metals_vs_factors_from_picks_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$cross_section_UK_metals_vs_factors_from_picks_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter show
        {
          if (
            input$cross_section_UK_metals_vs_factors_from_picks_show == "lambdas"
          ) dplyr::select(., -betas) %>% tidyr::unnest(lambdas)
          else dplyr::select(., -lambdas) %>% tidyr::unnest(betas)
        }
      
      results
    })
  
  output$cross_section_UK_metals_vs_factors_from_picks <- 
    renderTable({
      cross_section_UK_metals_vs_factors_from_picks_results()
    }, digits = 4L)
  output$cross_section_UK_metals_vs_factors_from_picks_download <- 
    downloadHandler(
      filename = "cross_section_UK_metals_vs_factors_from_picks.csv",
      content = function(file) {
        data <- cross_section_UK_metals_vs_factors_from_picks_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  ### UK metals ~ factors from picks (regressor: factor leg)
  
  # cross_section_UK_metals_vs_factors_from_picks_legs_results <-  
  #   reactive({
  #     
  #     factor_1 <- input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_1
  #     if (
  #       input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_2 == "none"
  #     ) {
  #       factor_2 <- NA_character_
  #     } else {
  #       factor_2 <- input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_2
  #     }
  #     if (
  #       input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_3 == "none"
  #     ) {
  #       factor_3 <- NA_character_
  #     } else {
  #       factor_3 <- input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_3
  #     }
  #     if (
  #       input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_4 == "none"
  #     ) {
  #       factor_4 <- NA_character_
  #     } else {
  #       factor_4 <- input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_4
  #     }
  #     if (
  #       input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_5 == "none"
  #     ) {
  #       factor_5 <- NA_character_
  #     } else {
  #       factor_5 <- input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_5
  #     }
  #     
  #     
  #     if (
  #       input$cross_section_UK_metals_vs_factors_from_picks_legs_factor_1 == "all"
  #     ) {model <- regression_factors} else {
  #       model <- paste(
  #         c(factor_1, factor_2, factor_3, factor_4, factor_5) %>% na.omit()
  #         , collapse = " + "
  #       )
  #     }
  #     
  #     # Filter picking factor asset pool, picking factor leg, picking factor name, 
  #     # update frequency, return frequency, ranking period,
  #     # long threshold, short threshold, regime and model (regressors)
  #     results <- dplyr::filter(
  #       cross_section_UK_metals_vs_factors_from_picks_legs, 
  #       `picking factor asset pool` ==
  #         input$cross_section_UK_metals_vs_factors_from_picks_legs_picking_factor_asset_pool,
  #       `picking factor leg` ==
  #         input$cross_section_UK_metals_vs_factors_from_picks_legs_picking_factor_leg,
  #       `picking factor name` ==
  #         input$cross_section_UK_metals_vs_factors_from_picks_legs_picking_factor,
  #       `update frequency` == input$factors_param_update_freq,
  #       `return frequency` == input$factors_param_return_freq,
  #       `ranking period` == input$factors_param_ranking_period,
  #       `long threshold` == input$factors_param_long_threshold,
  #       `short threshold` == input$factors_param_short_threshold,
  #       regime == input$cross_section_UK_metals_vs_factors_from_picks_legs_regime,
  #       regressors %in% model
  #     ) %>%
  #       dplyr::select(
  #         -c(
  #           `picking factor asset pool`, `picking factor leg`, 
  #           `picking factor name`, `asset pool`,
  #           `update frequency`, `return frequency`, `ranking period`, 
  #           `long threshold`, `short threshold`, regime, regressors
  #         )
  #       ) %>%
  #       # Filter factor leg
  #       {
  #         if (
  #           input$cross_section_UK_metals_vs_factors_from_picks_legs_leg == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., leg == 
  #               input$cross_section_UK_metals_vs_factors_from_picks_legs_leg
  #           ) %>%
  #           dplyr::select(-leg)
  #       } %>%
  #       # Filter period
  #       {
  #         if (
  #           input$cross_section_UK_metals_vs_factors_from_picks_legs_period == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., period == 
  #               input$cross_section_UK_metals_vs_factors_from_picks_legs_period
  #           ) %>%
  #           dplyr::select(-period)
  #       } %>%
  #       # Filter show
  #       {
  #         if (
  #           input$cross_section_UK_metals_vs_factors_from_picks_legs_show == "lambdas"
  #         ) dplyr::select(., -betas) %>% tidyr::unnest(lambdas)
  #         else dplyr::select(., -lambdas) %>% tidyr::unnest(betas)
  #       }
  #     
  #     results
  #   })
  # 
  # output$cross_section_UK_metals_vs_factors_from_picks_legs <- 
  #   renderTable({
  #     cross_section_UK_metals_vs_factors_from_picks_legs_results()
  #   }, digits = 4L)  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # correlations
  
  ## Among US commodities
  
  ### by year
  
  #### levels
  
  correlations_US_commodities_years_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_years_levels, 
        field == input$correlations_US_commodities_years_levels_field,
        frequency  == input$correlations_US_commodities_years_levels_frequency,
        regime == input$correlations_US_commodities_years_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_commodities_years_levels_year ==
            "all"
          ) .
          else
            dplyr::filter(
              ., year ==
                input$correlations_US_commodities_years_levels_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_years_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_years_levels <- 
    renderTable({
      correlations_US_commodities_years_levels_results()
    }, digits = 4L)
  output$correlations_US_commodities_years_levels_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_years_levels.csv",
      content = function(file) {
        data <- correlations_US_commodities_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  #### returns
  
  correlations_US_commodities_years_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_years_returns, 
        field == input$correlations_US_commodities_years_returns_field,
        frequency  == input$correlations_US_commodities_years_returns_frequency,
        regime == input$correlations_US_commodities_years_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_commodities_years_returns_year == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., year == 
                input$correlations_US_commodities_years_returns_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_years_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_years_returns <- 
    renderTable({
      correlations_US_commodities_years_returns_results()
    }, digits = 4L)
  output$correlations_US_commodities_years_returns_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_years_returns.csv",
      content = function(file) {
        data <- correlations_US_commodities_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### by period
  
  #### levels
  
  correlations_US_commodities_periods_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_periods_levels, 
        field == input$correlations_US_commodities_periods_levels_field,
        frequency  == input$correlations_US_commodities_periods_levels_frequency,
        regime == input$correlations_US_commodities_periods_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter period
        {
          if (
            input$correlations_US_commodities_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$correlations_US_commodities_periods_levels_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_periods_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_periods_levels <- 
    renderTable({
      correlations_US_commodities_periods_levels_results()
    }, digits = 4L)
  output$correlations_US_commodities_periods_levels_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_periods_levels.csv",
      content = function(file) {
        data <- correlations_US_commodities_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_US_commodities_periods_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_periods_returns, 
        field == input$correlations_US_commodities_periods_returns_field,
        frequency  == input$correlations_US_commodities_periods_returns_frequency,
        regime == input$correlations_US_commodities_periods_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_commodities_periods_returns_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$correlations_US_commodities_periods_returns_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_periods_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_periods_returns <- 
    renderTable({
      correlations_US_commodities_periods_returns_results()
    }, digits = 4L)
  output$correlations_US_commodities_periods_returns_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_periods_returns.csv",
      content = function(file) {
        data <- correlations_US_commodities_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  ## Among US commodities (no metals)
  
  ### by year
  
  #### levels
  
  correlations_US_commodities_no_metals_years_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_no_metals_years_levels, 
        field == input$correlations_US_commodities_no_metals_years_levels_field,
        frequency  == input$correlations_US_commodities_no_metals_years_levels_frequency,
        regime == input$correlations_US_commodities_no_metals_years_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_commodities_no_metals_years_levels_year ==
            "all"
          ) .
          else
            dplyr::filter(
              ., year ==
                input$correlations_US_commodities_no_metals_years_levels_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_no_metals_years_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_no_metals_years_levels <- 
    renderTable({
      correlations_US_commodities_no_metals_years_levels_results()
    }, digits = 4L)
  output$correlations_US_commodities_no_metals_years_levels_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_no_metals_years_levels.csv",
      content = function(file) {
        data <- correlations_US_commodities_no_metals_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_US_commodities_no_metals_years_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_no_metals_years_returns, 
        field == input$correlations_US_commodities_no_metals_years_returns_field,
        frequency  == input$correlations_US_commodities_no_metals_years_returns_frequency,
        regime == input$correlations_US_commodities_no_metals_years_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_commodities_no_metals_years_returns_year == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., year == 
                input$correlations_US_commodities_no_metals_years_returns_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_no_metals_years_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_no_metals_years_returns <- 
    renderTable({
      correlations_US_commodities_no_metals_years_returns_results()
    }, digits = 4L)
  output$correlations_US_commodities_no_metals_years_returns_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_no_metals_years_returns.csv",
      content = function(file) {
        data <- correlations_US_commodities_no_metals_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### by period
  
  #### levels
  
  correlations_US_commodities_no_metals_periods_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_no_metals_periods_levels, 
        field == input$correlations_US_commodities_no_metals_periods_levels_field,
        frequency  == input$correlations_US_commodities_no_metals_periods_levels_frequency,
        regime == input$correlations_US_commodities_no_metals_periods_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter period
        {
          if (
            input$correlations_US_commodities_no_metals_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$correlations_US_commodities_no_metals_periods_levels_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_no_metals_periods_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_no_metals_periods_levels <- 
    renderTable({
      correlations_US_commodities_no_metals_periods_levels_results()
    }, digits = 4L)
  output$correlations_US_commodities_no_metals_periods_levels_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_no_metals_periods_levels.csv",
      content = function(file) {
        data <- correlations_US_commodities_no_metals_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_US_commodities_no_metals_periods_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_commodities_no_metals_periods_returns, 
        field == input$correlations_US_commodities_no_metals_periods_returns_field,
        frequency  == input$correlations_US_commodities_no_metals_periods_returns_frequency,
        regime == input$correlations_US_commodities_no_metals_periods_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_commodities_no_metals_periods_returns_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$correlations_US_commodities_no_metals_periods_returns_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_commodities_no_metals_periods_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_commodities_no_metals_periods_returns <- 
    renderTable({
      correlations_US_commodities_no_metals_periods_returns_results()
    }, digits = 4L)
  output$correlations_US_commodities_no_metals_periods_returns_download <- 
    downloadHandler(
      filename = "correlations_US_commodities_no_metals_periods_returns.csv",
      content = function(file) {
        data <- correlations_US_commodities_no_metals_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  
  
  ## Among US metals
  
  ### by year
  
  #### levels
  
  correlations_US_metals_years_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_metals_years_levels, 
        field == input$correlations_US_metals_years_levels_field,
        frequency  == input$correlations_US_metals_years_levels_frequency,
        regime == input$correlations_US_metals_years_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_metals_years_levels_year ==
            "all"
          ) .
          else
            dplyr::filter(
              ., year ==
                input$correlations_US_metals_years_levels_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_metals_years_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_metals_years_levels <- 
    renderTable({
      correlations_US_metals_years_levels_results()
    }, digits = 4L)
  output$correlations_US_metals_years_levels_download <- 
    downloadHandler(
      filename = "correlations_US_metals_years_levels.csv",
      content = function(file) {
        data <- correlations_US_metals_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_US_metals_years_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_metals_years_returns, 
        field == input$correlations_US_metals_years_returns_field,
        frequency  == input$correlations_US_metals_years_returns_frequency,
        regime == input$correlations_US_metals_years_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_metals_years_returns_year == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., year == 
                input$correlations_US_metals_years_returns_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_metals_years_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_metals_years_returns <- 
    renderTable({
      correlations_US_metals_years_returns_results()
    }, digits = 4L)
  output$correlations_US_metals_years_returns_download <- 
    downloadHandler(
      filename = "correlations_US_metals_years_returns.csv",
      content = function(file) {
        data <- correlations_US_metals_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### by period
  
  #### levels
  
  correlations_US_metals_periods_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_metals_periods_levels, 
        field == input$correlations_US_metals_periods_levels_field,
        frequency  == input$correlations_US_metals_periods_levels_frequency,
        regime == input$correlations_US_metals_periods_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter period
        {
          if (
            input$correlations_US_metals_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$correlations_US_metals_periods_levels_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_metals_periods_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_metals_periods_levels <- 
    renderTable({
      correlations_US_metals_periods_levels_results()
    }, digits = 4L)
  output$correlations_US_metals_periods_levels_download <- 
    downloadHandler(
      filename = "correlations_US_metals_periods_levels.csv",
      content = function(file) {
        data <- correlations_US_metals_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_US_metals_periods_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_US_metals_periods_returns, 
        field == input$correlations_US_metals_periods_returns_field,
        frequency  == input$correlations_US_metals_periods_returns_frequency,
        regime == input$correlations_US_metals_periods_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_US_metals_periods_returns_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$correlations_US_metals_periods_returns_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_US_metals_periods_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_US_metals_periods_returns <- 
    renderTable({
      correlations_US_metals_periods_returns_results()
    }, digits = 4L)
  output$correlations_US_metals_periods_returns_download <- 
    downloadHandler(
      filename = "correlations_US_metals_periods_returns.csv",
      content = function(file) {
        data <- correlations_US_metals_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  
  
  ## Among UK metals
  
  ### by year
  
  #### levels
  
  correlations_UK_metals_years_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_UK_metals_years_levels, 
        field == input$correlations_UK_metals_years_levels_field,
        frequency  == input$correlations_UK_metals_years_levels_frequency,
        regime == input$correlations_UK_metals_years_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_UK_metals_years_levels_year ==
            "all"
          ) .
          else
            dplyr::filter(
              ., year ==
                input$correlations_UK_metals_years_levels_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_UK_metals_years_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_UK_metals_years_levels <- 
    renderTable({
      correlations_UK_metals_years_levels_results()
    }, digits = 4L)
  output$correlations_UK_metals_years_levels_download <- 
    downloadHandler(
      filename = "correlations_UK_metals_years_levels.csv",
      content = function(file) {
        data <- correlations_UK_metals_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_UK_metals_years_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_UK_metals_years_returns, 
        field == input$correlations_UK_metals_years_returns_field,
        frequency  == input$correlations_UK_metals_years_returns_frequency,
        regime == input$correlations_UK_metals_years_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_UK_metals_years_returns_year == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., year == 
                input$correlations_UK_metals_years_returns_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_UK_metals_years_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_UK_metals_years_returns <- 
    renderTable({
      correlations_UK_metals_years_returns_results()
    }, digits = 4L)
  output$correlations_UK_metals_years_returns_download <- 
    downloadHandler(
      filename = "correlations_UK_metals_years_returns.csv",
      content = function(file) {
        data <- correlations_UK_metals_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### by period
  
  #### levels
  
  correlations_UK_metals_periods_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_UK_metals_periods_levels, 
        field == input$correlations_UK_metals_periods_levels_field,
        frequency  == input$correlations_UK_metals_periods_levels_frequency,
        regime == input$correlations_UK_metals_periods_levels_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter period
        {
          if (
            input$correlations_UK_metals_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$correlations_UK_metals_periods_levels_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_UK_metals_periods_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_UK_metals_periods_levels <- 
    renderTable({
      correlations_UK_metals_periods_levels_results()
    }, digits = 4L)
  output$correlations_UK_metals_periods_levels_download <- 
    downloadHandler(
      filename = "correlations_UK_metals_periods_levels.csv",
      content = function(file) {
        data <- correlations_UK_metals_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_UK_metals_periods_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_UK_metals_periods_returns, 
        field == input$correlations_UK_metals_periods_returns_field,
        frequency  == input$correlations_UK_metals_periods_returns_frequency,
        regime == input$correlations_UK_metals_periods_returns_regime
      ) %>%
        dplyr::select(-c(field, frequency, regime)) %>%
        # Filter year
        {
          if (
            input$correlations_UK_metals_periods_returns_period == 
            "all"
          ) .
          else 
            dplyr::filter(
              ., period == 
                input$correlations_UK_metals_periods_returns_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_UK_metals_periods_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(., correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_UK_metals_periods_returns <- 
    renderTable({
      correlations_UK_metals_periods_returns_results()
    }, digits = 4L)
  output$correlations_UK_metals_periods_returns_download <- 
    downloadHandler(
      filename = "correlations_UK_metals_periods_returns.csv",
      content = function(file) {
        data <- correlations_UK_metals_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  
  # ## Among US energy
  # 
  # ### by year
  # 
  # #### levels
  # 
  # correlations_US_energy_years_levels_results <-  
  #   reactive({
  #     
  #     # Filter asset pool, update frequency, return frequency, ranking period,
  #     # long threshold, short threshold, regime
  #     results <- dplyr::filter(
  #       correlations_US_energy_years_levels, 
  #       field == input$correlations_US_energy_years_levels_field,
  #       frequency  == input$correlations_US_energy_years_levels_frequency,
  #       regime == input$correlations_US_energy_years_levels_regime
  #     ) %>%
  #       dplyr::select(-c(field, frequency, regime)) %>%
  #       # Filter year
  #       {
  #         if (
  #           input$correlations_US_energy_years_levels_year ==
  #           "all"
  #         ) .
  #         else
  #           dplyr::filter(
  #             ., year ==
  #               input$correlations_US_energy_years_levels_year
  #           ) %>%
  #           dplyr::select(-year)
  #       } %>%
  #       # Filter correlations type
  #       {
  #         if (
  #           input$correlations_US_energy_years_levels_correlations == 
  #           "all"
  #         ) dplyr::select(., -average_correlation) %>% 
  #           tidyr::unnest(., correlations)
  #         else 
  #           dplyr::select(., -correlations) %>% 
  #           dplyr::rename(average = average_correlation)
  #       }
  #     
  #     results
  #   })
  # 
  # output$correlations_US_energy_years_levels <- 
  #   renderTable({
  #     correlations_US_energy_years_levels_results()
  #   }, digits = 4L)
  # output$correlations_US_energy_years_levels_download <- 
  #   downloadHandler(
  #     filename = "correlations_US_energy_years_levels.csv",
  #     content = function(file) {
  #       data <- correlations_US_energy_years_levels_results() %>%
  #         dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
  #       readr::write_csv(data, file)
  #     }
  #   )
  # 
  # 
  # 
  # #### returns
  # 
  # correlations_US_energy_years_returns_results <-  
  #   reactive({
  #     
  #     # Filter asset pool, update frequency, return frequency, ranking period,
  #     # long threshold, short threshold, regime
  #     results <- dplyr::filter(
  #       correlations_US_energy_years_returns, 
  #       field == input$correlations_US_energy_years_returns_field,
  #       frequency  == input$correlations_US_energy_years_returns_frequency,
  #       regime == input$correlations_US_energy_years_returns_regime
  #     ) %>%
  #       dplyr::select(-c(field, frequency, regime)) %>%
  #       # Filter year
  #       {
  #         if (
  #           input$correlations_US_energy_years_returns_year == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., year == 
  #               input$correlations_US_energy_years_returns_year
  #           ) %>%
  #           dplyr::select(-year)
  #       } %>%
  #       # Filter correlations type
  #       {
  #         if (
  #           input$correlations_US_energy_years_returns_correlations == 
  #           "all"
  #         ) dplyr::select(., -average_correlation) %>% 
  #           tidyr::unnest(., correlations)
  #         else 
  #           dplyr::select(., -correlations) %>% 
  #           dplyr::rename(average = average_correlation)
  #       }
  #     
  #     results
  #   })
  # 
  # output$correlations_US_energy_years_returns <- 
  #   renderTable({
  #     correlations_US_energy_years_returns_results()
  #   }, digits = 4L)
  # output$correlations_US_energy_years_returns_download <- 
  #   downloadHandler(
  #     filename = "correlations_US_energy_years_returns.csv",
  #     content = function(file) {
  #       data <- correlations_US_energy_years_returns_results() %>%
  #         dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
  #       readr::write_csv(data, file)
  #     }
  #   )
  # 
  # 
  # ### by period
  # 
  # #### levels
  # 
  # correlations_US_energy_periods_levels_results <-  
  #   reactive({
  #     
  #     # Filter asset pool, update frequency, return frequency, ranking period,
  #     # long threshold, short threshold, regime
  #     results <- dplyr::filter(
  #       correlations_US_energy_periods_levels, 
  #       field == input$correlations_US_energy_periods_levels_field,
  #       frequency  == input$correlations_US_energy_periods_levels_frequency,
  #       regime == input$correlations_US_energy_periods_levels_regime
  #     ) %>%
  #       dplyr::select(-c(field, frequency, regime)) %>%
  #       # Filter period
  #       {
  #         if (
  #           input$correlations_US_energy_periods_levels_period ==
  #           "all"
  #         ) .
  #         else
  #           dplyr::filter(
  #             ., period ==
  #               input$correlations_US_energy_periods_levels_period
  #           ) %>%
  #           dplyr::select(-period)
  #       } %>%
  #       # Filter correlations type
  #       {
  #         if (
  #           input$correlations_US_energy_periods_levels_correlations == 
  #           "all"
  #         ) dplyr::select(., -average_correlation) %>% 
  #           tidyr::unnest(., correlations)
  #         else 
  #           dplyr::select(., -correlations) %>% 
  #           dplyr::rename(average = average_correlation)
  #       }
  #     
  #     results
  #   })
  # 
  # output$correlations_US_energy_periods_levels <- 
  #   renderTable({
  #     correlations_US_energy_periods_levels_results()
  #   }, digits = 4L)
  # output$correlations_US_energy_periods_levels_download <- 
  #   downloadHandler(
  #     filename = "correlations_US_energy_periods_levels.csv",
  #     content = function(file) {
  #       data <- correlations_US_energy_periods_levels_results() %>%
  #         dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
  #       readr::write_csv(data, file)
  #     }
  #   )
  # 
  # 
  # 
  # #### returns
  # 
  # correlations_US_energy_periods_returns_results <-  
  #   reactive({
  #     
  #     # Filter asset pool, update frequency, return frequency, ranking period,
  #     # long threshold, short threshold, regime
  #     results <- dplyr::filter(
  #       correlations_US_energy_periods_returns, 
  #       field == input$correlations_US_energy_periods_returns_field,
  #       frequency  == input$correlations_US_energy_periods_returns_frequency,
  #       regime == input$correlations_US_energy_periods_returns_regime
  #     ) %>%
  #       dplyr::select(-c(field, frequency, regime)) %>%
  #       # Filter year
  #       {
  #         if (
  #           input$correlations_US_energy_periods_returns_period == 
  #           "all"
  #         ) .
  #         else 
  #           dplyr::filter(
  #             ., period == 
  #               input$correlations_US_energy_periods_returns_period
  #           ) %>%
  #           dplyr::select(-period)
  #       } %>%
  #       # Filter correlations type
  #       {
  #         if (
  #           input$correlations_US_energy_periods_returns_correlations == 
  #           "all"
  #         ) dplyr::select(., -average_correlation) %>% 
  #           tidyr::unnest(., correlations)
  #         else 
  #           dplyr::select(., -correlations) %>% 
  #           dplyr::rename(average = average_correlation)
  #       }
  #     
  #     results
  #   })
  # 
  # output$correlations_US_energy_periods_returns <- 
  #   renderTable({
  #     correlations_US_energy_periods_returns_results()
  #   }, digits = 4L)
  # output$correlations_US_energy_periods_returns_download <- 
  #   downloadHandler(
  #     filename = "correlations_US_energy_periods_returns.csv",
  #     content = function(file) {
  #       data <- correlations_US_energy_periods_returns_results() %>%
  #         dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
  #       readr::write_csv(data, file)
  #     }
  #   )

  
  
  
  ## Among factor picks
  
  ### by year
  
  #### levels
  
  correlations_factor_picks_years_levels_results <-  
    reactive({
      
      # browser()
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_factor_picks_years_levels, 
        `asset pool` == input$correlations_factor_picks_years_levels_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        field == input$correlations_factor_picks_years_levels_field,
        frequency  == input$correlations_factor_picks_years_levels_frequency,
        regime == input$correlations_factor_picks_years_levels_regime
      ) %>%
        dplyr::select(
          -c(
            `asset pool`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, 
            field, frequency, regime
          )
        ) %>%
        # Filter picking factor
        {
          if (
            input$correlations_factor_picks_years_levels_factor ==
            "all"
          ) .
          else
            dplyr::filter(
              ., factor ==
                input$correlations_factor_picks_years_levels_factor
            ) %>%
            dplyr::select(-factor)
        } %>%
        # Filter picking factor leg
        {
          if (
            input$correlations_factor_picks_years_levels_leg ==
            "all"
          ) .
          else
            dplyr::filter(
              ., leg ==
                input$correlations_factor_picks_years_levels_leg
            ) %>%
            dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$correlations_factor_picks_years_levels_year ==
            "all"
          ) .
          else
            dplyr::filter(
              ., year ==
                input$correlations_factor_picks_years_levels_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_factor_picks_years_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_factor_picks_years_levels <- 
    renderTable({
      correlations_factor_picks_years_levels_results()
    }, digits = 4L)
  output$correlations_factor_picks_years_levels_download <- 
    downloadHandler(
      filename = "correlations_factor_picks_years_levels.csv",
      content = function(file) {
        data <- correlations_factor_picks_years_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_factor_picks_years_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      
      results <- dplyr::filter(
        correlations_factor_picks_years_returns, 
        `asset pool` == input$correlations_factor_picks_years_returns_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        field == input$correlations_factor_picks_years_returns_field,
        frequency  == input$correlations_factor_picks_years_returns_frequency,
        regime == input$correlations_factor_picks_years_returns_regime
      ) %>%
        dplyr::select(
          -c(
            `asset pool`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, 
            field, frequency, regime
          )
        ) %>%
        # Filter picking factor
        {
          if (
            input$correlations_factor_picks_years_returns_factor ==
            "all"
          ) .
          else
            dplyr::filter(
              ., factor ==
                input$correlations_factor_picks_years_returns_factor
            ) %>%
            dplyr::select(-factor)
        } %>%
        # Filter picking factor leg
        {
          if (
            input$correlations_factor_picks_years_returns_leg ==
            "all"
          ) .
          else
            dplyr::filter(
              ., leg ==
                input$correlations_factor_picks_years_returns_leg
            ) %>%
            dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$correlations_factor_picks_years_returns_year ==
            "all"
          ) .
          else
            dplyr::filter(
              ., year ==
                input$correlations_factor_picks_years_returns_year
            ) %>%
            dplyr::select(-year)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_factor_picks_years_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
      
    })
  
  output$correlations_factor_picks_years_returns <- 
    renderTable({
      correlations_factor_picks_years_returns_results()
    }, digits = 4L)
  output$correlations_factor_picks_years_returns_download <- 
    downloadHandler(
      filename = "correlations_factor_picks_years_returns.csv",
      content = function(file) {
        data <- correlations_factor_picks_years_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  ### by period
  
  #### levels
  
  correlations_factor_picks_periods_levels_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_factor_picks_periods_levels, 
        `asset pool` == input$correlations_factor_picks_periods_levels_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        field == input$correlations_factor_picks_periods_levels_field,
        frequency  == input$correlations_factor_picks_periods_levels_frequency,
        regime == input$correlations_factor_picks_periods_levels_regime
      ) %>%
        dplyr::select(
          -c(
            `asset pool`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, 
            field, frequency, regime
          )
        ) %>%
        # Filter picking factor
        {
          if (
            input$correlations_factor_picks_periods_levels_factor ==
            "all"
          ) .
          else
            dplyr::filter(
              ., factor ==
                input$correlations_factor_picks_periods_levels_factor
            ) %>%
            dplyr::select(-factor)
        } %>%
        # Filter picking factor leg
        {
          if (
            input$correlations_factor_picks_periods_levels_leg ==
            "all"
          ) .
          else
            dplyr::filter(
              ., leg ==
                input$correlations_factor_picks_periods_levels_leg
            ) %>%
            dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$correlations_factor_picks_periods_levels_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$correlations_factor_picks_periods_levels_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_factor_picks_periods_levels_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_factor_picks_periods_levels <- 
    renderTable({
      correlations_factor_picks_periods_levels_results()
    }, digits = 4L)
  output$correlations_factor_picks_periods_levels_download <- 
    downloadHandler(
      filename = "correlations_factor_picks_periods_levels.csv",
      content = function(file) {
        data <- correlations_factor_picks_periods_levels_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  #### returns
  
  correlations_factor_picks_periods_returns_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold, regime
      results <- dplyr::filter(
        correlations_factor_picks_periods_returns, 
        `asset pool` == input$correlations_factor_picks_periods_returns_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold,
        field == input$correlations_factor_picks_periods_returns_field,
        frequency  == input$correlations_factor_picks_periods_returns_frequency,
        regime == input$correlations_factor_picks_periods_returns_regime
      ) %>%
        dplyr::select(
          -c(
            `asset pool`, `update frequency`, `return frequency`, 
            `ranking period`, `long threshold`, `short threshold`, 
            field, frequency, regime
          )
        ) %>%
        # Filter picking factor
        {
          if (
            input$correlations_factor_picks_periods_returns_factor ==
            "all"
          ) .
          else
            dplyr::filter(
              ., factor ==
                input$correlations_factor_picks_periods_returns_factor
            ) %>%
            dplyr::select(-factor)
        } %>%
        # Filter picking factor leg
        {
          if (
            input$correlations_factor_picks_periods_returns_leg ==
            "all"
          ) .
          else
            dplyr::filter(
              ., leg ==
                input$correlations_factor_picks_periods_returns_leg
            ) %>%
            dplyr::select(-leg)
        } %>%
        # Filter year
        {
          if (
            input$correlations_factor_picks_periods_returns_period ==
            "all"
          ) .
          else
            dplyr::filter(
              ., period ==
                input$correlations_factor_picks_periods_returns_period
            ) %>%
            dplyr::select(-period)
        } %>%
        # Filter correlations type
        {
          if (
            input$correlations_factor_picks_periods_returns_correlations == 
            "all"
          ) dplyr::select(., -average_correlation) %>% 
            tidyr::unnest(correlations)
          else 
            dplyr::select(., -correlations) %>% 
            dplyr::rename(average = average_correlation)
        }
      
      results
    })
  
  output$correlations_factor_picks_periods_returns <- 
    renderTable({
      correlations_factor_picks_periods_returns_results()
    }, digits = 4L)
  output$correlations_factor_picks_periods_returns_download <- 
    downloadHandler(
      filename = "correlations_factor_picks_periods_returns.csv",
      content = function(file) {
        data <- correlations_factor_picks_periods_returns_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
  
  
  
  
  
  
  
  
  
  
  # Proportions
  
  proportions_results <-  
    reactive({
      
      # Filter asset pool, update frequency, return frequency, ranking period,
      # long threshold, short threshold
      results <- dplyr::filter(
        proportions, 
        `asset pool` == input$proportions_asset_pool,
        `update frequency` == input$factors_param_update_freq,
        `return frequency` == input$factors_param_return_freq,
        `ranking period` == input$factors_param_ranking_period,
        `long threshold` == input$factors_param_long_threshold,
        `short threshold` == input$factors_param_short_threshold
      ) %>%
        dplyr::select(
          -c(
            `asset pool`,
            `update frequency`, `return frequency`, `ranking period`, 
            `long threshold`, `short threshold`
          )
        ) %>%
        # Filter factor
        {
          if (input$proportions_factor == "all") .
          else 
            dplyr::filter(., factor == input$proportions_factor) %>%
            dplyr::select(-factor)
        } %>%
        # Filter period
        {
          if (input$proportions_period == "all") .
          else 
            dplyr::filter(., period == input$proportions_period) %>%
            dplyr::select(-period)
        }
      
      results
    })
  
  output$proportions <- renderTable({proportions_results()}, digits = 4L)
  output$proportions_download <- 
    downloadHandler(
      filename = "proportions.csv",
      content = function(file) {
        data <- proportions_results() %>%
          dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = 4L)
        readr::write_csv(data, file)
      }
    )
  
}

shinyApp(ui, server)