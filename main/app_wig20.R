library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(quantmod)
library(dplyr)
library(tidyquant)
library(tidyr)
library(googleCloudStorageR)

# Tickers
tickers <- c(
  "PKO.WA", "PEO.WA", "PZU.WA", "ALR.WA", "SPL.WA", "ING.WA", "MBK.WA", "MIL.WA", "BHW.WA", "KRU.WA", "GPW.WA", "XTB.WA",
  "PGE.WA", "TPE.WA", "ENA.WA", "PEP.WA", "ZEP.WA",
  "PKN.WA", "KGH.WA", "JSW.WA", "LWB.WA",
  "ATT.WA", "PCE.WA", "PUW.WA", "KTY.WA", "BRS.WA", "STP.WA", "COG.WA", "GEA.WA", "CRI.WA",
  "PKP.WA", "BDX.WA", "TOR.WA", "PXM.WA", "MRB.WA", "CAR.WA", "APR.WA",
  "ACP.WA", "OPL.WA", "CPS.WA", "WPL.WA", "ASB.WA",
  "DNP.WA", "ZAB.WA", "ALE.WA", "LPP.WA", "EUR.WA", "NEU.WA", "CDR.WA", "DOM.WA"
)

csv_path <- if (file.exists("active_positions.csv")) {
  "active_positions.csv" 
} else if (file.exists("../active_positions.csv")) {
  "../active_positions.csv"
} else {
  "active_positions.csv" # Create it if it doesn't exist
}

# --- Cloud Storage Logic ---
bucket_name <- Sys.getenv("GCS_BUCKET_NAME")

read_positions <- function() {
  if (bucket_name != "") {
    tryCatch({
      gcs_get_object("active_positions.csv", bucket = bucket_name, saveToDisk = "temp_active_positions.csv", overwrite = TRUE)
      return(read.csv("temp_active_positions.csv", stringsAsFactors = FALSE))
    }, error = function(e) {
      return(data.frame(
        EntryDate = character(), Symbol = character(), EntryPrice = numeric(), 
        StopLoss = numeric(), Target1 = numeric(), Shares = numeric(), 
        Phase = character(), stringsAsFactors = FALSE
      ))
    })
  } else {
    if (file.exists(csv_path)) {
      return(read.csv(csv_path, stringsAsFactors = FALSE))
    } else {
      return(data.frame(
        EntryDate = character(), Symbol = character(), EntryPrice = numeric(), 
        StopLoss = numeric(), Target1 = numeric(), Shares = numeric(), 
        Phase = character(), stringsAsFactors = FALSE
      ))
    }
  }
}

write_positions <- function(df) {
  if (bucket_name != "") {
    write.csv(df, "temp_active_positions.csv", row.names = FALSE)
    gcs_upload("temp_active_positions.csv", bucket = bucket_name, name = "active_positions.csv")
  } else {
    write.csv(df, csv_path, row.names = FALSE)
  }
}

ui <- dashboardPage(
  dashboardHeader(title = "WIG20 Quant Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Portfolio Manager", tabName = "portfolio", icon = icon("briefcase")),
      menuItem("Market Scanner", tabName = "scanner", icon = icon("search"))
    ),
    br(),
    actionButton("sync_data", "Sync Market Data", icon = icon("sync"), style = "margin: 15px; width: 85%;", class = "btn-primary")
  ),
  dashboardBody(
    tabItems(
      # --- Portfolio Tab ---
      tabItem(tabName = "portfolio",
        fluidRow(
          box(
            title = "Active Positions", width = 12, status = "primary", solidHeader = TRUE,
            withSpinner(DTOutput("portfolio_table"))
          )
        ),
        fluidRow(
          box(
            title = "Add New Position", width = 5, status = "success", solidHeader = TRUE,
            selectInput("add_symbol", "Symbol", choices = tickers),
            numericInput("add_entry", "Entry Price", value = 0, step = 0.01),
            numericInput("add_stop", "Stop Loss", value = 0, step = 0.01),
            numericInput("add_target", "Target 1 (Swing High)", value = 0, step = 0.01),
            numericInput("add_size", "Position Size (Shares)", value = 0, step = 1),
            dateInput("add_date", "Entry Date", value = Sys.Date()),
            actionButton("btn_add_pos", "Add Position", class = "btn-success")
          ),
          box(
            title = "Manage Existing Position", width = 4, status = "warning", solidHeader = TRUE,
            selectInput("manage_symbol", "Select Active Position", choices = NULL),
            actionButton("btn_mark_runner", "Mark as 'Runner' (T1 Hit)", class = "btn-warning", style="margin-bottom: 10px; width: 100%;"),
            hr(),
            actionButton("btn_remove_pos", "Close Position Completely", class = "btn-danger", style="width: 100%;")
          )
        )
      ),
      # --- Scanner Tab ---
      tabItem(tabName = "scanner",
        fluidRow(
          box(
            title = "Trade Setups Triggered Today (ACTION)", width = 12, status = "danger", solidHeader = TRUE,
            withSpinner(DTOutput("scanner_action_table"))
          )
        ),
        fluidRow(
          box(
            title = "Stocks on Watchlist", width = 12, status = "warning", solidHeader = TRUE,
            DTOutput("scanner_watch_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    active_positions = data.frame()
  )
  
  # Load CSV on start
  observe({
    rv$active_positions <- read_positions()
    if (nrow(rv$active_positions) > 0) {
      updateSelectInput(session, "manage_symbol", choices = rv$active_positions$Symbol)
    }
  })
  
  # Sync Market Data
  observeEvent(input$sync_data, {
    showNotification("Fetching 3 years of data for 50 WIG20 tickers. Please wait...", type = "message", duration = 10, id = "fetch_notif")
    
    start_date <- Sys.Date() - 1000
    
    tryCatch({
      raw <- suppressWarnings(tq_get(tickers, get = "stock.prices", from = start_date, to = Sys.Date()))
      
      df <- raw %>%
        group_by(symbol) %>%
        arrange(date) %>%
        drop_na(high, low, close) %>%
        mutate(
          SMA20 = SMA(close, n = 20),
          SMA200 = SMA(close, n = 200),
          RSI14 = RSI(close, n = 14),
          L14 = runMin(low, n = 14),
          H14 = runMax(high, n = 14),
          fastK = (close - L14) / (H14 - L14),
          fastK = ifelse(is.nan(fastK) | is.infinite(fastK), NA, fastK),
          fastK = zoo::na.locf(fastK, na.rm = FALSE),
          fastK = ifelse(is.na(fastK), 0.5, fastK),
          fastD = SMA(fastK, n = 7),
          stoch_bull = fastK > fastD
        ) %>%
        select(-L14, -H14, -fastK, -fastD) %>%
        ungroup()
      
      rv$raw_data <- raw
      rv$processed_data <- df
      removeNotification(id = "fetch_notif")
      showNotification("Data sync complete!", type = "message")
      
    }, error = function(e) {
      removeNotification(id = "fetch_notif")
      showNotification(paste("Error fetching data:", e$message), type = "error")
    })
  }, ignoreNULL = FALSE)
  
  # --- Scanner Logic ---
  scanner_results <- reactive({
    req(rv$processed_data)
    df <- rv$processed_data
    
    res_list <- list()
    for (sym in unique(df$symbol)) {
      sym_data <- df %>% filter(symbol == sym) %>% arrange(date)
      if (nrow(sym_data) < 201) next
      
      in_rsi_zone <- FALSE
      setup_armed <- FALSE
      recent_high <- NA
      swing_high <- NA
      lowest_low <- NA
      
      for (i in 201:nrow(sym_data)) {
        H <- sym_data$high[i]
        L <- sym_data$low[i]
        C <- sym_data$close[i]
        rsi <- sym_data$RSI14[i]
        
        if(is.na(rsi)) next
        
        if (rsi >= 30 && !in_rsi_zone) {
          if (is.na(recent_high) || H > recent_high) recent_high <- H
        }
        if (rsi < 30) {
          if (!in_rsi_zone) {
            in_rsi_zone <- TRUE
            swing_high <- recent_high
            lowest_low <- L
            setup_armed <- FALSE
            recent_high <- NA
          } else {
            if (L < lowest_low) lowest_low <- L
          }
        }
        if (rsi >= 30 && in_rsi_zone) {
          in_rsi_zone <- FALSE
          setup_armed <- TRUE
        }
        if (setup_armed && L <= (lowest_low * 0.99)) {
          setup_armed <- FALSE
        }
      }
      
      last_row <- sym_data[nrow(sym_data), ]
      status <- "NONE"
      details <- ""
      
      if (in_rsi_zone) {
        status <- "WATCH (Forming Support)"
        details <- paste("RSI < 30. Lowest Low:", round(lowest_low, 2))
      } else if (setup_armed) {
        if (last_row$close > last_row$SMA20 && last_row$close < last_row$SMA200) {
          stop_loss <- lowest_low * 0.99
          risk_pct <- (last_row$close - stop_loss) / last_row$close
          
          if (risk_pct <= 0.15 && !is.na(swing_high) && swing_high > last_row$close) {
            if (!is.na(last_row$stoch_bull) && last_row$stoch_bull) {
              status <- "ACTION (ENTRY TRIGGERED)"
              details <- paste("Entry:", round(last_row$close, 2), "| Stop:", round(stop_loss, 2), "| T1:", round(swing_high, 2), "| Risk:", round(risk_pct * 100, 1), "%")
            } else {
              status <- "WATCH (Waiting for Stoch 14,7)"
              details <- "Conditions met but Stochastic is not bullish."
            }
          } else {
             if(risk_pct > 0.15) {
               status <- "WATCH (Risk > 15%)"
               details <- paste("Current Risk:", round(risk_pct * 100, 1), "%")
             } else {
               status <- "WATCH (Invalid Target)"
               details <- paste("Target 1 (", round(swing_high, 2), ") is below current price.")
             }
          }
        } else {
          status <- "WATCH (Armed & Waiting)"
          details <- paste("Waiting for Close > SMA20. Close:", round(last_row$close, 2))
        }
      }
      
      if (status != "NONE") {
        res_list[[sym]] <- data.frame(Symbol = sym, Status = status, Details = details, stringsAsFactors = FALSE)
      }
    }
    
    if (length(res_list) > 0) bind_rows(res_list) else data.frame(Symbol = character(), Status = character(), Details = character())
  })
  
  output$scanner_action_table <- renderDT({
    req(scanner_results())
    df <- scanner_results() %>% filter(grepl("ACTION", Status))
    datatable(df, options = list(pageLength = 10, dom = 't'))
  })
  
  output$scanner_watch_table <- renderDT({
    req(scanner_results())
    df <- scanner_results() %>% filter(grepl("WATCH", Status))
    datatable(df, options = list(pageLength = 25))
  })
  
  # --- Portfolio Logic ---
  output$portfolio_table <- renderDT({
    req(rv$processed_data)
    
    if (nrow(rv$active_positions) == 0) {
      return(datatable(data.frame(Message = "No active positions found.")))
    }
    
    port_results <- list()
    for (i in 1:nrow(rv$active_positions)) {
      pos <- rv$active_positions[i, ]
      sym <- pos$Symbol
      
      sym_data <- rv$processed_data %>% filter(symbol == sym) %>% arrange(date)
      if (nrow(sym_data) == 0) next
      
      last_row <- sym_data[nrow(sym_data), ]
      current_price <- last_row$close
      sma200 <- last_row$SMA200
      
      profit_pct <- (current_price - pos$EntryPrice) / pos$EntryPrice * 100
      unrealized <- (current_price - pos$EntryPrice) * pos$Shares
      
      status <- "HOLD"
      action <- ""
      
      if (current_price <= pos$StopLoss) {
        status <- "SELL (Stop Hit)"
        action <- paste("Close position. Price", round(current_price, 2), "is below stop", pos$StopLoss)
      } else {
        if (pos$Phase == "Full" && current_price >= pos$Target1) {
          status <- "SELL (Target 1 Hit)"
          action <- paste("Sell half position. Price", round(current_price, 2), ">= Target", pos$Target1, ". Move Stop to Entry.")
        } else if (pos$Phase == "Runner" && current_price >= sma200) {
          status <- "SELL (SMA200 Target Hit)"
          action <- paste("Sell remaining. Price", round(current_price, 2), ">= SMA200", round(sma200, 2))
        }
      }
      
      port_results[[sym]] <- data.frame(
        Symbol = sym,
        CurrentPrice = round(current_price, 2),
        EntryPrice = pos$EntryPrice,
        StopLoss = pos$StopLoss,
        Target1 = pos$Target1,
        SMA200 = round(sma200, 2),
        Size = pos$Shares,
        ProfitPct = round(profit_pct, 1),
        UnrealizedPL = round(unrealized, 2),
        Status = status,
        Action = action,
        stringsAsFactors = FALSE
      )
    }
    
    if (length(port_results) > 0) {
      df <- bind_rows(port_results)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatStyle(
          'Status',
          color = styleEqual(c("HOLD", "SELL (Stop Hit)", "SELL (Target 1 Hit)", "SELL (SMA200 Target Hit)"), 
                             c("black", "red", "green", "blue"))
        ) %>%
        formatStyle(
          'ProfitPct',
          color = styleInterval(0, c('red', 'green'))
        )
    } else {
      datatable(data.frame(Message = "Unable to fetch data for current positions."))
    }
  })
  
  # --- Add/Remove Position Handlers ---
  observeEvent(input$btn_add_pos, {
    new_pos <- data.frame(
      EntryDate = as.character(input$add_date),
      Symbol = input$add_symbol,
      EntryPrice = input$add_entry,
      StopLoss = input$add_stop,
      Target1 = input$add_target,
      Shares = input$add_size,
      Phase = "Full",
      stringsAsFactors = FALSE
    )
    
    # Remove old entry if replacing
    rv$active_positions <- rv$active_positions %>% filter(Symbol != input$add_symbol)
    rv$active_positions <- bind_rows(rv$active_positions, new_pos)
    
    write_positions(rv$active_positions)
    updateSelectInput(session, "manage_symbol", choices = rv$active_positions$Symbol)
    showNotification(paste("Added", input$add_symbol, "to portfolio!"), type = "success")
  })
  
  observeEvent(input$btn_mark_runner, {
    req(input$manage_symbol)
    if (nrow(rv$active_positions) > 0) {
      idx <- which(rv$active_positions$Symbol == input$manage_symbol)
      if (length(idx) > 0) {
        rv$active_positions$Phase[idx] <- "Runner"
        rv$active_positions$Shares[idx] <- floor(rv$active_positions$Shares[idx] / 2)
        rv$active_positions$StopLoss[idx] <- rv$active_positions$EntryPrice[idx]
        
        write_positions(rv$active_positions)
        showNotification(paste("Marked", input$manage_symbol, "as Runner! Size halved, Stop moved to breakeven."), type = "message")
      }
    }
  })
  
  observeEvent(input$btn_remove_pos, {
    req(input$manage_symbol)
    rv$active_positions <- rv$active_positions %>% filter(Symbol != input$manage_symbol)
    write_positions(rv$active_positions)
    updateSelectInput(session, "manage_symbol", choices = rv$active_positions$Symbol)
    showNotification(paste("Closed", input$manage_symbol), type = "warning")
  })
}

shinyApp(ui, server)
