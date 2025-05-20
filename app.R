library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(opencv)
library(beepr)
library(DT)
library(shinyjs)
if (!requireNamespace("mailR", quietly = TRUE)) {
  install.packages("mailR")
}
library(mailR)
install.packages("sendmailR")
library(sendmailR)


# Database setup
db_path <- "supermarket.db"
con <- dbConnect(RSQLite::SQLite(), db_path)
# Initialize tables
dbExecute(con, "CREATE TABLE IF NOT EXISTS Products (
  product_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  price REAL NOT NULL,
  stock_qty INTEGER NOT NULL,
  sold_qty INTEGER NOT NULL DEFAULT 0,
  expire_date TEXT
);")
dbExecute(con, "CREATE TABLE IF NOT EXISTS Categories (
  category_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL
);")
dbExecute(con, "CREATE TABLE IF NOT EXISTS Users (
  user_id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT NOT NULL UNIQUE,
  password TEXT NOT NULL,
  title TEXT
);")

# (Helper functions remain unchanged...)

# UI
ui <- fluidPage(
  theme = bs_theme(
    version   = 5,
    bg        = "#f2f1ed",       # soft ivory
    fg        = "#2f2f2f",       # dark slate
    primary   = "#2c3e50",       # deep slate
    secondary = "#af9453",       # muted gold accent
    success   = "#28a745",
    info      = "#17a2b8",
    warning   = "#ffc107",
    danger    = "#dc3545",
    base_font = font_google("Poppins"),
    code_font = font_google("Source Code Pro")
  )
  ,
  tags$head(
    tags$style(HTML("
    /* Style for the Add-by-ID button */
    body {
      background-color: #f2f1ed;  /* soft ivory */
      color: #2f2f2f;             /* dark slate */
    }

    /* Cashier card */
    .bs-card {
      background: #faf9f6;        /* ivory */
      border: 1px solid #ddd6ce;
      box-shadow: 0 3px 10px rgba(47,47,47,0.08);
      padding: 1.75rem;
    }

    /* Scan button: deep slate */
    #scan_btn.btn-success {
      background-color: #2c3e50;
      border-color:    #2c3e50;
      color: #ecf0f1;
      font-size: 1.1rem;
    }
    #scan_btn.btn-success:hover {
      background-color: #1f2a36;
      border-color:    #1f2a36;
    }

    /* Send & Print buttons: fresh teal */
    #send_receipt_btn.btn-info,
    #print_btn.btn-info {
      background-color: #16a085;
      border-color:    #16a085;
      color: #fdfdfd;
      font-size: 1rem;
    }
    #send_receipt_btn.btn-info:hover,
    #print_btn.btn-info:hover {
      background-color: #12876b;
      border-color:    #117a65;
    }

    /* Logout: rich muted red */
    #logout_btn.btn-danger {
      background-color: #c0392b;
      border-color:    #c0392b;
      color: #faf9f6;
    }
    #logout_btn.btn-danger:hover {
      background-color: #922b21;
      border-color:    #922b21;
    }

    /* Inputs: rounded, subtle border */
    .bs-card .form-control {
      background: #fff;
      border: 1px solid #ccc4b9;
      border-radius: 0.5rem;
      padding: 0.6rem 1rem;
    }

    /* Headers */
    .bs-card h3, .bs-card h2 {
      color: #2c3e50;
      margin-bottom: 1.25rem;
    }
    #add_by_id {
      margin-top: 1rem;
      width: 100%;
      padding: 0.75rem 1.25rem;
      font-size: 1rem;
      font-weight: 500;
      color: var(--bs-primary);
      background-color: transparent;
      border: 2px solid var(--bs-primary);
      border-radius: 0.5rem;
      transition: background-color 0.2s, color 0.2s;
    }
    #add_by_id:hover {
      background-color: var(--bs-primary);
      color: #fff;
    }
  "))
  ),
  tags$head(
    tags$style(HTML(
      "      .bs-card { background: linear-gradient(135deg,#ffffff,#e0e4e9); border:1px solid #cbd5e0; box-shadow:0 4px 12px rgba(16,42,67,0.15); border-radius:.75rem; padding:1.5rem; margin-bottom:1.5rem; }
      .btn { padding:.8rem 1.6rem; font-size:1.1rem; border-radius:.75rem; transition:background-color .2s ease; }
      .btn-primary{background-color:#3f51b5;border-color:#3f51b5;} .btn-primary:hover{background-color:#303f9f;}
      .btn-success{background-color:#4caf50;border-color:#4caf50;} .btn-success:hover{background-color:#43a047;}
      .btn-warning{background-color:#ffc107;border-color:#ffc107;} .btn-warning:hover{background-color:#ffb300;}
      .btn-info{background-color:#00bcd4;border-color:#00bcd4;} .btn-info:hover{background-color:#00acc1;}
      .btn-danger{background-color:#f44336;border-color:#f44336;} .btn-danger:hover{background-color:#e53935;}
      .cart-btn{padding:.5rem .8rem;font-size:1rem;color:#102a43;} .cart-btn:hover{background-color:rgba(63,81,181,0.1);}
      .shiny-input-container{margin-bottom:1.25rem;}
      table th{background-color:#e8eaf6;color:#102a43;}
      /* Logout button global */
      #logout_btn { position: fixed; top: 1rem; right: 1rem; z-index: 999; padding: .4rem .8rem; font-size: .9rem; }
"    )),
    tags$script(HTML(
      "$(document).on('click','.cart-btn',function(){var id=$(this).data('id');var action=$(this).data('action');Shiny.setInputValue('cart_action',{id:id,action:action},{priority:'event'});});"
    ))
  ),
  # App title
  titlePanel(tags$h1(icon("store"), " Supermarket System")),
  
  # Dynamic UI
  uiOutput("login_ui"),
  uiOutput("main_ui")
)


server <- function(input, output, session) {
  coupon_discount <- reactiveVal(0)
  last_discount   <- reactiveVal(0)    # ← NEW
  last_df         <- data.frame()
  expired_data <- reactiveVal(data.frame())
  observeEvent(input$restock_btn, {
    # grab everything already past due
    df <- dbGetQuery(con, "
    SELECT product_id, name, stock_qty, expire_date
      FROM Products
     WHERE date(expire_date) < date('now')
  ")
    expired_data(df)
  })
  output$expired_ui <- renderUI({
    df <- expired_data()
    if (nrow(df) == 0) {
      return(h5("✅ No expired products."))
    }
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        pid <- row$product_id
        fluidRow(
          column(4, strong(row$name)),
          column(2,
                 numericInput(
                   inputId = paste0("exp_qty_", pid),
                   label   = "Qty",
                   value   = row$stock_qty,
                   min     = 0,
                   width   = "100%"
                 )
          ),
          column(3,
                 dateInput(
                   inputId = paste0("exp_date_", pid),
                   label   = "Expiry",
                   value   = as.Date(row$expire_date),
                   width   = "100%"
                 )
          ),
          column(3,
                 actionButton(
                   inputId = paste0("restock_expired_", pid),
                   label   = "Restock",
                   class   = "btn-success w-100"
                 )
          )
        )
      })
    )
  })
  observe({
    df <- expired_data()
    lapply(df$product_id, function(pid) {
      btn <- paste0("restock_expired_", pid)
      observeEvent(input[[btn]], {
        # pull the two inputs
        new_qty  <- input[[paste0("exp_qty_", pid)]]
        new_date <- input[[paste0("exp_date_", pid)]]
        # write back
        dbExecute(con, "
        UPDATE Products
           SET stock_qty  = ?,
               expire_date = ?
         WHERE product_id = ?
      ", params = list(as.integer(new_qty),
                       as.character(new_date),
                       pid))
        # refresh only the expired list
        df2 <- dbGetQuery(con, "
        SELECT product_id, name, stock_qty, expire_date
          FROM Products
         WHERE date(expire_date) < date('now')
      ")
        expired_data(df2)
        # single notification
        prod_name <- df$name[df$product_id == pid]
        showNotification(
          sprintf("✅ '%s' restocked to %d and expiry set to %s",
                  prod_name, new_qty, new_date),
          type = "message"
        )
      }, ignoreInit = TRUE)
    })
  })
  
  
  user <- reactiveValues(logged_in = FALSE, role = NULL)
  last_scanned_time <- reactiveVal(Sys.time())
  last_df<- data.frame()
  due_data <- reactiveVal(data.frame())
  email_address <- reactiveVal(NULL)
  
  get_due_products <- function(con, days_ahead) {
    df <- dbGetQuery(con,
                     "SELECT product_id, name, price, stock_qty, expire_date
       FROM Products
      WHERE date(expire_date)
            BETWEEN date('now')
                AND date('now', '+' || ? || ' days')",
                     params = list(as.integer(days_ahead))
    )
    df$expire_date <- as.Date(df$expire_date)
    df
  }
  
  observeEvent(input$user_email, {
    email_address(input$user_email)
  })
  # 2) In server(), define a reactiveVal to hold the low-stock data:
  lowstock_data <- reactiveVal(data.frame())
  
  # 3) When the button is clicked, fetch products with stock ≤ 3:
  observeEvent(input$lowstock_btn, {
    df <- dbGetQuery(con,
                     "SELECT product_id, name, stock_qty FROM Products WHERE stock_qty <= 3"
    )
    lowstock_data(df)
  })
  
  # 4) Render editable UI for low-stock items:
  output$lowstock_ui <- renderUI({
    df <- lowstock_data()
    if (nrow(df)==0) {
      return(h4("All products have sufficient stock."))
    }
    tagList(
      h4("Products Low on Stock (≤ 3):"),
      lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        pid <- row$product_id
        fluidRow(
          column(5, strong(row$name)),
          column(3,
                 numericInput(
                   inputId = paste0("restock_qty_", pid),
                   label   = "Qty:",
                   value   = row$stock_qty,
                   min     = 0,
                   width   = "100%"
                 )
          ),
          column(4,
                 actionButton(
                   inputId = paste0("restock_btn_", pid),
                   label   = "Update Stock",
                   class   = "btn-sm btn-success",
                   style   = "margin-top: 1.7rem; width:100%;"
                 )
          )
        )
      })
    )
  })
  
  
  # 5) Wire each “Update Stock” button to write back to the DB:
  # Whenever any restock_qty_<pid> changes, update the DB & refresh the list
  # After you define lowstock_data() and the button that populates it:
  
  observe({
    df <- lowstock_data()
    lapply(seq_len(nrow(df)), function(i) {
      pid      <- df$product_id[i]
      prodName <- df$name[i]
      qtyInput <- paste0("restock_qty_", pid)
      btnId    <- paste0("restock_btn_", pid)
      
      # For each product, observe its specific Update button:
      observeEvent(input[[btnId]], {
        new_qty <- input[[qtyInput]]
        # 1) Write to DB
        dbExecute(con,
                  "UPDATE Products SET stock_qty = ? WHERE product_id = ?",
                  params = list(as.integer(new_qty), pid)
        )
        # 2) Refresh the low-stock list
        lowstock_data(
          dbGetQuery(con,
                     "SELECT product_id, name, stock_qty
           FROM Products
           WHERE stock_qty <= 3"
          )
        )
        # 3) Notify using the captured prodName
        showNotification(
          sprintf("✅ Stock for '%s' updated to %d", prodName, new_qty),
          type = "message"
        )
      }, ignoreInit = TRUE)
    })
  })
  
  
  
  
  
  # new: click counter for + / − / delete
  click_count <- reactiveVal(0)
  register_state <- reactiveValues(show = FALSE)
  
  # Show register form
  observeEvent(input$register_btn, {
    register_state$show <- TRUE
  })
  
  # Hide register form
  observeEvent(input$close_register, {
    register_state$show <- FALSE
  })
  
  
  cart_data <- reactiveVal(data.frame(
    ProductID = integer(),
    Name = character(),
    Price = numeric(),
    Quantity = integer(),
    LineTotal = numeric(),
    stringsAsFactors = FALSE
  ))
  session_sold <- reactiveValues(counts = list())
  
  observe({
    session$sendCustomMessage("bindCartButtons", NULL)
  })
  
  
  # Login UI
  output$login_ui <- renderUI({
    if (!user$logged_in) {
      div(class = "bs-card", style = "max-width:400px; margin:40px auto;",
          h3(icon("lock"), " Please Log In"),
          textInput("username", "Username:"),
          passwordInput("password", "Password:"),
          actionButton("login_btn", tagList(icon("sign-in-alt"), " Log In"), class = "btn-primary w-100")
      )
    }
  })
  output$main_ui <- renderUI({
    req(user$logged_in)
    # Logout button
    logout_ui <- actionButton("logout_btn",tagList(icon("sign-out-alt")," Log Out"),class="btn-danger",style="position:absolute;top:1rem;right:1rem;z-index:1000;")
    
    if (tolower(user$role)=="cashier") {
      div(class="bs-card",style="position:relative;max-width:600px;margin:40px auto;",
          logout_ui,
          h3(icon("shopping-cart")," Cashier Dashboard"),
          actionButton("scan_btn",tagList(icon("camera")," Start Scanning"),class="btn-success"),
          br(),br(),
          # inside your cashier UI (replace the existing fluidRow for email & send button)
          fluidRow(
            column(8,
                   div(style = "margin-bottom: 0.5rem;",
                       textInput(
                         "user_email", 
                         "Customer Email", 
                         placeholder = "customer@example.com",
                         width = "100%"
                       )
                   )
            ),
            column(4,
                   div(style = "display: flex; justify-content: flex-end; align-items: center; height: 100%;",
                       actionButton(
                         "send_receipt_btn",
                         label = tagList(icon("envelope"), " Send Receipt"),
                         class = "btn btn-info",
                         style = "
          padding: 0.75rem 1.5rem;
          font-size: 1rem;
          white-space: nowrap;
        "
                       )
                   )
            )
          )
          ,
          actionButton("print_btn",tagList(icon("print")," Print Receipt"),class="btn-info w-100"),
          verbatimTextOutput("receipt_out")
      )
    } else {
      div(class="bs-card", style="position:relative;margin:40px;",
          logout_ui,
          h3(icon("chart-pie")," Manager Dashboard"),
          # first row: slider + two buttons
          fluidRow( class="mb-3",
                    column(6,
                           sliderInput("day_sel", "Days Ahead for Expiry:", min=0, max=365, value=30)
                    ),
                    column(3,
                           actionButton("manage_btn", tagList(icon("exclamation-triangle"), " Check Expiring"),
                                        class="btn-warning w-100")
                    ),
                    column(3,
                           actionButton("lowstock_btn", tagList(icon("exclamation-circle"), " Low Stock"),
                                        class="btn-secondary w-100")
                    )
          ),
          # second row: blank space then the Restock button under the Low Stock column
          fluidRow( class="mb-3",
                    column(6),  # empty
                    column(3),  # empty
                    column(3,
                           actionButton("restock_btn", tagList(icon("box-open"), " Restock Expired"),
                                        class="btn-secondary w-100")
                    )
          ),
          
          
          div(class="bs-card mb-4",
              h4("Expiry Checker"),
              uiOutput("manage_ui")
          ),
          div(class="bs-card mb-4",
              h4("Low Stock Items"),
              uiOutput("lowstock_ui")
          ),
          # placeholder for the “Expired Products” editor
          div(class="bs-card mb-4",
              h4("Expired Products"),
              uiOutput("expired_ui")
          ),
          
          fluidRow(class="g-4 mb-3",
                   column(4,
                          actionButton("sales_btn", tagList(icon("chart-bar"),       " Sales by Category"), class="btn-primary w-100")
                   ),
                   column(4,
                          actionButton("price_btn", tagList(icon("dollar-sign"),     " Mean Price by Category"), class="btn-primary w-100")
                   ),
                   column(4,
                          actionButton("expiry_btn", tagList(icon("clock"),          " Expiry Distribution"), class="btn-primary w-100")
                   )
          ),
          # register button + rest…
          fluidRow(
            column(12, align="center",
                   actionButton("register_btn", "Register User", class="btn-primary")
            )
          ),
          hr(),
          uiOutput("register_ui"),
          plotOutput("plot_area", height="400px")
      )
      
    }
  })  
  
  # Handle login
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    res <- dbGetQuery(con, sprintf("SELECT title FROM Users WHERE username='%s' AND password='%s'", input$username, input$password))
    if (nrow(res) == 1) {
      user$logged_in <- TRUE; user$role <- res$title[1]
    } else {
      showModal(modalDialog("Invalid credentials", easyClose=TRUE))
    }
  })
  
  output$cart_table <- renderUI({
    df <- cart_data()
    if (nrow(df) == 0) return(h4("Cart is empty."))
    
    tagList(
      tableOutput("cart_display")
    )
  })
  
  output$register_ui <- renderUI({
    if (register_state$show) {
      fluidPage(
        h3("Register", align = "center"),
        fluidRow(
          column(4, ""),  # Left padding
          column(4,
                 textInput("reg_username", "Username"),
                 passwordInput("reg_password", "Password"),
                 textInput("reg_firstname", "First Name"),
                 textInput("reg_lastname", "Last Name"),
                 textInput("reg_title", "Title"),
                 actionButton("submit_register", "Register", class = "btn-success"),
                 br(), br(),
                 actionButton("close_register", "❌ Cancel", class = "btn-danger"),
                 verbatimTextOutput("register_status")
          ),
          column(4, "")  # Right padding
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # Render the live cart in the main UI
  output$cart_overview <- renderUI({
    df <- cart_data()
    if (nrow(df) == 0) {
      h4("Cart is empty.")
    } else {
      # use your same render_cart_table() helper
      render_cart_table(df)
    }
  })
  
  observeEvent(input$logout_btn, {
    user$logged_in <- FALSE
  })
  
  # 1) In your server(), replace output$manage_ui with:
  
  # ─────────────────────────────────────────────────────────────────────────────
  # 1) In your server(), replace your existing output$manage_ui with:
  # ─────────────────────────────────────────────────────────────────────────────
  output$manage_ui <- renderUI({
    df <- due_data()
    if (nrow(df) == 0) return(h4("No products expiring within selected range."))
    
    tags$table(class = "table table-borderless",
               tags$thead(
                 tags$tr(
                   tags$th("Product"),
                   tags$th("Discount (%)"),
                   tags$th("Price"),
                   tags$th("Action")
                 )
               ),
               tags$tbody(
                 lapply(seq_len(nrow(df)), function(i) {
                   row    <- df[i, ]
                   pid    <- row$product_id
                   exp    <- row$expire_date
                   cutoff <- Sys.Date() + input$day_sel
                   
                   # only show if within window
                   if (exp < Sys.Date() || exp > cutoff) return(NULL)
                   
                   tags$tr(
                     tags$td(row$name),
                     tags$td(
                       numericInput(
                         inputId = paste0("discount_", pid),
                         label   = NULL,
                         value   = 0, min = 0, max = 100, width = "80px"
                       )
                     ),
                     tags$td(sprintf("$%.2f", row$price)),
                     tags$td(
                       actionButton(
                         inputId = paste0("apply_discount_", pid),
                         label   = "Apply Discount",
                         class   = "btn-sm btn-warning"
                       )
                     )
                   )
                 })
               )
    )
  })
  
  
  
  # ─────────────────────────────────────────────────────────────────────────────
  # 2) Then add this single observer block *once* in your server():
  # ─────────────────────────────────────────────────────────────────────────────
  observe({
    df <- due_data()
    lapply(df$product_id, function(pid) {
      btn_id <- paste0("apply_discount_", pid)
      observeEvent(input[[btn_id]], {
        # grab the % from the numericInput
        pct <- isolate(input[[paste0("discount_", pid)]])
        req(is.numeric(pct), pct >= 0, pct <= 100)
        
        # 1) compute the factor (e.g. 10% → 0.90)
        factor <- (100 - pct) / 100
        
        # 2) apply it in one SQL statement
        dbExecute(con,
                  "UPDATE Products
           SET price = price * ?
         WHERE product_id = ?",
                  params = list(factor, pid)
        )
        
        # 3) re‐fetch *only* the expiring products
        due_data(get_due_products(con, input$day_sel))
        
        # 4) single notification
        prod_name <- df$name[df$product_id == pid]
        showNotification(
          sprintf("✅ %d%% discount applied to '%s'", pct, prod_name),
          type = "message"
        )
      }, ignoreInit = TRUE)
    })
  })
  
  
  
  
  
  observeEvent(input$apply_coupon, {
    code <- toupper(trimws(input$coupon_code))
    disc <- switch(code,
                   "SAVE10"    = 10,
                   "HALFPRICE" = 50,
                   0
    )
    if (disc > 0) {
      coupon_discount(disc)
      showNotification(sprintf("Coupon applied: %d%% off", disc), type="message")
    } else {
      coupon_discount(0)
      showNotification("Invalid coupon code", type="error")
    }
    
    ## refresh the modal so totals update instantly
    removeModal()
    showCartModal()
  })
  
  observeEvent(input$add_by_id, {
    req(input$manual_id)
    pid <- as.integer(input$manual_id)
    
    # 1) Look up the product
    prod <- dbGetQuery(con,
                       sprintf("SELECT name, price, stock_qty FROM Products WHERE product_id=%d", pid)
    )
    if (nrow(prod)==0) {
      showNotification(sprintf("❌ No product with ID %d", pid), type="error")
      return()
    }
    if (prod$stock_qty[1] < 1) {
      showNotification(sprintf("⚠️ \"%s\" is out of stock", prod$name[1]), type="warning")
      return()
    }
    
    # 2) Update the reactive cart_data
    df  <- cart_data()
    idx <- which(df$ProductID==pid)
    if (length(idx)==0) {
      df <- rbind(df, data.frame(
        ProductID = pid,
        Name      = prod$name[1],
        Price     = prod$price[1],
        Quantity  = 1,
        LineTotal = prod$price[1],
        stringsAsFactors=FALSE
      ))
    } else {
      df$Quantity[idx]  <- df$Quantity[idx] + 1
      df$LineTotal[idx] <- df$Quantity[idx] * df$Price[idx]
    }
    cart_data(df)
    
    # 3) Re-open the modal so the new row appears immediately
    removeModal()
    showCartModal()
  })
 
  showCartModal <- function() {
    df    <- cart_data()
    total <- sum(df$LineTotal)
    disc  <- coupon_discount()
    discount_amt <- total * disc/100
    net_total    <- total - discount_amt
    
    showModal(modalDialog(
      title = tagList(icon("shopping-cart"), "Cart Summary"),
      
      # ── Your cart table (with +/–/🗑️ buttons) ──
      uiOutput("cart_overview"),
      
      # ── New: manual add by ID ──
      div(style="margin:1rem 0;",
          fluidRow(
            column(8,
                   numericInput(
                     "manual_id",
                     "Add by Product ID:",
                     value = NULL,
                     min   = 1,
                     width = "100%"
                   )
            ),
            column(4,
                   actionButton(
                     "add_by_id",
                     tagList(icon("plus-circle"), " Add Item"),
                     class = "btn",   # keep the base btn class for alignment
                     style = NULL     # remove any inline style so our CSS takes effect
                   )
            )
          )
      ),
      
      # ── Coupon & totals (unchanged) ──
      textInput("coupon_code","Coupon Code:",value=isolate(input$coupon_code %||% "")),
      actionButton("apply_coupon","Apply Coupon",class="btn-info"), br(),
      p(strong(sprintf("Subtotal: $%.2f",    total))),
      p(strong(sprintf("Discount (%d%%): -$%.2f", disc, discount_amt))),
      p(strong(sprintf("Total: $%.2f",       net_total))),
      
      footer = tagList(
        actionButton("confirm_cart","✅ Confirm Purchase",class="btn-success"),
        modalButton("Dismiss")
      ),
      easyClose = FALSE
    ))
    
    # re-bind your cart-button JS
    session$sendCustomMessage("bindCartButtons", NULL)
  }
  
  
  
  observeEvent(input$scan_btn, {
    # 1) Reset our in-memory session counts
    session_sold$counts <- list()
    scanned_codes      <- character()  # (if you still want to prevent duplicates)
    
    # 2) Launch the camera loop, but do NOT touch the DB or render receipt here
    safe_ocv_video(function(frame) {
      code <- ocv_qr_detect(frame)
      if (length(code) > 0) {
        code_str <- as.character(code[1])
        code_int <- suppressWarnings(as.integer(code_str))
        if (!is.na(code_int)) {
          if (code_int == 0) return(TRUE)  # end scan on “0”
          
          time_now <- Sys.time()
          if (difftime(time_now, last_scanned_time(), units = "secs") > 1) {
            last_scanned_time(time_now)
            
            # fetch stock
            stock_res <- dbGetQuery(con,
                                    sprintf("SELECT name, stock_qty, price FROM Products WHERE product_id = %d", code_int)
            )
            if (nrow(stock_res) == 0) {
              showNotification(paste("Invalid product ID:", code_int), type = "error")
              return(NULL)
            }
            
            product_name  <- stock_res$name[1]
            current_stock <- stock_res$stock_qty[1]
            unit_price    <- stock_res$price[1]
            existing      <- session_sold$counts[[as.character(code_int)]] %||% 0
            remaining     <- current_stock - existing
            
            if (remaining <= 0) {
              showNotification(paste("❌", product_name, "is OUT OF STOCK!"), type = "error")
              return(NULL)
            }
            if (remaining <= 3) {
              showNotification(
                paste("⚠️", product_name, "is LOW on stock (", remaining - 1, "left after this)!"),
                type = "warning", duration = 4
              )
            }
            
            # update in-memory counts & cart_data()
            session_sold$counts[[as.character(code_int)]] <- existing + 1
            df <- cart_data()
            idx <- which(df$ProductID == code_int)
            
            if (length(idx) == 0) {
              new_row <- data.frame(
                ProductID = code_int,
                Name      = product_name,
                Price     = unit_price,
                Quantity  = 1,
                LineTotal = unit_price,
                stringsAsFactors = FALSE
              )
              df <- rbind(df, new_row)
            } else {
              df$Quantity[idx]  <- df$Quantity[idx] + 1
              df$LineTotal[idx] <- df$Quantity[idx] * df$Price[idx]
            }
            cart_data(df)
            
            beep()
            
            isolate({
              showNotification(
                sprintf("📦 Scanned: %s   (Qty: %d)",
                        product_name,
                        session_sold$counts[[as.character(code_int)]]),
                type     = "message",
                duration = 2
              )
            })
          }
        }
      }
      return(NULL)
    })
    PASS <- as.character("jtfz drdu gwih dwep")
    observeEvent(input$send_receipt_btn, {
      req(email_address())
      df <- last_df
      if (nrow(df)==0) {
        showNotification("Cart is empty – nothing to email.", type="error")
        return()
      }
      
      # Compute totals & discount
      subtotal     <- sum(df$LineTotal)
      pct_discount <- last_discount()    # or coupon_discount(), if you delayed zeroing
      discount_amt <- subtotal * pct_discount/100
      total_due    <- subtotal - discount_amt
      
      # 1) Generate the attached .txt receipt
      receipt_lines <- c(
        "🛒 Supermarket System Receipt",
        "----------------------------------------",
        sapply(seq_len(nrow(df)), function(i) {
          row <- df[i, ]
          sprintf(
            "%-20s  %2d x $%6.2f = $%6.2f",
            substr(row$Name, 1, 20),
            row$Quantity,
            row$Price,
            row$LineTotal
          )
        }),
        "----------------------------------------",
        sprintf("Subtotal:        $%6.2f", subtotal),
        sprintf("Discount (%2d%%): -$%6.2f", pct_discount, discount_amt),
        sprintf("Total Due:       $%6.2f", total_due),
        "----------------------------------------",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      receipt_path <- paste0("receipt_", Sys.Date(), ".txt")
      writeLines(receipt_lines, receipt_path)
      
      # 2) Build HTML summary table
      # 2) Build HTML summary table safely
      rows_html <- vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        sprintf(
          "<tr>
       <td style='padding:8px;border-bottom:1px solid #ddd;'>%s</td>
       <td style='padding:8px;border-bottom:1px solid #ddd;text-align:center;'>%d</td>
       <td style='padding:8px;border-bottom:1px solid #ddd;text-align:right;'>$%.2f</td>
       <td style='padding:8px;border-bottom:1px solid #ddd;text-align:right;'>$%.2f</td>
     </tr>",
          row$Name, 
          as.integer(row$Quantity), 
          as.numeric(row$Price), 
          as.numeric(row$LineTotal)
        )
      }, FUN.VALUE = character(1))
      
      table_html <- paste0(
        "<table style='width:100%;border-collapse:collapse;font-family:Arial,sans-serif;margin-bottom:16px;'>",
        "<thead><tr style='background:#f5f5f5;'>",
        "<th style='padding:10px;border-bottom:2px solid #ccc;text-align:left;'>Product</th>",
        "<th style='padding:10px;border-bottom:2px solid #ccc;text-align:center;'>Qty</th>",
        "<th style='padding:10px;border-bottom:2px solid #ccc;text-align:right;'>Price</th>",
        "<th style='padding:10px;border-bottom:2px solid #ccc;text-align:right;'>Line Total</th>",
        "</tr></thead>",
        "<tbody>", paste(rows_html, collapse=""), "</tbody>",
        "</table>"
      )
      
      
      # 3) Assemble HTML body with summary + totals
      body_html <- paste0(
        "<html><body style='font-family:Arial,sans-serif;color:#333;'>",
        "<h2 style='color:#3f51b5;'>🛒 Purchase Summary</h2>",
        table_html,
        sprintf("<p style='text-align:right;margin:4px 0;'><strong>Subtotal:</strong> $%.2f</p>", subtotal),
        sprintf("<p style='text-align:right;margin:4px 0;'><strong>Discount (%d%%):</strong> -$%.2f</p>", pct_discount, discount_amt),
        sprintf("<p style='text-align:right;font-size:1.1em;margin:8px 0;'><strong>Total Due:</strong> $%.2f</p>", total_due),
        "<hr style='margin:24px 0;'>",
        "<p>Thank you for your purchase! Receipt attached as a file.</p>",
        "</body></html>"
      )
      
      # 4) Send email with HTML summary and attached file
      tryCatch({
        send.mail(
          from         = "remixologykitchen@gmail.com",
          to           = email_address(),
          subject      = "Your Supermarket Receipt",
          body         = body_html,
          html         = TRUE,
          smtp = list(
            host.name = "smtp.gmail.com",
            port      = 587,
            user.name = "remixologykitchen@gmail.com",
            passwd    = PASS,
            tls       = TRUE
          ),
          authenticate = TRUE,
          send         = TRUE,
          attach.files = receipt_path
        )
        showNotification("✅ Receipt emailed successfully!", type="message")
      }, error = function(e) {
        showNotification(paste0("❌ Email failed: ", e$message),
                         type="error", duration=10)
      })
    })
    
    
    
    
    
    
    
    # 3) After scanning stops, show the Cart Summary modal
    sold <- session_sold$counts
    if (length(sold) == 0) {
      showModal(modalDialog("No items scanned.", easyClose = TRUE))
    } else {
      ids   <- as.integer(names(sold))
      query <- sprintf(
        "SELECT product_id, name, price FROM Products WHERE product_id IN (%s)",
        paste(ids, collapse = ",")
      )
      prods <- dbGetQuery(con, query)
      
      cart <- prods %>%
        rowwise() %>%
        mutate(
          Quantity  = sold[[as.character(product_id)]],
          LineTotal = price * Quantity
        ) %>%
        select(
          ProductID = product_id,
          Name       = name,    # ← rename name → Name
          Price      = price,   # ← rename price → Price
          Quantity,
          LineTotal
        )
      cart_data(cart)
      
      showCartModal()
      session$sendCustomMessage("bindCartButtons", NULL)
    }
  })
  
  #---------------
  
  observeEvent(input$print_btn, {
    receipt_file <- tempfile(fileext = ".txt")
    writeLines(capture.output(print_fancy_receipt(last_df)), receipt_file)
    shell(paste("notepad /p", shQuote(receipt_file)))
  })
  
  observeEvent(input$manage_btn, {
    due_data(get_due_products(con, input$day_sel))
  })
  
  
  observeEvent(input$cart_action, {
    req(input$cart_action$id, input$cart_action$action)
    
    df     <- cart_data()
    id     <- input$cart_action$id
    action <- input$cart_action$action
    idx    <- which(df$ProductID == id)
    
    if (length(idx) == 1) {
      if (action == "plus") {
        # 1) In-memory update
        df$Quantity[idx]  <- df$Quantity[idx] + 1
        df$LineTotal[idx] <- df$Quantity[idx] * df$Price[idx]
        # 2) Database update
        dbExecute(con, sprintf("
        UPDATE Products
           SET stock_qty = stock_qty - 1,
               sold_qty  = sold_qty  + 1
         WHERE product_id = %d
      ", id))
        
      } else if (action == "minus" && df$Quantity[idx] > 1) {
        df$Quantity[idx]  <- df$Quantity[idx] - 1
        df$LineTotal[idx] <- df$Quantity[idx] * df$Price[idx]
        dbExecute(con, sprintf("
        UPDATE Products
           SET stock_qty = stock_qty + 1,
               sold_qty  = sold_qty  - 1
         WHERE product_id = %d
      ", id))
        
      } else if (action == "delete") {
        # restore all qty back into stock
        q <- df$Quantity[idx]
        dbExecute(con, sprintf("
        UPDATE Products
           SET stock_qty = stock_qty + %d,
               sold_qty  = sold_qty  - %d
         WHERE product_id = %d
      ", q, q, id))
        # remove from cart
        df <- df[-idx, , drop = FALSE]
      }
      
      # push the updated cart back into reactive storage
      cart_data(df)
    }
    showCartModal()
    session$sendCustomMessage("bindCartButtons", NULL)
  })
  
  
  
  
  
  observeEvent(input$confirm_cart, {
    df <- cart_data()
    last_df <<- df
    
    # 1) Compute totals & discount
    subtotal     <- sum(df$LineTotal)
    pct_discount <- coupon_discount()
    discount_amt <- subtotal * pct_discount / 100
    total_due    <- subtotal - discount_amt
    
    # 2) Persist stock changes
    for (i in seq_len(nrow(df))) {
      pid <- df$ProductID[i]
      qty <- df$Quantity[i]
      dbExecute(con, sprintf(
        "UPDATE Products
          SET stock_qty = stock_qty  - %d,
              sold_qty  = sold_qty   + %d
        WHERE product_id = %d",
        qty, qty, pid
      ))
    }
    last_discount(pct_discount)
    # 3) Close the modal & reset coupon
    removeModal()
    coupon_discount(0)
    
    # 4) Render the receipt, including discount & total due
    output$receipt_out <- renderPrint({
      if (nrow(df) == 0) {
        cat("No items in cart.\n")
      } else {
        # your existing pretty printer
        print_fancy_receipt(df)
        cat("\n")
        cat(sprintf("Subtotal:        $ %.2f\n", subtotal))
        cat(sprintf("Discount (%2d%%): –$ %.2f\n", pct_discount, discount_amt))
        cat(sprintf("**Total Due:**   $ %.2f\n", total_due))
      }
    })
    
    # 5) Clear the in-memory cart for next customer
    cart_data(data.frame(
      ProductID = integer(),
      Name      = character(),
      Price     = numeric(),
      Quantity  = integer(),
      LineTotal = numeric(),
      stringsAsFactors = FALSE
    ))
  })
  
  
  observeEvent(input$plus_id, {
    req(input$plus_id)
    id  <- as.integer(input$plus_id)
    df  <- cart_data()
    idx <- which(df$ProductID == id)
    
    if (length(idx) == 1) {
      # in‐memory
      df$Quantity[idx]  <- df$Quantity[idx] + 1
      df$LineTotal[idx] <- df$Quantity[idx] * df$Price[idx]
      cart_data(df)
      # DB update
      dbExecute(con, sprintf("
      UPDATE Products
         SET stock_qty = stock_qty - 1,
             sold_qty  = sold_qty  + 1
       WHERE product_id = %d
    ", id))
    }
    
    showCartModal()
    session$sendCustomMessage("bindCartButtons", NULL)
  })
  
  # 2) “−” button observer
  observeEvent(input$minus_id, {
    req(input$minus_id)
    id  <- as.integer(input$minus_id)
    df  <- cart_data()
    idx <- which(df$ProductID == id)
    
    if (length(idx) == 1 && df$Quantity[idx] > 1) {
      df$Quantity[idx]  <- df$Quantity[idx] - 1
      df$LineTotal[idx] <- df$Quantity[idx] * df$Price[idx]
      cart_data(df)
      dbExecute(con, sprintf("
      UPDATE Products
         SET stock_qty = stock_qty + 1,
             sold_qty  = sold_qty  - 1
       WHERE product_id = %d
    ", id))
    }
    
    showCartModal()
  })
  
  # 3) “🗑️” button observer
  observeEvent(input$delete_id, {
    req(input$delete_id)
    id  <- as.integer(input$delete_id)
    df  <- cart_data()
    idx <- which(df$ProductID == id)
    
    if (length(idx) == 1) {
      qty <- df$Quantity[idx]
      # restore all units to stock
      dbExecute(con, sprintf("
      UPDATE Products
         SET stock_qty = stock_qty + %d,
             sold_qty  = sold_qty  - %d
       WHERE product_id = %d
    ", qty, qty, id))
      df <- df[-idx, , drop = FALSE]
      cart_data(df)
    }
    
    showCartModal()
    session$sendCustomMessage("bindCartButtons", NULL)
  })
  
  observeEvent(input$submit_register, {
    conn <- dbConnect(RSQLite::SQLite(), "supermarket.db")
    
    tryCatch({
      dbExecute(conn, "INSERT INTO Users (username, password, first_name,last_name, title) VALUES (?, ?,?, ?, ?)",
                params = list(
                  input$reg_username,
                  input$reg_password,
                  input$reg_firstname, 
                  input$reg_lastname,
                  input$reg_title
                ))
      output$register_status <- renderText("✅ User registered successfully.")
    }, error = function(e) {
      output$register_status <- renderText(paste("❌ Error:", e$message))
    }, finally = {
      dbDisconnect(conn)
    })
  })
  # ————————————————
  update_stock_qty <- function(con, product_id, new_stock) {
    dbExecute(con,
              "UPDATE Products
        SET stock_qty = ?
      WHERE product_id = ?",
              params = list(new_stock, product_id)
    )
  }
  
  
  observe({
    df <- due_data()
    lapply(df$product_id, function(id) {
      observeEvent(input[[paste0("upd_", id)]], {
        # grab the current inputs
        new_stock <- isolate(input[[paste0("qty_", id)]])
        new_date  <- isolate(input[[paste0("date_", id)]])
        # format for SQLite
        new_date_str <- format(as_date(new_date), "%Y-%m-%d")
        
        # 1) write to DB
        dbExecute(con,
                  "UPDATE Products
              SET stock_qty  = ?,
                  expire_date = ?
            WHERE product_id = ?",
                  params = list(as.integer(new_stock), new_date_str, id)
        )
        
        # 2) update our reactiveVal so the UI instantly reflects it
        df2 <- due_data()
        idx <- which(df2$product_id == id)
        df2$stock_qty[idx]  <- as.integer(new_stock)
        df2$expire_date[idx] <- as_date(new_date)
        due_data(df2)
        
        # 3) confirm to the manager
        showNotification(
          paste("Product", id, "→ stock:", new_stock,
                "| expiry:", new_date_str),
          type = "message"
        )
      }, ignoreInit = TRUE)
    })
  })
  
  
  # Manager plots
  observeEvent(input$sales_btn, { output$plot_area <- renderPlot(plot_sales_by_category(con)) })
  observeEvent(input$price_btn, { output$plot_area <- renderPlot(plot_mean_price_by_category(con)) })
  observeEvent(input$expiry_btn, { output$plot_area <- renderPlot(plot_expiry_pie(con)) })
}

#dbDisconnect(con)
shinyApp(ui, server)