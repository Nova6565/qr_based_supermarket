Supermarket Management System

A Shiny application that provides end-to-end retail operations including real-time scanning, inventory control, and analytics via a single responsive interface.

Features

Cashier Dashboard

QR-code scanning (OpenCV)

In-browser cart with quantity controls & coupon support

Instant receipt preview, email (mailR) and print

Manager Toolkit

Automated expiry & low-stock checks

Inline restocking of quantities & expiry dates

Dynamic per-item discounts

Sales, price, and expiry analytics plots

Tech Stack

Language & Framework: R, Shiny

Database: SQLite (DBI, RSQLite)

Styling: Bootstrap 5 via bslib, Google Fonts

Enhancements: OpenCV (QR scanning), shinyjs, beepr, DT

Emailing: mailR, sendmailR

Installation

Clone the repo:

git clone https://github.com/Nova6565/qr_based_supermarket.git
cd supermarket-system

Install dependencies in R:

install.packages(c("shiny","bslib","DBI","RSQLite","dplyr",
                   "lubridate","opencv","beepr","DT","shinyjs","mailR","sendmailR"))

Run the app:

library(shiny)
runApp()

Usage

Cashier: Log in with a cashier account, click Start Scanning, add items via QR codes or manual ID, apply coupons, then Confirm Purchase, Email, or Print receipts.

Manager: Log in with a manager account to check expiring items, apply discounts, restock low/expired products, and view sales analytics.

Database Schema

Products: product_id, name, price, stock_qty, sold_qty, expire_date

Categories: category_id, name

Users: user_id, username, password, title (role)

Contributing

Contributions, issues, and feature requests are welcome! Please fork the repo and submit a pull request.
