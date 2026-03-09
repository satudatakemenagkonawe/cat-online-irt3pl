library(shiny)
library(httr)
library(jsonlite)
library(bslib)
library(shinyjs)

# --- KONFIGURASI ---
# Ganti dengan URL Google Apps Script (GAS) Anda yang terbaru
URL_GAS <- "https://script.google.com/macros/s/AKfycbz1QDXTJXoEZ92ds162ySujUFI3jea3DOBs16pL9230cYvMHJo2xsZxrZjVE63Dqk0/exec"

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .certificate-box { border: 5px double #2c3e50; padding: 30px; text-align: center; background-color: #f8f9fa; border-radius: 15px; margin-top: 20px; }
      .score-circle { font-size: 64px; font-weight: bold; color: #2ecc71; margin: 10px 0; }
      .progress { height: 25px; font-size: 14px; font-weight: bold; }
    "))
  ),

  titlePanel("CAT Online - Adaptive Test IRT 3PL"),
  
  sidebarLayout(
    sidebarPanel(
      id = "side-panel",
      h4("Informasi Peserta"),
      textInput("user_name", "Nama Lengkap:", ""),
      hr(),
      uiOutput("progress_bar_ui"),
      hr(),
      helpText("Tes ini bersifat adaptif. Soal akan menyesuaikan dengan tingkat kemampuan Anda.")
    ),
    
    mainPanel(
      # Area Ujian
      div(id = "quiz-container", 
          uiOutput("soal_ui")
      ),
      
      # Area Dashboard Hasil (Awalnya Tersembunyi)
      hidden(
        div(id = "result-dashboard",
            div(class = "certificate-box",
                h2("HASIL EVALUASI KOMPETENSI"),
                hr(),
                h4("Nama Peserta:"),
                h3(textOutput("res_name", inline = TRUE)),
                br(),
                p("Skor Kemampuan (Theta):"),
                div(class = "score-circle", textOutput("res_theta", inline = TRUE)),
                h4(uiOutput("res_cat_ui")),
                p(em("Data ini telah disinkronkan ke pusat data penilaian.")),
                hr(),
                actionButton("btn_ulang", "Ulangi Tes", class = "btn-outline-secondary")
            )
        )
      ),
      br(),
      # Tombol Kirim muncul hanya jika selesai
      hidden(actionButton("btn_kirim", class = "btn-success btn-lg w-100"))
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  vals <- reactiveValues(
    theta = 0,
    answered = c(),
    current_item = NULL,
    selesai = FALSE,
    start_time = NULL,
    time_left = 3600, # 60 menit dalam detik
    user_authenticated = FALSE
  )

  # --- A. LOGIKA RESUME & CEK STATUS (ANTI-REFRESH) ---
  observeEvent(input$btn_start, {
    req(input$user_name)
    
    # Cek ke Google Sheets apakah nama ini sudah pernah mengerjakan
    # (Gunakan POST ke GAS khusus untuk cek status)
    res <- POST(URL_GAS, body = list(action = "check_user", nama = input$user_name), encode = "json")
    user_status <- fromJSON(content(res, "text"))
    
    if (user_status$status == "Selesai") {
      showModal(modalDialog("Anda sudah menyelesaikan tes ini dan tidak dapat mengulang.", title = "Akses Ditolak"))
    } else {
      vals$user_authenticated <- TRUE
      if (user_status$status == "Mengerjakan") {
        # Ambil data lama jika dia refresh
        vals$theta <- user_status$theta
        vals$answered <- user_status$answered
        vals$start_time <- as.POSIXct(user_status$waktu_mulai)
      } else {
        # User baru mulai
        vals$start_time <- Sys.time()
      }
      shinyjs::hide("side-panel")
    }
  })

  # --- B. LOGIKA TIMER (60 MENIT) ---
  observe({
    invalidateLater(1000, session) # Update setiap 1 detik
    req(vals$user_authenticated, !vals$selesai)
    
    elapsed <- as.numeric(difftime(Sys.time(), vals$start_time, units = "secs"))
    vals$time_left <- 3600 - elapsed
    
    if (vals$time_left <= 0) {
      vals$selesai <- TRUE
      showModal(modalDialog("Waktu Anda habis!", title = "Waktu Habis"))
      # Otomatis klik kirim hasil
      click("btn_kirim")
    }
  })

  output$timer_ui <- renderUI({
    req(vals$user_authenticated)
    mins <- floor(vals$time_left / 60)
    secs <- floor(vals$time_left %% 60)
    color <- if(vals$time_left < 300) "red" else "black"
    
    h3(sprintf("%02d:%02d", mins, secs), style = paste0("color: ", color, "; font-weight: bold;"))
  })

  # --- C. UPDATE DATABASE SETIAP KLIK 'NEXT' ---
  observeEvent(input$next_soal, {
    # ... (Logika IRT 3PL Anda yang lama) ...

    # TAMBAHAN: Kirim status terbaru ke Sheets agar jika refresh, data tersimpan
    body_data <- list(
      action = "update_status",
      nama = input$user_name,
      theta = vals$theta,
      answered = vals$answered,
      status = "Mengerjakan"
    )
    POST(URL_GAS, body = toJSON(body_data, auto_unbox = TRUE), encode = "json")
  })
}
shinyApp(ui, server)
