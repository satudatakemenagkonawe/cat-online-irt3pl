library(shiny)
library(httr)
library(jsonlite)
library(bslib)
library(shinyjs)

# --- KONFIGURASI ---
URL_GAS <- "https://script.google.com/macros/s/AKfycby3mlqFtlr4O_ezpamFg6Roz4YDVMWDjOLAV3VrvJ_SK-g8mP0j7TxY6tSV_mH2Dh1-/exec"

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # JavaScript untuk Timer dan Mencegah Refresh
  tags$head(
    tags$script(HTML("
      // Mencegah penutupan tab secara tidak sengaja
      window.onbeforeunload = function() {
        return 'Tes sedang berlangsung, Anda yakin ingin meninggalkan halaman?';
      };
    "))
  ),

  titlePanel("CAT Online - Adaptive Test"),
  
  sidebarLayout(
    sidebarPanel(
      id = "side-panel",
      h4("Status Peserta"),
      textInput("user_name", "Nama Lengkap:", ""),
      actionButton("btn_mulai", "Mulai Ujian", class = "btn-primary w-100"),
      hr(),
      # Tampilan Timer
      wellPanel(
        h5("Sisa Waktu:"),
        div(id = "timer_display", style = "font-size: 30px; font-weight: bold; color: red;", "60:00")
      ),
      uiOutput("progress_bar_ui")
    ),
    
    mainPanel(
      hidden(div(id = "exam_area", uiOutput("soal_ui"))),
      hidden(div(id = "result_area", 
                 div(style="text-align:center; padding:50px; border:2px solid #ddd;",
                     h2("TES SELESAI"),
                     h3(textOutput("final_score")),
                     p("Data Anda telah tersimpan. Anda tidak dapat mengulang tes ini.")
                 )
      )),
      hidden(actionButton("btn_kirim", "Kirim Hasil Akhir", class = "btn-success btn-lg w-100"))
    )
  )
)

erver <- function(input, output, session) {
  vals <- reactiveValues(
    theta = 0,
    answered = c(),
    current_item = NULL,
    selesai = FALSE,
    time_left = 3600,      # Tetapkan 3600 detik (60 menit)
    ujian_mulai = FALSE,
    item_bank = NULL
  )
  # --- AMBIL DATA SOAL DENGAN PROTEKSI ---
  observe({
    req(URL_GAS)
    tryCatch({
      res <- GET(URL_GAS, timeout(10))
      if (status_code(res) == 200) {
        raw_content <- content(res, "text", encoding = "UTF-8")
        # Validasi apakah isinya JSON, bukan HTML error
        if (startsWith(raw_content, "[") || startsWith(raw_content, "{")) {
          vals$item_bank <- fromJSON(raw_content)
        }
      }
    }, error = function(e) {
      showNotification("Gagal mengambil soal. Coba refresh halaman.", type = "error")
    })
  })
  # --- LOGIKA TOMBOL MULAI ---
  observeEvent(input$btn_mulai, {
    if (is.null(vals$item_bank) || nrow(vals$item_bank) == 0) {
      showNotification("Data soal sedang diunduh, silakan tunggu 2 detik...", type = "warning")
      return()
    }
    
    # Konversi paksa kolom IRT ke numerik agar tidak error saat dihitung
    vals$item_bank$a <- as.numeric(vals$item_bank$a)
    vals$item_bank$b <- as.numeric(vals$item_bank$b)
    vals$item_bank$c <- as.numeric(vals$item_bank$c)
    
    vals$ujian_mulai <- TRUE
    shinyjs::hide("btn_mulai")
    shinyjs::show("exam_area")
    
    # Cari soal pertama (paling mendekati kemampuan rata-rata b = 0)
    # Gunakan as.numeric lagi untuk memastikan keamanan
    idx_awal <- which.min(abs(vals$item_bank$b - 0))
    vals$current_item <- vals$item_bank[idx_awal, ]
  })

  # Logika Timer yang Diperbaiki
  observe({
    invalidateLater(1000, session)
    
    # Timer HANYA boleh jalan jika:
    # 1. Tombol mulai sudah diklik (ujian_mulai == TRUE)
    # 2. Ujian belum selesai (!vals$selesai)
    # 3. Data soal sudah berhasil diambil (!is.null(vals$item_bank))
    
    if (vals$ujian_mulai && !vals$selesai && !is.null(vals$item_bank)) {
      vals$time_left <- vals$time_left - 1
      
      # Update Tampilan Timer ke UI via JavaScript
      mins <- floor(vals$time_left / 60)
      secs <- vals$time_left %% 60
      runjs(sprintf("$('#timer_display').html('%02d:%02d');", mins, secs))
      
      # Jika waktu benar-benar habis
      if (vals$time_left <= 0) {
        vals$selesai <- TRUE
        showModal(modalDialog(title = "Waktu Habis!", "Sistem mengirim jawaban otomatis."))
        click("btn_kirim") 
      }
    }
  })
  # 4. Render Soal
  output$soal_ui <- renderUI({
    req(vals$current_item, !vals$selesai)
    item <- vals$current_item
    tagList(
      wellPanel(
        h3(item$soal),
        radioButtons("user_ans", "Pilihan:",
                     choices = setNames(c("A", "B", "C", "D"), 
                                        c(item$pilihan_a, item$pilihan_b, item$pilihan_c, item$pilihan_d)),
                     selected = character(0))
      ),
      actionButton("next_soal", "Simpan & Lanjut", class = "btn-info")
    )
  })

  # 5. Lanjut Soal (Simpan State ke Variable)
  observeEvent(input$next_soal, {
    req(input$user_ans)
    item <- vals$current_item
    
    # Logika IRT 3PL (Update Theta)
    is_correct <- if(input$user_ans == item$kunci) 1 else 0
    # ... (Gunakan rumus update theta Anda di sini) ...
    vals$theta <- vals$theta + (if(is_correct == 1) 0.5 else -0.5) # Contoh simpel
    
    vals$answered <- c(vals$answered, item$id)
    
    if (length(vals$answered) >= 10) { # Batasi misal 10 soal
      vals$selesai <- TRUE
      shinyjs::show("btn_kirim")
      shinyjs::hide("exam_area")
    } else {
      # Cari soal adaptif berikutnya
      available <- vals$item_bank[!(vals$item_bank$id %in% vals$answered), ]
      vals$current_item <- available[which.min(abs(available$b - vals$theta)), ]
    }
  })

  # 6. Kirim Akhir
  observeEvent(input$btn_kirim, {
    # Kirim data akhir ke Google Sheets
    body_data <- list(
      nama = input$user_name,
      theta = round(vals$theta, 3),
      status = "Selesai"
    )
    POST(URL_GAS, body = toJSON(body_data, auto_unbox = TRUE), encode = "json")
    
    shinyjs::hide("btn_kirim")
    shinyjs::show("result_area")
    output$final_score <- renderText({ paste("Skor Kemampuan Anda:", round(vals$theta, 3)) })
  })
}

shinyApp(ui = ui, server = server)
