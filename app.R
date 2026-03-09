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
      hidden(actionButton("btn_kirim", "Kirim & Lihat Hasil Sertifikat", class = "btn-success btn-lg w-100"))
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # 1. Inisialisasi Data & State
  vals <- reactiveValues(
    theta = 0,
    answered = c(),
    current_item = NULL,
    selesai = FALSE,
    item_bank = NULL,
    total_target = 10 # Ubah jumlah soal yang ingin dikerjakan di sini
  )

  # 2. Fungsi Load Bank Soal dari GAS (Gunakan tab 'bank_soal')
  observe({
    req(URL_GAS)
    # Bungkus seluruh proses pengambilan data di dalam tryCatch
    tryCatch({
      res <- GET(URL_GAS, timeout(10)) # Tambahkan timeout 10 detik agar tidak reload selamanya
      
      if (status_code(res) == 200) {
        # Parsing data JSON
        data_soal <- fromJSON(content(res, "text", encoding = "UTF-8"))
        
        # Simpan ke reactive value
        vals$item_bank <- data_soal
        
        # Set soal pertama jika bank soal tidak kosong
        if (is.null(vals$current_item) && nrow(data_soal) > 0) {
          # Memilih soal dengan tingkat kesulitan (b) paling mendekati 0
          vals$current_item <- data_soal[which.min(abs(as.numeric(data_soal$b) - 0)), ]
        }
      } else {
        showNotification("Server Google Sheets tidak merespon (Error 200).", type = "warning")
      }
      
    }, error = function(e) {
      # Tampilkan notifikasi jika terjadi error koneksi atau JSON rusak
      showNotification(paste("Error Koneksi:", e$message), type = "error", duration = NULL)
      print(e) # Muncul di log shinyapps.io
    })
  })
  # 3. Fungsi Kategori Kemampuan
  get_category <- function(theta) {
    if (theta >= 1.5) return(list(label = "SANGAT MAHIR", color = "#1b5e20"))
    if (theta >= 0.5) return(list(label = "MAHIR", color = "#2ecc71"))
    if (theta >= -0.5) return(list(label = "CUKUP", color = "#f1c40f"))
    if (theta >= -1.5) return(list(label = "PERLU BELAJAR", color = "#e67e22"))
    return(list(label = "BUTUH BIMBINGAN", color = "#e74c3c"))
  }

  # 4. Render Progress Bar
  output$progress_bar_ui <- renderUI({
    done <- length(vals$answered)
    total <- vals$total_target
    persen <- round((done / total) * 100)
    tagList(
      p(paste0("Progres: ", done, " / ", total, " Soal")),
      div(class = "progress",
          div(class = "progress-bar progress-bar-striped progress-bar-animated bg-info", 
              style = paste0("width: ", persen, "%;"), paste0(persen, "%")))
    )
  })

  # 5. Render UI Soal
  output$soal_ui <- renderUI({
    if (vals$selesai) return(h3("Selesai! Silakan klik tombol hijau di bawah.", class="text-success"))
    item <- vals$current_item
    req(item)
    
    tagList(
      wellPanel(
        h3(item$soal),
        hr(),
        radioButtons("user_ans", "Pilih Jawaban Anda:",
                     choices = setNames(c("A", "B", "C", "D"), 
                                        c(item$pilihan_a, item$pilihan_b, item$pilihan_c, item$pilihan_d)),
                     selected = character(0))
      ),
      actionButton("next_soal", "Simpan & Lanjut", class = "btn-primary btn-md")
    )
  })

  # 6. Logika Tombol Lanjut (IRT 3PL)
  observeEvent(input$next_soal, {
    req(input$user_ans, input$user_name != "")
    item <- vals$current_item
    
    # Hitung Benar/Salah
    is_correct <- if(input$user_ans == item$kunci) 1 else 0
    
    # Update Theta (Rumus Sederhana Pendekatan IRT 3PL)
    # Probabilitas Benar P(theta)
    exp_val <- exp(item$a * (vals$theta - item$b))
    p_theta <- item$c + (1 - item$c) * (exp_val / (1 + exp_val))
    
    # Update theta berdasarkan error (is_correct - p_theta)
    vals$theta <- vals$theta + (item$a * (is_correct - p_theta) * 0.5)
    
    # Simpan ID yang sudah dijawab
    vals$answered <- c(vals$answered, item$id)
    
    # Cek Batas Soal
    if (length(vals$answered) >= vals$total_target) {
      vals$selesai <- TRUE
      shinyjs::show("btn_kirim")
    } else {
      # Cari soal berikutnya yang 'b' nya paling dekat dengan theta baru
      available <- vals$item_bank[!(vals$item_bank$id %in% vals$answered), ]
      vals$current_item <- available[which.min(abs(available$b - vals$theta)), ]
    }
  })

  # 7. Tombol Kirim & Tampilkan Dashboard
  observeEvent(input$btn_kirim, {
    cat_data <- get_category(vals$theta)
    
    # Kirim ke Google Sheets
    body_data <- list(
      nama = input$user_name,
      theta = round(vals$theta, 3),
      kategori = cat_data$label,
      jumlah_soal = length(vals$answered)
    )
    POST(url = URL_GAS, body = toJSON(body_data, auto_unbox = TRUE), encode = "json")
    
    # Tampilkan Dashboard
    output$res_name <- renderText({ input$user_name })
    output$res_theta <- renderText({ round(vals$theta, 3) })
    output$res_cat_ui <- renderUI({
      span(cat_data$label, style = paste0("color: ", cat_data$color, "; font-weight: bold; font-size: 24px;"))
    })
    
    shinyjs::hide("quiz-container")
    shinyjs::hide("side-panel")
    shinyjs::show("result-dashboard")
  })
  
  observeEvent(input$btn_ulang, { session$reload() })
}

shinyApp(ui, server)
