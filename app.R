library(shiny)
library(httr)
library(jsonlite)

# Masukkan URL dari Langkah 1 di sini
URL_GAS <- "https://script.google.com/macros/s/AKfycbz-q5DjvwhnG7Y3SmN39kUAivxLYEDiBSU1u7o9n4hVHVDXkxPKw-1r_OjNk21i_2Na/exec"

# --- UI ---
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("CAT Online - IRT 3PL Adaptive Test"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Status Peserta"),
      textInput("user_name", "Nama Peserta:", ""),
      hr(),
      # TEMPAT PROGRESS BAR
      uiOutput("progress_bar_ui"), 
      hr(),
      verbatimTextOutput("theta_display")
    ),
    
    mainPanel(
      uiOutput("soal_ui"),
      br(),
      actionButton("btn_kirim", "Kirim Hasil Akhir", class = "btn-success", style = "display:none;")
    )
  )
)
# --- SERVER ---
server <- function(input, output, session) {
  
  # Inisialisasi (sama seperti sebelumnya)
  vals <- reactiveValues(
    theta = 0,
    answered = c(),
    current_item = NULL,
    selesai = FALSE,
    total_soal_target = 10 # Tentukan target jumlah soal di sini
  )

  # --- FITUR PROGRESS BAR ---
  output$progress_bar_ui <- renderUI({
    # Hitung persentase
    done <- length(vals$answered)
    total <- vals$total_soal_target
    persen <- round((done / total) * 100)
    
    tagList(
      p(paste0("Progress: ", done, " dari ", total, " soal")),
      div(class = "progress",
          div(class = "progress-bar progress-bar-striped progress-bar-animated", 
              role = "progressbar", 
              style = paste0("width: ", persen, "%;"),
              paste0(persen, "%")
          )
      )
    )
  })

  # --- LOGIKA TOMBOL NEXT (Update) ---
  observeEvent(input$next_soal, {
    req(input$user_ans)
    item <- vals$current_item
    
    # ... (Logika perhitungan IRT 3PL tetap sama seperti sebelumnya) ...
    
    # Update status selesai jika sudah mencapai target
    if (length(vals$answered) >= vals$total_soal_target) {
      vals$selesai <- TRUE
      shinyjs::show("btn_kirim") # Menampilkan tombol kirim di akhir
    } else {
      vals$current_item <- get_next_item()
    }
  })

  # 3. Render Antarmuka Soal
  output$soal_ui <- renderUI({
    if (vals$selesai) return(h3("Tes Selesai. Silakan klik Kirim Hasil."))
    
    item <- vals$current_item
    req(item)
    
    tagList(
      wellPanel(
        h4(item$soal),
        radioButtons("user_ans", "Pilih Jawaban:",
                     choices = setNames(c("A", "B", "C", "D"), 
                                        c(item$pilihan_a, item$pilihan_b, item$pilihan_c, item$pilihan_d)),
                     selected = character(0))
      ),
      actionButton("next_soal", "Simpan & Lanjut Soal Berikutnya", class = "btn-info")
    )
  })

  # 4. Logika Update Theta (IRT 3PL) saat Tombol 'Next' Ditekan
  observeEvent(input$next_soal, {
    req(input$user_ans)
    item <- vals$current_item
    
    # Cek Jawaban
    is_correct <- if(input$user_ans == item$kunci) 1 else 0
    
    # Hitung Update Theta Sederhana (Step-Size Adjustment)
    # Jika benar, kemampuan naik. Jika salah, kemampuan turun.
    # Pengaruh perubahan dipengaruhi oleh daya beda (a)
    step <- (item$a / 2) * (is_correct - (item$c + (1 - item$c) / 2))
    vals$theta <- vals$theta + step
    
    # Catat history
    vals$answered <- c(vals$answered, item$id)
    
    # Cari soal berikutnya
    next_it <- get_next_item()
    
    # Berhenti jika soal habis atau sudah mencapai batas (misal 10 soal)
    if (is.null(next_it) || length(vals$answered) >= 10) {
      vals$selesai <- TRUE
      updateActionButton(session, "btn_kirim", label = "Kirim Hasil Akhir ke Server")
    } else {
      vals$current_item <- next_it
    }
  })

  # 5. Tombol Kirim Akhir ke Google Sheets
  observeEvent(input$btn_kirim, {
    req(vals$selesai)
    
    body_data <- list(
      nama = input$user_name,
      theta = round(vals$theta, 3),
      jumlah_soal = length(vals$answered)
    )

    POST(url = URL_GAS, 
         body = toJSON(body_data, auto_unbox = TRUE), 
         encode = "json")

    showModal(modalDialog(title = "Sukses", "Data kompetensi Anda telah tersimpan."))
  })
}
shinyApp(ui, server)
