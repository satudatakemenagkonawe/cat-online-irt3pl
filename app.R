library(shiny)
library(httr)
library(jsonlite)
library(bslib)
library(shinyjs)

# --- KONFIGURASI ---
URL_GAS <- "ISI_DENGAN_URL_DEPLOYMENT_ANDA_YANG_BARU"

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

server <- function(input, output, session) {
  
  vals <- reactiveValues(
    theta = 0,
    answered = c(),
    current_item = NULL,
    selesai = FALSE,
    time_left = 3600, # 60 menit dalam detik
    ujian_mulai = FALSE
  )

  # 1. Load Data Soal (Amankan dengan tryCatch)
  observe({
    req(URL_GAS)
    tryCatch({
      res <- GET(URL_GAS)
      if (status_code(res) == 200) {
        vals$item_bank <- fromJSON(content(res, "text"))
      }
    }, error = function(e) { 
      showNotification("Gagal terhubung ke pusat data.", type = "error") 
    })
  })

  # 2. Tombol Mulai & Cek Status (Anti-Refresh Sederhana)
  observeEvent(input$btn_mulai, {
    req(input$user_name)
    # Di sini Anda bisa menambahkan logika pengecekan ke Database Google Sheet
    # Untuk memastikan Nama tersebut belum pernah 'Selesai'
    
    vals$ujian_mulai <- TRUE
    shinyjs::disable("user_name")
    shinyjs::hide("btn_mulai")
    shinyjs::show("exam_area")
    
    # Pilih soal pertama
    if (is.null(vals$current_item)) {
      vals$current_item <- vals$item_bank[which.min(abs(vals$item_bank$b - 0)), ]
    }
  })

  # 3. Logika Timer
  observe({
    invalidateLater(1000, session)
    if (vals$ujian_mulai && !vals$selesai) {
      vals$time_left <- vals$time_left - 1
      
      # Update Tampilan Timer
      mins <- floor(vals$time_left / 60)
      secs <- vals$time_left %% 60
      runjs(sprintf("$('#timer_display').html('%02d:%02d');", mins, secs))
      
      if (vals$time_left <= 0) {
        vals$selesai <- TRUE
        showModal(modalDialog(title = "Waktu Habis!", "Sistem akan mengirimkan jawaban Anda secara otomatis."))
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

shinyApp(ui, server)
