library(shiny)
library(httr)
library(jsonlite)
library(bslib)
library(shinyjs)

# --- KONFIGURASI ---
# Ganti dengan URL GAS /exec terbaru Anda
URL_GAS <- "https://script.google.com/macros/s/AKfycbx-w1rcACttgfvm3fcDPRIsrsI8qOdNzFKnK0ks0H1t77nL8I1WrtxWRnNBXzuGywAK/exec"

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$script(HTML("
      window.onbeforeunload = function() {
        return 'Tes sedang berlangsung, jangan refresh halaman!';
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
      wellPanel(
        h5("Sisa Waktu:"),
        div(id = "timer_display", style = "font-size: 30px; font-weight: bold; color: red;", "60:00")
      )
    ),
    
    mainPanel(
      hidden(div(id = "exam_area", uiOutput("soal_ui"))),
      hidden(div(id = "result_area", 
                 div(style="text-align:center; padding:50px; border:2px solid #ddd; border-radius:15px;",
                     h2("TES SELESAI"),
                     hr(),
                     h3(textOutput("final_score")),
                     h4(uiOutput("final_cat")),
                     p("Data telah tersimpan secara otomatis.")
                 )
      )),
      hidden(actionButton("btn_kirim", "Lihat Hasil Akhir", class = "btn-success btn-lg w-100"))
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
    time_left = 3600,
    ujian_mulai = FALSE,
    item_bank = NULL
  )

  # 1. Ambil Data Soal
  observe({
    req(URL_GAS)
    tryCatch({
      res <- GET(URL_GAS, timeout(15))
      showNotification(raw)
      if (status_code(res) == 200) {
        raw <- content(res, "text", encoding = "UTF-8")
        raw <- trimws(raw)
      if (startsWith(raw, "[") || startsWith(raw, "{")) {
        vals$item_bank <- fromJSON(raw)
        }
      }
    }, error = function(e) {
      showNotification("Koneksi ke bank soal bermasalah.", type = "error")
    })
  })

  # 2. Tombol Mulai
  observeEvent(input$btn_mulai, {
    req(input$user_name != "")
    if (is.null(vals$item_bank)) {
      showNotification("Menunggu data soal...", type = "warning")
      return()
    }
    
    # Pastikan data numerik
    vals$item_bank$a <- as.numeric(vals$item_bank$a)
    vals$item_bank$b <- as.numeric(vals$item_bank$b)
    vals$item_bank$c <- as.numeric(vals$item_bank$c)
    
    vals$ujian_mulai <- TRUE
    shinyjs::hide("btn_mulai")
    shinyjs::disable("user_name")
    shinyjs::show("exam_area")
    
    # Pilih soal pertama
    req(nrow(vals$item_bank) > 0)
    vals$current_item <- vals$item_bank[
      which.min(abs(vals$item_bank$b - 0)) 
    ]
  })

  # 3. Timer Logic
  observeEvent(vals$ujian_mulai, {
    while(vals$ujian_mulai && !vals$selesai){
    invalidateLater(1000, session)
    vals$time_left <- vals$time_left - 1
    }
  })      
      mins <- floor(vals$time_left / 60)
      secs <- vals$time_left %% 60
      runjs(sprintf("$('#timer_display').html('%02d:%02d');", mins, secs))
      
      if (vals$time_left <= 0) {
        vals$selesai <- TRUE
        shinyjs::show("btn_kirim")
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
        radioButtons(
         "user_ans",
         "Jawaban:",
         choices = setNames(c("A","B","C","D"),
                            c(item$pilihan_a,item$pilihan_b,item$pilihan_c,item$pilihan_d)),
         selected = character(0)
        )
     ),
      actionButton("next_soal", "Simpan & Lanjut", class = "btn-info")
    )
  })

  # 5. Next Soal Logic
  observeEvent(input$next_soal, {

  req(input$user_ans)

  item <- vals$current_item
  is_correct <- if(input$user_ans == item$kunci) 1 else 0

  exp_val <- exp(item$a * (vals$theta - item$b))
  p_theta <- item$c + (1 - item$c) * (exp_val / (1 + exp_val))

  vals$theta <- vals$theta + (item$a * (is_correct - p_theta) * 0.5)

  vals$answered <- c(vals$answered, item$id)

  if (length(vals$answered) >= 10) {

    vals$selesai <- TRUE
    shinyjs::hide("exam_area")
    shinyjs::show("btn_kirim")

  } else {

    avail <- vals$item_bank[!(vals$item_bank$id %in% vals$answered), ]

    vals$current_item <- avail[
      which.min(abs(as.numeric(avail$b) - vals$theta))
    ]

  }

})
# 6. Kirim Hasil
observeEvent(input$btn_kirim, {

  cat_label <- "Cukup"

  if(vals$theta >= 1.0) cat_label <- "Tinggi"
  if(vals$theta <= -1.0) cat_label <- "Rendah"

  body <- list(
    nama = input$user_name,
    theta = round(vals$theta,3),
    kategori = cat_label
  )

  POST(URL_GAS,
       body = toJSON(body, auto_unbox = TRUE),
       encode = "json")

  output$final_score <- renderText({
    paste("Skor Akhir:", round(vals$theta,3))
  })

  output$final_cat <- renderUI({
    h4(paste("Kategori:", cat_label), style="color:green;")
  })

  shinyjs::hide("btn_kirim")
  shinyjs::show("result_area")

})
}  
# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)
