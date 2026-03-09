library(shiny)
library(httr)
library(jsonlite)

# Masukkan URL dari Langkah 1 di sini
URL_GAS <- "https://script.google.com/macros/s/AKfycbz-q5DjvwhnG7Y3SmN39kUAivxLYEDiBSU1u7o9n4hVHVDXkxPKw-1r_OjNk21i_2Na/exec"

# --- UI ---
ui <- fluidPage(
  titlePanel("CAT Online - Simple GAS Connection"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_name", "Nama Peserta:", ""),
      verbatimTextOutput("theta_now")
    ),
    mainPanel(
      uiOutput("soal_ui"),
      actionButton("btn_kirim", "Kirim Jawaban", class = "btn-success")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Reactive state
  vals <- reactiveValues(theta = 0, selesai = FALSE)
  
  # (Logika IRT 3PL Anda tetap di sini...)
  
  observeEvent(input$btn_kirim, {
    # Misalkan tes sudah selesai di soal ke-5
    if (length(vals$answered) >= 5) {
      vals$selesai <- TRUE
      
      # KIRIM DATA KE GOOGLE SHEETS VIA GAS
      body_data <- list(
        nama = input$user_name,
        theta = vals$theta
      )
      
      # Mengirim request POST
      POST(url = URL_GAS, 
           body = toJSON(body_data, auto_unbox = TRUE), 
           encode = "json")
      
      showModal(modalDialog(title = "Sukses", "Data Anda telah disimpan ke Google Sheets!"))
    }
  })
}

shinyApp(ui, server)
