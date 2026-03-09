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
    print("Tombol ditekan!")
    
    # Kita hapus syarat 'if length >= 5' agar bisa langsung tes kirim
    vals$selesai <- TRUE

    # Persiapkan data
    body_data <- list(
      nama = input$user_name,
      theta = vals$theta
    )

    # Kirim ke Google Sheets
    POST(url = URL_GAS, 
         body = toJSON(body_data, auto_unbox = TRUE), 
         encode = "json")

    # Tampilkan notifikasi di layar HP/Laptop peserta
    showModal(modalDialog(
      title = "Sukses", 
      "Data Anda telah disimpan ke Google Sheets!",
      footer = modalButton("Tutup")
    ))
})
}

shinyApp(ui, server)
