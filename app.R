library(shiny)
library(caret)
library(httr2)

# -----------------------------
# Prepare the Sorting Hat model
# -----------------------------

hogwarts <- read.csv("super_heroes_hogwarts_v4.csv", header = TRUE)
hogwarts <- hogwarts[ , -c(1:8, 18, 20:27)]

hogwarts$House <- as.factor(hogwarts$House)
hogwarts$MagicalBackground <- as.factor(hogwarts$MagicalBackground)

dummy_values <- dummyVars(~ .,
                          data = hogwarts[, names(hogwarts) != "House"],
                          fullRank = FALSE)

hogwarts_dummy <- as.data.frame(predict(dummy_values,
                                         newdata = hogwarts[, names(hogwarts) != "House"]))

names(hogwarts_dummy) <- make.names(names(hogwarts_dummy))
hogwarts_dummy$House <- hogwarts$House

set.seed(666)
train_index <- sample(1:nrow(hogwarts_dummy), 0.7 * nrow(hogwarts_dummy))
train_df <- hogwarts_dummy[train_index, ]
rownames(train_df) <- NULL

train_norm <- train_df

norm_values <- preProcess(train_df[, c(1:8)],
                          method = c("center",
                                     "scale"))

train_norm[, c(1:8)] <- predict(norm_values,
                                 train_df[, c(1:8)])


# -----------------------------
# User interface
# -----------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #09090b;
        color: #eeeaf0;
        font-family: Arial, Helvetica, sans-serif;
      }
      .container-fluid {
        max-width: 1050px;
        padding-top: 24px;
      }
      .title-panel {
        text-align: center;
        margin-bottom: 20px;
      }
      .title-panel h1 {
        color: #d4b4df;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 42px;
        margin-bottom: 4px;
      }
      .hat-line {
        color: #b79ac4;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 19px;
        font-style: italic;
        margin-bottom: 10px;
      }
      .subtitle {
        color: #d0c8d3;
        font-size: 15px;
      }
      .panel-box {
        background: #151218;
        border: 1px solid #3b3041;
        border-radius: 6px;
        padding: 20px;
        margin-bottom: 18px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.55);
      }
      .panel-box h3, .panel-box h4 {
        color: #d3b1df;
        font-family: Georgia, 'Times New Roman', serif;
      }
      .panel-box p {
        color: #e2dce5;
      }
      .control-label {
        color: #f0ebf2;
      }
      .form-control {
        background-color: #0f0d11;
        color: #f4f0f6;
        border-color: #765181;
      }
      .form-control:focus {
        border-color: #9b72aa;
        box-shadow: 0 0 6px rgba(155,114,170,0.4);
      }
      .btn-primary {
        background-color: #765181;
        border-color: #765181;
        color: white;
        font-weight: bold;
      }
      .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
        background-color: #8b6398 !important;
        border-color: #8b6398 !important;
        color: white !important;
      }
      .result-box {
        background: #100d12;
        border-left: 5px solid #765181;
        padding: 18px;
        margin: 12px 0 20px 0;
      }
      .house-result {
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 34px;
        font-weight: bold;
        color: #d8b6e3;
      }
      .ai-box {
        background: #0d0b0f;
        border: 1px solid #765181;
        border-radius: 6px;
        padding: 16px;
        margin-top: 10px;
        color: #eeeaf0;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 17px;
        line-height: 1.5;
      }
      table {
        color: #eeeaf0 !important;
      }
      .table > thead > tr > th {
        color: #d3b1df !important;
        background-color: #151218 !important;
        border-bottom: 2px solid #765181 !important;
      }
      .table > tbody > tr > td {
        color: #eeeaf0 !important;
        background-color: #151218 !important;
        border-top: 1px solid #3b3041 !important;
      }
      .irs--shiny .irs-line {
        background: #2b2530 !important;
        border-color: #2b2530 !important;
      }
      .irs--shiny .irs-bar {
        background: #765181 !important;
        border-top: 1px solid #765181 !important;
        border-bottom: 1px solid #765181 !important;
      }
      .irs--shiny .irs-handle {
        background: #cdb4d8 !important;
        border-color: #9b72aa !important;
      }
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        background: #765181 !important;
        color: white !important;
      }
      .irs--shiny .irs-single:before,
      .irs--shiny .irs-from:before,
      .irs--shiny .irs-to:before {
        border-top-color: #765181 !important;
      }
      .irs--shiny .irs-min,
      .irs--shiny .irs-max,
      .irs-grid-text {
        color: #d8d1db !important;
        background: transparent !important;
      }
    "))
  ),

  div(class = "title-panel",
      h1("The Sorting Hat"),
      p(class = "hat-line", "Hmm, difficult... VERY difficult..."),
      p(class = "subtitle", "Enter your magical traits and let kNN decide your Hogwarts House.")),

  fluidRow(
    column(6,
           div(class = "panel-box",
               sliderInput("Manipulative", "Manipulative", min = 1, max = 10, value = 9, step = 1),
               sliderInput("Resourceful", "Resourceful", min = 1, max = 10, value = 9, step = 1),
               sliderInput("Dismissive", "Dismissive", min = 1, max = 10, value = 8, step = 1),
               sliderInput("Intelligent", "Intelligent", min = 1, max = 10, value = 8, step = 1),
               sliderInput("Trusting", "Trusting", min = 1, max = 10, value = 6, step = 1),
               sliderInput("Loyal", "Loyal", min = 1, max = 10, value = 8, step = 1),
               sliderInput("Stubborn", "Stubborn", min = 1, max = 10, value = 7, step = 1),
               sliderInput("Brave", "Brave", min = 1, max = 10, value = 7, step = 1),
               selectInput("MagicalBackground", "Magical Background",
                           choices = levels(hogwarts$MagicalBackground),
                           selected = "Pure-blood"),
               selectInput("k_value", "Number of Neighbours (k)",
                           choices = seq(1, 15, 2),
                           selected = 5),
               actionButton("sort_me", "Sort Me!", class = "btn-primary", width = "100%"))),

    column(6,
           div(class = "panel-box",
               h4("Result"),
               p("Predicted House:"),
               uiOutput("house_result"),
               h4("Probabilities"),
               tableOutput("house_probabilities"),
               h4("The Sorting Hat speaks"),
               actionButton("ask_hat", "Ask the Sorting Hat",
                            class = "btn-primary", width = "100%"),
               uiOutput("ai_explanation")))
  )
)

# -----------------------------
# Gemini explanation
# -----------------------------

get_sorting_hat_explanation <- function(predicted_house,
                                        probabilities,
                                        new_padawan) {

  gemini_key <- Sys.getenv("GEMINI_API_KEY")

  if (nchar(gemini_key) == 0) {
    return("The Sorting Hat's voice is silent. GEMINI_API_KEY is not available.")
  }

  probability_text <- paste(names(probabilities),
                            paste0(round(as.numeric(probabilities) * 100, 1), "%"),
                            collapse = ", ")

  trait_text <- paste(names(new_padawan),
                      new_padawan[1, ],
                      sep = " = ",
                      collapse = ", ")

  sorting_prompt <- paste(
    "You are the Hogwarts Sorting Hat.",
    "The final House prediction has already been made by a k-nearest neighbours model.",
    "Do not change or question that prediction.",
    paste("The predicted House is", predicted_house, "."),
    paste("The House probabilities are:", probability_text, "."),
    paste("The student's traits are:", trait_text, "."),
    "Give a short, dramatic and slightly ominous Sorting Hat explanation in 2 or 3 sentences.",
    "Use the supplied traits and probabilities as evidence.",
    "Do not invent new traits or facts.",
    "End by naming the predicted House clearly."
  )

  request_body <- list(
    contents = list(
      list(
        parts = list(
          list(text = sorting_prompt)
        )
      )
    )
  )

  response <- request(
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.5-flash:generateContent"
  )

  response <- req_headers(response,
                          "x-goog-api-key" = gemini_key)

  response <- req_body_json(response,
                            request_body)

  response <- req_retry(response,
                        max_tries = 3)

  response <- req_timeout(response,
                          seconds = 20)

  response <- req_perform(response)

  response_data <- resp_body_json(response)

  response_data$candidates[[1]]$content$parts[[1]]$text
}


# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {

  values <- reactiveValues(
    sorting_result = NULL
  )

  observeEvent(input$sort_me, {

    new_padawan <- data.frame(Manipulative = input$Manipulative,
                              Resourceful = input$Resourceful,
                              Dismissive = input$Dismissive,
                              Intelligent = input$Intelligent,
                              Trusting = input$Trusting,
                              Loyal = input$Loyal,
                              Stubborn = input$Stubborn,
                              Brave = input$Brave,
                              MagicalBackground = factor(input$MagicalBackground,
                                                         levels = levels(hogwarts$MagicalBackground)))

    new_padawan_dummy <- as.data.frame(predict(dummy_values,
                                                newdata = new_padawan))
    names(new_padawan_dummy) <- make.names(names(new_padawan_dummy))

    new_padawan_norm <- new_padawan_dummy
    new_padawan_norm[, c(1:8)] <- predict(norm_values,
                                          new_padawan_dummy[, c(1:8)])

    k_selected <- as.numeric(input$k_value)

    knn_model <- caret::knn3(House ~ .,
                             data = train_norm,
                             k = k_selected)

    house_prediction <- predict(knn_model,
                                newdata = new_padawan_norm,
                                type = "class")

    house_probability <- predict(knn_model,
                                 newdata = new_padawan_norm,
                                 type = "prob")

    values$sorting_result <- list(
      prediction = as.character(house_prediction),
      probabilities = house_probability,
      new_padawan = new_padawan,
      k = k_selected
    )
  })

  sorting_result <- reactive({
    values$sorting_result
  })

  output$house_result <- renderUI({
    req(sorting_result())

    div(class = "result-box",
        div(class = "house-result", paste0(sorting_result()$prediction, "!")),
        p(paste("Using k =", sorting_result()$k)))
  })

  output$house_probabilities <- renderTable({
    req(sorting_result())

    probabilities <- as.data.frame(t(sorting_result()$probabilities))
    probabilities$House <- rownames(probabilities)
    rownames(probabilities) <- NULL
    names(probabilities)[1] <- "Probability"
    probabilities$Probability <- paste0(round(probabilities$Probability * 100, 1), "%")

    probabilities[, c("House", "Probability")]
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  ai_text <- reactiveVal(NULL)

  observeEvent(input$sort_me, {
    ai_text(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$ask_hat, {
    req(sorting_result())

    ai_text("The Sorting Hat is considering your fate...")

    explanation <- tryCatch(
      get_sorting_hat_explanation(
        predicted_house = sorting_result()$prediction,
        probabilities = sorting_result()$probabilities,
        new_padawan = sorting_result()$new_padawan
      ),
      error = function(e) {
        "The Sorting Hat's voice fades into the darkness. The AI explanation could not be retrieved."
      }
    )

    ai_text(explanation)
  })

  output$ai_explanation <- renderUI({
    if (is.null(ai_text())) {
      return(div(class = "ai-box",
                 "What sayest thou?"))
    }

    div(class = "ai-box", ai_text())
  })
}

shinyApp(ui = ui, server = server)
