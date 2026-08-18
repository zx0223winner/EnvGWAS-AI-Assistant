###############################################
# EnvGWAS AI Assistant
# R Shiny + Ollama + Local RAG
###############################################

library(shiny)
library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(readr)
library(tibble)

###############################################
# Configuration
###############################################

ollama_embed_url <- "http://localhost:11434/api/embed"
ollama_chat_url  <- "http://localhost:11434/api/chat"

embed_model <- "nomic-embed-text"
chat_model  <- "llama3"

# please modify the PATH accordingly
kb_folder <- "Desktop/envgwas_kb"
embedding_file <- "Desktop/embeddings.rds"

# Set TRUE only when adding/updating documents
REBUILD_EMBEDDINGS <- TRUE

###############################################
# Embedding Function
###############################################

get_embedding <- function(text) {
  
  req <- request(ollama_embed_url) |>
    req_body_json(list(
      model = embed_model,
      input = text
    ))
  
  resp <- req_perform(req)
  data <- resp_body_json(resp)
  
  unlist(data$embeddings)
}

###############################################
# Cosine Similarity
###############################################

cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
}

###############################################
# Build Knowledge Base
###############################################

build_kb_embeddings <- function(path = kb_folder) {
  
  files <- list.files(
    path,
    full.names = TRUE
  )
  
  kb <- tibble(
    id = basename(files),
    text = map_chr(files, read_file)
  )
  
  message("Generating embeddings...")
  
  kb$embedding <- map(
    kb$text,
    get_embedding
  )
  
  saveRDS(
    kb,
    embedding_file
  )
  
  message(
    sprintf(
      "Saved embeddings for %d documents.",
      nrow(kb)
    )
  )
  
  kb
}

###############################################
# Load Knowledge Base
###############################################

if (REBUILD_EMBEDDINGS || !file.exists(embedding_file)) {
  
  kb <- build_kb_embeddings()
  
} else {
  
  kb <- readRDS(embedding_file)
  
  message(
    sprintf(
      "Loaded %d indexed documents.",
      nrow(kb)
    )
  )
}

###############################################
# Semantic Retrieval
###############################################

# adjust the n to have more rank keywords
retrieve_context_embed <- function(
    question,
    kb,
    n = 3
){
  
  q_embed <- get_embedding(question)
  
  kb$score <- map_dbl(
    kb$embedding,
    ~ cosine_similarity(q_embed, .x)
  )
  
  kb |>
    arrange(desc(score)) |>
    slice_head(n = n) |>
    pull(text) |>
    paste(collapse = "\n\n---\n\n")
}

###############################################
# Prompt Builder
###############################################

build_envgwas_prompt <- function(
    question,
    context_text
){
  
  paste(
    "You are an envGWAS research assistant.",
    "Use the following retrieved context from the knowledge base.",
    "",
    context_text,
    "",
    "Question:",
    question,
    "",
    "Provide a clear and technically accurate answer suitable for a statistical genetics researcher.",
    sep = "\n"
  )
}

###############################################
# User Interface
###############################################

ui <- fluidPage(
  
  titlePanel("EnvGWAS AI Assistant"),
  
  radioButtons(
    "mode",
    "Mode:",
    choices = c(
      "General Chat" = "general",
      "envGWAS Assistant" = "envgwas"
    ),
    inline = TRUE
  ),
  
  textAreaInput(
    "prompt",
    "Your message:",
    "",
    width = "100%",
    height = "100px"
  ),
  
  actionButton(
    "send",
    "Send"
  ),
  
  hr(),
  
  h3("Conversation"),
  
  verbatimTextOutput(
    "conversation"
  )
)

###############################################
# Server
###############################################

server <- function(
    input,
    output,
    session
)
{
  
  messages <- reactiveVal(list())
  
  observeEvent(input$send, {
    
    user_msg <- trimws(input$prompt)
    
    if (!nzchar(user_msg)) {
      return()
    }
    
    mode <- input$mode
    
    ###########################################
    # RAG Mode
    ###########################################
    
    if (mode == "envgwas") {
      
      context <- retrieve_context_embed(
        user_msg,
        kb
      )
      
      prompt <- build_envgwas_prompt(
        user_msg,
        context
      )
      
      user_role_msg <- list(
        role = "user",
        content = prompt
      )
      
    } else {
      
      #########################################
      # General Chat
      #########################################
      
      user_role_msg <- list(
        role = "user",
        content = user_msg
      )
    }
    
    ###########################################
    # Store User Message
    ###########################################
    
    updated <- append(
      messages(),
      list(user_role_msg)
    )
    
    messages(updated)
    
    ###########################################
    # Call Ollama Chat API
    ###########################################
    
    req <- request(ollama_chat_url) |>
      req_body_json(list(
        model = chat_model,
        messages = messages(),
        stream = FALSE
      ))
    
    resp <- req_perform(req)
    
    data <- resp_body_json(resp)
    
    assistant_msg <- data$message$content
    
    ###########################################
    # Store Assistant Message
    ###########################################
    
    updated <- append(
      messages(),
      list(
        list(
          role = "assistant",
          content = assistant_msg
        )
      )
    )
    messages(updated)
  })
  
  #############################################
  # Render Conversation
  #############################################
  
  output$conversation <- renderText({
    
    conv <- messages()
    
    if (length(conv) == 0) {
      return("")
    }
    
    paste(
      vapply(
        conv,
        function(m) {
          
          paste0(
            if (m$role == "user")
              "You: "
            else
              "Assistant: ",
            m$content,
            "\n"
          )
          
        },
        character(1)
      ),
      collapse = "\n"
    )
  })
}

###############################################
# Run Application
###############################################

shinyApp(ui, server)
