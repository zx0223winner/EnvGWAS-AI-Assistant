###############################################
# Combined EnvGWAS AI Assistant (Tab 1)
# + Corrected AI GWAS Plotting Agent (Tab 2)
###############################################

smart_load <- function(pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

pkgs <- c(
  "shiny","bslib","httr","httr2","jsonlite","purrr",
  "dplyr","readr","tibble","htmltools","markdown",
  "later","ggplot2","qqman","glue","stringr"
)
smart_load(pkgs)

###############################################
# Configuration
###############################################

#set up the working directory
#setwd("Desktop/")

ollama_embed_url <- "http://localhost:11434/api/embed"
ollama_chat_url  <- "http://localhost:11434/api/chat"

# upload filemaximum size 50 Mb
options(shiny.maxRequestSize = 50 * 1024^2) 
# 1 GB: options(shiny.maxRequestSize = 1000 * 1024^2)

embed_model <- "nomic-embed-text"
chat_model  <- "llama3"

kb_folder      <- "Desktop/EnvGWAS_AI_Assistant_2026/EnvGWAS_AI_Assistant_Github/envgwas_kb"
embedding_file <- "Desktop/EnvGWAS_AI_Assistant_2026/EnvGWAS_AI_Assistant_Github/embeddings.rds"

#Set False provided no new files added to the knowledge base.
REBUILD_EMBEDDINGS <- TRUE

###############################################
# TAB 1: EnvGWAS AI Assistant
###############################################

tab1_ui <- tabPanel(
  "Knowledge Assistant",
  tags$head(
    tags$script(HTML("
      function scrollChatToBottom() {
        var chat = document.getElementById('chat_window');
        if (chat) { chat.scrollTop = chat.scrollHeight; }
      }
      Shiny.addCustomMessageHandler('scrollChat', function(m) {
        setTimeout(scrollChatToBottom, 50);
      });
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "mode", "Operational Mode:",
        choices = c("General Chat" = "general", "envGWAS Assistant" = "envgwas"),
        inline = TRUE
      ),
      p(span("Status: ", style = "font-weight: bold;"), "Connected to Local Ollama instance.")
    ),
    mainPanel(
      div(
        id = "chat_window",
        style = "height: 450px; overflow-y: auto; padding: 15px;
                border: 1px solid #ddd; border-radius: 8px; background-color: #fcfcfc;",
        uiOutput("conversation")
      ),
      br(),
      textAreaInput("prompt", "Your message:", "", width = "100%", height = "80px"),
      actionButton("send", "Send Message", class = "btn-primary")
    )
  )
)

tab1_server <- function(input, output, session) {
  
  get_embedding <- function(text) {
    tryCatch({
      req <- request(ollama_embed_url) |>
        req_body_json(list(model = embed_model, input = text)) |>
        req_timeout(30) # Increased from 10 to 30 to prevent HTTP connection drops
      resp <- req_perform(req)
      data <- resp_body_json(resp)
      unlist(data$embeddings)
    }, error = function(e) {
      warning("Embedding generation failed: ", e$message)
      return(rep(0, 768))
    })
  }
  
  cosine_similarity <- function(a, b) {
    if (all(a == 0) || all(b == 0)) return(0)
    sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
  }
  
  # Helper function to divide markdown files safely by headers and length
  chunk_text_structurally <- function(file_text, file_name) {
    # Standardize Windows newlines to UNIX style
    file_text <- stringr::str_replace_all(file_text, "\r\n", "\n")
    
    # Split cleanly right before any line starting with 1 to 4 '#' headers
    # Handles headers at the very start of the file or after a newline
    chunks <- unlist(stringr::str_split(file_text, "(?=(\n|^)#{1,4} )"))
    chunks <- chunks[stringr::str_trim(chunks) != ""]
    
    if (length(chunks) == 0) return(tibble::tibble(id = character(), text = character()))
    
    # If the first chunk does not start with a header, label it automatically
    if (!stringr::str_detect(chunks[1], "^#{1,4} ")) {
      chunks[1] <- paste0("# Introduction (Auto-generated Header)\n", chunks[1])
    }
    
    final_chunks <- character()
    
    # Secondary split safeguard: If a header section is excessively long, split it by paragraph
    for (chunk in chunks) {
      if (nchar(chunk) > 2000) {
        # Extract the original header title to prepend to sub-paragraphs for tracking context
        header_line <- stringr::str_extract(chunk, "^#{1,4} [^\n]+")
        if (is.na(header_line)) header_line <- "# Continued Section"
        
        # Split by paragraph
        sub_splits <- unlist(stringr::str_split(chunk, "\n\n"))
        sub_splits <- sub_splits[stringr::str_trim(sub_splits) != ""]
        
        for (i in seq_along(sub_splits)) {
          # Keep the context alive by attaching the parent header if it's not the line that already holds it
          if (i == 1) {
            final_chunks <- c(final_chunks, sub_splits[i])
          } else {
            final_chunks <- c(final_chunks, paste0(header_line, " (Part ", i, ")\n", sub_splits[i]))
          }
        }
      } else {
        final_chunks <- c(final_chunks, chunk)
      }
    }
    
    # Assemble structured tracking table
    tibble::tibble(
      id = paste0(file_name, "_chunk_", seq_along(final_chunks)),
      text = final_chunks
    )
  }
  
  build_kb_embeddings <- function(path = kb_folder) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    files <- list.files(path, full.names = TRUE)
    if (length(files) == 0) {
      message("Knowledge base directory is empty.")
      return(tibble(id = character(), text = character(), embedding = list()))
    }
    
    # 1. Map files to structural markdown text chunks
    kb <- purrr::map_dfr(files, function(file) {
      file_text <- readr::read_file(file)
      chunk_text_structurally(file_text, basename(file))
    })
    
    if (nrow(kb) == 0) {
      message("No valid text chunks could be parsed.")
      return(tibble(id = character(), text = character(), embedding = list()))
    }
    
    # NEW: Create a Shiny UI Progress bar
    withProgress(message = 'Building Knowledge Base', value = 0, {
      
      message(paste(" has generated embeddings for", nrow(kb), "Markdown structural chunks...,"))
      
      # 2. Iterate and match individual embeddings per target text chunk with progress tracking
      kb$embedding <- purrr::imap(kb$text, function(text, index) {
        # Update progress bar text and percentage
        incProgress(1 / nrow(kb), detail = paste("Embedding chunk", index, "of", nrow(kb)))
        get_embedding(text)
      })
      
    }) # Progress bar closes automatically here when finished
    
    # Filter out chunks that failed to get an embedding
    kb <- kb |> 
      dplyr::filter(purrr::map_lgl(embedding, ~ !all(.x == 0)))
    
    if (nrow(kb) == 0) {
      warning("All embeddings failed. Please verify Ollama is running and accessible.")
    }
    
    saveRDS(kb, embedding_file)
    kb
  }
  
  if (REBUILD_EMBEDDINGS || !file.exists(embedding_file)) {
    kb <- build_kb_embeddings()
  } else {
    kb <- readRDS(embedding_file)
  }
  
  retrieve_context_embed <- function(question, kb_df, n = 3) {
    if (nrow(kb_df) == 0) return("")
    q_embed <- get_embedding(question)
    
    # Safeguard if embedding failed for the question
    if (all(q_embed == 0)) return("Error: Failed to process vector space mapping for user question.")
    
    kb_df$score <- map_dbl(kb_df$embedding, ~ cosine_similarity(q_embed, .x))
    kb_df |>
      arrange(desc(score)) |>
      slice_head(n = n) |>
      pull(text) |>
      paste(collapse = "\n\n---\n\n")
  }
  
  build_envgwas_prompt <- function(context_text) {
    paste(
      "You are an envGWAS research assistant.",
      "Use the retrieved context below to answer the user's question.",
      "Provide concise, non-redundant, technically accurate answers.",
      "",
      "Context:",
      context_text,
      "",
      "Guidelines:",
      "- Do NOT repeat previous answers.",
      "- Do NOT restate the question.",
      "- Continue the conversation naturally.",
      "- Use domain knowledge when context is insufficient.",
      sep = "\n"
    )
  }
  
  
  
  messages    <- reactiveVal(list())
  typing      <- reactiveVal(FALSE)
  stream_text <- reactiveVal("")
  
  observeEvent(input$send, {
    user_msg <- trimws(input$prompt)
    if (!nzchar(user_msg)) return()
    
    updateTextAreaInput(session, "prompt", value = "")
    mode <- input$mode
    
    # Store user message in history
    current_history <- messages()
    new_history <- append(current_history, list(list(role = "user", content = user_msg)))
    messages(new_history)
    
    typing(TRUE)
    stream_text("")
    session$sendCustomMessage("scrollChat", list())
    
    # Build API messages
    api_messages <- list()
    
    if (mode == "envgwas") {
      
      # Retrieve context FIRST
      context_data <- retrieve_context_embed(user_msg, kb)
      
      # Build system prompt WITHOUT embedding the question
      system_msg <- build_envgwas_prompt(context_data)
      
      # Correct ordering:
      # 1. System message
      api_messages <- append(api_messages, list(list(role = "system", content = system_msg)))
      
      # 2. Conversation history
      api_messages <- append(api_messages, current_history)
      
      # 3. New user message
      api_messages <- append(api_messages, list(list(role = "user", content = user_msg)))
      
    } else {
      
      # General chat mode: no RAG
      api_messages <- append(api_messages, current_history)
      api_messages <- append(api_messages, list(list(role = "user", content = user_msg)))
      
    }
    
    ctx_domain <- shiny::getDefaultReactiveDomain()
    
    later::later(function() {
      shiny::withReactiveDomain(ctx_domain, {
        
        buffer <- ""
        
        tryCatch({
          req <- request(ollama_chat_url) |>
            req_body_json(list(model = chat_model, messages = api_messages, stream = TRUE)) |>
            req_timeout(60)
          
          resp <- req_perform_stream(req, callback = function(x) {
            shiny::withReactiveDomain(ctx_domain, {
              
              buffer <<- paste0(buffer, rawToChar(x))
              lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]
              
              if (!endsWith(buffer, "\n") && length(lines) > 0) {
                buffer <<- lines[length(lines)]
                lines <- lines[-length(lines)]
              } else {
                buffer <<- ""
              }
              
              for (line in lines) {
                if (!nzchar(trimws(line))) next
                part <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
                
                if (!is.null(part$message$content)) {
                  shiny::isolate({
                    if (typing()) typing(FALSE)
                    stream_text(paste0(stream_text(), part$message$content))
                  })
                  session$sendCustomMessage("scrollChat", list())
                }
              }
            })
            TRUE
          })
          
          shiny::isolate({
            final_response <- stream_text()
            messages(append(messages(), list(list(role = "assistant", content = final_response))))
          })
          
        }, error = function(e) {
          shiny::isolate({
            messages(append(messages(), list(list(
              role = "assistant",
              content = paste("Error processing request:", e$message)
            ))))
          })
        })
        
        shiny::isolate({
          stream_text("")
          typing(FALSE)
        })
        session$sendCustomMessage("scrollChat", list())
      })
    }, 0.1)
  })
  
  
  output$conversation <- renderUI({
    conv <- messages()
    active_stream <- stream_text()
    is_typing <- typing()
    
    ui_elements <- lapply(conv, function(m) {
      is_user <- m$role == "user"
      div(
        style = paste(
          "display: flex; margin-bottom: 15px;",
          if (is_user) "justify-content: flex-end;" else "justify-content: flex-start;"
        ),
        div(
          style = paste(
            "max-width: 75%; padding: 12px 16px; border-radius: 15px;",
            if (is_user) "background-color: #007bff; color: white;"
            else "background-color: #f1f3f4; color: #333;"
          ),
          strong(if (is_user) "You" else "Assistant"),
          tags$div(style = "margin-top: 5px;",
                   HTML(markdownToHTML(text = m$content, fragment.only = TRUE)))
        )
      )
    })
    
    if (is_typing || nzchar(active_stream)) {
      ui_elements <- append(ui_elements, list(
        div(
          style = "display: flex; justify-content: flex-start; margin-bottom: 15px;",
          div(
            style = "max-width: 75%; padding: 12px 16px; border-radius: 15px;
                     background-color: #e9ecef; color: #333;",
            strong("Assistant"),
            tags$div(
              style = "margin-top: 5px;",
              if (is_typing) em("Thinking...")
              else HTML(markdownToHTML(text = active_stream, fragment.only = TRUE))
            )
          )
        )
      ))
    }
    
    tagList(ui_elements)
  })
}

###############################################
# TAB 2: Corrected AI Plotting Agent
###############################################

tab2_ui <- tabPanel(
  "AI Plotting Agent",
  fluidRow(
    column(
      width = 4,
      card(
        card_header("Upload Data"),
        fileInput("plot_upload", "Upload data (.csv/.tsv/.txt)",
                  accept = c(".csv", ".tsv", ".txt")),
        actionButton(
          "plot_analyze_btn",
          "AI Auto‑Analyze & Recommend Plot",
          class = "btn-primary w-100",
          disabled = TRUE
        ),
        hr(),
        actionButton(
          "plot_update_btn",
          "Re‑generate Plot",
          class = "btn-secondary w-100"
        )
      ),
      br(),
      card(
        card_header("AI Code Tuning Chat"),
        textAreaInput(
          "plot_chat_input",
          "Ask AI how to tune the R code:",
          rows = 4,
          width = "100%"
        ),
        actionButton("plot_chat_send", "Send", class = "btn-success w-100"),
        div(
          id = "plot_chat_window",
          style = "height: 300px; overflow-y: auto; padding: 12px;
                  border: 1px solid #ddd; border-radius: 8px; background-color: #fcfcfc;",
          uiOutput("plot_chat_dialogue")
        )
      )
    ),
    column(
      width = 8,
      card(
        card_header("Data Preview"),
        tableOutput("plot_data_preview")
      ),
      br(),
      card(
        card_header("AI Analysis & Plot Recommendation"),
        verbatimTextOutput("plot_ai_recommendation"),
        textAreaInput(
          "plot_manual_code",
          "Modify R code (optional)",
          rows = 10,
          width = "100%"
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      card(
        card_header("Final Plot Canvas"),
        plotOutput("plot_main", height = "450px")
      )
    )
  )
)

tab2_server <- function(input, output, session) {
  
  call_llm_simple <- function(prompt) {
    res <- httr::POST(
      url = "http://localhost:11434/api/generate",
      httr::content_type_json(),
      body = jsonlite::toJSON(list(
        model = "llama3",
        prompt = prompt,
        stream = FALSE
      ), auto_unbox = TRUE)
    )
    jsonlite::fromJSON(httr::content(res, "text"))$response
  }
  
  plot_df_data       <- reactiveVal(NULL)
  plot_ai_code       <- reactiveVal("")
  plot_ai_suggestion <- reactiveVal("Please upload a file first...")
  plot_chat_messages <- reactiveVal(list())
  
  detect_gwas_columns <- function(df) {
    chr_candidates <- c("CHR","CHROM","X.CHROM","CHROMOSOME","chrom")
    pos_candidates <- c("POS","BP","POSITION")
    p_candidates   <- c("P","PVALUE","P_VALUE","pval")
    
    chr_col <- intersect(chr_candidates, colnames(df))
    pos_col <- intersect(pos_candidates, colnames(df))
    p_col   <- intersect(p_candidates, colnames(df))
    
    list(
      chr = if (length(chr_col) > 0) chr_col[1] else NULL,
      pos = if (length(pos_col) > 0) pos_col[1] else NULL,
      p   = if (length(p_col) > 0) p_col[1] else NULL
    )
  }
  
  observeEvent(input$plot_upload, {
    req(input$plot_upload)
    ext <- tools::file_ext(input$plot_upload$name)
    
    df <- tryCatch({
      if (ext == "csv") read.csv(input$plot_upload$datapath, nrows = 100)
      else read.table(input$plot_upload$datapath, header = TRUE, sep = "\t", nrows = 100)
    }, error = function(e) NULL)
    
    if (!is.null(df)) {
      plot_df_data(df)
      updateActionButton(session, "plot_analyze_btn", disabled = FALSE)
    }
  })
  
  output$plot_data_preview <- renderTable({
    req(plot_df_data())
    head(plot_df_data(), 5)
  })
  
  observeEvent(input$plot_analyze_btn, {
    req(plot_df_data())
    
    df <- plot_df_data()
    plot_ai_suggestion("Analyzing data structure...")
    
    data_summary <- list(
      columns = colnames(df),
      head_sample = head(df, 2)
    )
    
    prompt <- paste0(
      "You are a bioinformatics visualization expert specializing in GWAS and envGWAS plots.\n",
      "You may use ggplot2, qqman.\n\n",
      "Dataset preview:\n",
      toJSON(data_summary, auto_unbox = TRUE), "\n\n",
      "Tasks:\n",
      "1. Identify whether this dataset looks like GWAS summary statistics.\n",
      "2. Recommend suitable plot types.\n",
      "3. Generate R code using the best library.\n",
      "Return ONLY R code inside ```R ... ```."
    )
    
    resp_text <- call_llm_simple(prompt)
    plot_ai_suggestion(resp_text)
    
    code_match <- regmatches(resp_text, regexpr("```R[\\s\\S]*?```", resp_text, perl = TRUE))
    if (length(code_match) > 0) {
      clean_code <- gsub("```R|```", "", code_match[[1]])
      plot_ai_code(clean_code)
      
      updateTextAreaInput(session, "plot_manual_code", value = clean_code)
    }
    
    cols <- detect_gwas_columns(df)
    
    if (!is.null(cols$chr) && !is.null(cols$pos) && !is.null(cols$p)) {
      
      auto_code <- glue::glue("
library(ggplot2)
library(dplyr)
library(gridExtra)

df <- plot_df_data()

# --- Build cumulative BP for Manhattan ---
df_manhattan <- df %>%
  group_by({cols$chr}) %>%
  summarise(chr_len = max({cols$pos}, na.rm = TRUE)) %>%
  mutate(tot = cumsum(chr_len) - chr_len) %>%
  select(-chr_len) %>%
  left_join(df, ., by = '{cols$chr}') %>%
  arrange({cols$chr}, {cols$pos}) %>%
  mutate(BPcum = {cols$pos} + tot)

axis_df <- df_manhattan %>%
  group_by({cols$chr}) %>%
  summarise(center = (min(BPcum) + max(BPcum)) / 2)

# --- Color palette ---
n_chr <- length(unique(df_manhattan${cols$chr}))
manhattan_colors <- rep(c('#4C72B0','#55A868'), length.out = n_chr)

# --- Manhattan Plot ---
p1 <- ggplot(df_manhattan, aes(x = BPcum, y = -log10({cols$p}))) +
  geom_point(aes(color = as.factor({cols$chr})), alpha = 0.75, size = 1.1) +
  scale_color_manual(values = manhattan_colors, name = 'Chromosome') +
  scale_x_continuous(
    breaks = axis_df$center,
    labels = axis_df${cols$chr},
    expand = c(0.02, 0.02)
  ) +
  geom_hline(
    aes(yintercept = -log10(5e-8), color = 'Genome-wide sig (5e-8)'),
    linetype = 'dashed', size = 0.7
  ) +
  geom_hline(
    aes(yintercept = -log10(1e-5), color = 'Suggestive sig (1e-5)'),
    linetype = 'dashed', size = 0.7
  ) +
  scale_color_manual(
    values = c(
      manhattan_colors,
      'Genome-wide sig (5e-8)' = 'red',
      'Suggestive sig (1e-5)' = 'blue'
    ),
    breaks = c('Genome-wide sig (5e-8)', 'Suggestive sig (1e-5)')
  ) +
  labs(
    x = 'Chromosome',
    y = expression(-log[10](italic(P))),
    title = 'Manhattan Plot',
    color = 'Legend'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = 'right',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = 'black', size = 0.8),
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.margin = margin(12, 12, 12, 12)
  )

# --- QQ Plot with confidence band ---
df_qq <- df %>%
  arrange({cols$p}) %>%
  mutate(
    observed = -log10({cols$p}),
    expected = -log10(ppoints(n())),
    clower = -log10(qbeta(0.025, seq_len(n()), rev(seq_len(n())))),
    cupper = -log10(qbeta(0.975, seq_len(n()), rev(seq_len(n()))))
  )

p2 <- ggplot(df_qq, aes(x = expected, y = observed)) +
  geom_ribbon(
    aes(ymin = clower, ymax = cupper),
    fill = '#D0D0D0', alpha = 0.4
  ) +
  geom_point(color = '#4C72B0', alpha = 0.6, size = 1.2) +
  geom_abline(
    aes(color = 'Expected = Observed'),
    intercept = 0, slope = 1,
    linetype = 'dashed', size = 0.8
  ) +
  scale_color_manual(
    values = c('Expected = Observed' = 'red'),
    name = 'Legend'
  ) +
  labs(
    x = 'Expected -log10(P)',
    y = 'Observed -log10(P)',
    title = 'QQ Plot'
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = 'right',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = 'black', size = 0.8),
    plot.margin = margin(12, 12, 12, 12)
  )

# Display both plots together
gridExtra::grid.arrange(p1, p2, ncol = 1)
")
      
      
      
  
  plot_ai_code(auto_code)
  updateTextAreaInput(session, "plot_manual_code", value = auto_code)
    }
  })

output$plot_ai_recommendation <- renderText({
  plot_ai_suggestion()
})

observeEvent(input$plot_update_btn, {
  req(plot_df_data())
  if (!is.null(input$plot_manual_code) && nchar(input$plot_manual_code) > 5) {
    plot_ai_code(input$plot_manual_code)
  }
})

output$plot_main <- renderPlot({
  req(plot_df_data())
  req(plot_ai_code())
  
  df <- plot_df_data()
  code <- plot_ai_code()
  
  tryCatch({
    eval(parse(text = code))
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Plot error:\n", e$message), col = "red")
  })
})

observeEvent(input$plot_chat_send, {
  req(input$plot_chat_input)
  
  msgs <- plot_chat_messages()
  msgs <- append(msgs, list(list(role = "user", content = input$plot_chat_input)))
  plot_chat_messages(msgs)
  
  prompt <- paste0(
    "You are an R visualization expert specializing in GWAS.\n",
    "You may use qqman, ggplot2.\n\n",
    "Current R code:\n", plot_ai_code(), "\n\n",
    "User request:\n", input$plot_chat_input,
    "\n\nProvide improved R code."
  )
  
  resp <- call_llm_simple(prompt)
  
  msgs <- plot_chat_messages()
  msgs <- append(msgs, list(list(role = "assistant", content = resp)))
  plot_chat_messages(msgs)
})
output$plot_chat_dialogue <- renderUI({
  msgs <- plot_chat_messages()
  
  tagList(lapply(msgs, function(m) {
    is_user <- m$role == "user"
    
    div(
      style = paste(
        "display: flex; margin-bottom: 12px;",
        if (is_user) "justify-content: flex-end;" else "justify-content: flex-start;"
      ),
      div(
        style = paste(
          "max-width: 80%; padding: 10px 14px; border-radius: 12px;",
          "box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
          if (is_user)
            "background-color: #007bff; color: white;"
          else
            "background-color: #f1f3f4; color: #333;"
        ),
        strong(if (is_user) "You" else "AI"),
        tags$div(
          style = "margin-top: 5px;",
          HTML(markdownToHTML(text = m$content, fragment.only = TRUE))
        )
      )
    )
  }))
})
}

###############################################
# Combined App
###############################################

ui <- fluidPage(
  theme = bs_theme(version = 5),
  titlePanel("EnvGWAS AI Assistant & Plotting Agent"),
  tabsetPanel(
    tab1_ui,
    tab2_ui
  )
)

server <- function(input, output, session) {
  tab1_server(input, output, session)
  tab2_server(input, output, session)
}

shinyApp(ui = ui, server = server)