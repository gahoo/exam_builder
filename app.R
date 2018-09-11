library(shiny)
library(shinyAce)
library(plyr)
library(dplyr)

csv_files <- dir('questions', pattern='*.csv$')
names(csv_files) <- gsub('.csv$', '', csv_files)

hide_answer <- function(x, hide=FALSE){
  is_empty = nrow(x) == 0
  if(hide && !is_empty){
    x %>% mutate(answer = '')
  }else if(is_empty){
    x
  }else{
    x %>% mutate(answer = paste0('正确答案：', answer))
  }
}

filter_timu <- function(x, timu_type){
  if(timu_type == 'choice'){
    x %>% filter(answer %in% c('A', 'B', 'C', 'D', 'E', 'F'))
  }else if(timu_type == 'ture_false'){
    x %>% filter(answer %in% c('错误', '正确'))
  }else if(timu_type == 'essay'){
    x %>% filter(options == '' | is.na(options)) 
  }
}

sample_timu_n <- function(x, size){
  if(size > nrow(x)){
    size <- nrow(x)
  }
  x %>% sample_n(size)
}

add_timu_id <- function(x){
  if(nrow(x) == 0){
    x
  }else{
    x %>% mutate(question = paste0(id,'. ', question))
  }
}

question2txt <- function(x){
  x %>% mutate(options=format_options(options)) -> x
  x %>% with(paste(question, options, answer, sep='\n\n', collapse='\n\n'))
}

format_options <- function(x){
  x <- gsub(' +', ' ', x)
  x <- gsub('\\. ', '.', x)
  long_idx <- nchar(x) > 35
  x[long_idx] <- strsplit(x[long_idx], ' ') %>% sapply(paste0, collapse='\n\n')
  x
}

answer2txt <- function(x, join='  '){
  if(length(x$answer)){
    paste0(1:length(x$answer), '. ',
           gsub('正确答案：','', x$answer),
           collapse = join)
  }else{
    ""
  }

}

get_exam_template <- function(choice_number, true_false_number, essay_num){
  template <- "---\ntitle: '%s'\n---\n"
  numbering <- c('一. ', '二. ', '三. ', '四. ')
  timu_types <- c('选择题', '判断题', '问答题')
  idx <- as.logical(c(choice_number, true_false_number, essay_num))
  timu_types <- timu_types[idx]
  timu_titles <- paste0("# ", numbering[1:sum(idx)], timu_types, '\n\n%s', collapse = '\n\n')
  paste0(template, timu_titles)
}

get_timu <- function(tiku, timu_number, timu_type){
  if(timu_number == 0){
    return(head(tiku[[1]], 0))
  }
  each_cnt <- ceiling(timu_number / length(tiku))
  if(each_cnt == 0){
    each_cnt <- 1
  }
  ldply(tiku, function(x){
    x %>%
      mutate(answer = gsub('\n$', '', answer)) %>%
      filter_timu(timu_type) %>%
      sample_timu_n(size=each_cnt)
  }) %>%
    add_rownames(var = 'id') %>%
    add_timu_id() %>%
    head(timu_number)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("通用试题生成器"),
  sidebarLayout(
    sidebarPanel(
      #   selectInput('tiku', '题库', choices = csv_files, multiple = T,selected = csv_files),
      textInput('exam_name', '试卷名', value = 'XX试题'),
      selectInput('library', '题库',
                  choices = csv_files,
                  multiple = T,
                  selected = csv_files),
      checkboxInput('with_answer', '附答案'),
      numericInput('choice_num', '选择题数', min = 0, value = 40),
      numericInput('true_false_num', '判断题数', min = 0, value = 10),
      numericInput('essay_num', '问答题题数', min = 0, value = 2)
    ),
    mainPanel(
      aceEditor('exam_content', '', mode='markdown'),
      actionButton('generate_exam', '生成试题'),
      downloadButton('downloadExam', '下载试题'),
      downloadButton('downloadAnswer', '下载答案')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  tiku <- reactive({
    paste0('questions/', input$library) %>%
      lapply(function(x){
        timu <- read.csv(x, stringsAsFactors = F)
        timu[is.na(timu)] <- ''
        timu
        })
  })

  exam <- reactiveValues(content='', answer='')
  
  observeEvent(input$generate_exam, {
    message('generate exams')

    choice_timu <- get_timu(tiku(), input$choice_num, 'choice')
    ture_false_timu <- get_timu(tiku(), input$true_false_num, 'ture_false')
    essay_timu <- get_timu(tiku(), input$essay_num, 'essay')

    template <- get_exam_template(
      nrow(choice_timu),
      nrow(ture_false_timu),
      nrow(essay_timu))    
    template_args <- list(
      template,
      input$exam_name,
      choice_timu %>% hide_answer(!input$with_answer) %>% question2txt,
      ture_false_timu %>% hide_answer(!input$with_answer) %>% question2txt,
      essay_timu %>% hide_answer(!input$with_answer) %>% question2txt
    )

    template_args <- template_args[template_args!='']
    exam$content <- do.call(sprintf, template_args)
    updateAceEditor(session, "exam_content", exam$content, mode="markdown")
    
    template_args <- list(
      template,
      input$exam_name,
      choice_timu %>% answer2txt,
      ture_false_timu %>% answer2txt,
      essay_timu %>% answer2txt(join='\n\n')
    )
    
    template_args <- template_args[template_args!='']
    exam$answer <- do.call(sprintf, template_args)
  })

  output$downloadExam <- downloadHandler(
    filename = function() {
      paste0(input$exam_name, '.docx')
    },
    content = function(con) {
      rmd_file <- gsub('.docx', '.Rmd', con)
      message(rmd_file)
      write(exam$content, rmd_file)
      rmarkdown::render(
        rmd_file,
        output_format = rmarkdown::word_document(
          reference_docx = file.path(getwd(), "style.docx"))
        )
    }
  )
  
  output$downloadAnswer <- downloadHandler(
    filename = function() {
      paste0(input$exam_name, '-答案', '.docx')
    },
    content = function(con) {
      rmd_file <- gsub('.docx', '.Rmd', con)
      message(rmd_file)
      write(exam$answer, rmd_file)
      rmarkdown::render(
        rmd_file,
        output_format = rmarkdown::word_document(
          reference_docx = file.path(getwd(), "style.docx"))
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

