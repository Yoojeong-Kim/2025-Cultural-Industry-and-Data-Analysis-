library(shiny)
library(httr)
library(jsonlite)

# API 키 (실 서비스 시 서버 환경변수로 숨길 것)
api_key <- "Put your own API here"

ui <- fluidPage(
  # 로고 + 제목 영역
  tags$div(
    style = "display: flex; align-items: center; padding: 10px;",
    tags$img(src = "daoni.png", height = "100px", style = "margin-right: 15px;"),
    tags$h2("한국 결혼이민자를 위한 AI 비서", style = "margin: 0;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "언어 선택", choices = c("한국어", "중국어", "일본어", "베트남어", "몽골어")),
      selectInput("location", "거주 지역", choices = c("서울 종로구", "서울 성북구", "서울 중구")),
      selectInput("mode", "기능 선택", choices = c("AI 다문화가정 도우미", "AI 한국어 선생님", "AI 리터러시 퀴즈"))
    ),
    
    mainPanel(
      tags$div(
        style = "height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;",
        uiOutput("chat_box")
      ),
      textInput("user_input", "질문을 입력하세요", ""),
      actionButton("send", "보내기")
    )
  )
)

server <- function(input, output, session) {
  # 대화 저장용
  chat_history <- reactiveValues(messages = list())
  values <- reactiveValues(current_quiz = NULL)
  
  # 언어/모드/지역 변경 시 초기 메시지
  observeEvent(list(input$language, input$mode, input$location), {
    req(input$language, input$mode, input$location)
    
    default_messages <- list(
      "한국어" = list(
        "AI 다문화가정 도우미" = "무엇을 도와드릴까요? 알고 싶은 내용을 물어봐주세요!",
        "AI 한국어 선생님" = "저와 함께 한국어로 대화해봐요! 자유롭게 당신의 이야기를 알려주세요.",
        "AI 리터러시 퀴즈" = "AI를 더 잘 쓰기 위한 퀴즈를 시작해볼까요?"
      ),
      "중국어" = list(
        "AI 다문화가정 도우미" = "有什么我可以帮忙的吗？请告诉我您想了解的内容！",
        "AI 한국어 선생님" = "让我们一起练习韩语吧！请随意告诉我您的故事。",
        "AI 리터러시 퀴즈" = "让我们开始一个关于如何更好使用AI的测验吧!"
      ),
      "일본어" = list(
        "AI 다문화가정 도우미" = "何かお手伝いできることはありますか？知りたいことを教えてください！",
        "AI 한국어 선생님" = "一緒に韓国語を練習しましょう！あなたの話を自由に聞かせてください。",
        "AI 리터러시 퀴즈" = "AIをより上手に使うためのクイズを始めましょうか？"
      ),
      "베트남어" = list(
        "AI 다문화가정 도우미" = "Tôi có thể giúp gì cho bạn? Hãy hỏi những điều bạn muốn biết nhé!",
        "AI 한국어 선생님" = "Hãy cùng tôi luyện nói tiếng Hàn nhé! Hãy thoải mái chia sẻ câu chuyện của bạn.",
        "AI 리터러시 퀴즈" = "Chúng ta cùng bắt đầu một câu đố về cách sử dụng AI tốt hơn nhé!)"
      ),
      "몽골어" = list(
        "AI 다문화가정 도우미" = "Танд хэрхэн туслах вэ? Юу мэдэхийг хүсч байгаагаа асуугаарай!",
        "AI 한국어 선생님" = "Бид хамтдаа Солонгос хэлний яриаг дадъя! Чөлөөтэй өөрийнхөө түүхийг хуваалцаарай.",
        "AI 리터러시 퀴즈" = "AI-г илүү сайн ашиглах талаарх сорилыг эхлүүлцгээе!"
      )
    )
    
    system_msg <- paste0(
      "너는 ", input$language, "를 사용하는 ", input$location,
      "에 사는 다문화가정 사용자에게 친절하게 도와주는 AI 비서야. 선택된 기능은 ", input$mode, "야. 이에 맞춰서 답변해줘."
    )
    
    init_msg <- default_messages[[input$language]][[input$mode]]
    
    chat_history$messages <- list(
      list(role = "system", content = system_msg),
      list(role = "assistant", content = init_msg)
    )
  })
  
  # 채팅 출력 UI
  output$chat_box <- renderUI({
    msgs <- chat_history$messages
    if (length(msgs) == 0) return(NULL)
    
    tagList(
      lapply(msgs, function(m) {
        if (m$role == "user") {
          div(style = "text-align: right; background-color: #DCF8C6; padding: 8px; margin: 5px; border-radius: 10px; max-width: 80%; float: right;",
              strong("나:"), br(), m$content)
        } else if (m$role == "assistant") {
          div(style = "text-align: left; background-color: #EEE; padding: 8px; margin: 5px; border-radius: 10px; max-width: 80%; float: left;",
              strong("AI:"), br(), m$content)
        } else {
          NULL
        }
      })
    )
  })
  
  observeEvent(input$send, {
    req(input$user_input)
    
    # 사용자 입력 저장
    chat_history$messages <- append(chat_history$messages, list(
      list(role = "user", content = input$user_input)
    ))
    
    # 리터러시 퀴즈 기능
    if (input$mode == "AI 리터러시 퀴즈") {
      quiz_list <- list(
        list(question = "AI가 제공하는 정보는 항상 믿어도 될까요? (예/아니오)", answer = "아니오"),
        list(question = "인터넷에서 개인 정보를 마음대로 입력해도 될까요? (예/아니오)", answer = "아니오")
      )
      
      if (is.null(values$current_quiz)) {
        values$current_quiz <- sample(quiz_list, 1)[[1]]
        quiz_q <- paste0("🧠 퀴즈! ", values$current_quiz$question)
        
        chat_history$messages <- append(chat_history$messages, list(
          list(role = "assistant", content = quiz_q)
        ))
      } else {
        user_answer <- tolower(input$user_input)
        correct_answer <- tolower(values$current_quiz$answer)
        
        feedback <- if (user_answer == correct_answer) {
          "✅ 정답입니다! 훌륭해요! 🎉"
        } else {
          paste0("❌ 틀렸어요. 정답은 '", values$current_quiz$answer, "'입니다.")
        }
        
        chat_history$messages <- append(chat_history$messages, list(
          list(role = "assistant", content = feedback)
        ))
        
        values$current_quiz <- NULL
      }
      
      updateTextInput(session, "user_input", value = "")
      return()
    }
    
    # 일반 모드 (GPT 호출)
    res <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        messages = chat_history$messages
      )
    )
    
    result <- content(res, "parsed", simplifyVector = FALSE)
    
    if (!is.null(result$choices)) {
      reply <- result$choices[[1]]$message$content
      chat_history$messages <- append(chat_history$messages, list(
        list(role = "assistant", content = reply)
      ))
    } else {
      chat_history$messages <- append(chat_history$messages, list(
        list(role = "assistant", content = "❌ 오류가 발생했어요. API 키 또는 연결 상태를 확인해주세요.")
      ))
    }
    
    updateTextInput(session, "user_input", value = "")
  })
}

shinyApp(ui = ui, server = server)
