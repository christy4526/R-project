shinyUI(fluidPage(
  titlePanel("2020 인공지능 플랫폼 개발자 양성과정"),
  
  sidebarLayout(
    sidebarPanel(
      h2("2020 Shiny 특강"),
      br(),
      h5("Shiny를 설치해보자."),
      p("install.packages(Shiny)", style = "color:red"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      img(src="kpclogo.jpg", height = 72, width = 154),
      br(),
      br(),
      p("주소: 서울시 종로구 새문안로5가길 32 생산성빌딩", style = "color:blue")
      ),
    mainPanel(
      h1("교육 목적"),
      br(),
      em("인공지능 분야의 산업맞춤형 전문가로 성장할 인재 모집."),
      br(),
      br(),
      p(("kpc 한국생산성본부"), a("홈페이지", href="https://www.kpc.or.kr/index.asp")),
      br(),
      br(),
      br(),
      br(),
      h2("프로그램 소개"),
      p("*산업 맞춤형 교육, 커리큘럼의 70% 프로젝트 수행, 현업멘토링, 맞춤형 취업지원"),
      p(("*5.5개월간 진행되는"), strong("이미지 분석 기반의 인공지능 플랫폼 개발자 양성과정"),("이다.")),
      img(src="program.jpg", height = 300, width = 500),
      align = "center"
      
    )
  )
))
