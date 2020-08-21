library(shiny)
shinyUI(fluidPage(
  titlePanel("My Shiny App"),
  
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      p("p creats a paragraph of text."),
      p("A new p() command starts a new paragraph.",
        style="font-family:'times'; font-size:16pt"),
      strong("strong() makes bold text"),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style.",
          style="color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style="color:blue"),
        "that appear inside a paragraph.")
    )
    
  )
))
