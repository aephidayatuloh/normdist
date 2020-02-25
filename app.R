library(shiny)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  title = "Probability of Normal Distribution",
  fluidRow(
    wellPanel(style="background-color:skyblue;", width = "100%", height = "5%",
      h1("Probability of Normal Distribution", style = "text-align:center;font-weight:bold;color:white;")
    )
  ),
  fluidRow(
    column(3,
           wellPanel(style="background-color:skyblue;border:none;color:white;",
                     h3("Usage", style="font-weight:bold;"),
                     p(style = "color:white;text-align:justify;",
                       HTML('specify the parameter values (mu, sigma) and the lower and upper limits.
                       $$P(L \\lt X \\lt U) = \\int_{L}^{U}\\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}dx$$
                       <br/>
                       $$ = \\int_{-\\infty}^{U}\\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}dx - \\\\ \\int_{-\\infty}^{L}\\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}dx$$
                            <br/>
                            If you want to find the z-value for a significant value \\(\\alpha\\) (alpha) then enter the alpha value on the <strong>"Calculate Z-value Table"</strong> section. 
                            The Z-value will be calculated for \\(\\frac{\\alpha}{2}\\). \\(Z =  1 - P(X \\lt x)\\).'))
                     )
           ),
    column(width = 3,
           h4("Normal Distribution Parameter"),
           div(style="display:inline-block;vertical-align:top;width:49%;font-size:180%;", numericInput("mu", expression("\u03BC"), value = 0)),
           div(style="display:inline-block;vertical-align:top;width:49%;font-size:180%;", numericInput("sigma", expression("\u03C3"), value = 1, min = 0.1, step = 0.1)),
           hr(),
           helpText("Cumulative value between lower and upper limits"),
           # div(style="display:inline-block;vertical-align:top;width:49%;", numericInput("nilai1", "Lower", value = -1.96, step = 0.1)),
           # # div(style="display:inline-block;vertical-align:top;width:20%;", h2(" < X < ")),
           # div(style="display:inline-block;vertical-align:top;width:49%;", numericInput("nilai2", "Upper", value = 1.96, step = 0.1)),
           # p("$$P(L \\lt X \\lt U) = \\int_{L}^{U}\\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}dx$$"),
           conditionalPanel(condition = "input.nilai2 < input.nilai1",
                            p("The Upper Limit value cannot be smaller than the Lower Limit value.", style = "color:red;font-size:90%;")
           ),
           uiOutput("prob"),
           hr(),
           h4("Calculate Z-value Table"),
           div(style="display:inline-block;vertical-align:top;width:29%;font-size:180%;", numericInput("alpha", expression("\u03B1"), value = 0.05, step = 0.001, min = 0.00001, max = 0.99999)),
           div(style="display:inline-block;vertical-align:top;width:69%;font-size:180%;", numericInput("zv", paste0("Z-value (", expression("\u03B1"), "/2)"), value = 1.96)),
           hr()
           
    ),
    column(width = 6,
           # uiOutput("slider"),
           sliderInput("sliderrange", label = h4("Lower and Upper limits"), min = -4.5, max = 4.5, value = c(-1.96, 1.96), step = 0.01, width = "100%"),
           uiOutput("plotTitle"),
           plotOutput("plotPeluang")
           # plotlyOutput("plotPeluang")
    )
  )
                          
)

server <- function(input, output, session){
  # Menu Menghitung Peluang Normal
  output$plotTitle <- renderUI({
    h4(style = "text-align:center;", paste0("Normal Distribution with ", expression("\u03BC = "), input$mu, expression(" dan \u03C3 = "), input$sigma))
  })
  
  observeEvent(input$alpha,{
    req(input$alpha)
    disable("zv")
    updateNumericInput(session, "zv", paste0("Z-value (", expression("\u03B1"), "/2)"), value = round(qnorm(1 - as.numeric(input$alpha)/2), 2))
  })
  
  ip <- reactive({
    req(input$mu)
    req(input$sigma)
    mu = as.numeric(input$mu)
    sigma = as.numeric(input$sigma)
    mu*sigma
  })

  observeEvent(ip(), {
    mu = as.numeric(input$mu)
    sigma = as.numeric(input$sigma)
    
    # a = as.numeric(input$sliderrange[1])
    # b = as.numeric(input$sliderrange[2])
    if(input$mu == "" | input$sigma == ""
       | input$mu == "-" | input$sigma == "-"
       | is.null(input$mu) | is.null(input$sigma))
    {
      return(NULL)
    }
    
    bawah.x = mu - 4.5*sigma
    atas.x = mu + 4.5*sigma
    # output$slider <- renderUI({
      updateSliderInput(session, "sliderrange", label = "Lower and Upper limits", min = bawah.x, max = atas.x, value = c(-1.96, 1.96), step = 0.01)
    # })
  })

 
  probs <- reactive({
    mu = as.numeric(input$mu)
    sigma = as.numeric(input$sigma)
    
    a = as.numeric(input$sliderrange[1])
    b = as.numeric(input$sliderrange[2])
    if(input$mu == "" | input$sigma == ""
       | input$mu == "-" | input$sigma == "-"
       | is.null(input$mu) | is.null(input$sigma))
    {
      return(NULL)
    }
    
    bawah.x = mu - 4.5*sigma
    atas.x = mu + 4.5*sigma
    # posisi = b + 0.7
    
    peluang1 = pnorm(a, mu, sigma)
    if(b <= mu) {
      peluang2 = pnorm(b, mu, sigma) - peluang1
    } else {
      peluang2 = pnorm(mu, mu, sigma) - peluang1
    }
    if(a >= mu) {
      peluang3 = pnorm(b, mu, sigma) - pnorm(a, mu, sigma)
    } else {
      peluang3 = pnorm(b, mu, sigma) - pnorm(mu, mu, sigma)
    }
    peluang4 = 1 - pnorm(b, mu, sigma)
    
    if(b <= bawah.x) {
      bawah.x <- b
      # posisi <- b * 1.1 
      # peluang = 1 - peluang
    }
    if(a >= atas.x) {
      atas.x <- a
      # posisi <- a * 1.1
      # peluang = 1 - peluang
    }
    
    
    df <- data.frame(poly.x = c(a, seq(a, b, 0.01), b),
                     poly.y = c(0, dnorm(seq(a, b, 0.01), mu, sigma), 0)
    )
    df_norm <- data.frame(curve.x = c(bawah.x, seq(bawah.x, atas.x, 0.01), atas.x),
                          curve.y = c(0, dnorm(seq(bawah.x, atas.x, 0.01), mu, sigma), 0)
    )
    
    
    if(peluang2 >= 0 & peluang3 >= 0) {
      probtot1 <- peluang2
      probtot2 <- peluang3
    } else if(peluang2 < 0 & peluang3 >= 0) {
      probtot1 <- 0
      probtot2 <- peluang3
    } else if(peluang2 >= 0 & peluang3 < 0) {
      probtot1 <- peluang2
      probtot2 <- 0
    } else {
      probtot1 <- 0
      probtot2 <- 0
    }
    
    list(df_norm = df_norm, 
         df = df, 
         bawah.x = bawah.x, 
         atas.x = atas.x, 
         peluang1 = peluang1,
         peluang2 = peluang2,
         peluang3 = peluang3,
         peluang4 = peluang4,
         probtot1 = probtot1, 
         probtot2 = probtot2)
    
  })
  
  # ip <- reactive({
  #   mu = as.numeric(input$mu)
  #   sigma = as.numeric(input$sigma)
  #   mu*sigma
  # })
  # 
  # observeEvent(ip(), {
  #   updateSliderInput(session, "sliderrange", label = h3("Lower and Upper limits"), min = probs()$bawah.x, max = probs()$atas.x, value = c(-1.96, 1.96), step = 0.01)
  # })
  
  # observeEvent(input$sliderrange, {
  #   updateNumericInput(session, "nilai1", label = "Lower", value = input$sliderrange[1])
  #   updateNumericInput(session, "nilai2", label = "Upper", value = input$sliderrange[2])
  #   hideElement("nilai1")
  #   hideElement("nilai2")
  # })
  
  output$prob <- renderUI({
    probs <- probs()
    p(sprintf("P(%s < X < %s) = %s%%", input$sliderrange[1], input$sliderrange[2], round((probs$probtot1 + probs$probtot2)*100, 1)), style="text-align:center;font-weight:bold;color:black;font-size:150%;")
  })
  
  
  output$plotPeluang <- renderPlot({
    mu = as.numeric(input$mu)
    sigma = as.numeric(input$sigma)
    
    a = as.numeric(input$sliderrange[1])
    b = as.numeric(input$sliderrange[2])
    if(input$mu == "" | input$sigma == ""
       | input$mu == "-" | input$sigma == "-"
       | is.null(input$mu) | is.null(input$sigma))
    {
      return(NULL)
    }
    
    bawah.x = mu - 4.5*sigma
    atas.x = mu + 4.5*sigma
    # posisi = b + 0.7
    # 
    # peluang1 = pnorm(a, mu, sigma)
    # if(b <= mu) {
    #   peluang2 = pnorm(b, mu, sigma) - peluang1
    # } else {
    #   peluang2 = pnorm(mu, mu, sigma) - peluang1
    # }
    # if(a >= mu) {
    #   peluang3 = pnorm(b, mu, sigma) - pnorm(a, mu, sigma)
    # } else {
    #   peluang3 = pnorm(b, mu, sigma) - pnorm(mu, mu, sigma)
    # }
    # peluang4 = 1 - pnorm(b, mu, sigma)
    # 
    # if(b <= bawah.x) {
    #   bawah.x <- b
    #   # posisi <- b * 1.1 
    #   # peluang = 1 - peluang
    # }
    # if(a >= atas.x) {
    #   atas.x <- a
    #   # posisi <- a * 1.1
    #   # peluang = 1 - peluang
    # }
    # 
    probs <- probs()
    
    # df <- data.frame(poly.x = c(a, seq(a, b, 0.01), b),
    #                  poly.y = c(0, dnorm(seq(a, b, 0.01), mu, sigma), 0)
    # )
    # df_norm <- data.frame(curve.x = c(bawah.x, seq(bawah.x, atas.x, 0.01), atas.x),
    #                       curve.y = c(0, dnorm(seq(bawah.x, atas.x, 0.01), mu, sigma), 0)
    # )
    
    p9 <- ggplot(data = probs$df_norm, aes(x = curve.x, y = curve.y)) +
      geom_area(fill = "lightblue", colour = "skyblue", size = 1.2) +
      geom_polygon(data = probs$df, aes(x = poly.x, y = poly.y), fill = "#71A9CA") +
      geom_vline(xintercept = a, linetype = "dotted", colour = "#F24343") +
      geom_vline(xintercept = b, linetype = "dotted", colour = "#F24343") +
      geom_vline(xintercept = mu, linetype = "dotted", colour = "black") 
    # geom_segment(aes(x = a, xend = a, y = 0, yend = df_norm$curve.y[round(df_norm$curve.x, 2) == round(a, 2)]), linetype = "dotted", colour = "#F24343") +
    # geom_segment(aes(x = b, xend = b, y = 0, yend = df_norm$curve.y[round(df_norm$curve.x, 2) == round(b, 2)]), linetype = "dotted", colour = "#F24343") +
    # anotasi lower.tail
    if(probs$peluang1*100 >= 1) {
      p9 <- p9 + annotate("text",
                          label = paste0(round(probs$peluang1*100, 1), "%"),
                          x = a - 0.4, #posisi,
                          y = max(probs$df_norm$curve.y) * 1.2,
                          size = 5, colour = "lightblue")
    }
    x2 <- ifelse(b < mu, b - 0.4, mu - 0.4)
    # if(probs$peluang2*100 > 0) {
    #   p9 <- p9 + annotate("text",
    #                       label = paste0(round(probs$peluang2*100, 1), "%"),
    #                       x = x2, #posisi,
    #                       y = max(probs$df_norm$curve.y) * 1.1,
    #                       size = 5, colour = "#71A9CA") 
    # }
    # anotasi upper.tail
    x3 <- ifelse(a > mu, a + 0.4, mu + 0.4)
    # if(probs$peluang3*100 > 0) {
    #   p9 <- p9 + annotate("text",
    #                       label = paste0(round(probs$peluang3*100, 1), "%"),
    #                       x = x3, #posisi,
    #                       y = max(probs$df_norm$curve.y) * 1.1,
    #                       size = 5, colour = "#71A9CA")
    # }
    if(probs$peluang4*100 >= 1) {
      p9 <- p9 + annotate("text",
                          label = paste0(round(probs$peluang4*100, 1), "%"),
                          x = b + 0.4, #posisi,
                          y = max(probs$df_norm$curve.y) * 1.2,
                          size = 5, colour = "lightblue")
    }
    
    # annotate("text",
    #          label = expression("\u03BC"),
    #          x = mu,
    #          y = 0.1,
    #          size = 3, colour = "black") +
    # if(probs$peluang2 >= 0 & probs$peluang3 >= 0) {
    #   probtot <- peluang2 + peluang3
    # } else if(peluang2 < 0 & peluang3 >= 0) {
    #   probtot <- peluang3
    # } else if(peluang2 >= 0 & peluang3 < 0) {
    #   probtot <- peluang2
    # } else {
    #   probtot <- 0
    # }
    
    p9 <- p9 +
      xlim(probs$bawah.x, probs$atas.x) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(probs$df_norm$curve.y) * 1.3)) +
      labs(title = paste0("P(", a, " < X < ", b, ") = ", round(probs$probtot1*100, 1), "% + ", round(probs$probtot2*100, 1), "%\n"),
           x = "X") +
      # stat_function(fun = funcShaded, args = list(mu, sigma, a, b), geom = "area", fill = "#84CA72", alpha = 0.2) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#71A9CA", size = 14, face = "bold"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank()
      )
    p9
    # ggplotly(p9, tooltip = "text") %>%
    # plot_ly(xnorm, x = ~x, y = ~y,
    #         type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
    # layout(#title = paste0("Sebaran Normal dengan $mu$ = ", mu, ", $sigma$ = ", sigma),
    # paper_bgcolor = "rgba(0, 0, 0, 0)",
    # plot_bgcolor = "rgba(0, 0, 0, 0)")
    
  }, height = 350)
}

shinyApp(ui, server)
