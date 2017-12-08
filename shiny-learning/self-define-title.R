shinyApp(
  ui = fixedPage(
    tags$head(
      tags$title('窗口标题'),
      tags$style(
        rel = 'stylesheet',
        '.title-panel {background: #ABCDEF} ',
        '.title-panel h2 {text-align:center; color: #FF0000}'
      )
    ),
    div(
      class='col-md-12 title-panel',
      h2('页面标题')
    )
  ),
  server = function(input, output, session) {}
)
