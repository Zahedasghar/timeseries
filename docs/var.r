quarto::quarto_render(input = "VAR.qmd")

pagedown::chrome_print("VAR.html", 
                       options = 
                         list(
                           printBackground = FALSE,
                           preferCSSPageSize = FALSE, 
                           paperWidth = 8.3, paperHeight = 11.7, 
                           marginTop = 0.1, marginBottom = 0.1, 
                           marginLeft = 0.1, marginRight = 0.1))
