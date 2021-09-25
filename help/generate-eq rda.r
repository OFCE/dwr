# equations text
e_t  <- readr::read_csv("help/equations.txt")
equation_k  <- purrr:::map(stringr::str_remove_all(e_t$equation, "[$]"),~katex::katex_html(.x, displayMode = TRUE)) 
katex_equations <- do.call(HTML, equation_k)
saveRDS(katex_equations, file = "help/eqs.rda")

# pour transformer en katex le thml knité par rstudio
katex::render_math_in_html("help/equation-source.html", "help/equation-katex.html", include_css = FALSE)
