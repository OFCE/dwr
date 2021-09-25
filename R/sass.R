
sass::sass(
  input = list(
    sass::sass_file("www/sass/variables.scss"),
    sass::sass_file("www/sass/body.scss"),
    sass::sass_file("www/sass/title_panel.scss"),
    sass::sass_file("www/sass/main_panel.scss"),
    sass::sass_file("www/sass/sidebar_panel.scss")
  ),
  output = "www/css/sass.css",
  options = sass::sass_options(output_style = "compressed")
)
