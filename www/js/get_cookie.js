
// This function will fetch the cookie 'scenarii' and put it in input$scenarii.

function get_cookie() {
  $(document).on('shiny:sessioninitialized', function() {
    var scenarii_cookie = Cookies.get('scenarii');
    Shiny.setInputValue('scenarii', scenarii_cookie);
  })
  $(document).on('shiny:sessioninitialized', function() {
    var lang_cookie = Cookies.get('lang');
    Shiny.setInputValue('lang', lang_cookie);
  })
}

// Run it at startup
get_cookie()

