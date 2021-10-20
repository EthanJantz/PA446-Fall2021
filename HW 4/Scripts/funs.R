quickTable <- function(data, title = NULL, DT = FALSE, format = "html") { # Turns data into kable table
  if(!DT) {
    data %>%
      kableExtra::kbl(caption = title, format = format) %>%
      kableExtra::kable_styling()
  } else {
    data %>%
      DT::datatable()
  }
  
}

`%nin%` <- Negate(`%in%`)
