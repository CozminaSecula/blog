theme_ts <-
  function ()
  {
    font <- "Microsoft Sans Serif"
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = font,
        size = 26,
        face = "bold",
        color = "#222222"),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(
        family = font,
        size = 22,
        margin = ggplot2::margin(9, 0, 9, 0)),
      plot.caption = ggplot2::element_blank(),
      legend.position = "top",
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = font,
        size = 14,
        color = "#222222"),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        family = font,
        size = 14,
        color = "#222222"),
      axis.text.x = ggplot2::element_text
      (margin = ggplot2::margin(5, b = 10)),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
      panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(size = 18, hjust = 0))
    
  }