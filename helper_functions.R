# Function to both save a ggplot to file but also render it to the screen in the specified
# height and width
save_and_show_plot <- function(plot, width, height, file_name) {
  # Uncomment below to display the plot in a new window with the specified width/height which should match the ggsave output
  # dev.new(width = width, height = height, noRStudioGD = T)
  print(plot)
  ggsave(filename = file.path("plot output", file_name), plot = plot, width = width, height = height)
}

## Using an R function to write raw HTML or LaTeX code 

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}

## you can use the code in an inline R expression
# `r colorize("some words in red", "red")`, 
## which will create some words in red color