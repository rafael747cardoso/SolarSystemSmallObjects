
# Check if there is indeed a pattern in the distribution of w that could be explained by the presence of an outer planet

library(plotly)
library(RColorBrewer)

dt <- data.table::fread("sbdb_query_results.csv")

# Trans-neptunians
dt <- dt[!is.na(a)]
dt <- dt[a > 30]

# Uncertanty levels
dt <- dt[!is.na(sigma_w)]
dt <- dt[sigma_w/w < 0.1]
dt[, sigma_w_w := sigma_w/w]
dt[, sigma_w_level := cut(sigma_w/w, breaks = seq(from = 0, to = 0.01, length.out = 5),
                          labels = FALSE)]



# Plot
x_var <- "w"
y_var <- "e"
color_var <- "sigma_w_level"
my_palette <- c("#c70039", "#2a7b9b", "#eddd53", "#4caf50", "#ff9800")

plot_ly(
  data = dt,
  x = ~eval(parse(text = x_var)),
  y = ~eval(parse(text = y_var)),
  color = ~eval(parse(text = color_var)),
  colors = my_palette,
  text = ~color_var,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 10
  ),
  hovertemplate = paste0("<b>", x_var, ": %{x}<br>",
                         y_var, ": %{y}<br>",
                         color_var, ": %{text}</b><extra></extra>")
) %>%
  layout(
    xaxis = list(
      title = paste0("<b>", x_var, "</b>"),
      titlefont = list(
        size = 20
      ),
      tickfont = list(
        size = 18
      ),
      categoryorder = "array"
    ),
    yaxis = list(
      title = paste0("<b>", y_var, "</b>"),
      titlefont = list(
        size = 20
      ),
      tickfont = list(
        size = 18
      )
    ),
    margin = list(
      l = 10,
      r = 10,
      t = 10,
      b = 10
    ),
    hoverlabel = list(
      font = list(
        size = 18
      )
    ),
    showlegend = TRUE,
    legend = list(
      title = list(
        text = paste0("<br><b>", color_var, "</b>"),
        font = list(
          size = 18
        )
      )
    )
  )



