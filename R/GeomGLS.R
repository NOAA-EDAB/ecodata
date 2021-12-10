GeomGLS <- ggplot2::ggproto("GeomGLS",
                            ggplot2::Geom,
                            required_aes = c("x", "y"),

                            default_aes = ggplot2::aes(size = 2, color = NA,fill = NA,
                                                       linetype = 1, alpha = 0.5),

                            draw_key = ggplot2::draw_key_path,

                            draw_group = function(data, panel_params, coord) {

                              coords <- coord$transform(data, panel_params)
                              first_row <- coords[1, , drop = FALSE]

                              #Select default color based on positive/negative trend
                              if (coords$y[1] < coords$y[which.max(coords$x)]){
                                first_row$color <- "orange"
                              } else {
                                first_row$color <- "purple"
                              }

                              grid::linesGrob(
                                coords$x, coords$y,
                                gp = grid::gpar(
                                  col = first_row$color,
                                  alpha = first_row$alpha,
                                  lwd = first_row$size * ggplot2::.pt,
                                  lty = first_row$linetype
                                )
                              )
                            }
)
