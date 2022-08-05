## GeomLM based on GeomGLS - add dashed linetype
GeomLM <- ggplot2::ggproto("GeomLM",
                            ggplot2::Geom,
                            requird_aes = c("x", "y"),
                            extra_params = c("n", "na.rm", "pValThreshold", "nBootSamples"),


                            default_aes = ggplot2::aes(size = 2, color = NA,fill = NA,
                                                       linetype = 2, alpha = 0.7),

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
