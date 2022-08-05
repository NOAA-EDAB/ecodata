GeomREGIME <- ggplot2::ggproto("GeomREGIME", ggplot2::Geom,
                               draw_panel = function(data, panel_params, coord, lineend = "butt") {
                                 ranges <- coord$backtransform_range(panel_params)

                                 data$x    <- data$xintercept
                                 data$xend <- data$xintercept
                                 data$y    <- ranges$y[1]
                                 data$yend <- ranges$y[2]

                                 GeomSegment$draw_panel(unique(data), panel_params, coord, lineend = lineend)
                               },

                               default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
                               #required_aes = "xintercept",

                               draw_key = ggplot2::draw_key_vline,

                               rename_size = TRUE
)
