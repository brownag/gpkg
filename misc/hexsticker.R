library(hexSticker)
soil.color1 <- aqp::munsell2rgb("2.5PB", 3, 4)
soil.color2 <- aqp::munsell2rgb("5BG", 9, 8) # OK.. sorta a soil color...
s <- sticker(
  "~/geopkg.png",
  package = "gpkg",
  p_size = 23,
  p_y = 1.4,
  h_fill = soil.color2,
  p_color = soil.color1,
  h_color = soil.color1,
  s_x = 1,
  s_y = 0.7,
  s_width = 0.6,
  s_height = 0.6,
  filename = "misc/sandbox/hexstickers/gpkg_sticker_v1.png",
  url = "        github.com/brownag/gpkg/", u_size = 3.8, u_color = soil.color1,
)
