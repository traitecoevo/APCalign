library(hexSticker)

imgurl <- file.path("inst/figures/wattle.png")
sticker(imgurl, package="ausflora", 
        p_size = 26,
        p_x = 1, p_y = .57,
        s_x=1, s_y=1.3, 
        s_width=.5, s_height = .5,
        p_color = "wheat4",
        h_color = "goldenrod1",
        h_fill = "white",
        filename="inst/figures/ausflora_hex.png")


imgurl <- file.path("inst/figures/wattle2.png")
sticker(imgurl, package="ausflora", 
        p_size = 35,
        p_x = 1, p_y = 1,
        s_x=1, s_y=.95, 
        s_width=1.2, s_height = 1.2,
        p_color = "goldenrod4",
        h_color = "goldenrod1",
        h_fill = "white",
        filename="inst/figures/ausflora_hex2.png")
