library(hexSticker)

imgurl <- file.path("inst/figures/wattle2.png")
sticker(imgurl, package="APCalign", 
        p_size = 30,
        p_x = 1, p_y = 0.70,
        s_x=1.5, s_y=1, 
        s_width=0.9, s_height = 0.9,
        p_color = "goldenrod4",
        h_color = "goldenrod1",
        h_fill = "white",
        filename="man/figures/APCalign_hex.png")
