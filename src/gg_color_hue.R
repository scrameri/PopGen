gg_color_hue <- function (n, hue_min = 10, hue_max = 280, l = 62, c = 100) 
{
    hues = seq(hue_min, hue_max, length = n + 1)
    hcl(h = hues, l = l, c = c)[1:n]
}
