
import tgaimage

let
  white: TGAColor = constructTGAColor(255, 255, 255, 255)
  red: TGAColor = constructTGAColor(255, 0, 0, 255)
  blue = constructTGAColor(0, 0, 255, 255)
  green = constructTGAColor(0, 255, 0, 255)

when isMainModule:
  var image: TGAImage = constructTGAImage(100, 100, FORMAT.RGB)
  for y in 3..70:
    discard image.set(52, y, red)
  discard image.flip_vertically()
  discard image.write_tga_file("output.tga", false)
