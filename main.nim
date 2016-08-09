import parseopt2
import tgaimage
import model as m

# nim c -t="-pg" -l="-pg" main.nim && ./main && gprof main
# though -d:release is easier to parse.
let
  white: TGAColor = constructTGAColor(255, 255, 255, 255)
  red: TGAColor = constructTGAColor(255, 0, 0, 255)
  blue = constructTGAColor(0, 0, 255, 255)
  green = constructTGAColor(0, 255, 0, 255)
  width = 800
  height = 800

proc line(image: var TGAImage; x0,y0,x1,y1:int; color:TGAColor) =
  var steep = false
  var
    x0 = x0
    y0 = y0
    x1 = x1
    y1 = y1 # needed for swap mutation
  if abs(x0-x1) < abs(y0-y1): # width is less than height, i.e. steep, so transpose img
    swap(x0, y0)
    swap(x1, y1)
    steep = true
  if x0 > x1: # swap left-to-right draw order
    swap(x0, x1)
    swap(y0, y1)
  let
    dx = x1-x0
    dy = y1-y0
    derror2 = abs(dy)*2
  var
    x = x0
    y = y0
    error2 = 0
  while x <= x1:
    if not steep:
      image.set(x,y.int,color)
    else:
      image.set(y.int,x,color)
    error2 += derror2
    if error2 > dx:
      y = if y1 > y0: y+1
          else: y-1
      error2 -= dx*2
    x += 1

when isMainModule:
  var model_filename = "obj/african_head.obj"
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      model_filename = key
    else: assert(false, "no other opts supported")

  var image = constructTGAImage(width, height, FORMAT.RGB)
  var model = constructModel(model_filename)

  for i in 0..model.nfaces()-1:
    let face = model.face(i)
    for j in 0..2:
      let
        v0 = model.vert(face[j])
        v1 = model.vert(face[(j+1) mod 3])
        x0 = (v0.x+1)*width.float/2.0
        y0 = (v0.y+1)*height.float/2.0
        x1 = (v1.x+1)*width.float/2.0
        y1 = (v1.y+1)*height.float/2.0
      image.line(x0.int, y0.int, x1.int, y1.int, white)

  discard image.write_tga_file("output.tga", false)
