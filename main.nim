
import tgaimage

# nim c -t="-pg" -l="-pg" main.nim && ./main && gprof main
# though -d:release is easier to parse.
let
  white: TGAColor = constructTGAColor(255, 255, 255, 255)
  red: TGAColor = constructTGAColor(255, 0, 0, 255)
  blue = constructTGAColor(0, 0, 255, 255)
  green = constructTGAColor(0, 255, 0, 255)

proc slowLine(image: var TGAImage; x0,y0,x1,y1:int; color:TGAColor) =
  var t = 0.0
  while t < 1:
    let
      x = x0.float*(1-t) + x1.float*t
      y = y0.float*(1-t) + y1.float*t
    image.set(x.int, y.int, color)
    t += 0.01
proc slowLineDeterministicStep(image: var TGAImage; x0,y0,x1,y1:int; color:TGAColor) =
  var x = x0
  while x <= x1:
    let t = (x-x0)/(x1-x0)
    let y = y0.float*(1-t) + y1.float*t
    image.set(x,y.int,color)
    x += 1
proc slowLineDeterministicStepSteepFix(image: var TGAImage; x0,y0,x1,y1:int; color:TGAColor) =
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
  var x = x0
  while x <= x1:
    let t = (x-x0)/(x1-x0)
    let y = y0.float*(1-t) + y1.float*t
    if not steep:
      image.set(x,y.int,color)
    else:
      image.set(y.int,x,color)
    x += 1
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
  var image: TGAImage = constructTGAImage(100, 100, FORMAT.RGB)
  image.slowLineDeterministicStepSteepFix(13, 100-20, 80, 100-40, green)
  image.slowLineDeterministicStepSteepFix(20, 100-13, 40, 100-80, white)
  image.slowLineDeterministicStepSteepFix(80, 100-40, 13, 100-20, blue) # same line as 1

  # benchmark
  # -d:release: time was 5.72s with profiling, 3.3s without, for slow with steep fix
  # time for line was 3.3s without, 5.445s with. Conclusion: nim kicks ass at optims.
  for _ in 1..1000000:
    image.line(13, 100-20, 80, 100-40, green)
    image.line(20, 100-13, 40, 100-80, white)
    image.line(80, 100-40, 13, 100-20, blue) # same line as 1
  discard image.flip_vertically()
  discard image.write_tga_file("output.tga", false)
