import streams

proc ptr_offset[A](some: ptr A, b: int): ptr A =
  result = cast[ptr A](cast[int](some) + (b * sizeof(A)))

type
  TGAColor* = object
    color*: (char, char, char, char)

proc constructTGAColor*(R: char; G: char; B: char; A: char): TGAColor =
  ## ... interestingly, the memory layout is B G R A...
  return TGAColor(color: (B, G, R, A))
proc constructTGAColor*(R: int; G: int; B: int; A: int): TGAColor =
  return TGAColor(color: (B.char, G.char, R.char, A.char))
proc raw*(this: TGAColor, idx: int): char =
  result = case idx:
    of 0: this.color[0]
    of 1: this.color[1]
    of 2: this.color[2]
    of 3: this.color[3]
    else: 0.char
proc constructTGAColorFromRaw*(data: ptr char, bytespp: uint): TGAColor =
  var color:array[4,char] = [0.char,0.char,0.char,0.char]
  for i in 0..bytespp.int-1-1:
    color[i] = data.ptr_offset(i)[]
  return TGAColor(color: (color[0], color[1], color[2], color[3]))
proc `$`(color: TGAColor): string =
  return "R: " & $color.color[0].ord & ", G: " & $color.color[1].ord & ", B: " & $color.color[2].ord & ", A: " & $color.color[3].ord

type
  TGA_Header* {.packed.} = object
    idlength*: char ## 0-255, bytes arbitrary image id consists of
    colormaptype*: char ## 0 if no color map, 1 if present, 2-127 reserved, 128-255 ext
    datatypecode*: char ## 0 no image data, 1 uncompressed color-mapped,
        ## 2 uncompressed true-color, 3 uncompressed black-white,
        ## 9 run-length encoded color-mapped, 10 run-length encoded true-color,
        ## 11 run-length encoded black and white
    # color map spec: origin, length, depth
    colormaporigin*: uint16 ## index of first color map entry that is included in file
    colormaplength*: uint16 ## num entries of the color map included in file
    colormapdepth*: char ## number of bits per pixel
    # image spec: dimensions, format
    x_origin*: uint16 ## coord of lower-left corner for displays with origin at low-left
    y_origin*: uint16
    width*: uint16
    height*: uint16
    bitsperpixel*: char ## pixel depth
    imagedescriptor*: char ## bits 3-0 give alpha channel depth, bits 5-4 give direction

proc `$`(header:TGA_Header): string =
  return "Header: id: " & $header.idlength.ord & ", maptype: " & $header.colormaptype.ord & ", datatype: " & $header.datatypecode.ord & ", maporigin: " & $header.colormaporigin & ", maplen: " & $header.colormaplength & ", mapdep: " & $header.colormapdepth.ord & ", x:" & $header.x_origin & ", y:" & $header.y_origin & ", w:" & $header.width & ",h:" & $header.height & ",bpp:" & $header.bitsperpixel.ord & ",idesc:" & $header.imagedescriptor.ord

type
  TGAImage* = object
    data*: ptr char
    width*: uint16
    height*: uint16
    bytespp*: uint
    length: uint


type
  Format* = enum
    GRAYSCALE = 1, RGB = 3, RGBA = 4

proc constructTGAImage*(width: int, height: int, format: Format): TGAImage =
  result = TGAImage()
  let length = width * height * format.int
  result.data = cast[ptr char](alloc0(length))
  result.length = length.uint
  result.width = width.uint16
  result.height = height.uint16
  result.bytespp = format.uint

proc get*(this: var TGAImage; x: int; y: int): TGAColor =
  if this.data == nil or x < 0 or y < 0 or x >= this.width.int or y >= this.height.int:
    return TGAColor()
  return constructTGAColorFromRaw(this.data.ptr_offset(x+y*this.width.int), this.bytespp)

proc set*(this: var TGAImage; x: int; y: int; c: TGAColor): bool =
  if this.data == nil or x < 0 or y < 0 or x >= this.width.int or y >= this.height.int:
    return false
  for b in 0..this.bytespp.int-1:
    this.data.ptr_offset((x+y*this.width.int)*this.bytespp.int+b)[] = c.raw(b)
  return true


proc load_rle_data*(this: var TGAImage, s: Stream): bool =
  var
    pixelcount = int(this.width * this.height)
    currentpixel = 0
    currentbyte = 0
    colorbuffer = TGAColor()
  while currentpixel < pixelcount:
    var chunkheader = s.readChar().int
    if s.atEnd():
      echo "an error occured while reading the data"
      return false
    if chunkheader < 128:
      chunkheader += 1
      for _ in 0..chunkheader-1:
        let read = s.readData(addr colorbuffer, this.bytespp.int)
        if read != this.bytespp.int or s.atEnd():
          echo "an error occured while reading the data"
          return false
        for t in 0..this.bytespp-1:
          this.data.ptr_offset(currentbyte)[] = colorbuffer.raw(t.int)
          currentbyte += 1
        currentpixel += 1
        if currentpixel > pixelcount:
          echo "Too many pixels read"
          return false
    else:
      chunkheader -= 127
      let read = s.readData(addr colorbuffer, this.bytespp.int)
      if read != this.bytespp.int or s.atEnd():
        echo "an error occurred while reading the data"
        return false
      for _ in 0..chunkheader-1:
        for t in 0..this.bytespp-1:
          this.data.ptr_offset(currentbyte)[] = colorbuffer.raw(t.int)
          currentbyte += 1
        currentpixel += 1
        if currentpixel > pixelcount:
          echo "Too many pixels read"
          return false
  return true

proc unload_rle_data*(this: var TGAImage; `out`: Stream): bool =
  return false

proc flip_horizontally*(this: var TGAImage): bool =
  if this.data == nil:
    return false
  let half = this.width shl 1
  for i in 0..half.int-1:
    for j in 0..this.height.int-1:
      let c1 = this.get(i, j)
      let c2 = this.get(this.width.int-1-i, j)
      discard this.set(i, j, c2)
      discard this.set(this.width.int-1-i, j, c1)
  return true

proc flip_vertically*(this: var TGAImage): bool =
  if this.data == nil:
    return false
  return true

proc read_tga_file*(this: var TGAImage, filename: string): bool =
  if (this.data != nil):
    dealloc(this.data)
  this.data = nil
  var s = newFileStream(filename, fmRead)
  discard s.readInt64() # file size
  var header: TGA_Header = TGA_Header()
  let read = s.readData(addr header, header.sizeof)
  if read != header.sizeof:
    s.close()
    echo "an error occured while reading the header"
    return false

  let
    width = header.width
    height = header.height
    bytespp = header.bitsperpixel.uint shr 3
  if width <= 0 or height <= 0 or (bytespp != GRAYSCALE.uint and bytespp != RGB.uint and bytespp != RGBA.uint):
    s.close()
    echo "bad bpp (or width/height) value"
    return false

  let nbytes = bytespp * width * height
  this.data = cast[ptr char](alloc(nbytes))
  this.length = nbytes
  if 3.char == header.datatypecode or 2.char == header.datatypecode:
    let read = s.readData(this.data, nbytes.int)
    if read != nbytes.int:
      s.close()
      echo "an error occurred while reading the data"
      return false
  elif 10.char == header.datatypecode or 11.char == header.datatypecode:
    if not this.load_rle_data(s):
      s.close()
      echo "an error occurred while reading the data"
      return false
  else:
    s.close()
    echo "unknown file format ", header.datatypecode
    return false

  if (header.imagedescriptor.int and 0x20) == 0:
    discard this.flip_vertically()
  if (header.imagedescriptor.int and 0x10) != 0:
    discard this.flip_horizontally()
  s.close()
  return true

import strutils
proc write_tga_file*(this: var TGAImage; filename: string; rle: bool = true): bool =
  var 
    developer_area_ref = (0.char ,0.char ,0.char ,0.char)
    extension_area_ref = (0.char, 0.char, 0.char, 0.char)
    footer = ('T', 'R', 'U', 'E', 'V', 'I', 'S', 'I', 'O', 'N', '-', 'X', 'F', 'I', 'L', 'E', '.', '\0')
    stream = newFileStream(filename, fmWrite)
  var header = TGA_Header()
  header.bitsperpixel = char(this.bytespp shl 3)
  header.width = this.width
  header.height = this.height
  header.datatypecode = char(if this.bytespp == GRAYSCALE.uint: (if rle: 11 else: 3)
                             else: (if rle: 10 else: 2))
  header.imagedescriptor = 0x20.char # top-left origin
  stream.writeData(stream, addr header, sizeof(header))
  if not rle:
    stream.writeData(stream, this.data, this.length.int)
  else:
    if not this.unload_rle_data(stream):
      echo "Cannot unload rle data"
      stream.close()
      return false
  stream.writeData(stream, addr developer_area_ref, sizeof(developer_area_ref))
  stream.writeData(stream, addr extension_area_ref, sizeof(extension_area_ref))
  stream.writeData(addr footer, sizeof(footer))
  stream.close()
  return true

proc scale*(this: var TGAImage; w: cint; h: cint): bool =
  return false

proc destroyTGAImage*(this: var TGAImage) =
  if this.data != nil:
    dealloc(this.data)
    this.data = nil

proc get_width*(this: var TGAImage): uint16 =
  return this.width

proc get_height*(this: var TGAImage): uint16 =
  return this.height

proc get_bytespp*(this: var TGAImage): uint =
  return this.bytespp

proc buffer*(this: var TGAImage): ptr char =
  return this.data

proc clear*(this: var TGAImage) =
  for b in 0..this.length.int-1:
    this.data.ptr_offset(b)[] = 0.char

when isMainModule:
  discard
