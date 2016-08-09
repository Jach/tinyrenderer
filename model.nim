import basic3d
import streams
import strutils

type
  Model* = object
    verts: seq[Vector3d]
    faces: seq[seq[int]]
  
proc constructModel*(filename: string): Model =
  var verts: seq[Vector3d] = @[]
  var faces: seq[seq[int]] = @[]
  let s = newFileStream(filename, fmRead)
  while not s.atEnd():
    let ln = s.readLine()
    let parts = ln.split(" ")
    if parts.len < 1:
      continue
    if parts[0] == "v":
      verts.add(vector3d(parts[1].parseFloat, parts[2].parseFloat, parts[3].parseFloat))
    elif parts[0] == "f":
      var f: seq[int] = @[]
      for pi in 1..3:
        let fis = parts[pi].split("/")
        f.add(fis[0].parseInt - 1)
      faces.add(f)

  return Model(verts: verts, faces: faces)


proc nverts*(this: var Model): int =
  return this.verts.len

proc nfaces*(this: var Model): int =
  return this.faces.len

proc vert*(this: var Model; i: int): Vector3d =
  return this.verts[i]

proc face*(this: var Model; i: int): seq[int] =
  return this.faces[i]
