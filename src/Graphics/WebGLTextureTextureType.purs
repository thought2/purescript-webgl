module Graphics.WebGLTexture.TextureType where

import Graphics.WebGLRaw as Raw

data TextureType =
  UNSIGNED_BYTE
  | RGBA
  | FLOAT
  | UNSIGNED_SHORT_5_6_5
  | UNSIGNED_SHORT_4_4_4_4
  | UNSIGNED_SHORT_5_5_5_1

toGLenum :: TextureType -> Raw.GLenum
toGLenum UNSIGNED_BYTE = Raw._UNSIGNED_BYTE
toGLenum RGBA = Raw._RGBA
toGLenum FLOAT = Raw._FLOAT
toGLenum UNSIGNED_SHORT_5_6_5 = Raw._UNSIGNED_SHORT_5_6_5
toGLenum UNSIGNED_SHORT_4_4_4_4 = Raw._UNSIGNED_SHORT_4_4_4_4
toGLenum UNSIGNED_SHORT_5_5_5_1 = Raw._UNSIGNED_SHORT_5_5_5_1
