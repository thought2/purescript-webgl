module Graphics.WebGLTexture.InternalFormat where

import Graphics.WebGLRaw as Raw

data InternalFormat =
  ALPHA
  | LUMINANCE
  | LUMINANCE_ALPHA
  | RGB
  | RGBA

toGLenum :: InternalFormat -> Raw.GLenum
toGLenum ALPHA = Raw._ALPHA
toGLenum LUMINANCE = Raw._LUMINANCE
toGLenum LUMINANCE_ALPHA = Raw._LUMINANCE_ALPHA
toGLenum RGB = Raw._RGB
toGLenum RGBA = Raw._RGBA
