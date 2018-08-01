module Graphics.WebGLTexture.Target where

import Graphics.WebGLRaw as Raw

data Target = TEXTURE_2D | TEXTURE_CUBE_MAP

toGLenum :: Target -> Raw.GLenum
toGLenum TEXTURE_2D = Raw._TEXTURE_2D
toGLenum TEXTURE_CUBE_MAP = Raw._TEXTURE_CUBE_MAP
