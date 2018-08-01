module Graphics.WebGLTexture.MagFilter where

import Prelude
import Effect (Effect)

import Graphics.WebGLTexture.Target as T
import Graphics.WebGLRaw as Raw

data MagFilter = NEAREST | LINEAR

magFilter :: T.Target -> MagFilter -> Effect Unit
magFilter target filter = Raw.texParameteri_ (T.toGLenum target) Raw._TEXTURE_MAG_FILTER (toGLenum filter)

toGLenum :: MagFilter -> Raw.GLenum
toGLenum NEAREST = Raw._NEAREST
toGLenum LINEAR = Raw._LINEAR
