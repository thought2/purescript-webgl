module Graphics.WebGLTexture.MinFilter where

import Prelude
import Effect (Effect)

import Graphics.WebGLTexture.Target as T
import Graphics.WebGLRaw as Raw

data MinFilter =
  NEAREST
  | LINEAR
  | NEAREST_MIPMAP_NEAREST
  | LINEAR_MIPMAP_NEAREST
  | NEAREST_MIPMAP_LINEAR
  | LINEAR_MIPMAP_LINEAR

minFilter :: T.Target -> MinFilter -> Effect Unit
minFilter target filter = Raw.texParameteri_ (T.toGLenum target) Raw._TEXTURE_MIN_FILTER (toGLenum filter)

toGLenum :: MinFilter -> Raw.GLenum
toGLenum NEAREST = Raw._NEAREST
toGLenum LINEAR = Raw._LINEAR
toGLenum NEAREST_MIPMAP_NEAREST = Raw._NEAREST_MIPMAP_NEAREST
toGLenum LINEAR_MIPMAP_NEAREST = Raw._LINEAR_MIPMAP_NEAREST
toGLenum NEAREST_MIPMAP_LINEAR = Raw._NEAREST_MIPMAP_LINEAR
toGLenum LINEAR_MIPMAP_LINEAR = Raw._LINEAR_MIPMAP_LINEAR

isMipMapped :: MinFilter -> Boolean
isMipMapped NEAREST_MIPMAP_NEAREST = true
isMipMapped LINEAR_MIPMAP_NEAREST = true
isMipMapped NEAREST_MIPMAP_LINEAR = true
isMipMapped LINEAR_MIPMAP_LINEAR = true
isMipMapped _ = false
