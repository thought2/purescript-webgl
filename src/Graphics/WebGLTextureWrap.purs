module Graphics.WebGLTexture.Wrap where

import Prelude

import Effect (Effect)

import Graphics.WebGLTexture.Target as Tgt
import Graphics.WebGLRaw as Raw

data Mode =
  REPEAT
  | MIRRORED_REPEAT
  | CLAMP_TO_EDGE

data Dir = S | T

wrap :: Tgt.Target -> Dir -> Mode -> Effect Unit
wrap target dir mode = Raw.texParameteri_ (Tgt.toGLenum target) (dirToGLenum dir) (modeToGLenum mode)

modeToGLenum :: Mode -> Raw.GLenum
modeToGLenum REPEAT = Raw._REPEAT
modeToGLenum MIRRORED_REPEAT = Raw._MIRRORED_REPEAT
modeToGLenum CLAMP_TO_EDGE = Raw._CLAMP_TO_EDGE

dirToGLenum :: Dir -> Raw.GLenum
dirToGLenum S = Raw._TEXTURE_WRAP_S
dirToGLenum T = Raw._TEXTURE_WRAP_T
