-----------------------------------------------------------------------------
--
-- Module      :  Graphics.WebGLTexture
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Textures for the WebGL binding for purescript
--
-----------------------------------------------------------------------------

module Graphics.WebGLTexture
(
  WebGLTex(..)
  , texture2DFor
  , withTexture2D
  , activeTexture
  , bindTexture
  , unbindTexture
  , texImage2D
  , texImage2DPixels
  , handleLoad2D
  , handleSubLoad2D
  , createTexture
  , newTexture
  , newTextureInit
)where

import Prelude
import Effect (Effect)
import Data.Int.Bits ((.&.),(.|.))
import Graphics.Canvas(CanvasImageSource())

import Graphics.WebGL (Uniform(Uniform))

import Graphics.WebGLTexture.InternalFormat as IF
import Graphics.WebGLTexture.MinFilter as Min
import Graphics.WebGLTexture.MagFilter as Mag
import Graphics.WebGLTexture.TargetType as TgtT
import Graphics.WebGLTexture.Target as Tgt
import Graphics.WebGLTexture.TextureType as TxT
import Graphics.WebGLTexture.Wrap as W
import Graphics.WebGLTexture.Pack as P

import Graphics.WebGLRaw (texImage2D_, GLenum, GLint, GLsizei, WebGLUniformLocation, WebGLTexture, uniform1i_, createTexture_,
    _TEXTURE0, activeTexture_, _MAX_COMBINED_TEXTURE_IMAGE_UNITS, bindTexture_, pixelStorei_, _TEXTURE_2D, generateMipmap_,
     ArrayBufferView)

import Partial.Unsafe (unsafeCrashWith)
import Data.TypedArray (newUint8Array)
import Data.ArrayBuffer.Types (ArrayView)

newtype WebGLTex = WebGLTex WebGLTexture

texture2DFor :: forall a. String -> Min.MinFilter -> Mag.MagFilter -> (WebGLTex -> Effect a) -> Effect Unit
texture2DFor name minFilter magFilter continuation = do
  texture <- createTexture
  loadImage_ name \image -> do
    handleLoad2D texture minFilter magFilter image
    continuation texture

handleLoad2D :: forall a. WebGLTex -> Min.MinFilter -> Mag.MagFilter -> a -> Effect Unit
handleLoad2D texture minFilter magFilter whatever = do
  bindTexture TgtT.TEXTURE_2D texture
  Mag.magFilter Tgt.TEXTURE_2D magFilter
  Min.minFilter Tgt.TEXTURE_2D minFilter
  pixelStorei P.UNPACK_FLIP_Y_WEBGL 0
  pixelStorei P.UNPACK_PREMULTIPLY_ALPHA_WEBGL 0
  texImage2D TgtT.TEXTURE_2D 0 IF.RGBA IF.RGBA TxT.UNSIGNED_BYTE whatever
  case Min.isMipMapped minFilter of
    true -> generateMipmap_ _TEXTURE_2D
    _ -> pure unit
  unbindTexture TgtT.TEXTURE_2D

handleSubLoad2D :: forall a. WebGLTex -> Int -> Int -> Int -> Int -> Min.MinFilter -> Mag.MagFilter -> a -> Effect Unit
handleSubLoad2D texture x y w h minFilter magFilter  whatever = do
  bindTexture TgtT.TEXTURE_2D texture
  Mag.magFilter Tgt.TEXTURE_2D magFilter
  Min.minFilter Tgt.TEXTURE_2D minFilter
  pixelStorei P.UNPACK_FLIP_Y_WEBGL 0
  pixelStorei P.UNPACK_PREMULTIPLY_ALPHA_WEBGL 0
  texSubImage2D TgtT.TEXTURE_2D 0 x y IF.RGBA TxT.UNSIGNED_BYTE whatever
  case Min.isMipMapped minFilter of
    true -> generateMipmap_ _TEXTURE_2D
    _ -> pure unit
  unbindTexture TgtT.TEXTURE_2D

newTexture :: Int -> Int -> Min.MinFilter -> Mag.MagFilter -> Effect WebGLTex
newTexture width height minFilter magFilter  = do
  texture <- createTexture
  bindTexture TgtT.TEXTURE_2D texture
  Mag.magFilter Tgt.TEXTURE_2D magFilter
  Min.minFilter Tgt.TEXTURE_2D minFilter
  when (((width .|. height) .&. 1) == 1) $ do
    W.wrap Tgt.TEXTURE_2D W.S W.CLAMP_TO_EDGE
    W.wrap Tgt.TEXTURE_2D W.T W.CLAMP_TO_EDGE
  texImage2DNull TgtT.TEXTURE_2D 0 IF.RGBA width height IF.RGBA TxT.UNSIGNED_BYTE
  case Min.isMipMapped minFilter of
    true -> generateMipmap_ _TEXTURE_2D
    _ -> pure unit
  unbindTexture TgtT.TEXTURE_2D
  pure texture

newTextureInit :: Int -> Int -> Min.MinFilter -> Mag.MagFilter -> Effect WebGLTex
newTextureInit width height minFilter magFilter = do
  texture <- createTexture
  let pixels = newUint8Array (width * height * 4)
  bindTexture TgtT.TEXTURE_2D texture
  Mag.magFilter Tgt.TEXTURE_2D magFilter
  Min.minFilter Tgt.TEXTURE_2D minFilter
  when (((width .|. height) .&. 1) == 1) $ do
    W.wrap Tgt.TEXTURE_2D W.S W.CLAMP_TO_EDGE
    W.wrap Tgt.TEXTURE_2D W.T W.CLAMP_TO_EDGE
  texImage2DPixels TgtT.TEXTURE_2D 0 IF.RGBA width height IF.RGBA TxT.UNSIGNED_BYTE (asArrayBufferView_ pixels)
  case Min.isMipMapped minFilter of
    true -> generateMipmap_ _TEXTURE_2D
    _ -> pure unit
  unbindTexture TgtT.TEXTURE_2D
  pure texture

pixelStorei :: P.Pack -> Int -> Effect Unit
pixelStorei pack num = pixelStorei_ (P.toGLenum pack) num

withTexture2D :: forall typ. WebGLTex -> Int -> Uniform typ -> Int -> Effect Unit -> Effect Unit
withTexture2D texture index (Uniform sampler) pos continuation = do
  activeTexture index
  bindTexture TgtT.TEXTURE_2D texture
  uniform1i sampler.uLocation pos
  continuation
  unbindTexture TgtT.TEXTURE_2D

bindTexture :: TgtT.TargetType -> WebGLTex -> Effect Unit
bindTexture tt (WebGLTex texture) = bindTexture_ (TgtT.toGLenum tt) texture

unbindTexture :: TgtT.TargetType -> Effect Unit
unbindTexture tt = bindTexture__ (TgtT.toGLenum tt)

texImage2D :: forall a. TgtT.TargetType -> GLint -> IF.InternalFormat -> IF.InternalFormat -> TxT.TextureType -> a -> Effect Unit
texImage2D target level internalFormat format typ pixels =
  texImage2D__ (TgtT.toGLenum target) level (IF.toGLenum internalFormat) (IF.toGLenum format) (TxT.toGLenum typ) pixels

texImage2DNull :: TgtT.TargetType -> GLint -> IF.InternalFormat -> GLsizei -> GLsizei -> IF.InternalFormat -> TxT.TextureType -> Effect Unit
texImage2DNull target level internalFormat width height format typ =
  texImage2DNull_ (TgtT.toGLenum target) level (IF.toGLenum internalFormat)
    width height 0 (IF.toGLenum format) (TxT.toGLenum typ)

texImage2DPixels :: TgtT.TargetType -> GLint -> IF.InternalFormat -> GLsizei -> GLsizei -> IF.InternalFormat -> TxT.TextureType -> ArrayBufferView -> Effect Unit
texImage2DPixels target level internalFormat width height format typ pixels =
  texImage2D_ (TgtT.toGLenum target) level (IF.toGLenum internalFormat)
    width height 0 (IF.toGLenum format) (TxT.toGLenum typ) pixels

texSubImage2D :: forall a. TgtT.TargetType -> GLint -> GLint -> GLint -> IF.InternalFormat -> TxT.TextureType -> a -> Effect Unit
texSubImage2D target level x y format typ pixels =
  texSubImage2D__ (TgtT.toGLenum target) level x y (IF.toGLenum format) (TxT.toGLenum typ) pixels

activeTexture :: Int -> Effect Unit
activeTexture n | n < _MAX_COMBINED_TEXTURE_IMAGE_UNITS = activeTexture_ (_TEXTURE0 + n)
                | otherwise                             = unsafeCrashWith "WebGLTexture>>activeTexture: wrong argument!"

createTexture :: Effect WebGLTex
createTexture = do
          texture <- createTexture_
          pure (WebGLTex texture)

uniform1i :: WebGLUniformLocation -> GLint -> Effect Unit
uniform1i = uniform1i_

foreign import  asArrayBufferView_ :: forall a . ArrayView a -> ArrayBufferView

foreign import loadImage_ :: forall a. String
                     -> (CanvasImageSource -> Effect a)
                     -> Effect Unit

foreign import texImage2D__ :: forall a. GLenum
                   -> GLint
                   -> GLenum
                   -> GLenum
                   -> GLenum
                   -> a
                   -> Effect Unit

foreign import texSubImage2D__:: forall a. GLenum
                                                -> GLint
                                                -> GLint
                                                -> GLint
                                                -> GLenum
                                                -> GLenum
                                                -> a
                                                -> Effect Unit

foreign import texImage2DNull_ :: GLenum
                   -> GLint
                   -> GLenum
                   -> GLsizei
                   -> GLsizei
                   -> GLint
                   -> GLenum
                   -> GLenum
                   -> Effect Unit

foreign import bindTexture__ :: GLenum -> Effect Unit
