module Main where


import Control.Monad.Eff.WebGL
import Control.Monad.Eff.WebGLRaw
import qualified Data.Matrix4 as M4
import qualified Data.Vector3 as V3
import Control.Monad.Eff.Alert

import Control.Monad.Eff
import Debug.Trace
import Data.Tuple

fshaderSource :: String
fshaderSource =
"""precision mediump float;

void main(void) {
  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
"""

vshaderSource :: String
vshaderSource =
"""
    attribute vec3 aVertexPosition;

    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;

    void main(void) {
        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    }
"""


main :: Eff (trace :: Trace, alert :: Alert) Unit
main = runWebGL "glcanvas" (\s -> alert s)
  \ context -> do
    trace "WebGL started"
    withShaders fshaderSource
                vshaderSource
                [Tuple "aVertexPosition" 3]
                ["uPMatrix","uMVMatrix"]
                (\s -> alert s)
      \ shaderProgram [aVertexPosition] [uPMatrix,uMVMatrix] -> do
        clearColor 0.0 0.0 0.0 1.0
        enable _DEPTH_TEST

        canvasWidth <- context.getCanvasWidth
        canvasHeight <- context.getCanvasHeight
        viewport 0 0 canvasWidth canvasHeight
        clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)

        let pMatrix = M4.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
        setMatrix shaderProgram uPMatrix pMatrix

        let mvMatrix = M4.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M4.identity
        setMatrix shaderProgram uMVMatrix mvMatrix

        buf1 <- makeBuffer [0.0,  1.0,  0.0,
                           (-1.0), (-1.0),  0.0,
                            1.0, (-1.0),  0.0]
        drawBuffer shaderProgram buf1 aVertexPosition _TRIANGLES 3

        let mvMatrix' = M4.translate (V3.vec3 3.0 0.0 0.0) mvMatrix
        setMatrix shaderProgram uMVMatrix mvMatrix'

        buf2 <- makeBuffer [1.0,  1.0,  0.0,
                           (-1.0), 1.0,  0.0,
                            1.0, (-1.0),  0.0,
                           (-1.0), (-1.0),  0.0]
        drawBuffer shaderProgram buf2 aVertexPosition _TRIANGLE_STRIP 4

        trace "WebGL completed"