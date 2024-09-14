module Graphics.Renderer where
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Foreign (withArray, nullPtr)
import Foreign.Safe (Storable(..))



triangleVertexPos:: [Float]
triangleVertexPos = [
         -0.5, 0, 0
        , 0, 0.5, 0
        , 0.5, 0, 0
    ]

sizeOfFloat = sizeOf (0::Float)

foo:: GL.Program -> IO()
foo program = do
    vao <- GL.genObjectName
    [vertexArrayBuffer, colorBuffer] <- GL.genObjectNames 2

    GL.currentProgram $= Just program
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vertexArrayBuffer

    let vertexAttribLoc = GL.AttribLocation 0
    GL.vertexAttribArray vertexAttribLoc $= GL.Enabled 
    withArray triangleVertexPos $ \pVertexPosition -> do
        GL.bufferData GL.ArrayBuffer $= (toEnum $ (length triangleVertexPos * sizeOfFloat), pVertexPosition, GL.StaticDraw)
        GL.vertexAttribPointer vertexAttribLoc  $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)


    GL.bindBuffer GL.ArrayBuffer $= Just colorBuffer 

    GL.drawArrays GL.Triangles 0 3

    GL.currentProgram $= Nothing
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing

