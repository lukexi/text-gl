module GlyphQuad where

import ShaderLoader

import Graphics.GL
import Foreign
import Linear
import Data.Foldable


data GlyphQuad = GlyphQuad
        { glyphQuadVAO            :: VertexArrayObject
        , glyphQuadShader         :: GLProgram
        , glyphQuadIndexCount     :: GLsizei
        , glyphQuadTextureID      :: TextureID
        , glyphQuadUniformMVP     :: UniformLocation
        , glyphQuadUniformTexture :: UniformLocation
        }

----------------------------------------------------------
-- Make GlyphQuad
----------------------------------------------------------

renderGlyphQuad :: GlyphQuad -> M44 GLfloat -> IO ()
renderGlyphQuad glyphQuad mvp = do

    glBindTexture GL_TEXTURE_2D (unTextureID (glyphQuadTextureID glyphQuad))

    glUseProgram (fromIntegral (unGLProgram (glyphQuadShader glyphQuad)))

    let mvpUniformLoc = fromIntegral (unUniformLocation (glyphQuadUniformMVP glyphQuad))
        textureUniformLoc = fromIntegral (unUniformLocation (glyphQuadUniformTexture glyphQuad))
    
    withArray (concatMap toList (transpose mvp)) (\mvpPointer ->
        glUniformMatrix4fv mvpUniformLoc 1 GL_FALSE mvpPointer)

    glUniform1i textureUniformLoc 0

    glBindVertexArray (unVertexArrayObject (glyphQuadVAO glyphQuad))

    glDrawElements GL_TRIANGLES (glyphQuadIndexCount glyphQuad) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0


makeGlyphQuad :: GLProgram -> TextureID -> (GLfloat, GLfloat, GLfloat, GLfloat) -> IO GlyphQuad
makeGlyphQuad program textureID (s0, t0, s1, t1) = do

    aPosition <- getShaderAttribute program "aPosition"
    aColor    <- getShaderAttribute program "aColor"
    aTexCoord <- getShaderAttribute program "aTexCoord"
    uMVP      <- getShaderUniform   program "uMVP"
    uTexture  <- getShaderUniform   program "uTexture"

    -- Setup a VAO
    vaoGlyphQuad <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoGlyphQuad


    ----------------------
    -- GlyphQuad Positions
    ----------------------
    
    -- Buffer the glyphQuad vertices
    let glyphQuadVertices = 
            --- front
            [ -1.0 ,  1.0 ,  0.0  
            , -1.0 , -1.0 ,  0.0  
            ,  1.0 , -1.0 ,  0.0  
            ,  1.0 ,  1.0 ,  0.0 ] :: [GLfloat]

    vaoGlyphQuadVertices <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoGlyphQuadVertices

    let glyphQuadVerticesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length glyphQuadVertices)

    withArray glyphQuadVertices $ 
        \glyphQuadVerticesPtr ->
            glBufferData GL_ARRAY_BUFFER glyphQuadVerticesSize (castPtr glyphQuadVerticesPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aPosition))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aPosition)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    -------------------
    -- GlyphQuad Colors
    -------------------

    -- Buffer the glyphQuad colors
    let glyphQuadColors = 
            -- front colors
            [ 1.0, 0.0, 0.0, 1.0
            , 0.0, 1.0, 0.0, 1.0
            , 0.0, 0.0, 1.0, 1.0
            , 1.0, 1.0, 1.0, 1.0 ] :: [GLfloat]

    vboGlyphQuadColors <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vboGlyphQuadColors


    let glyphQuadColorsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length glyphQuadColors)
    withArray glyphQuadColors $
        \glyphQuadColorsPtr ->
            glBufferData GL_ARRAY_BUFFER glyphQuadColorsSize (castPtr glyphQuadColorsPtr) GL_STATIC_DRAW

    
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aColor))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aColor)) -- attribute
        4                 -- number of elements per vertex, here (R,G,B,A)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    --------------------------------
    -- GlyphQuad Texture Coordinates
    --------------------------------

    -- Buffer the glyphQuad ids
    let glyphQuadTexCoords = 
            [ s0,t0
            , s0,t1
            , s1,t1
            , s1,t0 ] :: [GLfloat]
    -- To visualize the whole atlas:
    -- let glyphQuadTexCoords = 
    --         [ 0,0
    --         , 0,1
    --         , 1,1
    --         , 1,0 ] :: [GLfloat]
    -- print glyphQuadTexCoords
    vboGlyphQuadTexCoords <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vboGlyphQuadTexCoords

    let glyphQuadTexCoordsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length glyphQuadTexCoords)

    withArray glyphQuadTexCoords $
        \glyphQuadTexCoordsPtr ->
            glBufferData GL_ARRAY_BUFFER glyphQuadTexCoordsSize (castPtr glyphQuadTexCoordsPtr) GL_STATIC_DRAW

    
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aTexCoord))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aTexCoord)) -- attribute
        2                 -- number of elements per vertex, here (u,v)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    ----------------
    -- GlyphQuad Indicies
    ----------------

    -- Buffer the glyphQuad indices
    let glyphQuadIndices = 
            -- front
            [ 0, 1, 2
            , 0, 2, 3 ] :: [GLuint]
    
    iboGlyphQuadElements <- overPtr (glGenBuffers 1)
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER iboGlyphQuadElements

    let glyphQuadElementsSize = fromIntegral (sizeOf (undefined :: GLuint) * length glyphQuadIndices)
    
    withArray glyphQuadIndices $ 
        \glyphQuadIndicesPtr ->
            glBufferData GL_ELEMENT_ARRAY_BUFFER glyphQuadElementsSize (castPtr glyphQuadIndicesPtr) GL_STATIC_DRAW
    
    glBindVertexArray 0

    return $ GlyphQuad 
        { glyphQuadVAO              = VertexArrayObject vaoGlyphQuad
        , glyphQuadShader           = program
        , glyphQuadIndexCount       = fromIntegral (length glyphQuadIndices)
        , glyphQuadTextureID        = textureID
        , glyphQuadUniformMVP       = uMVP
        , glyphQuadUniformTexture   = uTexture
        } 


