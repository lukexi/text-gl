{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Freetype.GlyphQuad where

import Graphics.GL.Pal.Shader
import Graphics.GL.Pal.Types
import Graphics.GL.Freetype.API

import Graphics.GL
import Foreign
import Linear

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map, (!))

data GlyphQuad = GlyphQuad
    { glyphQuadVAO            :: VertexArrayObject
    , glyphQuadIndexCount     :: GLsizei
    , glyphMetrics            :: GlyphMetrics
    }

type GlyphQuads = Map Char GlyphQuad

data FontGlyphs = FontGlyphs 
    { fgQuads                 :: GlyphQuads
    , fgFont                  :: Font
    , fgAtlas                 :: TextureAtlas
    , fgTextureID             :: TextureID
    , fgUniformModel          :: UniformLocation
    , fgUniformViewProjection :: UniformLocation
    , fgUniformTexture        :: UniformLocation
    , fgUniformXOffset        :: UniformLocation
    , fgShader                :: GLProgram
    }

-- Aka ASCII codes 32-126
asciiChars :: String
asciiChars = [' '..'~']

makeGlyphs :: String -> Float -> GLProgram -> String -> IO FontGlyphs
makeGlyphs fontFile pointSize glyphProg characters = do
    -- Create an atlas to hold the characters
    atlas <- newTextureAtlas 1024 1024 BitDepth1
    -- Create a font and associate it with the atlas
    font  <- newFontFromFile atlas pointSize fontFile
    -- Load the characters into the atlas
    missed <- loadFontGlyphs font characters
    putStrLn $ "Missed: " ++ show missed
    -- Cache the quads that will render each character
    quads <- glypyQuadsFromText characters font glyphProg

    let textureID = TextureID (atlasTextureID atlas)

    uModel          <- getShaderUniform glyphProg "uModel"
    uViewProjection <- getShaderUniform glyphProg "uViewProjection"
    uTexture        <- getShaderUniform glyphProg "uTexture"
    uXOffset        <- getShaderUniform glyphProg "uXOffset"

    return FontGlyphs 
        { fgQuads = quads
        , fgFont = font
        , fgAtlas = atlas 
        , fgTextureID = textureID
        , fgUniformTexture = uTexture
        , fgUniformModel   = uModel
        , fgUniformViewProjection = uViewProjection
        , fgUniformXOffset = uXOffset
        , fgShader = glyphProg
        }

renderGlyphQuad :: GlyphQuad -> IO ()
renderGlyphQuad glyphQuad = do

    glBindVertexArray (unVertexArrayObject (glyphQuadVAO glyphQuad))

    glDrawElements GL_TRIANGLES (glyphQuadIndexCount glyphQuad) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0

----------------------------------------------------------
-- Make GlyphQuad
----------------------------------------------------------

glypyQuadsFromText :: String -> Font -> GLProgram -> IO GlyphQuads
glypyQuadsFromText text font glyphQuadProg = 
    foldM (\quads character -> do
        glyph        <- getGlyph font character
        glyphMetrics <- getGlyphMetrics glyph
        glyphQuad    <- makeGlyphQuad glyphQuadProg glyphMetrics
        return $ Map.insert character glyphQuad quads
        ) Map.empty text

renderText :: FontGlyphs -> String -> M44 GLfloat -> IO Float
renderText FontGlyphs{..} text model = do

    glBindTexture GL_TEXTURE_2D (unTextureID fgTextureID)

    glUseProgram (fromIntegral (unGLProgram fgShader))

    let textureUniformLoc = fromIntegral (unUniformLocation fgUniformTexture)
        xOffsetUniformLoc = fromIntegral (unUniformLocation fgUniformXOffset)
    
    uniformM44 fgUniformModel model

    glUniform1i textureUniformLoc 0

    (xOffset, _) <- foldM (\(lastXOffset, maybeLastChar) thisChar -> do
        glyph <- getGlyph fgFont thisChar
        kerning <- case maybeLastChar of
            Nothing       -> return 0
            Just lastChar -> getGlyphKerning glyph lastChar

        let glyphQuad   = fgQuads ! thisChar
            charXOffset = lastXOffset + kerning
            nextXOffset = charXOffset + gmAdvanceX (glyphMetrics glyphQuad)

        glUniform1f xOffsetUniformLoc charXOffset
        renderGlyphQuad glyphQuad

        return (nextXOffset, Just thisChar)
        ) (0, Nothing) text
    return xOffset

makeGlyphQuad :: GLProgram -> GlyphMetrics -> IO GlyphQuad
makeGlyphQuad program metrics@GlyphMetrics{..} = do
    let x0  = gmOffsetX
        y0  = gmOffsetY
        x1  = x0 + gmWidth
        y1  = y0 - gmHeight

    
    -- Setup a VAO
    vaoGlyphQuad <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoGlyphQuad


    ----------------------
    -- GlyphQuad Positions
    ----------------------
    aVertex   <- getShaderAttribute program "aVertex"
    -- Buffer the glyphQuad vertices
    let glyphQuadVertices = 
            --- front
            [ x0 , y0 , 0.0  
            , x0 , y1 , 0.0  
            , x1 , y1 , 0.0  
            , x1 , y0 , 0.0 ] :: [GLfloat]

    vaoGlyphQuadVertices <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoGlyphQuadVertices

    let glyphQuadVerticesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length glyphQuadVertices)

    withArray glyphQuadVertices $ 
        \glyphQuadVerticesPtr ->
            glBufferData GL_ARRAY_BUFFER glyphQuadVerticesSize (castPtr glyphQuadVerticesPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aVertex))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aVertex)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    ----------------------
    -- GlyphQuad Normals
    ----------------------
    aNormal   <- getShaderAttribute program "aNormal"
    -- Buffer the glyphQuad normals
    let glyphQuadNormals = 
            --- front
            [ 0.0, 0.0, 1.0  
            , 0.0, 0.0, 1.0  
            , 0.0, 0.0, 1.0  
            , 0.0, 0.0, 1.0 ] :: [GLfloat]

    vaoGlyphQuadNormals <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoGlyphQuadNormals

    let glyphQuadNormalsSize = fromIntegral (sizeOf (undefined :: GLfloat) * length glyphQuadNormals)

    withArray glyphQuadNormals $ 
        \glyphQuadNormalsPtr ->
            glBufferData GL_ARRAY_BUFFER glyphQuadNormalsSize (castPtr glyphQuadNormalsPtr) GL_STATIC_DRAW 

    -- Describe our normals array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aNormal))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aNormal)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element

    --------------------------------
    -- GlyphQuad Texture Coordinates
    --------------------------------
    aTexCoord <- getShaderAttribute program "aTexCoord"
    -- Buffer the glyphQuad ids
    let glyphQuadTexCoords = 
            [ gmS0, gmT0
            , gmS0, gmT1
            , gmS1, gmT1
            , gmS1, gmT0 ] :: [GLfloat]
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

    
    return GlyphQuad 
        { glyphQuadVAO              = VertexArrayObject vaoGlyphQuad
        , glyphQuadIndexCount       = fromIntegral (length glyphQuadIndices)
        , glyphMetrics              = metrics
        }


