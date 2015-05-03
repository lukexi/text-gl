{-# LANGUAGE RecordWildCards #-}
module Graphics.GL.Freetype.GlyphQuad where

import Graphics.GL.Pal.Shader
import Graphics.GL.Pal.Types
import Graphics.GL.Freetype.API

import Graphics.GL
import Foreign
import Linear
import Data.Foldable

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map, (!))

data GlyphQuad = GlyphQuad
    { glyphQuadVAO            :: VertexArrayObject
    , glyphQuadShader         :: GLProgram
    , glyphQuadIndexCount     :: GLsizei
    , glyphQuadTextureID      :: TextureID
    , glyphQuadUniformMVP     :: UniformLocation
    , glyphQuadUniformTexture :: UniformLocation
    , glyphQuadUniformXOffset :: UniformLocation
    , glyphMetrics            :: GlyphMetrics
    }

type GlyphQuads = Map Char GlyphQuad

data FontGlyphs = FontGlyphs 
    { fgQuads :: GlyphQuads
    , fgFont  :: Font
    , fgAtlas :: TextureAtlas
    }

-- Aka ASCII codes 32-126
asciiChars :: String
asciiChars = [' '..'~']

makeGlyphs :: String -> Float -> GLProgram -> String -> IO FontGlyphs
makeGlyphs fontFile pointSize glyphProg characters = do
    -- Create an atlas to hold the characters
    atlas <- newTextureAtlas 512 512 BitDepth1
    -- Create a font and associate it with the atlas
    font  <- newFontFromFile atlas pointSize fontFile
    -- Load the characters into the atlas
    missed <- loadFontGlyphs font characters
    putStrLn $ "Missed: " ++ show missed
    -- Cache the quads that will render each character
    quads <- glypyQuadsFromText characters font atlas glyphProg

    return FontGlyphs { fgQuads = quads, fgFont = font, fgAtlas = atlas }

renderGlyphQuad :: GlyphQuad -> Float -> M44 GLfloat -> IO ()
renderGlyphQuad glyphQuad xOffset mvp = do

    glBindTexture GL_TEXTURE_2D (unTextureID (glyphQuadTextureID glyphQuad))

    glUseProgram (fromIntegral (unGLProgram (glyphQuadShader glyphQuad)))

    let mvpUniformLoc = fromIntegral (unUniformLocation (glyphQuadUniformMVP glyphQuad))
        textureUniformLoc = fromIntegral (unUniformLocation (glyphQuadUniformTexture glyphQuad))
        xOffsetUniformLoc = fromIntegral (unUniformLocation (glyphQuadUniformXOffset glyphQuad))
    
    withArray (concatMap toList (transpose mvp)) (\mvpPointer ->
        glUniformMatrix4fv mvpUniformLoc 1 GL_FALSE mvpPointer)

    glUniform1i textureUniformLoc 0
    glUniform1f xOffsetUniformLoc xOffset

    glBindVertexArray (unVertexArrayObject (glyphQuadVAO glyphQuad))

    glDrawElements GL_TRIANGLES (glyphQuadIndexCount glyphQuad) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0

----------------------------------------------------------
-- Make GlyphQuad
----------------------------------------------------------

glypyQuadsFromText :: String -> Font -> TextureAtlas -> GLProgram -> IO GlyphQuads
glypyQuadsFromText text font atlas glyphQuadProg = do
    let textureID = TextureID (atlasTextureID atlas)
    foldM (\quads character -> do
        glyph        <- getGlyph font character
        glyphMetrics <- getGlyphMetrics glyph
        glyphQuad    <- makeGlyphQuad glyphQuadProg textureID glyphMetrics
        return $ Map.insert character glyphQuad quads
        ) Map.empty text

renderText :: FontGlyphs -> String -> M44 GLfloat -> IO Float
renderText FontGlyphs{..} text mvp = do
    (xOffset, _) <- foldM (\(xOffset, maybeLastChar) thisChar -> do
        glyph <- getGlyph fgFont thisChar
        kerning <- case maybeLastChar of
            Nothing       -> return 0
            Just lastChar -> getGlyphKerning glyph lastChar

        let glyphQuad   = fgQuads ! thisChar
            charXOffset = xOffset + kerning
            nextXOffset = charXOffset + gmAdvanceX (glyphMetrics glyphQuad)

        renderGlyphQuad glyphQuad xOffset mvp

        return (nextXOffset, Just thisChar)
        ) (0, Nothing) text
    return xOffset

makeGlyphQuad :: GLProgram -> TextureID -> GlyphMetrics -> IO GlyphQuad
makeGlyphQuad program textureID metrics@GlyphMetrics{..} = do
    let x0  = gmOffsetX
        y0  = gmOffsetY
        x1  = x0 + gmWidth
        y1  = y0 - gmHeight

    aVertex   <- getShaderAttribute program "aVertex"
    aColor    <- getShaderAttribute program "aColor"
    aTexCoord <- getShaderAttribute program "aTexCoord"
    uMVP      <- getShaderUniform   program "uMVP"
    uTexture  <- getShaderUniform   program "uTexture"
    uXOffset  <- getShaderUniform   program "uXOffset"

    -- Setup a VAO
    vaoGlyphQuad <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoGlyphQuad


    ----------------------
    -- GlyphQuad Positions
    ----------------------
    
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
        , glyphQuadShader           = program
        , glyphQuadIndexCount       = fromIntegral (length glyphQuadIndices)
        , glyphQuadTextureID        = textureID
        , glyphQuadUniformMVP       = uMVP
        , glyphQuadUniformTexture   = uTexture
        , glyphQuadUniformXOffset   = uXOffset
        , glyphMetrics              = metrics
        }


