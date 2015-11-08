{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Graphics.GL.Freetype.GlyphQuad where

import Graphics.GL.Freetype.API

import Graphics.GL.Pal
import Foreign

import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Map (Map, (!))
import System.Random
import Control.Lens
import Data.Foldable

data GlyphQuad = GlyphQuad
    { gqVAO            :: VertexArrayObject
    , gqIndexCount     :: GLsizei
    , gqMetrics        :: GlyphMetrics
    , gqGlyph          :: Glyph
    }

data GlyphUniforms = GlyphUniforms
    { uMVP             :: UniformLocation (M44 GLfloat)
    , uTexture         :: UniformLocation GLint
    , uXOffset         :: UniformLocation GLfloat
    , uYOffset         :: UniformLocation GLfloat
    , uColor           :: UniformLocation (V3 GLfloat)
    } deriving Data

data Font = Font 
    { fgQuads          :: Map Char GlyphQuad
    , fgFont           :: FontPtr
    , fgAtlas          :: TextureAtlas
    , fgTextureID      :: TextureID
    , fgUniforms       :: GlyphUniforms
    , fgShader         :: Program
    , fgPointSize      :: Float
    }

-- Aka ASCII codes 32-126
asciiChars :: String
asciiChars = cursorChar:blockChar:[' '..'~']

blockChar :: Char
blockChar = '█'

cursorChar :: Char
cursorChar = '▏'

makeGlyphs :: String -> Float -> Program -> IO Font
makeGlyphs fontFile pointSize glyphProg = makeGlyphsFromChars fontFile pointSize glyphProg asciiChars

makeGlyphsFromChars :: String -> Float -> Program -> String -> IO Font
makeGlyphsFromChars fontFile pointSize glyphProg characters = do
    -- Create an atlas to hold the characters
    atlas  <- newTextureAtlas 1024 1024 BitDepth1
    -- Create a font and associate it with the atlas
    font   <- newFontFromFile atlas pointSize fontFile
    -- Load the characters into the atlas
    missed <- loadFontGlyphs font characters
    when (missed > 0) $
        putStrLn ("Tried to load too many characters! Missed: " ++ show missed)
    -- Cache the quads that will render each character
    quads  <- glyphQuadsFromText characters font glyphProg

    let textureID = TextureID (atlasTextureID atlas)

    uniforms <- acquireUniforms glyphProg

    return Font 
        { fgQuads     = quads
        , fgFont      = font
        , fgAtlas     = atlas 
        , fgTextureID = textureID
        , fgUniforms  = uniforms
        , fgShader    = glyphProg
        , fgPointSize = pointSize
        }

renderGlyphQuad :: MonadIO m => GlyphQuad -> m ()
renderGlyphQuad glyphQuad = do

    glBindVertexArray (unVertexArrayObject (gqVAO glyphQuad))

    glDrawElements GL_TRIANGLES (gqIndexCount glyphQuad) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0

----------------------------------------------------------
-- Make GlyphQuad
----------------------------------------------------------

glyphQuadsFromText :: String -> FontPtr -> Program -> IO (Map Char GlyphQuad)
glyphQuadsFromText text font glyphQuadProg = 
    foldM (\quads character -> do
        glyph        <- getGlyph font character
        gqMetrics    <- getGlyphMetrics glyph
        glyphQuad    <- makeGlyphQuad glyphQuadProg glyph gqMetrics
        return $ Map.insert character glyphQuad quads
        ) Map.empty text

renderText :: (Foldable f, MonadIO m) 
           => Font -> f Char -> (Int, Int) -> M44 GLfloat -> m ()
renderText Font{..} string (selStart, selEnd) mvp = do

    let GlyphUniforms{..} = fgUniforms

    glBindTexture GL_TEXTURE_2D (unTextureID fgTextureID)

    useProgram fgShader
    
    uniformM44 uMVP     mvp
    uniformI   uTexture 0
    uniformV3  uColor (V3 1 1 1)
    uniformF   uYOffset 0

    let blockQuad  = fgQuads ! blockChar
        cursorQuad = fgQuads ! cursorChar

    let renderChar (charNum, lineNum, lastXOffset, maybeLastChar) thisChar = do
            -- Render newlines as spaces
            let thisChar' = if thisChar == '\n' then ' ' else thisChar
                glyphQuad = fgQuads ! thisChar'

            -- Find the optimal kerning between this character and the last one rendered (if any)
            kerning <- maybe (return 0) (getGlyphKerning (gqGlyph glyphQuad)) maybeLastChar

            let charXOffset = lastXOffset + kerning
                nextXOffset = charXOffset + gmAdvanceX (gqMetrics glyphQuad)

            -- Adjust the character's x offset to nestle against the previous character
            uniformF uXOffset charXOffset
            uniformF uYOffset (-lineNum * fgPointSize)

            -- Render the selection and cursor characters
            if  | charNum == selStart && charNum == selEnd -> do
                    uniformV3  uColor (V3 1 1 1)
                    renderGlyphQuad cursorQuad
                | charNum >= selStart && charNum < selEnd -> do
                    uniformV3  uColor (V3 0.3 0.3 0.4)
                    renderGlyphQuad blockQuad
                | otherwise -> return ()

            -- Randomize the color
            hue <- liftIO randomIO
            uniformV3 uColor ((hslColor hue 0.9 0.9 1) ^. _xyz)
            
            renderGlyphQuad glyphQuad

            return $ if thisChar == '\n'
                then (charNum + 1, lineNum + 1,       0, Nothing)
                else (charNum + 1, lineNum, nextXOffset, Just thisChar)
    _ <- foldlM renderChar (0, 0, 0, Nothing) string
    return ()

makeGlyphQuad :: Program -> Glyph -> GlyphMetrics -> IO GlyphQuad
makeGlyphQuad program glyph metrics@GlyphMetrics{..} = do
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

    ---------------------
    -- GlyphQuad Indicies
    ---------------------

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
        { gqVAO        = VertexArrayObject vaoGlyphQuad
        , gqIndexCount = fromIntegral (length glyphQuadIndices)
        , gqMetrics    = metrics
        , gqGlyph      = glyph
        }


