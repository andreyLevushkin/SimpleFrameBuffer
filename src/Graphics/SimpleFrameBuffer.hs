module Graphics.SimpleFrameBuffer (createWindow, drawBuffer, Buffer(pixelValue)) where

import qualified Graphics.UI.GLFW as GLFW 
import           Graphics.Rendering.OpenGL hiding(drawBuffer)

import Foreign.Marshal.Array
import Data.Word


class Buffer a where
	-- | Return the RGB color of the pixel
	pixelValue :: a -> (Int,Int) -> (Word8, Word8, Word8)

instance Buffer [[(Word8, Word8, Word8)]] where
	pixelValue buffer (x,y) = (!! x) . (!! y) $ buffer

-- | Create a new window to draw on.
createWindow :: (Int, Int) -> IO (Maybe GLFW.Window)
createWindow (width, height) = do
	GLFW.init
	GLFW.createWindow width height "" Nothing Nothing

-- | Draws the buffer object on screen resizing the window to 
--   the correct resolution
drawBuffer :: Buffer buffer =>  GLFW.Window -> (Int, Int) -> buffer -> IO ()
drawBuffer window (width, height) buffer = do
	GLFW.makeContextCurrent $ Just 	window
	cBuf <- newArray $ pixelList  (width, height) buffer
	let pixelData = PixelData RGBA UnsignedByte cBuf	
	drawPixels (Size (fromIntegral width) (fromIntegral height)) pixelData
	GLFW.swapBuffers window

pixelList :: Buffer buffer => (Int,Int) -> buffer -> [Color4 Word8]
pixelList (width, height) buffer 
	= [(Color4 r g b 255) | x <- [0..width-1], y <- [0..height-1], 
			let c@(r,g,b) = pixelValue buffer (x, y)]
