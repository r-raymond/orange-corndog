module OrangeCorndog.Main
    ( main
    ) where

import Protolude

import qualified SDL

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo, SDL.InitEvents]

    v <- SDL.version
    print v

    w <- SDL.createWindow "Yolo" SDL.defaultWindow

    r <- SDL.createRenderer w (-1) SDL.defaultRenderer

    loop r

    SDL.quit

loop :: SDL.Renderer -> IO ()
loop r = do
    t <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessStreaming (SDL.V2 640 480)
    t2 <- SDL.updateTexture t (Just (SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 2 2)))
                            "abcdefghiklmandr" 8
    SDL.clear r
    SDL.copy r t2 Nothing Nothing
    SDL.present r
