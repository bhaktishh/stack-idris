module Main

import Control.Monad.State
import IdrisGL
import IdrisGL.Color as Color 
import IdrisGL.Random as Random 

windowWidth : Double
windowWidth = 1000

windowWidthInt : Int 
windowWidthInt = cast windowWidth

windowHeight : Double
windowHeight = 800

windowHeightInt : Int 
windowHeightInt = cast windowHeight

blockWidth : Int 
blockWidth = cast (0.3 * windowWidth) 

blockHeight : Int 
blockHeight = cast (0.1 * windowHeight)

window : Display
window = InWindow "basic game" (MkRect 30 50 windowWidthInt windowHeightInt)

bgColor : Color 
bgColor = Color.black

-- highest frame rate possible 
tpf : Double 
tpf = 0

data GameState : Type where 
    MkGameState : (score : Nat) -> 
                  (speed : Int) -> 
                  (streak : Nat) -> 
                  (pos : (Int, Int)) ->
                  (dir : Bool) -> -- left to right = true, right to left = false
                  (past : List ((Int, Int), Int)) -> -- ((x, y), width)
                  (size : Int) ->  -- width of current block
                  GameState

drawRects : List ((Int, Int), Int) -> Bool -> Picture
drawRects ls init = Pictures $ (map (\((x, y), size) => Rectangle (MkRect x y size (if init then (cast (0.6 * windowHeight)) else blockHeight)) Color.white True) ls)

initRect : Picture
initRect = Rectangle (MkRect (cast (0.35 * windowWidth)) 
                    (cast (0.4 * windowHeight) + blockHeight) 
                    blockWidth 
                    (cast (0.6 * windowHeight))) Color.blue True

initGame : GameState 
initGame = MkGameState 0 2 0 (0, cast (0.4 * windowHeight)) True 
            [(((cast (0.35 * windowWidth)), (cast (0.4 * windowHeight) + blockHeight)), blockWidth)] blockWidth

drawOneBlockAt : Int -> Int -> Color -> Picture 
drawOneBlockAt x y c = Rectangle (MkRect x y blockWidth blockHeight) c True

-- from a game state, generate a picture
showGame : StateT GameState IO Picture
showGame = do 
    (MkGameState score speed streak (x,y) dir past size) <- get
    let cur = (drawOneBlockAt x y Color.white)
    pure (Pictures [drawRects past (score == 0), cur])

-- event handler - mouse and keyboard events 
e2w : Eve -> StateT GameState IO ()
e2w (E_KEYDOWN EK_SPACE) = do
    (MkGameState score speed streak (x,y) dir past size) <- get
    ?idk 

e2w e = pure ()

updatePos : Int -> Bool -> Int -> Int -> (Int, Bool)
updatePos x True speed size = (x + speed, x + speed + size < windowWidthInt)
updatePos x False speed size = (x - speed, x - speed < 0)

-- time handler: argument is the amount of time since game start
t2w : Double -> StateT GameState IO () 
t2w t = do
    (MkGameState score speed streak (x,y) dir past size) <- get
    let (x, dir) = updatePos x dir speed size
    put $ (MkGameState score speed streak (x, y) dir past size)

-- playStateT
--    :  (window    : Display)
--    -> (bgColor   : Color)
--    -> (tpf       : Double)
--    -> (stateType : a)
--    -> (w2p       : StateT a IO Picture)
--    -> (e2w       : Eve    -> StateT a IO ())
--    -> (t2w       : Double -> StateT a IO ())
--    -> IO ()

-- record Rect where
--     constructor MkRect
--     ||| @ x X coordinate of the start point.
--     x : Int
--     ||| @ y Y coordinate of the start point.
--     y : Int
--     ||| @ w The width of the rectangle area.
--     w : Int
--     ||| @ h The height of the rectangle area.
--     h : Int

main : IO () 
main =
    -- play window bgColor tpf world w2p ew2w twtw
    playStateT window bgColor tpf initGame showGame e2w t2w
