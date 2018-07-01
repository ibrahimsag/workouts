{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Protolude

import Data.Text (pack, unpack)
import qualified Data.Text as T

import Foreign.C.Types (CInt(..))

import qualified Codec.Picture as P

import qualified Data.Vector.Storable as V

import SDL (($=), V2(..), V4(..), Point(..))
import qualified SDL as SDL

import qualified SDL.Font as Font

import qualified SDL.Mixer as Mixer

data Fonts = Fonts
  { futura24, futura72
  , helvetica24, helvetica72 :: Font.Font
  }

pngs :: [Text]
pngs =
  [ "abcrunches_1-3d3599cb3b894be1012aa9c2dcbe8043d0d51bcb.png"
  , "abcrunches_2-992b8937a211b908463e9ed60ecc2641819e9196.png"
  , "chair_1-94f472535b2697994151d989b1c1802421867f35.png"
  , "chair_2-6e9370450c5661106c68123a80c353661496255c.png"
  , "jumpingjacks_1-f56280043639febaedd116b1577b218ecd4d1774.png"
  , "jumpingjacks_2-d6be541eccb6460f6c648b52f9b118fa79173219.png"
  , "lunges_1-f2b0553a71692c159ae6b36d36145958e2f59adc.png"
  , "lunges_2-f5edeafabe1be1bd2ca297f6306dcf26b2bdc998.png"
  , "plank_1-64b2b51d322cec79e199cddd077fe539c52660ac.png"
  , "pushupsn_1-41f77a24090cfb032f39c505105cd23832ce811a.png"
  , "pushupsn_2-3db0915be0f6782212ba1311297520b60bb47825.png"
  , "pushupsrotation_1-cde373cf8279fed28c2c5b4cf1965ec6d185599b.png"
  , "pushupsrotation_2-59b41215200f18e5431a0fe7296728016ba1b792.png"
  , "run_1-0e45f6a1198b65b9c1f8fd99d9c474da31382353.png"
  , "run_2-61a3076da054d42802af9b47cde800e466567226.png"
  , "sideplank_1-65d4d243649af6f6f333c27ab00835b6f7f19e6d.png"
  , "sideplank_2-90252ce45502cb25fa7d5b1f7fd666fb8c9e9f91.png"
  , "squats_1-092009c2d075fe615297927690998298e93a7327.png"
  , "squats_2-8f8d44593dbaf60b3f4a9467a10b6acbbb56b884.png"
  , "tricep_1-ce067045a14fa2b881eefc7ca0080fc48b29a4dc.png"
  , "tricep_2-32f46240bab285e5ef4e3878213f50bc2ed7820e.png"
  , "wallsit_1-5724ff6ece6ef2dbf2bad948fcee3efd329df6c0.png"
  ]

workouts :: [(Text, Text)]
workouts =
 -- 5
 -- 1,1 .. 30
 [ ("jumpingjacks", "Jumping jacks")
 -- 10
 -- 30
 , ("wallsit", "Wall sit")
 -- 10
 -- 2,2 .. 30
 , ("pushupsn", "Push up")
 -- 10
 -- 2,2 .. 30
 , ("abcrunches", "Abdominal crunch")
 -- 10
 -- 2,2 .. 30
 , ("chair", "Step-up onto chair")
 -- 10
 -- 2,2 .. 30
 , ("squats", "Squat")
 -- 10
 -- 2,2 .. 30
 , ("tricep", "Triceps dip on chair")
 -- 10
 -- 30
 , ("plank", "Plank")
 -- 10
 -- 1,1 .. 30
 , ("run", "High knees running in place")
 -- 10
 -- 2,2 .. 30
 , ("lunges", "Lunge")
 -- 10
 -- 2,2 .. 30
 , ("pushupsrotation", "Push ups with rotation")
 -- 10
 -- 2, 28 .. 30
 , ("sideplank", "Side plank")
 ]

readPngIntoSurface :: FilePath -> IO SDL.Surface
readPngIntoSurface path = do
  readingImage <- P.readImage path

  let pngImage = case readingImage of
          Left err -> panic ("encountered error during load of image: " <> pack err)
          Right dynamicImage -> P.convertRGBA8 dynamicImage

  let pngWidth = P.imageWidth pngImage
      pngHeight = P.imageHeight pngImage
      pngData = P.imageData pngImage

  rawData <- V.thaw pngData

  -- putText ("Loaded png image with size: " <> show pngWidth <> "x" <> show pngHeight)

  SDL.createRGBSurfaceFrom rawData
       (V2 (CInt $ fromIntegral pngWidth)
           (CInt $ fromIntegral pngHeight))
       (4 * (CInt $ fromIntegral pngWidth))
       SDL.ABGR8888

main :: IO ()
main = do
  workoutFigureSurface <- readPngIntoSurface "png-large/chair_1-94f472535b2697994151d989b1c1802421867f35.png"

  pngFiles <- traverse (\path -> (path,) <$> readPngIntoSurface (unpack ("png-large/" <> path))) pngs
  let ws = map (\(id, logos) -> (id, logos, (map snd . filter (T.isPrefixOf id . fst)) pngFiles)) workouts

  SDL.initializeAll
  Mixer.openAudio Mixer.defaultAudio 1024
  Font.initialize

  wavTick :: Mixer.Chunk <- Mixer.load "tick.wav"
  wavTock :: Mixer.Chunk <- Mixer.load "tock.wav"

  let asciiLetters = pack ['0'..'z']

  futuraRegularFont24 <- Font.load "FuturaLT.ttf" 24
  helveticaWorldRegularFont24 <- Font.load "HelveticaWorld-Regular.ttf" 24

  futuraRegularFont72 <- Font.load "FuturaLT.ttf" 72
  helveticaWorldRegularFont72 <- Font.load "HelveticaWorld-Regular.ttf" 72

  let fonts = Fonts futuraRegularFont24 futuraRegularFont72
                    helveticaWorldRegularFont24 helveticaWorldRegularFont72

  window <- SDL.createWindow "My SDL Application" (SDL.defaultWindow { SDL.windowInitialSize = (V2 1600 900) } )
  windowSurface <- SDL.getWindowSurface window

  overallSurface <- SDL.createRGBSurface (V2 1600 900) SDL.RGBA8888

  let eachW (ind, (id, logos, (illustrationSurface:_))) = do
                {- Cartesian grid
                let x = 700 + 200 * (mod ind 4)
                    y = 100 + 224 * (div ind 4)
                 -}
                {- Dodecacagon -}
                let alph = (fromIntegral ind) * (pi / 12)
                    x = 1050 + (floor (400 * cos alph)) - 100
                    y = 400 + (floor (300 * sin alph))  - 100
                 {- -}

                SDL.surfaceFillRect overallSurface (Just $ SDL.Rectangle (P $ V2 x (y + 200)) ( V2 190 5)) (V4 255 255 255 255)

                SDL.surfaceBlit illustrationSurface Nothing overallSurface (Just $ P $ V2 x y)

                textSurface <- Font.blended (helvetica24 fonts) (V4 255 255 255 255) id
                SDL.surfaceBlit textSurface Nothing overallSurface (Just $ P $ V2 x (y + 205))

                SDL.freeSurface textSurface

  traverse eachW (zip [0..] (take 6 ws))

  SDL.freeSurface overallSurface

  initialTime <- SDL.time

  sdlCycle workoutFigureSurface windowSurface window fonts ws wavTick wavTock initialTime initialTime False False

sdlCycle :: SDL.Surface -> SDL.Surface -> SDL.Window -> Fonts -> [(Text, Text, [SDL.Surface])] -> Mixer.Chunk -> Mixer.Chunk -> Double -> Double -> Bool -> Bool -> IO ()
sdlCycle workoutFigureSurface windowSurface window fonts ws wavTick wavTock secondsLastChecked timerStart spacePressedLastChecked timerDream = do
  events <- SDL.pollEvents

  seconds <- SDL.time

  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      eventIsSpacePress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeSpace
          _ -> False
      eventIsEscapePress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
          _ -> False

      qPressed
        = any eventIsQPress events

      spacePressedThisCycle
        = any eventIsSpacePress events

      escapePressedThisCycle
        = any eventIsEscapePress events

      -- S
      spaceSignal
        = (not spacePressedLastChecked) && spacePressedThisCycle

      timerDreamThisCycle
       = if escapePressedThisCycle then False else (if spaceSignal then not timerDream else timerDream)

  SDL.surfaceFillRect windowSurface Nothing (V4 0 0 0 255)

  (startTextWidth, startTextHeight) <- Font.size (helvetica24 fonts) "Space to start"
  startTextSurface <- Font.blended (helvetica24 fonts) (V4 255 255 255 255) "Space to start"

  (stopTextWidth, stopTextHeight) <- Font.size (helvetica24 fonts) "Space to stop"
  stopTextSurface <- Font.blended (helvetica24 fonts) (V4 255 255 255 255) "Space to stop"

  case timerDreamThisCycle of
    True -> void $ SDL.surfaceBlit stopTextSurface Nothing windowSurface (Just $ P $ V2 ((1600 - fromIntegral stopTextWidth) `div` 2) 670)
    False -> void $ SDL.surfaceBlit startTextSurface Nothing windowSurface (Just $ P $ V2 ((1600 - fromIntegral startTextWidth) `div` 2) 670)

  SDL.freeSurface startTextSurface
  SDL.freeSurface stopTextSurface

  let
      intervalSinceLastChecked
        = seconds - secondsLastChecked

      timerStartThisCycle
        = if escapePressedThisCycle then seconds else (if timerDreamThisCycle then timerStart else timerStart + intervalSinceLastChecked)

      secondsInProgress = (seconds - timerStartThisCycle)

  SDL.surfaceFillRect windowSurface (Just $ SDL.Rectangle (P $ V2 0 500) (V2 500 400)) (V4 255 255 255 255)

  case secondsInProgress < 5 of
    True -> do
      timerSurfaceBlack <- Font.blended (futura72 fonts) (V4 0 0 0 255) (show $ round (5 - secondsInProgress))

      textSurfaceBlack <- Font.blended (helvetica24 fonts) (V4 0 0 0 255) "Get Ready"

      SDL.surfaceBlit timerSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 570)
      SDL.surfaceBlit textSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 700)

      SDL.freeSurface timerSurfaceBlack
      SDL.freeSurface textSurfaceBlack

    False -> do
      let secondsIntoWorkout :: Int = (round secondsInProgress - 5) `mod` 40
          inRest = secondsIntoWorkout > 30
          workoutInProgressIndex = (round secondsInProgress - 5) `div` 40 + (if inRest then 1 else 0)
          workoutInProgress = headMay $ drop workoutInProgressIndex ws
      case workoutInProgress of
        Nothing -> do
          doneSurfaceBlack <- Font.blended (helvetica72 fonts) (V4 0 0 0 255) "Finite!"

          SDL.surfaceBlit doneSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 700)

          SDL.freeSurface doneSurfaceBlack

        Just (id, logos, surfaces) -> do
          let surfaceCount = length surfaces
              surfaceIndex = ((secondsIntoWorkout) `mod` surfaceCount)
              surfaceMay = if inRest then headMay surfaces else workoutSpecificAlternation
              workoutSpecificAlternation = case id of
                "sideplank" -> lastMay surfaces
                _           -> atMay surfaces surfaceIndex

          case surfaceMay of
            Nothing -> do
              missTextSurface <- Font.blended (futura72 fonts) (V4 0 0 0 255) "missing illustration"
              SDL.surfaceBlit missTextSurface Nothing windowSurface (Just $ P $ V2 50 570)
              SDL.freeSurface missTextSurface
            Just surface ->
              void $ SDL.surfaceBlit surface Nothing windowSurface (Just $ P $ V2 0 0)

          case inRest of
            False -> do
              when (floor secondsLastChecked /= floor seconds) $ do
                case floor seconds `mod` 2 == 0 of
                  True -> Mixer.play wavTick
                  False -> Mixer.play wavTock

              let countdown = 30 - secondsIntoWorkout
              countdownTextSurface <- Font.blended (futura72 fonts) (V4 0 0 0 255) (show $ countdown)
              SDL.surfaceBlit countdownTextSurface Nothing windowSurface (Just $ P $ V2 50 570)
              SDL.freeSurface countdownTextSurface

            True -> do
              let countdown = 40 - secondsIntoWorkout
              countdownTextSurface <- Font.blended (futura72 fonts) (V4 0 0 0 255) (show $ countdown)
              SDL.surfaceBlit countdownTextSurface Nothing windowSurface (Just $ P $ V2 50 570)
              SDL.freeSurface countdownTextSurface

              restTextSurface <- Font.blended (helvetica24 fonts) (V4 0 0 0 255) "Rest now, next:"
              SDL.surfaceBlit restTextSurface Nothing windowSurface (Just $ P $ V2 50 670)
              SDL.freeSurface restTextSurface

          logosSurfaceBlack <- Font.blended (helvetica24 fonts) (V4 0 0 0 255) logos
          SDL.surfaceBlit logosSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 700)
          SDL.freeSurface logosSurfaceBlack

  -- SDL.surfaceBlit overallSurface Nothing windowSurface Nothing

  SDL.updateWindowSurface window

  unless qPressed (sdlCycle workoutFigureSurface windowSurface window fonts ws wavTick wavTock seconds timerStartThisCycle spacePressedThisCycle timerDreamThisCycle)
