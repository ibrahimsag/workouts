{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Protolude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
 [ ("jumpingjacks", "Jumping jacks")
 , ("wallsit", "Wall sit")
 , ("pushupsn", "Push up")
 , ("abcrunches", "Abdominal crunch")
 , ("chair", "Step-up onto chair")
 , ("squats", "Squat")
 , ("tricep", "Triceps dip on chair")
 , ("plank", "Plank")
 , ("run", "Run in place")
 , ("lunges", "Lunges")
 , ("pushupsrotation", "Push ups with rotation")
 , ("sideplank", "Side plank")
 ]

sectionData :: [(Int, Text)]
sectionData =
  [ (5, "ready")
  , (5, "tick-five")
  , (5, "jumpingjacks")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "wallsit")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "pushupsn")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "abcrunches")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "chair")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "squats")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "tricep")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "plank")
  , (30, "tick-thirty")
  , (14, "rest")
  , (5, "run")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "lunges")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "pushupsrotation")
  , (30, "tick-thirty")
  , (14, "rest")
  , (4, "sideplank")
  , (30, "tick-thirty")
  , (7, "done")
  ]

readPngIntoSurface :: FilePath -> IO SDL.Surface
readPngIntoSurface path = do
  readingImage <- P.readImage path

  let pngImage = case readingImage of
          Left err -> panic ("ERROR when loding image: " <> pack err)
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
  pngFiles <- traverse (\path -> (path,) <$> readPngIntoSurface (unpack ("png-large/" <> path))) pngs
  let ws = Map.fromList (map (\(id, workoutTitle) -> (id, (workoutTitle, (map snd . filter (T.isPrefixOf id . fst)) pngFiles))) workouts)

  let
    (_, sections) = foldl' (\(l, m) (d, t) -> (l+d, Map.insert (l+d) t m)) (0, Map.empty) sectionData
  SDL.initializeAll
  Mixer.openAudio Mixer.defaultAudio 1024

  chunks <- ((Map.fromList <$>) . traverse (\t -> (t,) <$> Mixer.load ("audio/" <> toS t <> ".wav")) . ordNub . map snd) sectionData

  Font.initialize

  futuraRegularFont24 <- Font.load "FuturaLT.ttf" 24
  helveticaWorldRegularFont24 <- Font.load "HelveticaWorld-Regular.ttf" 24

  futuraRegularFont72 <- Font.load "FuturaLT.ttf" 72
  helveticaWorldRegularFont72 <- Font.load "HelveticaWorld-Regular.ttf" 72

  let fonts = Fonts futuraRegularFont24 futuraRegularFont72
                    helveticaWorldRegularFont24 helveticaWorldRegularFont72

  window <- SDL.createWindow "My SDL Application" (SDL.defaultWindow { SDL.windowInitialSize = (V2 1600 900) } )
  windowSurface <- SDL.getWindowSurface window

  initialTime <- SDL.time

  step (K windowSurface window fonts ws sections chunks) (V initialTime initialTime False Nothing Nothing)

data K = K
  { windowSurface :: SDL.Surface
  , window :: SDL.Window
  , fonts :: Fonts
  , ws :: Map Text (Text, [SDL.Surface])
  , sections :: Map Int Text
  , chunks :: Map Text Mixer.Chunk
  }

data V = V
  { timeLastChecked :: Double
  , timerStart :: Double
  , timing :: Bool
  , lastFrameSection :: Maybe Text
  , lastWorkout :: Maybe (Text, Text, [SDL.Surface])
  }

data E = Quit | Cont | Start | Reset | Skip

step :: K -> V -> IO ()
step kdata vdata = do
  let
    K{windowSurface, window, fonts, ws, sections, chunks} = kdata
    V{timeLastChecked, timerStart, timing, lastFrameSection, lastWorkout} = vdata
  mevent <- SDL.pollEvent

  time <- SDL.time

  let
    e = case mevent of
      Nothing -> Cont
      Just event ->
        case SDL.eventPayload event of
          SDL.QuitEvent -> Quit
          SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
              SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
              -> Quit
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
              SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeSpace
              -> Start
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
              SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeL
              -> Skip
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
              SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
              -> Reset
          _ -> Cont

  case e of
    Quit -> pure ()
    Skip -> step kdata vdata{timerStart=timerStart-1}
    Reset -> step kdata vdata{timing = False, timeLastChecked=time}
    Start -> step kdata vdata{timing = not timing}
    Cont ->
     do
      let
        intervalSinceLastChecked
          = time - timeLastChecked
        timerStartThisCycle
          = if timing then timerStart else timerStart + intervalSinceLastChecked
        secondsInProgress = (time - timerStartThisCycle)

        (sectionEnd, sectionTitle) = maybe (0, "end") identity (Map.lookupGT (floor secondsInProgress) sections)

        (_, nextSectionTitle) = maybe (0, "end") identity (Map.lookupGT sectionEnd sections)

        sectionChunk = Map.lookup sectionTitle chunks
        currentSection = if timing then Just sectionTitle else lastFrameSection

        upcoming = sectionTitle == "rest" || sectionTitle == "tick-five"
        workoutTitle = if upcoming then nextSectionTitle else sectionTitle

        workout :: Maybe (Text, Text, [SDL.Surface])
        workout =
          if sectionTitle == "tick-thirty"
          then lastWorkout
          else (maybe Nothing (\(t, ss) -> Just (sectionTitle, t, ss)) (Map.lookup workoutTitle ws))

      SDL.surfaceFillRect windowSurface Nothing (V4 0 0 0 255)
      when (timing && lastFrameSection /= (Just sectionTitle))
       do
        case sectionChunk of
          Nothing -> pure ()
          Just chunk -> Mixer.fadeOut 300 Mixer.AllChannels >> Mixer.play chunk
        putText sectionTitle

      case timing of
        True -> 
         do
          (textWidth, textHeight) <- Font.size (helvetica24 fonts) "Space to stop"
          textSurface <- Font.blended (helvetica24 fonts) (V4 255 255 255 255) "Space to stop"

          SDL.surfaceBlit textSurface Nothing windowSurface (Just $ P $ V2 ((1600 - fromIntegral textWidth) `div` 2) 670)
          SDL.freeSurface textSurface
        False ->
         do
          (textWidth, textHeight) <- Font.size (helvetica24 fonts) "Space to start"
          textSurface <- Font.blended (helvetica24 fonts) (V4 255 255 255 255) "Space to start"

          SDL.surfaceBlit textSurface Nothing windowSurface (Just $ P $ V2 ((1600 - fromIntegral textWidth) `div` 2) 670)
          SDL.freeSurface textSurface

      SDL.surfaceFillRect windowSurface (Just $ SDL.Rectangle (P $ V2 0 500) (V2 500 400)) (V4 255 255 255 255)
      SDL.surfaceFillRect windowSurface (Just $ SDL.Rectangle (P $ V2 0 505) (V2 495 395)) (V4 0 0 0 255)

      case workout of
        Nothing -> pure ()
        Just (workoutId, workoutTitle, surfaces) ->
         do
          let
            surfaceCount = length surfaces
            surfaceIndex = ((floor secondsInProgress) `mod` surfaceCount)
            surfaceMay = if upcoming then headMay surfaces else workoutSpecificAlternation
            workoutSpecificAlternation = case workoutId of
              "sideplank" -> lastMay surfaces
              _           -> atMay surfaces surfaceIndex

          case surfaceMay of
            Nothing ->
             do
              missTextSurface <- Font.blended (futura72 fonts) (V4 255 255 255 255) "missing illustration"
              SDL.surfaceBlit missTextSurface Nothing windowSurface (Just $ P $ V2 50 570)
              SDL.freeSurface missTextSurface
            Just surface ->
             do
              SDL.surfaceBlit surface Nothing windowSurface (Just $ P $ V2 0 0)
              pure ()

          if upcoming
          then
           do
            titleSurfaceBlack <- Font.blended (helvetica24 fonts) (V4 255 255 255 155) ("..." <> workoutTitle)
            SDL.surfaceBlit titleSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 550)
            SDL.freeSurface titleSurfaceBlack
          else
           do
            titleSurfaceBlack <- Font.blended (helvetica24 fonts) (V4 255 255 255 255) workoutTitle
            SDL.surfaceBlit titleSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 550)
            SDL.freeSurface titleSurfaceBlack

      when ("rest" == sectionTitle)
       do
        textSurfaceBlack <- Font.blended (helvetica72 fonts) (V4 255 255 255 255) "Rest for now"
        SDL.surfaceBlit textSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 700)
        SDL.freeSurface textSurfaceBlack

      when ("ready" == sectionTitle || "tick-five" == sectionTitle)
       do
        textSurfaceBlack <- Font.blended (helvetica72 fonts) (V4 255 255 255 255) "Get Ready"
        SDL.surfaceBlit textSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 700)
        SDL.freeSurface textSurfaceBlack

      when ("done" == sectionTitle || "end" == sectionTitle)
       do
        doneSurfaceBlack <- Font.blended (helvetica72 fonts) (V4 255 255 255 255) "Done"
        SDL.surfaceBlit doneSurfaceBlack Nothing windowSurface (Just $ P $ V2 50 700)
        SDL.freeSurface doneSurfaceBlack

      let countdown = sectionEnd - (floor secondsInProgress)
      when (T.isPrefixOf "tick" sectionTitle || ("rest" == sectionTitle && countdown < 11))
       do
        countdownTextSurface <- Font.blended (futura72 fonts) (V4 255 255 255 155) (show $ countdown)
        SDL.surfaceBlit countdownTextSurface Nothing windowSurface (Just $ P $ V2 50 600)
        SDL.freeSurface countdownTextSurface

      SDL.updateWindowSurface window

      step kdata vdata{timeLastChecked = time, timerStart=timerStartThisCycle, lastFrameSection = currentSection, lastWorkout = workout}
