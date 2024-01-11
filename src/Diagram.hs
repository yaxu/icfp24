import           Control.Monad
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.List                (intercalate)
import           Data.Maybe
import           Data.Ratio
import           Pattern

import qualified Graphics.Rendering.Cairo as C

cyclesperinch = 1

l = 0.008
lh = l/2
linewidth = 0.008
rowheight = 0.25
gap = l
cellheight = rowheight-gap
cyclewidth = 0.4
  
dashed = C.setDash [0.05, 0.025] 0

pat = stack [atom "red", interlace [atom "green", atom "blue"]]

intersects a b = begin c < end c
  where c = intersect a b

layer :: [Event a] -> [[Event a]]
layer []     = []
layer (e:[]) = [[e]]
layer (e:es) = check $ layer es
  where fits e' es' = not $ or $ catMaybes $ map (\e'' -> intersects <$> (whole e'') <*> (whole e')) es'
        check []       = [[e]]
        check (es':xs) | fits e es' = (e:es'):xs
                       | otherwise = es':(check xs)

main = do renderPattern 12 "fig1.pdf" fig1
          renderPattern 12 "fig2.pdf" fig2
          renderPattern 12 "fig3.pdf" fig3
          renderPattern 12 "fig4.pdf" fig4

renderPattern cycles name pat =
  do let events = query pat $ TimeSpan 0 (toRational cycles)
         eventses = map (map drawEvent) $ reverse $ layer events
         layercount = length eventses
     C.withPDFSurface ("figures/" ++ name)  (72*cyclewidth*cycles) (72*rowheight*(fromIntegral layercount)) $ \surf ->
       C.renderWith surf $ do
         C.scale (cyclesperinch*72*cyclewidth) (cyclesperinch*72)
         C.setOperator C.OperatorOver
         sequence $ intercalate [C.translate 0 rowheight] $ eventses

drawEvent (Event (Just (TimeSpan wb we)) (TimeSpan ab ae) v) =
  do let colour = fromMaybe grey $ readColourName v
         RGB r g b = toSRGB colour
         RGB rl gl bl = toSRGB $ blend 0.3 colour white
         wb', we', ab', ae' :: Double
         wb' = fromRational wb
         we' = fromRational we
         ab' = fromRational ab
         ae' = fromRational ae
         lwb = wb' + lh
         lwe = we' - lh
         lab = ab' + lh
         lae = ae' - lh

     C.save
     when (wb' < ab' || we' > ab') $ do
       C.setSourceRGB rl gl bl
       C.rectangle lwb 0 (we'-lwb) cellheight
       C.fill
     C.setSourceRGB r g b
     C.rectangle ab' 0 (ae'-ab') cellheight
     C.fill

     -- top line
     C.setSourceRGB 0 0 0
     C.setLineWidth linewidth
     C.setLineCap C.LineCapRound
     C.moveTo (lab+lh) lh
     C.lineTo (lae+lh) lh
     C.stroke

     -- bottom line
     C.moveTo lab cellheight
     C.lineTo lae cellheight
     C.stroke

     C.setDash [] 0
     when (wb' < ab') dashed
     C.moveTo lwb lh
     C.lineTo lwb cellheight
     C.stroke

     C.setDash [] 0
     when (we' > ae') $ C.setDash [0.025, 0.025] 0
     C.moveTo lwe lh
     C.lineTo lwe cellheight
     C.stroke

     when (wb' < ab') $ do
       dashed
       C.moveTo lwb lh
       C.lineTo lab lh
       C.stroke
       C.moveTo lwb cellheight
       C.lineTo lab cellheight
       C.stroke

     when (we' > ae') $ do
       dashed
       C.moveTo lae lh
       C.lineTo lwe lh
       C.stroke
       C.moveTo lae cellheight
       C.lineTo lwe cellheight
       C.stroke
     C.restore
{-
     C.setDash [] 4
     C.moveTo 0 0
     C.lineTo 100 0
     C.stroke
     C.moveTo 100 0
     C.setDash [4,4] 4
     C.lineTo 150 0
     C.stroke

     C.setDash [] 0
     C.moveTo 0 50
     C.lineTo 100 50
     C.stroke
     C.moveTo 100 50
     C.setDash [4,4] 4
     C.lineTo 150 50
     C.stroke
-}
