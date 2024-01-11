{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Pattern

myFigure :: Diagram B
myFigure = do square 1 # fc fig2
              

main = mainWith myFigure
