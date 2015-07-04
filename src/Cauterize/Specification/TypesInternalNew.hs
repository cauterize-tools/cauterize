module Cauterize.Specification.TypesInternalNew
  ( Size
    , mkSize
    , mkConstSize
  ) where

data Size = Size Integer Integer
  deriving (Show)

mkSize :: Integer -> Integer -> Size
mkSize rmin rmax | rmin < 1 = error ("Min size less than 1: " ++ show rmin)
                 | rmax < 1 = error ("Max size less than 1: " ++ show rmax)
                 | rmin < rmax = Size rmin rmax
                 | otherwise = error ("Bad min and max: min " ++ show rmin ++ " >= max " ++ show rmax ++ ".")

mkConstSize :: Integer -> Size
mkConstSize sz = mkSize sz sz
