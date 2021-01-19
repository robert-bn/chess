{-#Language ScopedTypeVariables#-}
module ProductEnum where

instance forall a b. (Enum a, Bounded a, Bounded b, Enum b) => Enum (a,b) where
    -- Tested using quickCheck
    fromEnum (x,y) | maxA >= maxB = fromEnum y * (maxA + 1) + fromEnum x
                   | otherwise    = fromEnum (y,x)
                   where maxA = fromEnum (maxBound :: a)
                         maxB = fromEnum (maxBound :: b)
                  
    toEnum n | maxA >= maxB = let
                                  x = mod n (maxA + 1)       
                                  y = div (n - x) (maxA + 1)
                              in
                                  (toEnum x, toEnum y)
             | otherwise = let
                               x = mod n (maxB + 1)
                               y = div (n - x) (maxB + 1)
                           in
                               (toEnum y, toEnum x)
             where maxA = fromEnum (maxBound :: a)
                   maxB = fromEnum (maxBound :: b)

instance (Enum a) => Enum (Maybe a) where
    fromEnum Nothing  = 0
    fromEnum (Just x) = 1 + fromEnum x

    toEnum 0 = Nothing
    toEnum n = Just (toEnum (n-1))
