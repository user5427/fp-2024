{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
module Lessons.Lesson02 (Wheel(..)) where


-- >>> sum' []
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')
-- >>> sum' [1,2,3]
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8216')
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (h:t) = h + sum' t

-- >>> sumF []

sumB :: [Integer] -> Integer
sumB [] = 0
sumB (h:t) = h + sumB t

-- >>> sumB [1, 5, 6]
-- 12


-- >>> sum' [1]
-- 1
sum'' :: [Integer] -> Integer
sum'' l = sumTail l 0
    where
        sumTail [] acc = acc
        sumTail (h:t) acc = sumTail t (acc + h)

-- >>> dup []
-- []
-- >>> dup [1,2,3]
-- [1,1,2,2,3,3]
dup :: [a] -> [a]
dup [] = []
dup (h:t) = [h, h] ++ dup t

-- >>> dup'' []
-- []
-- >>> dup'' [1]
-- [1,1]
dup'' :: [a] -> [a]
dup'' l = 
    let
        empty = []
        dupTail [] acc = acc
        dupTail (h:t) acc = dupTail t ([h, h] ++ acc)
    in
        dupTail l empty

-- >>> safeDiv 1 0
-- Nothing
-- >>> safeDiv 10 2
-- Just 5
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)



-- >>> safeDiv' 1 0
-- No instance for (Show (MyBetterMaybe Integer))
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_aauy
-- >>> safeDiv' 10 2
-- No instance for (Show (MyBetterMaybe Integer))
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_aawy
data MyBetterMaybe a = MyNothing | MyJust a

safeDiv' :: Integer -> Integer -> MyBetterMaybe Integer
safeDiv' _ 0 = MyNothing
safeDiv' a b = MyJust (a `div` b)

data Wheel = Wheel Integer
data Pedals = One | Two
data Seat = Wooden | Plastic
data Unicycle = Unicycle Wheel Pedals Seat
--- >>> isSeatWooden (Unicycle (Wheel 100) Two Plastic)
-- False
-- >>> isSeatWooden (Unicycle (Wheel 200) Two Wooden)
-- True
isSeatWooden :: Unicycle -> Bool
isSeatWooden (Unicycle (Wheel x) _ Wooden) = True
isSeatWooden _ = False

