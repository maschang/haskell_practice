doubleMe x = x + x
-- doubleUs x y = x * 2 + y * 2
doubleUs x y = doubleMe x + doubleMe y

-- 型の条件文
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
-- 関数の型宣言
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- パターンマッチ
lucky :: Int -> String -- 型の宣言
lucky 7 = "Lucky Number Seven"
lucky x = "Sorry, you're out of luck, pal!"

-- タプルのパターンマッチ
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2 )
badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

-- asパターン
firstLetter :: String -> String
firstLetter "" = "Empty!!"
firstLetter all@(x:xs) = all ++ " and "  ++ [x] 

-- ガード
bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "super nomal"
  | bmi <= 30.0 = "overweight"
  | otherwise  = "check congraturations"

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

-- 中置関数
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

-- where の利用
bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= fat = "fat! you're ugly!"
  | otherwise = "congratulation!"
  where bmi = weight / height ^ 2
--        skinny = 18.5
--        normal = 25.0
--        fat = 30.0
--        多重代入っぽい感じ
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- let式
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-- ※内包的記法を参照

-- case 式
describeList :: [a] -> String
describeList ls = "The list is " ++
                    case ls of [] -> "empty"
                               [x] -> "a singleton list"
                               xs -> "a longer list"
