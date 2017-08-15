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

-- 再帰
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- take' 3 [5,4,3,2,1]
-- return [5,4,3]
take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = [] -- 第二引数が空リストの時に空のリストを返す
take' n (x:xs) = x : take' (n-1) xs

-- クイックソート
-- はじめの要素をピボット（軸）にそのほかの要素を大きい要素を右に、それよりも小さい要路を左に置く。
-- その後それぞれ同じことを繰り返し、空リストになるまで行う。
-- その結果ソート済のリストは得ることができる。

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
--  let smallerOrEqual = [a | a <- xs, a <= x]
--      larger = [a | a <- xs, a > x]
--  in quicksort smallerOrEqual ++ [x] ++ quicksort larger
    let smallerOrEqual = filter (<=x) xs
        larger = filter (>x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- 高階プログラミング
-- 関数を引数にしたり、戻り値を関数にするような関数のこと

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


-- コラッツの予想
-- n が偶数の場合、n を 2 で割る
-- n が奇数の場合、n に 3 をかけて 1 を足す
-- これを繰り返すとどんな初期値から始めても、有限回の操作のうちに必ず 1 に到達する(そして 1→4→2→1 というループに入る)

collaz :: Int -> [Int]
collaz 1 = [1]
collaz n
  | even n = n : collaz (n `div` 2)
  | odd n = n : collaz(n * 3 + 1)

-- 1~100までの数のうち、リストの長さが15以上のコラッツ列の開始数になるものはいくつあるか。
numLongCollaz1 :: Int
numLongCollaz1 = length (filter isLong (map collaz [1..100]))
  where isLong xs = length xs > 15

-- ラムダ式を利用した関数
numLongCollaz :: Int
numLongCollaz = length (filter (\xs -> length xs > 15)
                               (map collaz [1..100]))

-- 畳込み
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0

-- 合成関数
-- map (negate . abs) [1,2,3]
-- [-1,-2,-3]
-- これは以下の式と同じ
-- map (\x -> negate (abs x)) [1,2,3]
