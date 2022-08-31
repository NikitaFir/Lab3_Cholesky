import Data.Time
import Control.Parallel.Strategies
import System.IO

-- create a new matrix with equal dimension
createEmptyMatrix :: Int -> [Float]
createEmptyMatrix dim = take (dim*dim) (repeat 0.0)

-- get element from matrix via row, column notation
get :: (RealFrac p, Floating p) => p -> p -> [a] -> a
get row col mat = mat !! (round (row * (sqrt (fromIntegral (length mat))) + col))

-- alters the element with matching row, col coordinates
-- assuming the given r,c values are within matrix range
alterListElement :: (RealFrac p, Floating p) => p -> p -> [a] -> a -> [a]
alterListElement r c list new_element = 
  take element_idx list ++ [new_element] ++ drop (element_idx + 1) list
  where element_idx = round (r * (sqrt (fromIntegral (length list))) + c)

-- calculate the indices of a the lower left triangle
-- returning a list of lists, each containing row and col values
-- e.g [ [0,0], [0,1], [1,0], ... ]
calcLowerTriangleIndices :: (Num p, Enum p) => p -> [[p]]
calcLowerTriangleIndices dimension = 
  concat [ [ [x,y] | y <- [0..x] ] | x <- [0..dimension-1] ]

-- cholesky helper for processing diagonal matrix values with equal indices
choleskyHelperDiag :: (Enum p, RealFrac p, Floating a, Floating p) => p -> p -> [a] -> [a] -> [a]
choleskyHelperDiag i j matrix result = alterListElement i j result value
  where value = sqrt( (get i i matrix) - s)
        s = sum [ (get i k result) * (get j k result) | k <- [0..j] ]

-- cholesky helper for processing the lower matrix result
-- without considering diagonal values
choleskyHelperLower :: (Fractional a, Enum p, RealFrac p, Floating p) => p -> p -> [a] -> [a] -> [a]
choleskyHelperLower i j matrix result = alterListElement i j result value
  where value = (1.0 / (get j j result) * (get i j matrix - s))
        s = sum [ (get i k result) * (get j k result) | k <- [0..j] ]

-- cholesky helper for checking current matrix indices
-- infering resulting action 
choleskyIndexCheck :: (Floating p, Floating a, RealFrac p, Enum p) => p -> p -> [a] -> [a] -> [a]
choleskyIndexCheck i j result matrix
    | i == j = choleskyHelperDiag i j matrix result
    | otherwise = choleskyHelperLower i j matrix result

-- recursive cholesky decomposition
-- assuming the given matrix has equal dimensions
cholesky:: (Enum p, RealFrac p, Floating a, Floating p) => [a] -> [a] -> [[p]] -> Int -> [a]
cholesky matrix result indices start
    | start <= ((length indices)-1) = cholesky matrix (choleskyIndexCheck i j result matrix) indices (succ start) 
    | otherwise = result 
      where i = indices !! start !! 0
            j = indices !! start !! 1

-- main
-- a matrix is stored in txt file 
-- where it is described by a simple list with n^2 elements

main :: IO()
main =  do

  -- matrix dimension equal to 10

  contents <- readFile "10.txt"
  let matrix_1 = map readInt . words $ contents
  let result = createEmptyMatrix 10
  let indices = calcLowerTriangleIndices 10
  
  start_1 <- getCurrentTime
  let res = cholesky matrix_1 result indices 0
  stop_1 <- getCurrentTime
  
  print $ diffUTCTime stop_1 start_1
  
  putStrLn ""
  putStrLn ""

  -- matrix dimension equal to 100
  
  contents <- readFile "100.txt"
  let matrix_2 = map readInt . words $ contents
  let result_2 = createEmptyMatrix 100
  let indices_2 = calcLowerTriangleIndices 100
  
  start_2 <- getCurrentTime
  let res_2 = cholesky matrix_2 result indices 0
  stop_2 <- getCurrentTime
  
  print $ diffUTCTime stop_2 start_2
  
  putStrLn ""
  putStrLn ""

  -- matrix dimension equal to 1000
  
  contents <- readFile "1000.txt"
  let matrix_3 = map readInt . words $ contents
  let result = createEmptyMatrix 1000
  let indices = calcLowerTriangleIndices 1000
  
  start_3 <- getCurrentTime
  let res_3 = cholesky matrix_3 result indices 0
  stop_3 <- getCurrentTime
  print $ diffUTCTime stop_3 start_3
  
  putStrLn ""
  putStrLn ""

  -- matrix dimension equal to 3000

  contents <- readFile "3000.txt"
  let matrix_4 = map readInt . words $ contents
  let result_2 = createEmptyMatrix 3000
  let indices_2 = calcLowerTriangleIndices 3000
  
  start_4 <- getCurrentTime
  let res_4 = cholesky matrix_4 result indices 0
  stop_4 <- getCurrentTime
  print $ diffUTCTime stop_4 start_4
   
  putStrLn ""
  putStrLn ""
  
  -- matrix dimension equal to 6000
  
  contents <- readFile "6000.txt"
  let matrix_5 = map readInt . words $ contents
  let result_2 = createEmptyMatrix 6000
  let indices_2 = calcLowerTriangleIndices 6000
  
  start_5 <- getCurrentTime
  let res_5 = cholesky matrix_5 result indices 0
  stop_4 <- getCurrentTime
  print $ diffUTCTime stop_5 start_5
  