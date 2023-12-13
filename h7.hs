safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x >= 0    = Just (sqrt x)
  | otherwise = Nothing

safeReciprocal :: Double -> Maybe Double
safeReciprocal x
  | x /= 0    = Just (1 / x)
  | otherwise = Nothing

combinedOperations :: Double -> Maybe Double
combinedOperations x = do
  sqrtResult <- safeSqrt x
  recipResult <- safeReciprocal sqrtResult
  return recipResult
