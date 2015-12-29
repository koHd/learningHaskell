import Dist

winAmount :: Num a => a -> a
winAmount bet = 10 * bet

choose :: Integral a => a -> a -> a
choose trials 0 = 1
choose 0 successes = 0
choose trials successes = choose (trials-1) (successes-1) * trials `div` successes 

binomialProbability :: Int -> Int -> Double -> Double
binomialProbability trials successes probability = 
    (fromIntegral (choose trials successes)) * (probability^successes) 
    * ((1-probability) ^ (trials - successes))

binomialDistribution :: Int -> Double -> [Double]
binomialDistribution trials probability = 
    [(binomialProbability trials successes probability) | 
        successes <- [0..(trials-1)]] 

expectedValue :: Double -> Double -> Double
expectedValue betAmount winProbability = 
    (winAmount (betAmount)) * winProbability + (-betAmount) * (1-winProbability)

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel startMoney percentageToBet numRounds = 
    Dist[((expectedValue (startMoney * percentageToBet) (binomialProbability numRounds successes 0.25))
            * (fromIntegral numRounds) + startMoney,
            binomialProbability numRounds successes 0.25) 
            | successes <- [0..(numRounds-1)]]

mean :: Dist Double -> Double
mean (Dist distribution) = sum (map (\x -> (fst x) * (snd x)) distribution)
