import Dist

outcomeProbability = 0.25
odds = 10

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

expectedValue :: Double -> Double -> Double -> Double
expectedValue betAmount winProbability odds = 
    (odds*betAmount) * winProbability + (-betAmount) * (1-winProbability)

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel startMoney percentageToBet numRounds = 
    Dist[(expectedTotalMoney, sequenceProbability) 
            | successes <- [0..(numRounds)],
            let sequenceProbability = binomialProbability numRounds successes outcomeProbability,
            let winRatio = (fromIntegral successes) / (fromIntegral numRounds),
            let betAmount = expectedValue (startMoney*percentageToBet) outcomeProbability odds,
            let value = expectedValue betAmount winRatio odds,
            let expectedNetGain = value * (fromIntegral numRounds),
            let expectedTotalMoney = expectedNetGain + startMoney]

mean :: Dist Double -> Double
mean (Dist distribution) = sum (map (\x -> (fst x) * (snd x)) distribution)

prExceeds :: Double -> Dist Double -> Double
prExceeds target dist = mean $ fmap (fromIntegral . fromEnum . (>= target)) dist

kellyCriterion p b = (p * (b-1) - (1-p))/(b-1)

recommendedBetRatio = kellyCriterion outcomeProbability odds
