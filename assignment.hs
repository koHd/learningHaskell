import Dist

numOutcomes = 4
outcomeProbability = 1/numOutcomes
loseProbability = 1 - outcomeProbability
betAmount totalMoney percentageToBet = (totalMoney / 100) * percentageToBet
winAmount bet = 10 * bet

expectedReturn :: Double -> Double -> Double -> Double
expectedReturn totalMoney percentageToBet probability = 
    probability * winAmount (betAmount totalMoney percentageToBet)

possibleOutcomes :: [(Double, Double)]
possibleOutcomes = [(outcome, outcomeProbability) | outcome <- [0..(numOutcomes-1)]]

choose :: Double -> Double -> Double
choose trials 0 = 1
choose 0 successes = 0
choose trials successes = choose (trials-1) (successes-1) * trials / successes 

binomialProbability :: Double -> Double -> Double -> Double
binomialProbability trials successes probability = 
    (choose trials successes) * (probability**successes) * ((1-probability) ** (trials - successes))

binomialDistribution :: Double -> Double -> Dist Double
binomialDistribution trials probability = 
    Dist [(successes, (binomialProbability trials successes probability)) | 
        successes <- [0..(trials-1)]] 

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel startMoney percentageToBet rounds = 
    Dist [x | x <- [0..percentageToBet]] 

mean :: Dist Double -> Double
mean (Dist distribution) = sum (map (\x -> (fst x) * (snd x)) distribution)

