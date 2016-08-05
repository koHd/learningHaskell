-- simple game of hangman (guess the word via a sequence of letter guesses)
{- in each round:
	: display secret word to the player with unknown letters encoded as underscore
	: if all letters are known, player WON and game over
	: if player's lives == 0, player LOST and game over
	: else the player guesses a letter
	:: if the letter is in the secret word, the letter becomes a known letter in the secret word
	:: else the player loses a life
	: go to next round -}
	
-- later on the I/O functionality will go in main to make the game playable from command line
-- main = do
	-- putStrLn "Play Hangman!"

-- a round of hangman
playHangman :: (Show a, Ord a, Num a) => a -> a -> [Char] -> [Char] -> IO ()
playHangman _ _ [] _ = error "no secret word set"
playHangman _ 0 _ _ = putStrLn "You didn't get the secret this time. Better luck next time!"
playHangman round lives secret guesses 
	| secret == known = do
		putStrLn ("You Win! The word is: " ++ known)
	| otherwise = do
		putStrLn "-----"
		putStrLn ("Round: " ++ (show round))
		putStrLn ("Known so far: " ++ known)
		putStrLn ("Lives: " ++ (show lives))
		putStrLn "Make a guess: "
		guess <- getLine
		let newLives = if letterIsInWord (head guess) secret then lives else lives-1
		playHangman (round+1) newLives secret ((head guess):guesses)
	where known = hideUnknownLetters secret guesses

-- hide the unknown letters in the secret
hideUnknownLetters :: [Char] -> [Char] -> [Char]
hideUnknownLetters [] _ = error "no secret word"
hideUnknownLetters secret guesses = [if c `elem` guesses then c else '_' | c <- secret]

-- check if guess is in the secret
letterIsInWord :: Char -> [Char] -> Bool
letterIsInWord c [] = error "no secret word"
letterIsInWord c secret = if c `elem` secret then True else False
		