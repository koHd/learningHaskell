-- simple game of hangman (guess the word via a sequence of letter guesses)
{- in each round:
	: display secret word to the player with unknown letters encoded as underscore
	: if all letters are known, player WON and game over
	: if player's lives == 0, player LOST and game over
	: else the player guesses a letter
	:: if the letter is in the secret word, the letter becomes a known letter in the secret word
	:: else the player loses a life
	: go to next round -}

-- hide the unknown letters in the secret
hideUnknownLetters :: [Char] -> [Char] -> [Char]
hideUnknownLetters [] _ = error "no secret word"
hideUnknownLetters secret guesses = [if c `elem` guesses then c else '_' | c <- secret]

-- check if guess is in the secret
checkGuess :: Char -> [Char] -> Bool
checkGuess c [] = error "no secret word"
checkGuess c secret = if c `elem` secret then True else False

playHangman :: (Eq a, Num a) => a -> a -> [Char] -> [Char] -> [Char]
playHangman _ _ [] _ = error "no secret word set"
playHangman _ 0 _ _ = "You lose. Game over. "
playHangman _ lives secret guesses 
	| secret == hideUnknownLetters secret guesses = "You win"
	| otherwise = "Nope"
