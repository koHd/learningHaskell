doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

numbers = [1,2,3,4]

moreNumbers = 5 : 6 : 7 : 8 : []

allTheNumbers = numbers ++ moreNumbers

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

numbersEnglish = zip numbers ["one", "two", "three", "four"]

rightTriangles = [ (a,b,c) | c <- [1..10], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2 ]
