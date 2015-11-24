sum_of_multiples_of_three_and_five_under_y y = sum [x|x<-[0..y-1], x `mod` 3 == 0 || x `mod` 5 == 0]

fibs :: [Int]
fibs = 0 : 1 : [ a + b | (a,b) <- zip fibs (tail fibs)]

sum_of_even_fibonacci_numbers = sum [x|x<-take 60 fibs, x < 4000000, even x]

nth_prime n = take n [x | x<-[1..], x `mod` [2..x]]