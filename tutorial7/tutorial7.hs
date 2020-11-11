-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 13/14 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (com :#: coms) = split com ++ split coms
split Sit = []
split com = [com]

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join [x] = x
join (x:xs) = x :#: join xs

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent xs ys = split xs == split ys

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = join (split c) `equivalent` c

prop_split :: Command -> Bool
prop_split xs = all f (split xs)
  where
    f Sit = False
    f (_ :#: _) = False
    f _ = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n xs = join (concat (replicate n (split xs)))

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon len = copy 5 (Go len :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d n = copy n (Go d :#: Turn (360/fromIntegral n))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral 0 n m d = Sit
spiral s 0 m d = Sit
spiral s n m d = Go side :#: Turn d :#: (spiral (s + m) (n-1) m d)


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise c = join (xiao (filter (/= Turn 0) (xiao (filter (/= Go 0) (split c)))))
  where
    xiao [] = []
    xiao (Turn x:Turn y:xs) = xiao (Turn (x+y):xs)
    xiao (Go x:Go y:xs) = xiao (Go (x+y):xs)
    xiao (x:xs) = x : xiao xs



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x=f x
    where 
        f 0=GrabPen red :#: Go 10
        f x=g (x-1):#:p:#:f(x-1):#:p:#:g(x-1)
        g 0=GrabPen blue :#: Go 10
        g x=f (x-1):#:n:#:g(x-1):#:n:#:f(x-1)
        n=Turn 60
        p=Turn (-60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x= f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where 
        f 0=Go 10
        f x=f(x-1):#: p :#: f(x-1) :#: n :#: n :#: f(x-1):#: p :#: f(x-1) 
        n=Turn 60
        p=Turn (-60)
    

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined


