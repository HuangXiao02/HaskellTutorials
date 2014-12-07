-- Informatics 1 - Functional Programming
-- Tutorial 8
--
-- Week 11 - due: 28/29 Nov.

import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int]
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (u, _, _, _, _) = u
alph   (_, a, _, _, _) = a
start  (_, _, s, _, _) = s
final  (_, _, _, f, _) = f
trans  (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm st tr = deltaHelper (trans fsm) st tr
  where
    deltaHelper :: (Eq q) => [Transition q] -> q -> Char -> [q]
    deltaHelper [] _ _ = []
    deltaHelper ((s,l,f):xs) sta ch
      | s == sta && l == ch  = f : deltaHelper xs sta ch
      | otherwise            = deltaHelper xs sta ch

-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs
  where
    acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
    acceptsFrom m q []     = q `elem` final m
    acceptsFrom m q (x:xs) = any (\q' -> acceptsFrom m q' xs) (delta m q x)
    -- acceptsFrom m q (x:xs) = or [acceptsFrom m q' xs | q' <- delta m q x ]


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = nub . sort


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m qs s = canonical $ concat (map (\q -> delta m q s) qs)


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m qs = canonical $ [ ddelta m x y | x <- qs, y <- alph m ] ++ qs


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m qs
  | next m qs /= qs = reachable m (next m qs)
  | otherwise = qs


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m qss = filter (\qs -> any (\f -> elem f qs) (final m)) qss


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm states = [ (y,x, (ddelta fsm y x)) | y <- states, x <- alph fsm ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (states, alph m, [start m], dfinal m states, dtrans m states)
  where
    states = reachable m [[start m]]


