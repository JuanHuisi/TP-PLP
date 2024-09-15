module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

--import Test.HUnit


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
{- t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])] -}

-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []

procId :: Procesador a a
procId a = [a]

procCola :: Procesador [a] a
procCola l = if length l > 0 then tail l else []

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ children) = children

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAt (Tern _ a b c) = [a, b, c]

-- TODO: me parece que el TrieNodo Nothing esta raro
procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo Nothing b) = [Nothing]
procRaizTrie (TrieNodo t _ ) = [t]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo Nothing hijos) = hijos


--Ejercicio 2

foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b 
foldAT _ b Nil = b 
foldAT f b (Tern r i c d)  = f r (foldAT f b i)  (foldAT f b c) (foldAT f b d)

foldRose :: (a -> [RoseTree a] -> [b] -> b) -> RoseTree a -> b
foldRose fRose (Rose n hijos) = fRose n hijos (map (foldRose fRose) hijos)

-- data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
foldTrie :: (Maybe a -> ([(Char, b)] -> b)) -> Trie a -> b
foldTrie fTrie (TrieNodo v h) = fTrie v (map(\(b, subtrie) -> (b, foldTrie fTrie subtrie)) h)


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = foldr(\x acc -> [x] : acc) []

sufijos :: Procesador [a] [a]
sufijos = foldr (\c acc -> (c : if null (head acc) then [] else head acc) : acc) [[]];
--sufijos = foldr(\x acc -> if null(head acc) then x : acc else (x:head acc) : acc) [[]]

--Ejercicio 4
preorder :: AT a -> [a]
preorder = foldAT(\r i c d -> r : (i ++ c ++ d)) []


inorder :: AT a -> [a]
inorder = foldAT(\r i c d -> (i ++ c ++ [r] ++ d)) []

postorder :: AT a -> [a]
postorder = foldAT(\r i c d -> reverse (r : (d ++ c ++ i))) []
 
--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose(\r _ h -> r : concat h)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose(\r x h -> if length x == 0 then [r] else concat h )

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\r _ h -> if null h then [[r]] else concatMap (map (r:)) h);

--Ejercicio 6
caminos :: Trie a -> [[Char]]
caminos =  foldTrie(\v lst -> [""] ++ concatMap (\(c, sublist) -> map (c:) sublist) lst)

--Ejercicio 7
palabras :: Trie a -> [[Char]]
palabras = foldTrie((\v lst -> concatMap (\(c, sublist) -> map (c:) sublist) lst ++
  case v of
    Just _  -> [""]
    Nothing -> []))


--Ejercicio 8
-- 8.a) Procesador (RoseTree a) (RoseTree a)
{-
Rdo: Las funciones de los procesadores necesitan si o si que se le mande un input.
Entonces, necesito de alguna forma ese input con f. Si eso evalua en true, entonces aplico la funcion al input correspondiente, caso contrario, aplico la otra funcion al input.
Ej.: ifProc (\x -> x == ["a"]) (\x -> x ++ ["b"]) id ["a"] esperaria ["a", "b"]
-}
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f p1 p2 = \v -> if f v then p1 v else p2 v

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) p1 p2 = \v -> p1 v ++ p2 v

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) p1 p2 = \x -> concatMap p1 (p2 x)


--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}
{- 
main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  0             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  1     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  (0,0)       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  'a'      -- Caso de test 1 - expresión a testear
    ~=? 'a'            -- Caso de test 1 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  ""       -- Caso de test 1 - expresión a testear
    ~=? ""                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  0       -- Caso de test 1 - expresión a testear
    ~=? 0                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  False       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
 -}
