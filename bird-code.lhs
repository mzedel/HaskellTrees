> module TreeTalk where
> import Prelude

Literate Haskell: Trees

1) Bäume
2) Operationen auf Bäumen
3) Map/ Fold... - Bsp. foldBtree
4) Show/ treeShow
5) Rose-Trees
6) Funktionssynthese
	toB
8) Anwendungsbeispiel: Red-Black-Trees
7) Anwendungsbeispiel: BinarySearchTrees
	Deforestation
		Bsp.: Quicksort

--------------------------------------------------------------------
-- Binärbäume
---------------------------------------------------------------------

> data Btree a = Leaf a | Fork (Btree a) (Btree a)


Größe und Höhe

Die Größe eines Baumes entspricht der Anzahl seiner Blätter. Die Anzahl der inneren Knoten entspricht immer der Anzahl der Blätter - 1.

> size (Leaf x) = 1
> size (Fork xt yt) = size xt + size yt

> nodes (Leaf x) = 0
> nodes (Fork xt yt) = 1 + nodes xt + nodes yt

Die Höhe eines Baumes entspricht seiner größten Schachtelungstiefe. Ein Baum, der nur aus einem Blatt besteht, hat die Höhe 0.

> height (Leaf x) = 0
> height (Fork xt yt) = 1 + max (height xt) (height yt)

> max 

Die Funktion "depths" errechnet die Tiefe eines Baumes und ersetzt dabei jeden Knoten durch seine entsprechende Höhe.

> depths = down 0
> down n (Leaf x) = Leaf n
> down n (Fork xt yt) = Fork (down (n+1) xt) (down (n+1) yt)

> flatten (Leaf x) = [x]
> flatten (Fork xt yt) = flatten xt ++ flatten yt


"map" und "fold"

Die oft eingesetzten Methoden "map" und "fold" funktionieren natürlich auch auf Bäumen. "map" wendet hierbei eine Funktion auf jedes Blatt des Baumes an, während "fold" eine Funktion für Blätter und eine für Knoten erwartet.

> mapBtree f (Leaf x) = Leaf (f x)
> mapBtree f (Fork xt yt) = Fork (mapBtree f xt) (mapBtree f yt)

> foldBtree f g (Leaf x) = f x
> foldBtree f g (Fork xt yt) = g (foldBtree f g xt) (foldBtree f g yt)

Damit lassen sich wieder einige bisherige Funktionen ausdrücken:

> size2 = foldBtree (const 1) (+)

> nodes2 = 1 + foldBtree (const 1) (+)

> height2 = foldBtree (const 0) (lmax)
>           where lmax m n = 1 + (max m n)

Zur Erinnerung: "height" war definiert als
< height (Leaf x) = 0
< height (Fork xt yt) = 1 + max (height xt) (height yt)

> flatten2 = foldBtree (\x -> [x]) (++)

Sogar "mapBtree" lässt sich mit Hilfe von "foldBtree" ausdrücken.

> mapBtree2 f = foldBtree (Leaf . f) Fork

---------------------------------------------------------------------
-- Show
---------------------------------------------------------------------

-- declare BinTree a to be an instance of Show
> instance (Show a) => Show (BinTree a) where
>	-- will start by a '<' before the root
>	-- and put a : a begining of line
>	show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
>	where
		-- treeshow pref Tree shows a tree and starts each line with pref
		-- We don't display the Empty tree
>		treeshow pref Empty = ""
		-- Leaf
>		treeshow pref (Node x Empty Empty) =
>					  (pshow pref x)
		-- Right branch is empty
>		treeshow pref (Node x left Empty) =
>					  (pshow pref x) ++ "\n" ++
>					  (showSon pref "`--" "   " left)
		-- Left branch is empty
>		treeshow pref (Node x Empty right) =
>					  (pshow pref x) ++ "\n" ++
>					  (showSon pref "`--" "   " right)
		-- Tree with left and right children non empty
>		treeshow pref (Node x left right) =
>					  (pshow pref x) ++ "\n" ++
>					  (showSon pref "|--" "|  " left) ++ "\n" ++
>					  (showSon pref "`--" "   " right)

		-- shows a tree using some prefixes to make it nice
>		showSon pref before next t =
>					  pref ++ before ++ treeshow (pref ++ next) t

		-- pshow replaces "\n" by "\n"++pref
>		pshow pref x = replace '\n' ("\n"++pref) (show x)

		-- replaces one char by another string
>		replace c new string =
>		  concatMap (change c new) string
>		  where change c new x
>			| x == c = new
>			| otherwise = x:[] -- "x"

---------------------------------------------------------------------
-- Funktionssynthese (S.198/199) Rose-Tree -> BTree
---------------------------------------------------------------------

Rose a = Node a [Rose a]

toB :: Rose a -> Btree a
toB (Node x xts) = foldl Fork (Leaf x) (map toB xts)
toB (Node x []) = Leaf x
toB (Node x (xts ++ [xt])) = Fork (toB (Node x xts)) (toB xt)
	
	
toB (Node x xts) = foldr (flip Fork) (Leaf x) (reverse (map toB xts))


Case (Node x []):
	toR (toB (Node x [])) = Node x []
	= {first Property of toB}
	toR (Leaf x) = Node x []
Case (Node x (xts ++ [xt])):
	toR (toB (Node x (xts ++ [xt]))) = Node x (xts ++ [xt])
	= {second Property of toB}
	toR (Fork (toB (Node x xts))) (toB xt) = Node x (xts ++ [xt])
	= {introducing xb = toB (Node x xts) and yb = toB xt}
	toR (Fork xb yb) = Node x (xts ++ [xt])
		where Node x xts = toR xb
				xt = toR yb

---------------------------------------------------------------------
-- Red-Black-Trees
---------------------------------------------------------------------


---------------------------------------------------------------------
-- Anwendungsbeispiel: BinarySearchTrees
---------------------------------------------------------------------
BinarySearchTrees

> data Stree a = Null | Fork (Stree a) a (Stree a)

Deforestation
~unnütz aufgebaute Bäume vermeiden
	Bsp.: um die Tiefe eines Baumes zu errechnen wird zunächst der Baum
		voll aufgebaut, danach bis zu den Blättern traversiert um die
		maximale Tiefe zu ermitteln.
< depth Leaf = 0
< depth (Node t1 t2) = 1 + max (depth t1) (depth t2)
		Besser:
< depth' n | n == 0 = 0
< depth' n = 1 + max (depth' n) (depth' n)
		
	Bsp. für die Nutzung von Deforestation & BinarySearchTrees: Quicksort
>		mkStree :: (Ord a) -> [a] -> Stree a
>		mkStree[] = Null
>		mkStree(x:xs) = Fork (mkStree ys) x (mkStree zs)
>							where (ys, zs) = partition (=< x) xs
		
>		partition :: (a -> Bool) -> [a] -> ([a], [a])
>		partition p xs = (filter p xs, filter (not p) xs)
		
>		flatten :: (Ord a) -> Stree a -> [a]
>		flatten Null = []
>		flatten (Fork xt x yt) = flatten xt ++ [x] ++ flatten yt
		
>		qSort :: (Ord a) -> [a] -> [a]
>		qSort = flatten mkStree
		
		vs.
		
>		qSort' :: (Ord a) -> [a] -> [a]
>		qSort' [] = []
>		qSort' (x:xs) = qSort' ys ++ [x] ++ qSort' zs
>					where (ys, zs) = partition (<= x) xs


---------------------------------------------------------------------
-- Klassenliste
---------------------------------------------------------------------
Problem: Zuordnung Namen, Matrikelnummern und Klausurnoten
Name			Matrikelnummer
Anderson		123456
Banderson		234561
Canderson		345612
Danderson		456123

Matrikelnummer	Note
123456			99
234561			96
345612			98
456123			97




Unendliche (ungeordnete) binäre Bäume:

> infiniTree = Leaf 0 infiniTree infiniTree

> takeDepth _ Empty = Empty
> takeDepth 0 _ = Empty
> takeDepth n (Leaf x left right) = Leaf x (takeDepth (n-1) left) (takeDepth (n-1) right)
