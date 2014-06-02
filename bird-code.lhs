> module TreeTalk where
> import Prelude

Literate Haskell: Trees

1) Erzeugung von Bäumen
2) Operationen auf Bäumen
3) Map/ Filter/ Zip/ Fold/ Scan/... - Bsp. foldBtree
6) Funktionssynthese
	toB
7) Show 
7) Anwendungsbeispiel: BinarySearch
	BinarySearchTrees
	Deforestation
		Bsp.: Quicksort
8) Anwendungsbeispiel: Red-Black-Trees

---------------------------------------------------------------------
-- Binärbäume
---------------------------------------------------------------------

> data Btree a = Leaf a | Fork (Btree a) (Btree a)

> showBtree (Leaf a) = show a
> showBtree (Fork xt yt) = "("++ (showBtree xt) ++" "++ (showBtree yt) ++")"

> btree = (Fork (Leaf 3) (Fork (Fork (Leaf 4) (Leaf 5)) (Leaf 6)))

Und umgekehrt am Beispiel von binären Bäumen:

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList [] = Empty
> treeFromList (x:xs) = Leaf x (treeFromList (filter (<x) xs))
								(treeFromList (filter (>x) xs))

Bäume lassen sich auch auf Listen abbilden:

> flatten (Leaf x) = [x]
> flatten (Fork xt yt) = flatten xt ++ flatten yt

Größe und Höhe

Die Größe eines Baumes entspricht der Anzahl seiner Blätter. Die Anzahl der inneren Knoten entspricht immer der Anzahl der Blätter - 1.

> size (Leaf x) = 1
> size (Fork xt yt) = size xt + size yt

Die Höhe eines Baumes entspricht seiner größten Schachtelungstiefe. Ein Baum, der nur aus einem Blatt besteht, hat die Höhe 0.

> height (Leaf x) = 0
> height (Fork xt yt) = 1 + max (height xt) (height yt)

Díe Funktion "depths" errechnet die Tiefe eines Baumes und ersetzt dabei jeden Knoten durch seine entsprechende Höhe.

> depths = down 0

> down n (Leaf x) = Leaf n
> down n (Fork xt yt) = Fork (down (n+1) xt) (down (n+1) yt)

"map" und "fold"

Die oft eingesetzten Methoden "map" und "fold" funktionieren natürlich auch auf Bäumen. "map" wendet hierbei eine Funktion auf jedes Blatt des Baumes an, während "fold" eine Funktion für Blätter und eine für Knoten erwartet.

> mapBtree f (Leaf x) = Leaf (f x)
> mapBtree f (Fork xt yt) = Fork (mapBtree f xt) (mapBtree f yt)

> foldBtree f g (Leaf x) = f x
> foldBtree f g (Fork xt yt) = g (foldBtree f g xt) (foldBtree f g yt)

Damit lassen sich wieder einige bisherige Funktionen ausdrücken:

> size2 = foldBtree (const 1) (+)

> height2 = foldBtree (const 0) (lmax)
>           where lmax m n = 1 + (max m n)

Zur Erinnerung: "height" war definiert als
< height (Leaf x) = 0
< height (Fork xt yt) = 1 + max (height xt) (height yt)

> flatten2 = foldBtree (\x -> [x]) (++)

Sogar "mapBtree" lässt sich mit Hilfe von "foldBtree" ausdrücken.

> mapBtree2 f = foldBtree (Leaf . f) Fork

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
-- Show
---------------------------------------------------------------------

	Data.Tree -> drawTree

---------------------------------------------------------------------
-- Anwendungsbeispiel: BinarySearch
---------------------------------------------------------------------
BinarySearchTrees

> data Stree a = Null | Fork (Stree a) a (Stree a)

flatten :: (Ord a) -> Stree a -> [a]
flatten Null = []
flatten (Fork xt x yt) = flatten xt ++ [x] ++ flatten yt

Deforestation
~unnütz aufgebaute Bäume vermeiden
Bsp.:	um die Tiefe eines Baumes zu errechnen wird zunächst der Baum
		voll aufgebaut, danach bis zu den Blättern traversiert um die
		maximale Tiefe zu ermitteln.
		(-- depth Leaf = 0 --)
		(-- depth (Node t1 t2) = 1 + max (depth t1) (depth t2) --)
		Besser:
		(-- depth' n | n == 0 = 0 --)
		(-- depth' n = 1 + max (depth' n) (depth' n) --)
		
Bsp. für die Nutzung von BinarySearchTrees: Quicksort
		partition :: (a -> Bool) -> [a] -> ([a], [a])
		partition p xs = (filter p xs, filter (not p) xs)
		
		mkStree :: (Ord a) -> [a] -> Stree a
		mkStree[] = Null
		mkStree(x:xs) = Fork (mkStree ys) x (mkStree zs)
							where (ys, zs) = partition (=< x) xs
		
		sort :: (Ord a) -> [a] -> [a]
		sort = flatten mkStree


---------------------------------------------------------------------
-- Red-Black-Trees
---------------------------------------------------------------------


---------------------------------------------------------------------
-- Klassenliste
---------------------------------------------------------------------



Unendliche (ungeordnete) binäre Bäume:

> infiniTree = Leaf 0 infiniTree infiniTree

> takeDepth _ Empty = Empty
> takeDepth 0 _ = Empty
> takeDepth n (Leaf x left right) = Leaf x (takeDepth (n-1) left) (takeDepth (n-1) right)
