> module TreeTalk where
> import Prelude

Literate Haskell: Trees

...................................................:+..........................
................................................:-.:+-.........................
........................................-:+-..-.-:+++:+-.......................
.........................-++-..::--++++:+:++:++*-+=**+:-.......................
........................-+++++:***:++*****+++++**++**+--:+:....................
...............::.....-+::*++*+++++***+++*==*+**==*=*****+:....................
...............-+++++***+*:--+***++*=*+**=*==#=*=#=***:---.....................
................:---*#***+**:*++****=#*====**==***=#*+-..---...................
............::+:-..-++#=*=*:-+***====*=*=#****====***=**++:::-::+-.............
............-:+:+=-:+##*#*==+***=*==#====+***=##@#===**#=*:-=++*+-:............
............-::::+***:-*==**==*=#==#@@@@@##+#=*=#==****+:--:.-+:*+-:-..........
........-::::+*++++=*++:**#==*=###@@###@###=##=#=*+==*=+*+**+-+++:--::.........
.......-:***::***++==***+++==*==*+@@###@@##+===+**#==++#*==*=*=#+-::...........
.........-*====**+=+****==#*==#@#@##=@@++##=##=#+=@###*=#=*==++-.--..-:+:......
......::+::**=*+*:*#=#=*====*=**==#**#@==###==#@#@@##@===***+**=#*++++:........
.........+****==##=#####*=##==#===+==##==*==+=***===#=*=###**=#**==*+++:--.....
.........:*=*==##**=###*#==*===#=*==**#@###=##=###==##*=###=+=-*==*=:+**+::....
..--....:+:*=##=**=#####=======#@##=-:#@=@==@=#=#+#@#==*=*+**=**+=#*++:........
.-:+:::-::**=#*#++*+*===+*=#=*=#@@##=#@W@@@@#==##=*=**==#==##-.:---:-*+:+::-...
..-+*+:+:*=###=####@*=#==@=+##=*=#@##=#@W@@@@##===*=#===#@*=##==##@@=*=#::-....
--:-+=*===#@#@###@@##=#@#@@@#*=#@#=#+*=@#W@####:-#@#@@#+###*++*##==##*-++-.....
....:+*+==*=###*@#@@@@@#@@@@#=@@@####+++==@**#==#@*+:=*:#@#::=+*...............
..--*+=######@#==@W#@#:-+=@+:+:=@@@=#*#####*@#@@#@@@*@@@#=*#+:-.-.--+*+-.......
+:-+:*=##==####=#=##=@#*+##@##@@@@@@@@##@@@@@##=*=*@@@@@@##+-+#*=*=*+*:-.......
..+#*+====+*####@@==@@@@#@==#@=@@@#=#@==#@*@#*=@####=@#*++*=#@@@###=*+*=*++:...
...-:*#####=##@@@@@@=+=#=@@@=#W@###=*#@WWW@#@W@@@#@@@@@=*##=##=**+==:-.---==#=+
.....+=:===:++*##@#@@##*#@@@:*++W=:*@@*#WW@W###+*==#@@@@@###=*#@@@#=:-=:.-:++++
..........:+*+**#@@@W@@=@@++==##@@@##*#WWWW##=##@@=*##=+*==*=##@###*-::-+*=:.--
....::-*===*#*###*=#@*:=#***#@####@@@@@@W@@#@@=@#*#*:+#*##:+##@#@@##=#=##+:....
.....::---+=@@+###@@#:=##*=*##=@@@@+@W@@@@=##@@@@==@#*-**#-+#:.....:-+.........
.............---*=+.++#@W@#=*=@@@@@=##WWW#@@@###@@##:...--..-..................
...............++:*=@@@@@==*---.:*+:#@WWW##@@@####:+##+........................
.............-*#@@=#@=:--:-:.:*+++--.+*WW:-.-.......-:*+.......................
.............::-.............-........@WW......................................
......................................@W@......................................
.....................................-WW@......................................
.....................................:WWW......................................
.....................................*WWW-.....................................
...................................:#WWWW@+-...................................

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

> data Btree a = Leaf a | Fork Int (Btree a) (Btree a)

Größe und Höhe

Die Größe eines Baumes entspricht der Anzahl seiner Blätter. Die Anzahl der inneren Knoten entspricht immer der Anzahl der Blätter - 1.

> size (Leaf x) = 1
> size (Fork n xt yt) = size xt + size yt

> nodes (Leaf x) = 0
> nodes (Fork n xt yt) = 1 + nodes xt + nodes yt

Die Höhe eines Baumes entspricht seiner größten Schachtelungstiefe. Ein Baum, der nur aus einem Blatt besteht, hat die Höhe 0.

> height (Leaf x) = 0
> height (Fork n xt yt) = 1 + max (height xt) (height yt)

-- > max 

Die Funktion "depths" errechnet die Tiefe eines Baumes und ersetzt dabei jeden Knoten durch seine entsprechende Höhe.

> depths = down 0
> down n (Leaf x) = Leaf n
> down n (Fork m xt yt) = Fork m (down (n+1) xt) (down (n+1) yt)

> flatten (Leaf x) = [x]
> flatten (Fork n xt yt) = flatten xt ++ flatten yt


"map" und "fold"

Die oft eingesetzten Methoden "map" und "fold" funktionieren natürlich auch auf Bäumen. "map" wendet hierbei eine Funktion auf jedes Blatt des Baumes an, während "fold" eine Funktion für Blätter und eine für Knoten erwartet.

> mapBtree f (Leaf x) = Leaf (f x)
> mapBtree f (Fork m r l) = Fork (f m) (mapBtree f l) (mapBtree f r)

> foldBtree f g (Leaf x) = g x
> foldBtree f g (Fork m l r) = f m (foldBtree f g l) (foldBtree f g r)

Damit lassen sich wieder einige bisherige Funktionen ausdrücken:

> size2 :: Btree a -> Int
> size2 = foldBtree (\_ l r -> (const 1)) (+)

> height2 = foldBtree (\_ m n -> 1 + max m n) (const 0)

> flatten2 = foldBtree (\n l r -> l ++ [n] ++ r) (\x -> x)

---------------------------------------------------------------------
-- Show
---------------------------------------------------------------------
beginnt mit "<" an der Wurzel und fügt ein ":" an den Anfang jeder Zeile

> instance (Show a) => Show (Btree a) where
>	show t = "< " ++ replace '\n' "\n: " (treeshow "" t) where
> --	treeshow pref Tree erzeugt einen Baum mit pref am Zeilenanfang
>		treeshow pref (Leaf x) = "" -- Leere Bäume werden nicht angezeigt
>		treeshow pref (Fork x (Leaf _) (Leaf _)) = --	Blätter
>					  (pshow pref x)
>		treeshow pref (Fork x left (Leaf _)) = --	Nur linke Äste anzeigen
>					  (pshow pref x) ++ "\n" ++
>					  (showSon pref "`--" "   " left)
>		treeshow pref (Fork x (Leaf _) right) = -- Nur rechte Äste anzeigen
>					  (pshow pref x) ++ "\n" ++
>					  (showSon pref "`--" "   " right)
>		treeshow pref (Fork x left right) = -- Astgabelungen anzeigen
>					  (pshow pref x) ++ "\n" ++
>					  (showSon pref "|--" "|  " left) ++ "\n" ++
>					  (showSon pref "`--" "   " right)

>		showSon pref before next t = -- Hilfsmethode um Platzhalter und Vorzeichen anzuzeigen
>					  pref ++ before ++ treeshow (pref ++ next) t

>		pshow pref x = replace '\n' ("\n" ++ pref) (show x) -- ersetzt "\n" mit "\n" ++ pref
> -- Stringersetzung
>		replace c new string = concatMap (change c new) string
>			where change c new x
>				| x == c = new
>				| otherwise = x:[] -- "x"

---------------------------------------------------------------------
-- Funktionssynthese (S.198/199) Rose-Tree -> Btree
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

> data Stree a = Null | StreeFork (Stree a) a (Stree a)
>	deriving (Eq, Ord, Show)

Deforestation
unnütz aufgebaute Bäume vermeiden
	Bsp.: um die Tiefe eines Baumes zu errechnen wird zunächst der Baum
		voll aufgebaut, danach bis zu den Blättern traversiert um die
		maximale Tiefe zu ermitteln.
< depth Leaf = 0
< depth (Node t1 t2) = 1 + max (depth t1) (depth t2)
Besser:
< depth' n | n == 0 = 0
< depth' n = 1 + max (depth' n) (depth' n)
		
Bsp. für die Nutzung von Deforestation und BinarySearchTrees: Quicksort


> mkStree	:: (Ord a) => [a] -> Stree a
> mkStree [] = Null
> mkStree (x:xs) = StreeFork (mkStree ys) x (mkStree zs)
>	where (ys, zs) = partition (< x) xs
		
> partition	:: (a -> Bool) => [a] -> ([a], [a])
> partition p xs = (filter p xs, filter (not.p) xs)

		
> sTreeflatten	:: (Ord a) => Stree a -> [a]
> sTreeflatten Null = []
> sTreeflatten (StreeFork xt x yt) = sTreeflatten xt ++ [x] ++ sTreeflatten yt
		
> qSort		:: (Ord a) => [a] -> [a]
> qSort = sTreeflatten.mkStree
		
		vs.
		
> qSort'	:: (Ord a) => [a] -> [a]
> qSort' [] = []
> qSort' (x:xs) = qSort' ys ++ [x] ++ qSort' zs
>	where (ys, zs) = partition (< x) xs


---------------------------------------------------------------------
-- Klassenliste
---------------------------------------------------------------------
-- Problem: Zuordnung Namen, Matrikelnummern und Klausurnoten
-- Name			Matrikelnummer
-- Anderson		123456
-- Banderson		234561
-- Canderson		345612
-- Danderson		456123

-- Matrikelnummer	Note
-- 123456			99
-- 234561			96
-- 345612			98
-- 456123			97

-- > type Name = String
-- > type Iden = Integer
-- > type Mark = Int
-- > type Rank = Int

-- > type Codes = [(Name, Iden)]
-- > type Marks = [(Iden, Mark)]
-- > type Ranks = [(Name, Mark, Rank)]

-- > classlist :: (Codes, Marks) -> Ranks
-- > classlist = rank collate

-- > collate :: (Codes, Marks) -> [(Name, Mark)]

-- > rank :: [(Name, Mark)] -> Ranks

-- > display :: Ranks -> String






-- Unendliche (ungeordnete) binäre Bäume:

-- > infiniTree = Leaf 0 infiniTree infiniTree

-- > takeDepth _ Empty = Empty
-- > takeDepth 0 _ = Empty
-- > takeDepth n (Leaf x left right) = Leaf x (takeDepth (n-1) left) (takeDepth (n-1) right)
