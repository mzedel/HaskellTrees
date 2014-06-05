Helfer: Binärbäume

> data Btree a = Leaf a | Fork (Btree a) (Btree a)
> showBtree (Leaf a) = show a
> showBtree (Fork xt yt) = "("++ (showBtree xt) ++" "++ (showBtree yt) ++")"


#####################################
#                                   #
#  RoseTrees und Programmsynthese  #
#                                   #
#####################################

Das besondere an RoseTrees ist die unbegrenzte Anzahl an Söhnen an jedem Knoten. Daraus ergibt sich auch die Definition von "Knoten" und "Blatt": Knoten haben eine nicht-leere Liste von Söhnen, Blätter haben eine leere Liste von Söhnen.

> data Rose a = NodeR a [Rose a]
> roseTree = NodeR 1 [NodeR 2 [NodeR 5 []],NodeR 3 [],NodeR 4 []]

Natürlich lassen sich die üblichen Baum-Operationen auch auf RoseTrees definieren.

> foldRose :: (a -> [b] -> b) -> Rose a -> b
> foldRose f (NodeR x xts) = f x (map (foldRose f) xts)

> flattenR :: Rose a -> [a]
> flattenR (NodeR x xts) = x : concat (map flattenR xts)

> size = foldRose f where f x ns = 1 + sum ns

Zwischen Binärbäumen und RoseTrees existiert eine Abbildung, die sogar ein-eindeutig ist. Die Abbildung eines RoseTrees in einen Binärbaum ist wie folgt definiert:

> toB :: Rose a -> Btree a
> toB (NodeR x xts) = foldl Fork (Leaf x) (map toB xts)

Die Umkehrfunktion toR ist schwieriger zu implementieren. Anstatt sich selbst eine Funktionsdefinition auszudenken kann diese über Programmsynthese gebaut werden.

< toR :: Btree a -> Rose a
< toR (toB xt) = xt

Um eine Funktionsdefinition über Programmsynthese zu erstellen benötigen wir zwei Eigenschaften der bereits bekannten Funktion toB:

< toB (Node x []) = Leaf x
< toB (Node x (xts ++ [xt])) = Fork (toB (Node x xts)) (toB xt)

-----------------------
-- Fall 1: Node x [] --
-----------------------

< toR (toB (Node x [])) = Node x []
= { erste Eigenschaft von toB }
< toR (Leaf x) = Node x []

----------------------------------
-- Fall 2: Node x (xts ++ [xt]) --
----------------------------------

< toR (toB (Node x (xts ++ [xt]))) = Node x (xts ++ [xt])
= { zweite Eigenschaft von toB }
< toR (Fork (toB (node x xts)) (toB xt)) = Node x (xts ++ [xt])
= { definiere xb = toB (Node x xts) und yb = toB xt }
< toR (Fork xb yb) = Node x (xts ++ [xt])
< where Node x xts = toR xb
<       xt         = toR yb

Damit ist toR, die Umkehrfunktion von toB, vollständig errechnet worden. Dieses Vorgehen heißt Programmsynthese.

Zu Beginn der Programmsynthese haben wir uns auf zwei Eigenschaften von toB gestützt. Diese Eigenschaften sind natürlich nicht aus der Luft gegriffen, sondern können bewiesen werden. Beide gehen zurück auf die Definition von toB:

< toB :: Rose a -> Btree a
< toB (NodeR x xts) = foldl Fork (Leaf x) (map toB xts)

Für xts werden zwei Fälle unterschieden: Fall 1 (xts ist leer) und Fall 2 (xts besitzt mindestens ein Element). Fall 1 zuerst.

< toB (Node x []) = foldl Fork (Leaf x) (map toB [])
< = { def. map }
< toB (Node x []) = foldl Fork (Leaf x) []
< = { def. foldl }
< toB (Node x []) = Leaf x

Fall 2 stützt sich auf das dritte duality theorem, das besagt:

< foldr op u xs = foldl (flip op) u (reverse xs)

Angewendet auf toB bedeutet das:

< toB (NodeR x (xts ++ [xt])) = foldl Fork (Leaf x) (map toB (xts ++ [xt]))
< = { duality theorem }
< toB (NodeR x (xts ++ [xt])) = foldr (flip Fork) (Leaf x) (reverse (map toB (xts ++ [xt])))
< = { commutativity of reverse and map }
< toB (NodeR x (xts ++ [xt])) = foldr (flip Fork) (Leaf x) (map toB (reverse (xts ++ [xt])))
< = { solve reverse }
< toB (NodeR x (xts ++ [xt])) = foldr (flip Fork) (Leaf x) (map toB ([xt] ++ reverse xts))
< = { solve map }
< toB (NodeR x (xts ++ [xt])) = foldr (flip Fork) (Leaf x) ([toB xt] ++ map toB (reverse xts))
< = { solve foldr }
< toB (NodeR x (xts ++ [xt])) = toB xt `(flip Fork)` (foldr (flip Fork) (Leaf x) (map toB (reverse xts)))
< = { put (flip Fork) in front }
< toB (NodeR x (xts ++ [xt])) = (flip Fork) (toB xt) (foldr (flip Fork) (Leaf x) (map toB (reverse xts)))
< = { solve flip }
< toB (NodeR x (xts ++ [xt])) = Fork (foldr (flip Fork) (Leaf x) (map toB (reverse xts))) (toB xt)
< = { duality theorem }
< toB (NodeR x (xts ++ [xt])) = Fork (foldl Fork (Leaf x) (map toB xts)) (toB xt)
< = { rev. def. toB }
< toB (NodeR x (xts ++ [xt])) = Fork (toB (NodeR x xts)) (toB xt)

Damit sind die Herleitung beider Fälle gezeigt, die beiden verwendeten Eigenschaften von toB bewiesen und somit die Programmsynthese abgeschlossen.


#####################
#                   #
#  Red-Black-Trees  #
#                   #
#####################

Bei Red-Black-Trees handelt es sich um eine Spezialform von binären Suchbäumen, die zwar ausbalanciert sind, in ihrer Balance aber nicht so strikt wie beispielsweise AVL-Trees (Set-Vortrag). Alle Operationen (einfügen, lesen, entfernen) haben eine Laufzeit von log(n). Red-Black-Trees werden unter anderem im Linux-Scheduler eingesetzt. Jeder Knoten in einem Red-Black-Tree besitzt eine Farbe (Rot oder Schwarz), die Wurzel ist immer schwarz, Blätter sind immer schwarz und besitzen kein Datum.

Für Red-Black-Trees gelten die folgenden Regeln:


data Color = R | B
data RB a = RBLeaf | RBTree Color (RB a) a (RB a)

balance (RBTree)