enqueue
q ->  a  -> q
q ->  a  -> Either q (q, a)
q ->  a  -> (q, Maybe a)
q -> [a] -> (q, [a])

dequeque
q -> a
q -> Either q (q, a)
q -> (q, Maybe a)
q -> (q, [a])

q -> a -> (q, Maybe a) -- maybe discarded item
q ->      (q, Maybe a) -- maybe next      item


---

Exercise on denotational design, rethinking queues

* what is denotational design
* alternative view on queues
* discovering properties of sorting function
* minimal queue implementation

q a :: [a] -> [a]

functor should transform each item into b
so you enqueue a but dequeue b

fmap f q = f . q

contravariant transforms each input into a
so you enqueue b but dequeue a

comap f q = q . f

profunctor allows to transform both enqueued and dequeued elements

dimap f g q = f . q . g

applicative allows to apply queue of functions to queue of items pointwise

order can be encoded in data structure like in linked list

does queue form a monad or comonad ?

comonad duplicate generates queue of all queue tails
