data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append <> (tag l1) (tag l2) l1 l2
