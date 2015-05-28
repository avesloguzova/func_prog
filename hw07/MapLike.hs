import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.
class MapLike m where
	empty :: m k v
	lookup :: Ord k => k -> m k v -> Maybe v
	insert :: Ord k => k -> v -> m k v -> m k v
	delete :: Ord k => k -> m k v -> m k v
	fromList :: Ord k => [(k, v)] -> m k v
	fromList [] = empty
	fromList ((key,value):xs) = insert key value (fromList xs)

-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

newtype ListMap k v = ListMap [(k,v)]
instance MapLike ListMap where
	empty = ListMap []
	lookup _ (ListMap []) = Nothing
	lookup k (ListMap ((key,value):xs)) = if k == key then Just value
	                                      else lookup k (ListMap xs)
	insert key value (ListMap xs) = ListMap $ insert' xs where
		insert' [] = [(key,value)]
		insert' (y:ys) = if key == (fst y) then (key,value):ys
	                     else  y:(insert' ys)
	delete key (ListMap xs) = ListMap $ filter (\(k,v) -> k /= key) xs

newtype ArrMap k v = ArrMap (k -> Maybe v)
instance MapLike ArrMap where
	empty = ArrMap $ \_ -> Nothing
	insert key value (ArrMap fn) = 
		ArrMap $ \x -> if key == x then Just value else fn x
	lookup key (ArrMap fn) = fn key
	delete key (ArrMap fn) = ArrMap $ \x -> if key == x then Nothing else fn x

-- 3. Написать instace Functor для ListMap k и ArrMap k.
instance Functor (ListMap k) where
	fmap fn (ListMap xs) = ListMap $ map (\(key,val) -> (key, fn val)) xs 
instance Functor (ArrMap k) where
	fmap fn (ArrMap m) = \key -> fmap fn (m key)
