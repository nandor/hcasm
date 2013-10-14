

a :: Eq x => x -> x
a x = x


b :: (Eq x, Eq y, Eq z, Eq w) => x -> y -> z -> w -> x
b x y z w = x 
