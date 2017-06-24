module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp { empFun = ef} (GL gl fun) = GL (e:gl) (fun + ef)

instance Monoid GuestList where
    mempty   = GL [] 0
    mappend (GL gl1 f1) (GL gl2 f2) = GL (gl1 ++ gl2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
    | f1 > f2   = gl1
    | otherwise = gl2
