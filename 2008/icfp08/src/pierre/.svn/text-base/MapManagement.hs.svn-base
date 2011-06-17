module MapManagement where
import ParseMsg

type World =  [Obj]

-- For now: naively adds the last object to the list
updateWorld :: World -> Obj -> World
updateWorld w obj@(Obj t pos r)
    | elem obj w = obj : w
updateWorld w _ = w


-- Given 2 objects, return the one that encompasses the other
chooseObj :: Obj -> Obj -> Obj
chooseObj o1@(Obj t1 (x1, y1) r1) o2@(Obj t2 (x2, y2) r2)
	| r1 > dist + r2 = o1
        | otherwise = o2
        where
          xd = x2 - x1
	  yd = y2 - y1
	  dist = sqrt (xd*xd + yd*yd)


distanceSqr (x1, y1) (x2, y2) = (x1 - x2)**2 + (y1 - y2)**2

inCircle ( pc , r) pp = d < (r*r)
    where d = distanceSqr pc pp
