(fun main (List Int -> Int) (xs)
  (print (length xs) 0)
)

(fun length (List a -> Int) (xs)
  (case xs
    (nil 0)
    ((cons y ys) (add 1 (length ys)))
  )
)

(fun map ((a -> b) -> (List a -> List b)) (f)
  (lambda (List a -> List b) (xs)
    (case xs
      (nil nil)
      ((cons y ys) (cons (f y) (map f ys)))
    )
  )
)
