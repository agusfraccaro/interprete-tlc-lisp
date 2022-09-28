(ns tp-final.core-test
  (:require [clojure.test :refer :all]
            [tp-final.core :refer :all]))

(deftest controlar-aridad-test
  (testing "Lista de 3 elementos controlo que tenga aridad 3 y devuelve 3"
    (is (= 3 (controlar-aridad '(1 2 3) 3)))
  )

  (testing "Lista de 2 elementos digo que tiene aridad 1 da error"
    (is (= (list (symbol "*error*") 'too-many-args) (controlar-aridad '(1 2) 1)))  
  )

  (testing "Lista de 2 elementos digo que tiene aridad 3 da error"
    (is (= (list (symbol "*error*") 'too-few-args) (controlar-aridad '(1 2) 3)))
  )
)

(deftest igual?-test
  (testing "Mismo numero son iguales, distintos no."
    (is (true? (igual? 1 1)))
    (is (false? (igual? 1 2)))  
  )
  
  (testing "Mismos simbolos son iguales, distintos no. Case insensitive."
    (is (true? (igual? 'a 'a)))
    (is (true? (igual? 'a 'A)))
    (is (false? (igual? 'a 'b)))  
  )
  (testing "Listas con mismos elementos en mismo orden son iguales, case insensitive."
    (is (true? (igual? '(1 2 3) '(1 2 3))))
    (is (true? (igual? '(A B C) '(a b c))))
    (is (true? (igual? '(a B c) '(A b C))))
    (is (false? (igual? '(a b c) '(b c a))))
  )

  (testing "Los valores nil, lista vacia y simbolo NIL son iguales"
    (is (true? (igual? nil nil)))
    (is (true? (igual? nil 'NIL)))
    (is (true? (igual? nil '())))
    (is (true? (igual? 'NIL '())))
    (is (true? (igual? 'NIL 'NIL)))
    (is (false? (igual? nil '(nil))))
    (is (false? (igual? 'NIL '(nil))))
  )

  (testing "Strings son iguales, case sensitive"
    (is (true? (igual? "a" "a")))
    (is (false? (igual? "a" "A")))   
  )

  (testing "Simbolos y strings no son iguales"
    (is (false? (igual? 'a "a")))  
  )
)

(deftest error?-test
  (testing "Un error es una lista si su primer elemento es *error*. Case insensitive"
    (is (true? (error? '(*error* too-few-args))))
    (is (true? (error? (list '*error* 'too-few-args))))
    (is (true? (error? (list '*error*))))
    (is (true? (error? (list '*ERROR*))))
    (is (false? (error? ())))
    (is (false? (error? nil)))
    (is (false? (error? '*error*)))
  )   
)

(deftest revisar-fnc-test
  (testing "Mando error, devuelve el error"
    (is (= (revisar-fnc '(*error* too-few-args)) (list (symbol "*error*") 'too-few-args)))
    (is (= (revisar-fnc '(*error* too-many-args)) (list (symbol "*error*") 'too-many-args)))
  ) 

  (testing "Si no es un error, devuelve nil"
    (is (nil? (revisar-fnc '(too-few-args))))
    (is (nil? (revisar-fnc ())))
    (is (nil? (revisar-fnc nil)))
  )
)

(deftest buscar-test
  (testing "Devuelve el valor asociado a la clave."
    (is (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3))
    (is (= (buscar 'a '(a 1 b 2 c 3 d 4 e 5)) 1))
    (is (= (buscar 'e '(a 1 b 2 c 3 d 4 e 5)) 5))
  ) 

  (testing "Clave que no está en la lista da error."
    (is (error? (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))  
  )
)

(deftest actualizar-amb-test
  (testing "Clave que no está en el ambiente se agrega al final de la lista"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))
  )

  (testing "Si el valor es un error devuelve el ambiente sin modificar"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd (list (symbol "*error*"))) '(a 1 b 2 c 3)))   
  )

  (testing "Si la clave ya existe, se modifica el valor por el nuevo"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'a 58) '(a 58 b 2 c 3)))  
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'c "Hola") '(a 1 b 2 c "Hola")))
  )
)

(deftest fnc-env-test
  (testing "Si el primer argumento no está vacío, devuelve error."
    (is (= (fnc-env '(5) '(a 1 b 2) '(c 3 d 4)) (list (symbol "*error*") 'too-many-args)))  
    (is (= (fnc-env '(6 5) '(a 1 b 2) '(c 3 d 4)) (list (symbol "*error*") 'too-many-args)))
  )

  (testing "Si el primer argumento está vacío, devuelve la fusión de los dos ambientes."
    (is (= (fnc-env () '(a 1 b 2) '(c 3 d 4)) '(a 1 b 2 c 3 d 4)))      
  )
)

(deftest fnc-equal-test
  (testing "Da error si la lista tiene mas o menos de 2 elementos"
    (is (= (fnc-equal '(A)) (list (symbol "*error*") 'too-few-args)))
    (is (= (fnc-equal '()) (list (symbol "*error*") 'too-few-args)))
    (is (= (fnc-equal '(A a A)) (list (symbol "*error*") 'too-many-args)))    
  )

  (testing "Lista con dos elementos iguales da 't'. Case insensitive."
    (is (= (fnc-equal '(1 1)) (symbol "t")))
    (is (= (fnc-equal '(a a)) (symbol "t")))
    (is (= (fnc-equal '(a A)) (symbol "t")))
    (is (= (fnc-equal '(nil NIL)) (symbol "t")))
  )

  (testing "Lista con dos elementos distintos da nil."
    (is (nil? (fnc-equal '("1" 1))))
    (is (nil? (fnc-equal '(A B))))    
  )
)

(deftest fnc-add-test
  (testing "Suma elementos de listas de cualquier longitud"
    (is (= (fnc-add '(1 2 3)) 6))
    (is (= (fnc-add '(2 2)) 4))
    (is (= (fnc-add '(1 1 1 1 1 1 1 1 1 1)) 10))  
  ) 
  
  (testing "Enviarle lista con 0 o 1 elementos da error"
    (is (error? (fnc-add '())))
    (is (error? (fnc-add '(1))))  
  )

  (testing "Enviarle lista con algún elemento que no es número da error"
    (is (error? (fnc-add '(A 1 2))))
    (is (error? (fnc-add '(1 C 3))))
    (is (= (fnc-add '(1 C 3 A B C)) '(*error* number-expected C)))
    (is (error? (fnc-add '(1 2 3 4 5 8 7 4 5 9 D))))  
  )
)

(deftest fnc-sub-test
  (testing "Enviar lista sin elementos da error"
    (is (error? (fnc-sub '())))  
  )  

  (testing "Enviar lista con algún elemento que no es número da error"
    (is (error? (fnc-sub '(A 4 1 5))))
    (is (error? (fnc-sub '(4 4 A 5))))
    (is (error? (fnc-sub '(1 2 3 4 5 8 7 4 5 9 D))))
  )

  (testing "Enviar una lista con un solo elemento devuelve el elemento con el signo contrario."
    (is (= (fnc-sub '(3)) -3))
    (is (= (fnc-sub '(-3)) 3))
  )

  (testing "Resta los elementos de una lista de cualquier longitud."
    (is (= (fnc-sub '(1 1)) 0))
    (is (= (fnc-sub '(3 1 1)) 1))
    (is (= (fnc-sub '(20 1 1 1 1 1 1 1 1 1 1 1 1)) 8))  
    (is (= (fnc-sub '(1 5)) -4))
  )
)

(deftest fnc-reverse-test
  (testing "Lista con cantidad de argumentos distinto de lo requerido devuelve error."
    (is (error? (fnc-reverse ())))
    (is (error? (fnc-reverse '((1 2 3)(4)) )))
  )  

  (testing "Si el argumento no es lista de listas devuelve error."
    (is (error? (fnc-reverse '(1))))
    (is (error? (fnc-reverse '(A))))  
  )

  (testing "Devuelve la lista revertida correctamente."
    (is (= (fnc-reverse '((1 2 3))) '(3 2 1)))
    (is (= (fnc-reverse '((1)) ) '(1)))  
  )
)

(deftest fnc-append-test
  (testing "Devuelve error si el argumento tiene mas o menos de un elemento."
    (is (error? (fnc-append '( (1 2) ))))
    (is (error? (fnc-append '( (1 2) (3) (4 5) (6 7) ))))
    (is (error? (fnc-append '( (1 2) 3 ))))
    (is (error? (fnc-append '( (1 2) A ))))  
  )  

  (testing "Casos que da nil."
    (is (nil? (fnc-append '(nil nil))))
    (is (nil? (fnc-append '(nil ()))))
    (is (nil? (fnc-append '(() ()))))
    (is (nil? (fnc-append '(nil NIL))))
  )

  (testing "Concatena dos listas."
    (is (= (fnc-append '( (1 2) (3))) '(1 2 3)))
    (is (= (fnc-append '( (1 2) nil )) '(1 2)))
    (is (= (fnc-append '( () (1 2) )) '(1 2)))
  )
)

(deftest evaluar-escalar-test
  (testing "Enviar algo distinto a un símbolo devuelve una lista con el número y el ambiente global"
    (is (= (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 32 '(v 1 w 3 x 6))))  
    (is (= (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "chau" '(v 1 w 3 x 6))))
  )  

  (testing "Evaluar un símbolo que está en el ambiente local devuelve el valor asociado a ese símbolo y el ambiente global."
    (is (= (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "hola" '(v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list "hola" '(v 1 w 3 x 6))))  
  )

  (testing "Evaluar un símbolo que está en el ambiente global devuelve el valor asociado a ese simbolo y el ambiente global."
    (is (= (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 3 '(v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'W '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 3 '(v 1 w 3 x 6))))  
  )

  (testing "Evaluar un símbolo que está en ambos ambientes devuelve el valor asociado al símbolo en el ambiente local y el ambiente global."
    (is (= (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 5 '(v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'X '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) (list 5 '(v 1 w 3 x 6))))  
  )

  (testing "Evaluar un escalar que no se encuentra en ningún ambiente da error."
    (is (error? (first (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  )
)

(deftest fnc-terpri-test
  (testing "Devuelve siempre nill."
    (is (nil? (fnc-terpri '())))  
  ) 
  
  (testing "Devuelve error si se le pasa como parámetro una lista con elementos."
    (is (error? (fnc-terpri '(1))))
    (is (error? (fnc-terpri '(1 2))))  
  )

  (testing "Genera un salto de línea."
    (is (= "\n" (with-out-str (fnc-terpri '()))))  
  )
)

(deftest fnc-lt-test
  (testing "Devuelve nil si el primer numero es mayor que el segundo"
    (is (nil? (fnc-lt '(2 1))))
    (is (nil? (fnc-lt '(558 2))))  
  )  
  
  (testing "Devuelve error si la lista enviada tiene una cantidad de elementos distinta de 2."
    (is (error? (fnc-lt '())))
    (is (error? (fnc-lt '(1 2 3))))  
    (is (error? (fnc-lt '(1 2 3 4 5))))
  )

  (testing "Devuelve error si alguno de los elementos no es un numero."
    (is (error? (fnc-lt '(A 1))))
    (is (error? (fnc-lt '(2 A))))  
  )
  
  (testing "Devuelve 't' si el primer numero es menor que el segundo."
    (is (= (fnc-lt '(1 2)) 't))
    (is (= (fnc-lt '(199 200)) 't))  
  )
)

(deftest fnc-gt-test
  (testing "Devuelve nil si el primer numero es menor que el segundo"
    (is (nil? (fnc-gt '(1 2))))
    (is (nil? (fnc-gt '(2 558))))  
  )  
  
  (testing "Devuelve error si la lista enviada tiene una cantidad de elementos distinta de 2."
    (is (error? (fnc-gt '())))
    (is (error? (fnc-gt '(1 2 3))))  
    (is (error? (fnc-gt '(1 2 3 4 5))))
  )

  (testing "Devuelve error si alguno de los elementos no es un numero."
    (is (error? (fnc-gt '(A 1))))
    (is (error? (fnc-gt '(2 A))))  
  )
  
  (testing "Devuelve 't' si el primer numero es mayor que el segundo."
    (is (= (fnc-gt '(2 1)) 't))
    (is (= (fnc-gt '(201 200)) 't))  
  )
)

(deftest fnc-ge-test
  (testing "Devuelve nil si el primer numero es menor que el segundo"
    (is (nil? (fnc-ge '(1 2))))
    (is (nil? (fnc-ge '(2 558))))  
  )  
  
  (testing "Devuelve error si la lista enviada tiene una cantidad de elementos distinta de 2."
    (is (error? (fnc-ge '())))
    (is (error? (fnc-ge '(1 2 3))))  
    (is (error? (fnc-ge '(1 2 3 4 5))))
  )

  (testing "Devuelve error si alguno de los elementos no es un numero."
    (is (error? (fnc-ge '(A 1))))
    (is (error? (fnc-ge '(2 A))))  
  )
  
  (testing "Devuelve 't' si el primer numero es mayor o igual que el segundo."
    (is (= (fnc-ge '(2 1)) 't))
    (is (= (fnc-ge '(2 2)) 't))
    (is (= (fnc-ge '(201 200)) 't))  
  )
)

(deftest fnc-read-test
  (testing "Da error cuando se le mandan argumentos."
    (is (error? (fnc-read '(1))))
    (is (error? (fnc-read '(1 2)))) 
  )
      
  (testing "Casos que da nil."
    (is (= (with-in-str "()" (fnc-read ())) nil))
    (is (= (with-in-str "nil" (fnc-read ())) nil))  
  )

  (testing "Lee correctamente."
    (is (= (with-in-str "2" (fnc-read ())) '2))
    (is (= (with-in-str "a" (fnc-read ())) 'a))
    (is (= (with-in-str "\"hola\"" (fnc-read ())) "hola"))
    (is (= (with-in-str "(hola mundo)" (fnc-read ())) '(hola mundo)))
  )
)

(deftest evaluar-setq-test
  (testing "Casos de error."
    (is (error? (first (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)))))
    (is (error? (first (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)))))
    (is (error? (first (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))) 
    (is (error? (first (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)))))
    (is (error? (first (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))) 
    (is (error? (first (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3)))))
  )  
  
  (testing "Casos felices"
    (is (= (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 4 m 7))))
    (is (= (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 7))))
    (is (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(2 (nil nil t t + add w 5 x 2))))  
    (is (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(5 (nil nil t t + add w 5 x 5))))
    (is (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(8 (nil nil t t + add w 5 x 7 m 8))))
    (is (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(14 (nil nil t t + add w 5 x 7 m 14))))
    (is (= (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(9 (nil nil t t + add w 5 x 7 y 8 z 9))))
    (is (= (evaluar-setq '(setq X 1) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(1 (nil nil t t + add w 5 x 1))))
    (is (= (evaluar-setq '(SETQ x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(5 (nil nil t t + add w 5 x 5))))
  )
)

(deftest evaluar-if-test
  (testing "El if solo tiene condición."
    (is (= (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))  
    (is (= (evaluar-if '(IF x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))      
  )  

  (testing "El if tiene condición y lo que sucede si da true."
    (is (= (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(IF nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
  )

  (testing "Casos donde el if tiene condición, acción en caso de true y acción en caso de false."
    (is (= (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(3 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (gt gt nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 a 6) '(x 5 y 11 z "hola")) '(6 (gt gt nil nil t t v 1 w 3 a 6))))
    (is (= (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8))))
    (is (= (evaluar-if '(IF nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))  
  )

  (testing "Casos que dan error."
    (is (error? (first (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
    (is (error? (first (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
    (is (error? (first (evaluar-if '(4 5) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
    (is (error? (first (evaluar-if 4 '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
    (is (error? (first (evaluar-if '(if) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  )

  (testing "Varias acciones en el caso de que la condición sea false."
    (is (= (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil a (setq m 8) (setq k 7)) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(7 (nil nil t t v 1 w 3 x 6 m 8 k 7))))  
    (is (= (evaluar-if '(IF nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
      
  ) 
)

(deftest evaluar-de-test
  (testing "Casos de error."
    (is (error? (first (evaluar-de '(de) '(x 1)))))
    (is (error? (first (evaluar-de '(de f) '(x 1)))))
    (is (error? (first (evaluar-de '(f f) '(x 1)))))
    (is (error? (first (evaluar-de '(de f 2) '(x 1)))))
    (is (error? (first (evaluar-de '(de f 2 3) '(x 1)))))
    (is (error? (first (evaluar-de '(de (f)) '(x 1)))))
    (is (error? (first (evaluar-de '(de 2 x) '(x 1)))))
    (is (error? (first (evaluar-de '(de 2 (x)) '(x 1)))))
    (is (error? (first (evaluar-de '(de nil (x) 2) '(x 1)))))
    (is (error? (first (evaluar-de '((x)) '(x 1)))))
  )

  (testing "Defino la funcion correctamente."
    (is (= (evaluar-de '(de f (x)) '(x 1)) '(f (x 1 f (lambda (x))))))
    (is (= (evaluar-de '(de f (x) 2) '(x 1)) '(f (x 1 f (lambda (x) 2)))))  
    (is (= (evaluar-de '(de f (x) (+ x 1)) '(x 1)) '(f (x 1 f (lambda (x) (+ x 1))))))
    (is (= (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1)) '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y)))))
    (is (= (evaluar-de '(DE FUNCION (x) (+ x 1)) '(x 1)) '(FUNCION (x 1 funcion (lambda (x) (+ x 1))))))
  )
)

(deftest evaluar-or-test
  (testing "Or evalua correctamente."
    (is (= (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(t (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(5 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(8 (nil nil t t w 5 x 4 b 8)))) 
    (is (= (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(t (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(OR nil nil (setq b 9) t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(9 (nil nil t t w 5 x 4 b 9))))
    (is (= (evaluar-or '(OR nil nil (setq b 9) (setq a 5) (setq y 6)) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(9 (nil nil t t w 5 x 4 b 9))))
  )

  (testing "Casos de error."
    (is (= (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '((*error* unbound-symbol r) (nil nil t t w 5 x 4))))  
    (is (error? (first (evaluar-or '(r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)))))
    (is (error? (first (evaluar-or 'r '(nil nil t t w 5 x 4) '(x 1 y nil z 3)))))
  )

)