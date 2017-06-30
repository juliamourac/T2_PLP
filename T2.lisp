;Função que concatena duas listas
(defun concatena (lista1 lista2)
(cond ((null lista1) lista2)
(t (cons (car lista1) (concatena (cdr lista1) lista2)))))

;função que conta os átomos das listas, inclusiva das sublistas
(defun conta-atomos (lista)
(cond ((null lista) 0)
((atom lista) 1)
(t (+ (conta-atomos (car lista))
(conta-atomos (cdr lista))))))

;retira as ocorrências do elemento e de lista
(defun tira_elem (lista e)
(do ((l-aux lista (cdr l-aux)) (res ( )))
((null l-aux) res)
(if (not (equal (car l-aux) e))
(setq res (append res (list (car l-aux)))))))

;Desparentizar elementos da lista
(defun desparentize(lista)
    (cond
      ((null lista) NIL)
      ((atom(car lista)) (cons (car lista) (desparentize (cdr lista))))
      (t (append (desparentize (car lista)) (desparentize(cdr lista))))
    )
)
