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

;Intersecção entre duas listas
(defun inter (L1 L2)
  (cond ((null L1) NIL)
        ((null L2) NIL)
        ((member (car L1) L2) (cons (car L1) (inter (cdr L1) L2)))
        (t (inter (cdr L1) L2))
  )
)

;Desparentizar elementos da lista
(defun desparentize(lista)
    (cond
      ((null lista) NIL)
      ((atom(car lista)) (cons (car lista) (desparentize (cdr lista))))
      (t (append (desparentize (car lista)) (desparentize(cdr lista))))
    )
)

;Tirar elementos que não são comuns nas duas listas
(defun tira_nao_comuns(l1 l2)
  (desparentize(cons (inter l1 l2) (inter l2 l1)))
)
