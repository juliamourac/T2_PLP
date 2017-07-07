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
)(setq lista1 '(1 2 3 4 1 1 2))

;Conta ocorrência de elemento na lista
(defun conta_ocorrencia(elemento lista)
  (setq contador 0)
  (cond
      ((null lista) 0)
      (t (dolist (e lista)
        (if (equal e elemento) (setq contador (+ contador 1))))
        (setq contador contador))
    )
 )

(defun monta_pares(li)
  (cond
    ((null li) NIL)
    (t (cons (cons (car li) (conta_ocorrencia (car li) li)) (monta_pares(cdr li))))
  )
)

(defun conta_atomos(lis1 lis2)
  (monta_pares(tira_nao_comuns (desparentize lis1) (desparentize lis2)))
)
