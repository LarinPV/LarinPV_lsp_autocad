; =======================================
; NNText.LSP
; кнопка (^C^C^NNText)
; выбирает текст, в котором есть символы отличные от цифр и точки
; =======================================
; Удачной работы!
; LarinPV, 2026г.

(defun c:NNText ( / ss i result ent text match char)
  ; 1. Пытаемся получить предварительно выделенные текстовые объекты
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  
  ; 2. Если ничего не выделено, выбираем все текстовые объекты в чертеже
  (if (not ss)
    (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))
  )

  ; Если текстовых объектов нет вообще, завершаем работу
  (if (not ss)
    (progn
      (princ "\nВ чертеже отсутствуют текстовые объекты TEXT/MTEXT.")
      (princ)
      (exit)
    )
  )

  (setq i 0)
  (setq result (ssadd)) ; создаем пустой набор для результата

  ; Перебираем все текстовые объекты
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq text (cdr (assoc 1 (entget ent)))) ; извлекаем строку
    (setq match nil)
    
    ; Проверяем символы строки
    (foreach char (vl-string->list text)
      ; Оптимизация: проверяем только если ещё не нашли "лишний" символ
      (if (and (not match)
               (not (member char '(46 48 49 50 51 52 53 54 55 56 57)))) ; 46=".", 48-57="0-9"
        (setq match t)
      )
    )
    
    ; Если нашли символы кроме цифр и точки, добавляем в результат
    (if match
      (ssadd ent result)
    )
    (setq i (1+ i))
  )
  
  ; Вывод результата и подсветка
  (if (> (sslength result) 0)
    (progn
      (sssetfirst nil result) ; делаем набор текущим выделением
      (princ (strcat "\n? Выбрано объектов: " (itoa (sslength result))))
    )
    (princ "\n? Нет объектов с неподобающими символами.")
  )
  (princ)
)