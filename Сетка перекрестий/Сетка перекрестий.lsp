; =======================================
; кнопка (^C^C_Сетка_перекрестий)
; Начертить сетку перекрестий вокруг объекта с автосозданием блока
; =======================================
; LarinPV, 2025г. Модификация 2026г.

(defun c:Сетка_перекрестий (/ ss i ent obj minp maxp
                   minx miny maxx maxy
                   scl step offset half_len blk_name
                   x y pt lay
                   my-floor my-ceil)

  (vl-load-com)

  ;; Локальные функции для правильного математического округления (в т.ч. отрицательных координат)
  (defun my-floor (val)
    (if (and (< val 0) (/= val (float (fix val))))
      (1- (fix val))
      (fix val)
    )
  )
  (defun my-ceil (val)
    (if (and (> val 0) (/= val (float (fix val))))
      (1+ (fix val))
      (fix val)
    )
  )

  ;; ---------- Запрос масштаба ----------
  (setq scl (getint "\nВведите масштаб (например: 500, 1000, 2000) <500>: "))
  (if (not scl) (setq scl 500))

  ;; ---------- Пропорции ----------
  (setq step (/ scl 10.0))                   ; Шаг: 500 -> 50, 1000 -> 100, 2000 -> 200
  (setq offset (* 1.5 step))                 ; Отступ расширения рамки (1.5 от шага)
  (setq half_len (* 1.5 (/ scl 500.0)))      ; Половина линии креста: 500 -> 1.5 (общая длина 3)
  (setq blk_name (strcat (itoa scl) "_012")) ; Имя блока, например "500_012"

  (setq lay "Геопункты")

  ;; ---------- Создание слоя, если нет ----------
  (if (not (tblsearch "LAYER" lay))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 lay)
                   '(70 . 0)))
  )

  ;; ---------- Автосоздание блока, если нет ----------
  (if (not (tblsearch "BLOCK" blk_name))
    (progn
      (entmake (list '(0 . "BLOCK") (cons 2 blk_name) '(70 . 0) '(10 0.0 0.0 0.0)))
      ;; Горизонтальная линия (цвет 92)
      (entmake (list '(0 . "LINE") '(8 . "0") '(62 . 92)
                     (list 10 (- half_len) 0.0 0.0)
                     (list 11 half_len 0.0 0.0)))
      ;; Вертикальная линия (цвет 92)
      (entmake (list '(0 . "LINE") '(8 . "0") '(62 . 92)
                     (list 10 0.0 (- half_len) 0.0)
                     (list 11 0.0 half_len 0.0)))
      (entmake (list '(0 . "ENDBLK")))
      (princ (strcat "\nСгенерирован новый блок: " blk_name))
    )
  )

  ;; ---------- Выбор объектов ----------
  (if (setq ss (ssget))
    (progn
      ;; первые габариты
      (setq ent (ssname ss 0))
      (setq obj (vlax-ename->vla-object ent))
      (vla-getboundingbox obj 'minp 'maxp)

      (setq minp (vlax-safearray->list minp))
      (setq maxp (vlax-safearray->list maxp))

      (setq minx (car minp)
            miny (cadr minp)
            maxx (car maxp)
            maxy (cadr maxp))

      ;; остальные объекты
      (setq i 1)
      (repeat (1- (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (vla-getboundingbox obj 'minp 'maxp)

        (setq minp (vlax-safearray->list minp))
        (setq maxp (vlax-safearray->list maxp))

        (setq minx (min minx (car minp)))
        (setq miny (min miny (cadr minp)))
        (setq maxx (max maxx (car maxp)))
        (setq maxy (max maxy (cadr maxp)))

        (setq i (1+ i))
      )

      ;; ---------- Добавление отступа ----------
      (setq minx (- minx offset)
            miny (- miny offset)
            maxx (+ maxx offset)
            maxy (+ maxy offset))

      ;; ---------- Округление к шагу сетки ----------
      (setq minx (* step (my-floor (/ minx step))))
      (setq miny (* step (my-floor (/ miny step))))
      (setq maxx (* step (my-ceil (/ maxx step))))
      (setq maxy (* step (my-ceil (/ maxy step))))

      ;; ---------- Вставка блоков ----------
      (setq x minx)
      (while (<= (+ x 0.001) maxx) ;; +0.001 для защиты от погрешности float при последнем шаге
        (setq y miny)
        (while (<= (+ y 0.001) maxy)
          (setq pt (list x y 0.0))
          ;; Вставляем через entmake напрямую в базу чертежа
          (entmake (list '(0 . "INSERT")
                         (cons 2 blk_name)
                         (cons 10 pt)
                         (cons 8 lay)))
          (setq y (+ y step))
        )
        (setq x (+ x step))
      )

      (princ (strcat "\nСетка расставлена. Шаг: " (rtos step 2 1) ", Блок: " blk_name))
    )
    (princ "\nОбъекты не выбраны.")
  )

  (princ)
)