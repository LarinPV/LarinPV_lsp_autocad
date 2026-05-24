; =======================================
; кнопка (^C^C_DelFrozenLayers)
; Принудительное удаление замороженных слоев вместе с объектами (кроме заблокированных)
; =======================================
; Удачной работы!
; LarinPV, 2026г.

(defun c:DelFrozenLayers ( / doc layers lay curlay layName ss i ent obj)
  (vl-load-com)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (setq curlay (getvar "CLAYER"))

  (princ "\nЗапуск принудительной очистки замороженных слоев...")
  (vla-StartUndoMark doc) ; Начало точки отмены (откат одной кнопкой)

  ;; Перебираем коллекцию слоев чертежа
  (vlax-for lay layers
    (setq layName (vla-get-Name lay))
    
    ;; Условия: заморожен, НЕ заблокирован, НЕ текущий и НЕ "0"
    (if (and
          (= :vlax-true (vla-get-Freeze lay))
          (= :vlax-false (vla-get-Lock lay))
          (/= layName curlay)
          (/= layName "0")
        )
      (progn
        ;; 1. Очищаем объекты на этом слое, если они есть
        ;; Используем быстрый выбор по имени слоя (DXF код 8)
        (if (setq ss (ssget "_X" (list (cons 8 layName))))
          (progn
            (setq i 0)
            (repeat (sslength ss)
              (setq ent (ssname ss i))
              (setq obj (vlax-ename->vla-object ent))
              
              ;; Стираем объект напрямую из базы данных чертежа
              (vl-catch-all-apply 'vla-delete (list obj))
              (setq i (1+ i))
            )
            (princ (strcat "\nНа слое \"" layName "\" уничтожено объектов: " (itoa i)))
          )
        )

        ;; 2. Размораживаем слой перед удалением (требование ядра AutoCAD)
        (vla-put-Freeze lay :vlax-false)
        (vla-put-LayerOn lay :vlax-true)

        ;; 3. Удаляем сам слой
        (if (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-delete (list lay))))
          (princ (strcat "\n[Удален] Слой: " layName))
          (princ (strcat "\n[Ошибка] Не удалось удалить структуру слоя: " layName))
        )
      )
    )
  )

  (vla-EndUndoMark doc) ; Конец точки отмены
  (princ "\nОчистка завершена.")
  (princ)
)