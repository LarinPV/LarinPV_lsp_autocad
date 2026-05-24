; =======================================
; кнопка (^C^C_TUKLON)
; Расчет уклона трубопровода по двум отметкам и расстоянию
; =======================================
; Удачной работы!
; LarinPV, 2025г.

(defun c:TUKLON (/ ent1 ent2 ent_dist ent_result text1 text2 num1 num2 dist result ent_type)
    (princ "\nTUKLON - Расчет уклона трубопровода. ESC для завершения")
    
    (while T
        (princ "\n--- ВЫБОР ОБЪЕКТОВ ДЛЯ РАСЧЕТА УКЛОНА ---")
        
        ; Выбор первой отметки трубы
        (setq ent1 (entsel "Выберите ПЕРВУЮ отметку трубы (текст): "))
        (if (not ent1) (exit))
        (setq ent1 (car ent1))
        
        ; Получение и проверка первого числа
        (setq text1 (cdr (assoc 1 (entget ent1))))
        (if (not (numberp (setq num1 (atof text1))))
            (progn (princ "Ошибка: текст не содержит число!") (exit))
        )
        
        ; Выбор второй отметки трубы
        (setq ent2 (entsel "Выберите ВТОРУЮ отметку трубы (текст): "))
        (if (not ent2) (exit))
        (setq ent2 (car ent2))
        
        ; Получение и проверка второго числа
        (setq text2 (cdr (assoc 1 (entget ent2))))
        (if (not (numberp (setq num2 (atof text2))))
            (progn (princ "Ошибка: текст не содержит число!") (exit))
        )
        
        ; Выбор расстояния (размер или текст)
        (setq ent_dist (entsel "Выберите РАССТОЯНИЕ (размер или текст): "))
        (if (not ent_dist) (exit))
        (setq ent_dist (car ent_dist))
        (setq ent_data (entget ent_dist))
        
        ; Определение типа объекта и получение расстояния
        (setq ent_type (cdr (assoc 0 ent_data)))
        
        (cond
            ; Если выбран размер (DIMENSION)
            ((= ent_type "DIMENSION")
                (setq dist (cdr (assoc 42 ent_data)))
                (if (not (numberp dist))
                    (progn (princ "Ошибка: размер не содержит числовое значение!") (exit))
                )
            )
            
            ; Если выбран текст (TEXT или MTEXT)
            ((or (= ent_type "TEXT") (= ent_type "MTEXT"))
                (setq dist (atof (cdr (assoc 1 ent_data))))
                (if (not (numberp dist))
                    (progn (princ "Ошибка: текст не содержит число!") (exit))
                )
            )
            
            ; Если выбран другой тип объекта
            (T
                (princ "Ошибка: выбран не размер и не текст!")
                (exit)
            )
        )
        
        ; Проверка что расстояние не ноль
        (if (<= dist 0)
            (progn (princ "Ошибка: расстояние должно быть больше 0!") (exit))
        )
        
        ; Расчет уклона (всегда положительный)
        (setq result (/ (abs (- num1 num2)) dist))
        
        ; Выбор текста для результата
        (setq ent_result (entsel "Выберите текст для ЗАПИСИ результата уклона: "))
        (if (not ent_result) (exit))
        (setq ent_result (car ent_result))
        
        ; Замена текста на результат в формате x.xxxx
        (setq entdata (entget ent_result))
        (setq entdata (subst (cons 1 (rtos result 2 4)) (assoc 1 entdata) entdata))
        (entmod entdata)
        (entupd ent_result)
        
        ; Вывод подробной информации в командную строку
        (princ "\n--- РЕЗУЛЬТАТ РАСЧЕТА ---")
        (princ (strcat "\nПервая отметка:  " (rtos num1 2 3)))
        (princ (strcat "\nВторая отметка:  " (rtos num2 2 3)))
        (princ (strcat "\nПерепад:         " (rtos (abs (- num1 num2)) 2 3)))
        (princ (strcat "\nРасстояние:      " (rtos dist 2 3)))
        (princ (strcat "\nУклон:           " (rtos result 2 4))) ; 4 знака после запятой
        (princ "\n---")
        (princ (strcat "\n? Уклон записан в выбранный текст в формате x.xxxx!"))
        (princ "\nПродолжайте выбор или ESC для выхода...")
    )
    (princ)
)

; Загрузка функции
(princ "\nКоманда TUKLON загружена. Введите TUKLON для расчета уклона.")
(princ "\nТеперь можно выбирать РАЗМЕРЫ или ТЕКСТ с расстоянием.")
(princ "\nESC в любой момент для завершения работы.")
(princ)