; =======================================
; кнопка (^C^C_TGgaza)
; Расчет проектной/фактической отметки газопровода "г." со случайным допуском
; =======================================
; Удачной работы!
; LarinPV, 2025г.

(defun c:TGgaza (/ depth prefix_text prefix_data text_content numeric_value random_offset new_depth new_text insertion_point new_point text_props)
    (vl-load-com)
    
    ; Запрос глубины у пользователя
    (setq depth (getreal "\nВведите глубину (например 1.91): "))
    
    (if (not depth)
        (progn
            (princ "\nГлубина не введена.")
            (quit)
        )
    )
    
    ; Выбор текста с префиксом "з."
    (setq prefix_text (car (entsel "\nВыберите текст с префиксом 'з.': ")))
    
    (if (not prefix_text)
        (progn
            (princ "\nТекст не выбран.")
            (quit)
        )
    )
    
    ; Получение данных выбранного текста
    (setq prefix_data (entget prefix_text))
    
    ; Проверка, что выбранный объект является текстом
    (if (not (or (= (cdr (assoc 0 prefix_data)) "TEXT")
                 (= (cdr (assoc 0 prefix_data)) "MTEXT")))
        (progn
            (princ "\nВыбранный объект не является текстом.")
            (quit)
        )
    )
    
    ; Получение содержимого текста
    (setq text_content (cdr (assoc 1 prefix_data)))
    
    ; Проверка наличия префикса "з."
    (if (not (wcmatch text_content "з.*"))
        (progn
            (princ "\nВыбранный текст не содержит префикс 'з.'.")
            (quit)
        )
    )
    
    ; Извлечение числового значения из текста
    (setq numeric_value (atof (substr text_content 4)))
    
    ; Вычисление нового значения с учетом глубины и случайного диапазона
    (setq random_offset (* (rem (getvar "CPUTICKS") 100) 0.0005)) ; Генерация случайного числа от 0 до 0.05
    (setq new_depth (- numeric_value (+ depth random_offset)))
    
    ; Форматирование нового значения
    (setq new_text (strcat "г." (rtos new_depth 2 2)))
    
    ; Получение точки вставки исходного текста
    (setq insertion_point (cdr (assoc 10 prefix_data)))
    
    ; Создание новой точки ниже исходной (смещение по Y)
    (setq new_point (list (car insertion_point) 
                         (- (cadr insertion_point) (* (getvar "TEXTSIZE") 1.5)) 
                         (caddr insertion_point)))
    
    ; Подготовка свойств для нового текста
    (setq text_props (list 
        '(0 . "TEXT")
        (cons 1 new_text)
        (cons 10 new_point)
        (cons 40 (getvar "TEXTSIZE"))
        (cons 7 (getvar "TEXTSTYLE"))
        (cons 8 (cdr (assoc 8 prefix_data))) ; Тот же слой
    ))
    
    ; Добавляем цвет только если он есть у исходного текста
    (if (assoc 62 prefix_data)
        (setq text_props (append text_props (list (cons 62 (cdr (assoc 62 prefix_data))))))
    )
    
    ; Создание нового текста
    (entmake text_props)
    
    (princ (strcat "\nСоздан текст: " new_text))
    (princ)
)

; Загрузка функции при запуске
(princ "\nКоманда TGgaza загружена. Введите TGgaza для запуска.")
(princ)