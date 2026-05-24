; =======================================
; кнопка (^C^C_TGLUB)
; Автоматический расчет глубины заложения (Отметка Земли - Отметка Трубы)
; =======================================
; Удачной работы!
; LarinPV, 2025г

(defun c:TGLUB (/ ent1 ent2 ent3 text1 text2 num1 num2 result result_str entdata)
    (princ "\nTGLUB - Расчет глубины. ESC для выхода.")
    
    (while T
        (setq ent1 (car (entsel "\nВыберите ОТМЕТКУ ЗЕМЛИ: ")))
        (if (not ent1) (exit))
        
        (setq text1 (cdr (assoc 1 (entget ent1))))
        (if (not (numberp (setq num1 (atof text1))))
            (progn (princ "Ошибка: не число!") (continue))
        )
        
        (setq ent2 (car (entsel "Выберите ОТМЕТКУ ТРУБЫ: ")))
        (if (not ent2) (exit))
        
        (setq text2 (cdr (assoc 1 (entget ent2))))
        (if (not (numberp (setq num2 (atof text2))))
            (progn (princ "Ошибка: не число!") (continue))
        )
        
        (setq result (- num1 num2))
        
        ; Форматирование с гарантированными 2 знаками после запятой
        (setq result_str (strcat (rtos (fix result) 2 0) "." 
                                (if (< (abs (- result (fix result))) 0.1) "0" "")
                                (rtos (* (- result (fix result)) 100) 2 0)))
        
        (setq ent3 (car (entsel "Выберите ТЕКСТ ГЛУБИНЫ для замены: ")))
        (if (not ent3) (exit))
        
        (setq entdata (entget ent3))
        (setq entdata (subst (cons 1 result_str) (assoc 1 entdata) entdata))
        (entmod entdata)
        (entupd ent3)
        
        (princ (strcat "\n? Глубина: " result_str " (" (rtos num1 2 2) " - " (rtos num2 2 2) ")"))
    )
    (princ)
)

; Сообщение о загрузке
(princ "\nКоманда TGLUB загружена. Введите TGLUB для запуска.")
(princ "\nESC в любой момент для завершения работы.")
(princ)