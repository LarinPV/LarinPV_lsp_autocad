; =======================================
; кнопка (^C^C_TZG)
; Перенос отметок без префиксов "з." или "г." с возможностью удаления исходного текста
; =======================================
; Удачной работы!
; LarinPV, 2025г.

(defun c:TZG (/ *error* ent1 ent2 text1 entData1 entData2 new_text typ1 typ2 del_choice loop)
  (vl-load-com)
  
  ;; Локальный обработчик ошибок для чистого выхода по Esc
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*"))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ "\nКоманда TZG завершена.")
    (princ)
  )
  
  (princ "\nКоманда TZG: Замена текста с префиксом 'з.' или 'г.'")
  
  ;; ---------- Запрос на удаление исходного текста ----------
  (initget "Да Нет Yes No")
  (setq del_choice (getkword "\nУдалять исходный текст после переноса? [Да/Нет] <Нет>: "))
  (if (not del_choice) (setq del_choice "Нет"))

  (princ "\nДля выхода из команды нажмите ENTER или ESC на любом шаге.")
  
  (setq loop T)
  (while loop
    ;; 1. Выбор первого текста
    (setq ent1 (car (entsel "\nВыберите первый текст с префиксом 'з.' или 'г.': ")))
    
    (if (not ent1)
      (setq loop nil) ;; Мягкий выход, если ничего не выбрано (нажат Enter/Пробел)
      (progn
        (setq entData1 (entget ent1))
        (setq typ1 (cdr (assoc 0 entData1)))
        
        ;; Проверяем, что выбран именно текст
        (if (or (= typ1 "TEXT") (= typ1 "MTEXT"))
          (progn
            (setq text1 (cdr (assoc 1 entData1)))
            
            ;; Проверяем префикс (длина строки должна быть >= 2 символов)
            (if (and (>= (strlen text1) 2)
                     (or (wcmatch (strcase (substr text1 1 2)) "З.*")
                         (wcmatch (strcase (substr text1 1 2)) "Г.*")))
              
              (progn
                ;; Получаем чистый текст без префикса
                (setq new_text (substr text1 3))
                
                ;; 2. Выбор второго текста для замены
                (setq ent2 (car (entsel (strcat "\nТекст скопирован [" new_text "]. Выберите текст для замены: "))))
                
                (if ent2
                  (progn
                    (setq entData2 (entget ent2))
                    (setq typ2 (cdr (assoc 0 entData2)))
                    
                    (if (or (= typ2 "TEXT") (= typ2 "MTEXT"))
                      (progn
                        ;; Обновляем содержимое второго текста
                        (entmod (subst (cons 1 new_text) (assoc 1 entData2) entData2))
                        (entupd ent2) ;; Принудительно регенерируем измененный объект
                        
                        ;; Если пользователь выбрал удаление — удаляем исходный текст
                        (if (or (= del_choice "Да") (= del_choice "Yes"))
                          (progn
                            (entdel ent1)
                            (princ "\nТекст изменен, исходный объект удален.")
                          )
                          (princ "\nТекст успешно изменен.")
                        )
                      )
                      (princ "\nОшибка: Второй объект не является текстом!")
                    )
                  )
                  (princ "\nЗамена отменена.")
                )
              )
              (princ "\nОшибка: Текст не содержит префикса 'з.' или 'г.'!")
            )
          )
          (princ "\nОшибка: Выбранный объект не является текстом!")
        )
      )
    )
  )
  (princ "\nРабота TZG завершена.")
  (princ)
)