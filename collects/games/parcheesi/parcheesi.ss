
(module parcheesi mzscheme
  (require (lib "unit.ss"))
  
  (provide game-unit)
  (define game-unit
    (unit (import)
          (export)
          (dynamic-require '(lib "admin-gui.ss" "games" "parcheesi") #f))))
    