
(lambda (request failure)
  (case request
    [(name) "Games"]
    [(mred-launcher-libraries) (list "games.ss")]
    [(mred-launcher-names) (list "Games")]
    [else (failure)]))
