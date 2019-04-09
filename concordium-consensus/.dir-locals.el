;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (eval . (setq intero-stack-executable (expand-file-name "./build" (locate-dominating-file default-directory ".dir-locals.el")))
        )))



