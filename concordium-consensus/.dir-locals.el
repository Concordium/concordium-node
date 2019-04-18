;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (intero-targets "concordium-crypto:lib" "concordium-crypto:test:tests")
  (eval setq intero-stack-executable
        (expand-file-name "./build"
                          (locate-dominating-file default-directory ".dir-locals.el")))))





