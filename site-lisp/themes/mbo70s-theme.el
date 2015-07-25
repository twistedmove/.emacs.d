;;; mbo70s-theme.el --- 70s style palette, with similarities to mbo theme
;;
;;; Author: Jason Milkins
;;; Version: 20141122.0000
;; Package-Version: 20141122.642
;;; Url: https://github.com/emacsfodder/tmtheme-to-deftheme
;;; Package-Requires: ((emacs "24.0"))
;;
;;; Commentary:
;;
;;  Re-edit don't fade away...
;;
;;; Code:

(deftheme mbo70s "mbo70s-theme")

(custom-theme-set-variables 'mbo70s)

(custom-theme-set-faces
 'mbo70s
 ;; basic theming.

 '(default ((t (:foreground "#ffffe9" :background "#2c2c2c" ))))
 '(region  ((t (:background "#716C62"))))
 '(cursor  ((t (:background "#ffffec"))))

 ;; Temporary defaults
 '(linum                               ((t (:foreground "#565652"  :background "#41413f" ))))
 '(fringe                              ((t (                       :background "#41413f" ))))

 '(minibuffer-prompt                   ((t (:foreground "#1278A8"  :background nil       :weight bold                                  ))))
 '(escape-glyph                        ((t (:foreground "orange"   :background nil                                                     ))))
 '(highlight                           ((t (:foreground "orange"   :background nil                                                     ))))
 '(shadow                              ((t (:foreground "#777777"  :background nil                                                     ))))

 '(trailing-whitespace                 ((t (:foreground "#FFFFFF"  :background "#C74000"                                               ))))
 '(link                                ((t (:foreground "#00b7f0"  :background nil       :underline t                                  ))))
 '(link-visited                        ((t (:foreground "#4488cc"                        :underline t :inherit (link)                  ))))
 '(button                              ((t (:foreground "#FFFFFF"  :background "#444444" :underline t :inherit (link)                  ))))
 '(next-error                          ((t (                                             :inherit (region)                             ))))
 '(query-replace                       ((t (                                             :inherit (isearch)                            ))))
 '(header-line                         ((t (:foreground "#EEEEEE"  :background "#444444" :box nil :inherit (mode-line)                 ))))

 '(mode-line-highlight                 ((t (                                             :box nil                                      ))))
 '(mode-line-emphasis                  ((t (                                             :weight bold                                  ))))
 '(mode-line-buffer-id                 ((t (                                             :box nil :weight bold                         ))))

 '(mode-line-inactive                  ((t (:foreground "#ffff87"  :background "#41413f" :box nil :weight light :inherit (mode-line)   ))))
 '(mode-line                           ((t (:foreground "#ffffe9"  :background "#41413f" :box nil ))))

 '(isearch                             ((t (:foreground "#99ccee"  :background "#444444"                                               ))))
 '(isearch-fail                        ((t (                       :background "#ffaaaa"                                               ))))
 '(lazy-highlight                      ((t (                       :background "#77bbdd"                                               ))))
 '(match                               ((t (                       :background "#3388cc"                                               ))))

 '(tooltip                             ((t (:foreground "black"    :background "LightYellow" :inherit (variable-pitch)                 ))))

 '(js3-function-param-face             ((t (:foreground "#BFC3A9"                                                                      ))))
 '(js3-external-variable-face          ((t (:foreground "#F0B090"  :bold t                                                             ))))

 '(secondary-selection                 ((t (                       :background "#342858"                                               ))))
 '(cua-rectangle                       ((t (:foreground "#E0E4CC"  :background "#342858" ))))

 ;; Magit hightlight
 '(magit-item-highlight                ((t (:foreground "white" :background "#1278A8" :inherit nil ))))

 ;; flyspell-mode
 '(flyspell-incorrect                  ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flyspell-duplicate                  ((t (:underline "#009945" :background nil :inherit nil ))))

 ;; flymake-mode
 '(flymake-errline                     ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flymake-warnline                    ((t (:underline "#009945" :background nil :inherit nil ))))

 ;;git-gutter
 '(git-gutter:added                    ((t (:foreground "#609f60" :bold t))))
 '(git-gutter:modified                 ((t (:foreground "#3388cc" :bold t))))
 '(git-gutter:deleted                  ((t (:foreground "#cc3333" :bold t))))

 '(diff-added                          ((t (:background "#305030"))))
 '(diff-removed                        ((t (:background "#903010"))))
 '(diff-file-header                    ((t (:background "#362145"))))
 '(diff-context                        ((t (:foreground "#E0E4CC"))))
 '(diff-changed                        ((t (:foreground "#3388cc"))))
 '(diff-hunk-header                    ((t (:background "#242130"))))


 '(font-lock-comment-face ((t (:foreground "#726c74"  ))))
 '(font-lock-string-face ((t (:foreground "#ac9a74"  ))))
 '(font-lock-builtin-face ((t (:foreground "#ac9a74"  ))))
 '(font-lock-variable-name-face ((t (  ))))
 '(font-lock-keyword-face ((t (:foreground "#ac9a74"  ))))
 '(font-lock-type-face ((t (:foreground "#c0c0b9"  ))))
 '(font-lock-function-name-face ((t (:foreground "#326c77"  ))))
 '(js3-function-param-face ((t (:foreground "#ac9a74"  ))))
 '(js2-function-param ((t (:foreground "#ac9a74"  ))))
 '(font-lock-warning-face ((t (:foreground "#c1c1ba" :background "#1f373c" ))))
 '(diff-removed ((t (:foreground "#393939"  ))))
 '(diff-added ((t (:foreground "#aea798"  ))))
 '(diff-changed ((t (:foreground "#bfbfb1"  ))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#726c74"  ))))

;; Rainbow delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#796c52"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#85775a"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#918262"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#9c8c6c"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#a49678"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#ac9f84"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#b4a990"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#bcb29c"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#c4bba8"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000"))))
) ;; End face definitions

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mbo70s)

;;; mbo70s-theme.el ends here
