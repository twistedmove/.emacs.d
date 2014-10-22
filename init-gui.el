(when (display-graphic-p)
  ;; Start (or restart) the server
  (require 'server)
  (server-force-delete)
  (server-start))

