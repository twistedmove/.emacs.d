#!/bin/bash

INSTALL_PATH="${HOME%%/}/.emacs.d/site-lisp"
[ ! -e "$INSTALL_PATH" ] && mkdir "$INSTALL_PATH"
cd "$INSTALL_PATH"

REPO="https://github.com/magnars/multiple-cursors.el.git"
git clone "$REPO" "multiple-cursors" \
    || { sleep 3 && git clone "$REPO" "multiple-cursors" ; }

REPO="https://github.com/zk-phi/phi-search.git"
git clone "$REPO" \
    || { sleep 3 && git clone "$REPO" ; } \
    || { sleep 3 && git clone "$REPO" ; }

REPO="https://github.com/emacs-helm/helm.git"
git clone "$REPO" \
    || { sleep 3 && git clone "$REPO" ; } \
    || { sleep 3 && git clone "$REPO" ; }

REPO="https://github.com/zk-phi/sublimity.git"
git clone "$REPO" \
    || { sleep 3 && git clone "$REPO" ; } \
    || { sleep 3 && git clone "$REPO" ; }

REPO="https://github.com/stsquad/emacs_chrome.git"
git clone "$REPO" \
    || { sleep 3 && git clone "$REPO" ; } \
    || { sleep 3 && git clone "$REPO" ; }

wget "http://www.emacswiki.org/emacs/download/buffer-move.el"
wget "http://www.emacswiki.org/emacs/rect-mark.el"
wget "http://www.emacswiki.org/emacs/download/column-marker.el"

wget -O "matlab-emacs.tar.gz" \
    "http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar" \
    && tar xf "matlab-emacs.tar.gz" && rm "matlab-emacs.tar.gz"

wget -O "auctex.tar.gz" \
    "http://ftp.gnu.org/pub/gnu/auctex/auctex-11.87.tar.gz" \
    && tar xf "auctex.tar.gz" && rm "auctex.tar.gz" \
    && mv "auctex-11.87" "auctex"

wget -O "dejavu-fonts.tar.bz2" \
    "http://sourceforge.net/projects/dejavu/files/dejavu/2.34/dejavu-fonts-ttf-2.34.tar.bz2" \
    && tar xf "dejavu-fonts.tar.bz2" && rm "dejavu-fonts.tar.bz2" \
    && echo "-- REMINDER: DejaVu fonts downloaded, but not installed."
