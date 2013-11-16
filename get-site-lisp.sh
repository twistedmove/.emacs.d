#!/bin/bash

INSTALL_PATH="${HOME%%/}/.emacs.d/site-lisp"
[ ! -e "$INSTALL_PATH" ] && mkdir "$INSTALL_PATH"
cd "$INSTALL_PATH"

URL="https://github.com/magnars/multiple-cursors.el.git"
git clone "$URL" "multiple-cursors"

git clone "https://github.com/zk-phi/phi-search.git"
git clone "https://github.com/emacs-helm/helm.git"
git clone "https://github.com/zk-phi/sublimity.git"
git clone "https://github.com/stsquad/emacs_chrome.git"

curl -O "http://www.emacswiki.org/emacs/download/buffer-move.el"
curl -O "http://www.emacswiki.org/emacs/rect-mark.el"
curl -O "http://www.emacswiki.org/emacs/download/column-marker.el"

URL="http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar"
NAME="matlab-emacs.tar.gz"
curl -L "$URL" -o "$NAME" && tar xf "$NAME" && rm "$NAME"

NAME="dejavu-fonts.tar.bz2"
VER="2.34"
URL="http://sourceforge.net/projects/dejavu/files/dejavu/$VER/dejavu-fonts-ttf-$VER.tar.bz2/download"
curl -L "$URL" -o "$NAME" \
    && tar xf "$NAME" && rm "$NAME" \
    && mv "dejavu-fonts-ttf-$VER" "dejavu-fonts" \
    && echo "-- REMINDER: DejaVu fonts downloaded, but not installed."

NAME="auctex.tar.gz"
VER="11.87"
URL="http://ftp.gnu.org/pub/gnu/auctex/auctex-$VER.tar.gz"
curl -L "$URL" -o "$NAME" \
    && tar xf "$NAME" && rm "$NAME" \
    && mv "auctex-$VER" "auctex" && cd "auctex" \
    &&  echo "-- REMINDER: AUCTeX must be built and installed before use."
