#!/bin/bash

INSTALL_PATH="${HOME%%/}/.emacs.d/site-lisp"
[ ! -e "$INSTALL_PATH" ] && mkdir "$INSTALL_PATH"
pushd "$INSTALL_PATH"

URL="https://github.com/magnars/multiple-cursors.el.git"
git clone "$URL" "multiple-cursors"

URL="https://github.com/knu/phi-search-mc.el.git"
git clone "$URL" "phi-search-mc"

git clone "https://github.com/zk-phi/phi-search.git"
git clone "https://github.com/emacs-helm/helm.git"
git clone "https://github.com/zk-phi/sublimity.git"
git clone "https://github.com/stsquad/emacs_chrome.git"

curl -O "http://www.emacswiki.org/emacs/download/buffer-move.el"
curl -O "http://www.emacswiki.org/emacs/download/rect-mark.el"
curl -O "http://www.emacswiki.org/emacs/download/column-marker.el"
curl -O "http://www.emacswiki.org/emacs/download/lorem-ipsum.el"
curl -O "http://www.emacswiki.org/emacs/download/sr-speedbar.el"
curl -O "http://www.emacswiki.org/emacs/download/dired+.el"
curl -O "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ibuffer.el"
curl -O "https://raw.githubusercontent.com/tovbinm/emacs-24-mac/master/etc/themes/manoj-dark-theme.el"

URL="http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/?view=tar"
NAME="matlab-emacs.tar.gz"
curl -L "$URL" -o "$NAME" && tar xf "$NAME" && rm "$NAME"

NAME="auto-complete.tar.bz2"
VER="1.3.1"
URL="http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2"
curl -L "$URL" -o "$NAME" \
    && tar xf "$NAME" && rm "$NAME" \
    && mv "auto-complete-$VER" "auto-complete" \
    && echo "-- REMINDER: auto-complete may require installation."

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
    && mv "auctex-$VER" "auctex" \
    &&  echo "-- REMINDER: AUCTeX must be built and installed before use."

NAME="cedet.tar.gz"
VER="1.1"
URL="http://downloads.sourceforge.net/project/cedet/cedet/cedet-$VER.tar.gz"
curl -L "$URL" -o "$NAME" \
    && tar xf "$NAME" && rm "$NAME" \
    && mv "cedet-$VER" "cedet" \
    &&  echo "-- REMINDER: CEDET must be built before use."

git clone "https://github.com/auto-complete/auto-complete.git" \
    && cd "$INSTALL_PATH/auto-complete" \
	&& curl -L "http://elpa.gnu.org/packages/cl-lib-0.5.el" -o "cl-lib.el" \
	&& git checkout -b legacy 14203fa \
	&& git submodule init && git submodule update \
    && echo "-- REMINDER: auto-complete downloaded, but not installed."
	cd "$INSTALL_PATH"

popd
