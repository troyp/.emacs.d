#!/bin/bash

EMACS_INSTALL_DIR=${EMACS_INSTALL_DIR-/usr/share/emacs/24.3}

if update-alternatives --display etags | grep 'currently points to'| grep 'emacs'
then emacs_etags='t'
else emacs_etags='f'
fi

cd $EMACS_INSTALL_DIR
# gunzip gz'd .el files
find . -name "*.el.gz" | xargs sudo gunzip
# build TAGS file from .el, .c. .h files
if [ emacs_etags = t ]
then sudo find . -type f -iregex '.*[.]\(c\|h\|el\)' | sudo etags - 
else etags -R .
fi

# build TAGS-.emacs file in home dir
cd ~
if [ emacs_etags = t ]
then etags -l lisp -o TAGS-.emacs .emacs
else etags --language-force=Lisp -f TAGS-.emacs .emacs
fi

# build TAGS file for .emacs.d/
cd .emacs.d
if [ emacs_etags = t ]
then find . -type f -iregex '.*[.]\(c\|h\|el\)' | etags -
else etags -R .
fi 

