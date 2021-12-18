#!/bin/bash
name=workgroups2
version=1.2.1
pkg=$name-$version
mkdir $pkg
cp src/*.el $pkg
cat << EOF > $pkg/$name-pkg.el
(define-package "$name" "$version"
                "whatever")
EOF
if [[ $(uname -s) == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg
else
   tar cvf $pkg.tar $pkg
fi
rm -rf $pkg
