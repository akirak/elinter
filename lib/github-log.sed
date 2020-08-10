s/^\(.\+\.el\):\([[:digit:]]\+\):\([[:digit:]]\+\): \(error\|warning\): \(.\+\?\)$/\0\n::\4 file=\1,line=\2,col=\3::\5/
s/^\(.\+\.el\): \(error\|warning\): \(.\+\?\)$/\0\n::\2 file=\1::\3/
