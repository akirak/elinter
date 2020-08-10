s/^\(.\+\.el\):\([[:digit:]]\+\):\([[:digit:]]\+\): \?\([Ee]rror\|[Ww]arning\): \(.\+\?\)$/\0\n::\L\4 file=\1,line=\2,col=\3::\5/
s/^\(.\+\.el\): \?\([Ee]rror\|[Ww]arning\): \(.\+\?\)$/\0\n::\L\2 file=\1::\3/
