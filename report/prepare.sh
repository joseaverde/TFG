if [ -d layout ]; then
   rm -r layout
fi

mkdir -p layout
cp ~/uc3m-thesis-ieee-typst/*.typ layout/
cp -r ~/uc3m-thesis-ieee-typst/img layout/

