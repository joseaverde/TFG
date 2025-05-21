for profile in profiles/*; do
   echo -e "\033[1mProfile $profile\033[m"
   cat alire.toml.in $profile > alire.toml
   alr build --release || exit -1
   # alr build --validation || exit -1
done
