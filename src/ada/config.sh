for profile in profiles/*; do
   echo -e "\033[1mProfile $profile\033[m"
   cat alire.toml.in $profile > alire.toml
   alr build --release
done
