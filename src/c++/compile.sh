for preset in $(cmake --list-presets | awk '/^ /{print substr($1,2,length($1)-2)}'); do
   echo -e "\n\033[31;1m[$preset]\033[0m"
   cmake --build --preset $preset
done
