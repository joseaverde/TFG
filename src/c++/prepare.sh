for profile in profiles/*; do
   conan install . --build=missing -pr=$profile || exit -1
done

for preset in $(cmake --list-presets | awk '/^ /{print substr($1,2,length($1)-2)}'); do
   cmake --preset $preset
done
