for profile in profiles/*; do
   uv run conan install . --build=missing -pr=$profile
done

for preset in $(cmake --list-presets | awk '/^ /{print substr($1,2,length($1)-2)}'); do
   cmake --preset $preset
done
