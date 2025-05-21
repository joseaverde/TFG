from ubuntu:25.10

copy ada-detector/ /app/ada-detector/
copy ada/ /app/ada
copy c++/ /app/c++

# Prepare
workdir /app
run apt-get update
run apt-get install -y gcc-14-arm-linux-gnueabihf gnat-14-arm-linux-gnueabihf gprbuild alire git curl
run alr -n index --reset-community
run alr -n toolchain --select
run gprconfig --batch --target=arm-linux-gnueabihf --config=Ada,14.2.0,default,/usr/bin/,GNAT --config=Asm,14.2.0,,/usr/bin/,GCC-ASM --config=Asm2,14.2.0,,/usr/bin/,GCC-ASM --config=Asm_Cpp,14.2.0,,/usr/bin/,GCC-ASM --config=C,14.2.0,,/usr/bin/,GCC

arg CROSS_CONFIG=/app/arm-linux-gnueabihf.cgpr

# Compile ada
workdir /app/ada-detector/main

# run cd ada-detector/main

# copy ../ada /app/ada
# copy ../c++ /app/c++
