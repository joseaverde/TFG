cd build
python3 $IDF_PATH/components/esptool_py/esptool/esptool.py write_flash @flash_project_args  || exit -1
python3 -m esp_idf_monitor detector.elf
