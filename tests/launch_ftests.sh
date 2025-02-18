uv sync
source ./.venv/bin/activate
cat chb01.batch chb01.in | ../src/c++/build/Release/tests/seizure_detector_ftests | python3 ftests.py chb01
