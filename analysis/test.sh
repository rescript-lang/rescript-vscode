function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

echo "cat -A test.sh"
cat -A tests/src/expected/Auto.res.txt
echo "done---------"

# node ./checkErrors.js

for file in tests/src/*.{res,resi}; do
  ./rescript-editor-analysis.exe test $file &> $(exp $file)
  # CI
  # if [ "$RUNNER_OS" == "Windows" ]; then
  #   dos2unix $(exp $file)
  # fi
done

echo "cat -A test.sh second time"
cat -A tests/src/expected/Auto.res.txt
echo "done 2---------"

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests/src/expected)
echo "cat -A test.sh third time (after ls-files)"
cat -A tests/src/expected/Auto.res.txt
echo "done 3---------"
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  # node ./checkErrors.js
  git --no-pager diff --word-diff-regex=. tests/src/expected/Auto.res.txt
  echo "cat -A test.sh last time (after git diff)"
  cat -A tests/src/expected/Auto.res.txt
  echo "done 4last---------"
  exit 1
fi
