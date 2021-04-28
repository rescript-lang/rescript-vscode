function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

node ./checkErrors.js

cp tests/src/expected/Auto.res.txt tests/src/Auto.res.txt_old

for file in tests/src/*.{res,resi}; do
  ./rescript-editor-analysis.exe test $file &> $(exp $file)
done

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests/src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  node ./checkErrors.js
  git --no-pager diff --word-diff-regex=. tests/src/expected/Auto.res.txt
  diff -u tests/src/Auto.res.txt_old tests/src/expected/Auto.res.txt
  exit 1
fi
