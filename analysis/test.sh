function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

echo "cat -A test.sh"
cat -A tests/src/expected/Auto.res.txt
echo "done---------"
echo "git diff test.sh"
git diff tests/src/expected/Auto.res.txt
echo "done---------"

for file in tests/src/*.{res,resi}; do
  ./rescript-editor-analysis.exe test $file &> $(exp $file)
done

# # CI
# if [ "$RUNNER_OS" == "Windows" ]; then
#   dos2unix tests/src/expected/*
# fi

echo "cat -A test.sh last"
cat -A tests/src/expected/Auto.res.txt
echo "done cat last---------"
echo "git diff test.sh last"
diff -u tests/src/expected/Auto.res.txt tests/src/expected/Auto.res.2.txt
echo "done git last---------"

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests/src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  git --no-pager diff --word-diff-regex=. tests/src/expected/Auto.res.txt
  exit 1
fi
