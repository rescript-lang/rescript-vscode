for file in tests/src/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ./rescript-editor-analysis.exe test $file &> $output
  # CI
  if [ "$RUNNER_OS" == "Windows" ]; then
    echo "sedding..."
    sed -i "" $output
  fi
done

echo "cat auto"
cat -A tests/src/expected/Auto.res.txt
echo "---"
echo "sed with regex"
sed -i "s/\r\n/\n/g" tests/src/expected/Auto.res.txt
cat -A tests/src/expected/Auto.res.txt
echo "---"
echo "sed with empty regex"
sed -i "" tests/src/expected/Auto.res.txt
cat -A tests/src/expected/Auto.res.txt
echo "---"
echo "perl======"
perl -pi -e 's/\r\n/\n/g' -- tests/src/expected/Auto.res.txt
cat -A tests/src/expected/Auto.res.txt
echo "---"
echo "zip unzip"
zip -ll tests.zip tests/src/expected/*
unzip -ll tests.zip
cat -A tests/src/expected/Auto.res.txt
echo "---"

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
