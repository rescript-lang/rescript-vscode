for file in tests/src/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ./rescript-editor-analysis.exe test $file &> $output
  # CI
  echo "inside loop now"
  echo $RUNNER_OS
  if [ "$RUNNER_OS" == "Windows" ]; then
    echo "sedding..."
    sed -i "s/\r\n/\n/g" $output
  fi
  cat -A $output
done

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
