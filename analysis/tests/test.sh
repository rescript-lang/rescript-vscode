for file in document/src/*.{res,resi}; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ../rescript-editor-analysis.exe test $file &> $output
  # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

for file in document/not_compiled/*.res; do
  output="$(dirname $file)/expected/$(basename $file).txt"
  ../rescript-editor-analysis.exe test $file &> $output
  # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done

# Workspaces tests
for dir in workspaces/*/; do
  testname="$(basename $dir)"
  output="$(dirname $dir)/$testname/expected/$testname.txt"
  dirtest="$(dirname $dir)/$testname"
  ../rescript-editor-analysis.exe test $dirtest &> $output
  # CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
  if [ "$RUNNER_OS" == "Windows" ]; then
    perl -pi -e 's/\r\n/\n/g' -- $output
  fi
done


warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified src/expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  git --no-pager diff src/expected
  exit 1
fi
