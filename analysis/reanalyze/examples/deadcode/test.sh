output="expected/deadcode.txt"
dune exec -- reanalyze -config -debug -ci -exclude-paths src/noalloc,src/exception -live-names globallyLive1 -live-names globallyLive2,globallyLive3 &> $output
# CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- $output
fi

output="expected/exception.txt"
dune exec reanalyze -- -exception -ci -suppress src -unsuppress src/exception &> $output
# CI. We use LF, and the CI OCaml fork prints CRLF. Convert.
if [ "$RUNNER_OS" == "Windows" ]; then
  perl -pi -e 's/\r\n/\n/g' -- $output
fi


warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified expected)
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests/! Did you break a test?\n${diff}\n${reset}"
  git --no-pager diff expected
  exit 1
fi
