alias d := dev
alias r := run
alias p := prep

dev:
  ghcid --target=main -T main --warnings --reload src/

run:
  cabal run .#main

prep day: (refresh_input day)
  sed "s/Day N/Day {{day}}/;s/DayN/Day{{day}}/" template/Main.hs > src/Day{{day}}.hs
  touch src/Day{{day}}/example

refresh_input day:
  mkdir -p src/Day{{day}}
  curl -H "Cookie: session=$TOKEN" https://adventofcode.com/2023/day/{{day}}/input > src/Day{{day}}/input

reload:
  direnv reload
