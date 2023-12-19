alias d := dev
alias r := run
alias p := prep

devargs := "--target=solns --warnings --reload src/ --reload lib/"

dev day="":
  ghcid {{devargs}} --test {{ if day == "" { "main" } else { "\"hspec Day" + day + ".test\"" } }}

run:
  nix run .#main

prep day: (refresh_input day)
  mkdir -p src/Day{{day}}

  if [ -e src/Day{{day}}.hs ]; then \
    echo "WARNING: Day{{day}}.hs already exists! Not overriding..." 1>&2; \
  else \
    sed "s/N/{{day}}/" template/Main.hs > src/Day{{day}}.hs; \
  fi
  touch src/Day{{day}}/example1

refresh_input day:
  mkdir -p src/Day{{day}}
  curl -H "Cookie: session=$TOKEN" https://adventofcode.com/2023/day/{{day}}/input > src/Day{{day}}/input

reload:
  direnv reload
