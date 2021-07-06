// --- Day 4: Passport Processing ---
// part1
// Count valid passports.
// cid(Country ID) is optional and the others are required.
let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.txt")

// 참고 code
// https://github.com/JsonKim/aoc-2020/tree/main/src/aoc4
//passport type 참고
// https://github.com/JsonKim/aoc-2020/blob/main/src/aoc4/aoc4part2.res#L73-L84
/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/

type height = Cm(int) | In(int)

type eye = Amb | Blu | Grn | Gry | Hzl | Oth | Brn | None

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: height,
  hcl: string,
  ecl: eye,
  pid: string,
  cid: option<string>,
}

let initPassport = {
  byr: 0,
  iyr: 0,
  eyr: 0,
  hgt: Cm(0),
  hcl: "",
  ecl: None,
  pid: "",
  cid: None,
}

let parser = p =>
  p->Belt.Array.reduce(initPassport, (acc, el) => {
    switch el {
    | Some(el) =>
      switch el->Js.String2.split(":") {
      | ["byr", v] => {...acc, byr: v->Belt.Int.fromString->Belt.Option.getWithDefault(0)}
      | ["iyr", v] => {...acc, iyr: v->Belt.Int.fromString->Belt.Option.getWithDefault(0)}
      | ["eyr", v] => {...acc, eyr: v->Belt.Int.fromString->Belt.Option.getWithDefault(0)}
      | ["hgt", v] => {
          ...acc,
          hgt: v->(
            v =>
              switch v->Js.String2.sliceToEnd(~from=-2) {
              | "cm" =>
                v
                ->Js.String2.slice(~from=0, ~to_=-2)
                ->Belt.Int.fromString
                ->Belt.Option.map(h => Cm(h))
                ->Belt.Option.getWithDefault(Cm(0))
              | "in" =>
                v
                ->Js.String2.slice(~from=0, ~to_=-2)
                ->Belt.Int.fromString
                ->Belt.Option.map(h => In(h))
                ->Belt.Option.getWithDefault(Cm(0))
              | _ => Cm(0)
              }
          ),
        }
      | ["hcl", v] => {...acc, hcl: v}
      | ["ecl", v] => {
          ...acc,
          ecl: v->(
            v =>
              switch v {
              | "amb" => Amb
              | "blu" => Blu
              | "brn" => Brn
              | "gry" => Gry
              | "grn" => Grn
              | "hzl" => Hzl
              | "oth" => Oth
              | _ => None
              }
          ),
        }
      | ["pid", v] => {...acc, pid: v}
      | ["cid", v] => {...acc, cid: Some(v)}
      | _ => acc
      }
    | _ => acc
    }
  })
/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)

   1. string => array<string>
   2. array<string> => array<array<string>>
   3. array<array<string>> => array<passport>
      3-1. array<string> => passport ?
*/
let parsePassport = (input: string) =>
  input
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(p => p->Js.String2.splitByRe(%re("/[\\s,\\n]+/")))
  ->Belt.Array.map(p =>
    p->Belt.Array.keep(el =>
      switch el {
      | Some(el) => el != ""
      | _ => false
      }
    )
  )
  ->Belt.Array.map(parser)
let passports = input->parsePassport

passports->Js.log

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.

   1. array<passport> => int
*/
let countPassport = passports =>
  passports->Belt.Array.reduce(0, (acc, passport) => {
    let {byr, iyr, eyr, hgt, hcl, ecl, pid} = passport

    let hasRequired =
      byr !== 0 &&
      iyr !== 0 &&
      eyr !== 0 &&
      hgt !== Cm(0) &&
      hcl !== "" &&
      ecl !== None &&
      pid !== ""

    hasRequired ? acc + 1 : acc
  })

passports->countPassport->Js.log

// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
   각 key 에 맞는 validator를 추가하세요.

   1. array<passport> => array<passport>
   2. array<Passport> => int
*/
type validator = {
  byr: int => bool,
  iyr: int => bool,
  eyr: int => bool,
  hgt: height => bool,
  hcl: string => bool,
  ecl: eye => bool,
  pid: string => bool,
}

let checkRange = (num, min, max) => num >= min && num <= max

let validatePassport = (passports: array<passport>) => {
  let validator = {
    byr: by => by->checkRange(1920, 2002),
    iyr: iy => iy->checkRange(2010, 2020),
    eyr: ey => ey->checkRange(2020, 2030),
    hgt: hg => {
      switch hg {
      | Cm(h) => h->checkRange(150, 193)
      | In(h) => h->checkRange(59, 76)
      }
    },
    hcl: hc => {
      let c = hc->Js.String2.get(0)

      let code = hc->Js.String2.sliceToEnd(~from=1)

      c == "#" && Js.Re.test_(%re("/^[0-9a-f]{6}$/"), code)
    },
    ecl: ec => {
      switch ec {
      | None => false
      | _ => true
      }
    },
    pid: pi => {
      pi->Js.String2.length == 9
    },
  }

  passports->Belt.Array.keep(passport => {
    switch passport {
    | {byr, iyr, eyr, hgt, hcl, ecl, pid} => {
        let {
          byr: vbyr,
          iyr: viyr,
          eyr: veyr,
          hgt: vhgt,
          hcl: vhcl,
          ecl: vecl,
          pid: vpid,
        } = validator

        vbyr(byr) && viyr(iyr) && veyr(eyr) && vhgt(hgt) && vhcl(hcl) && vecl(ecl) && vpid(pid)
      }
    }
  })
}

passports->validatePassport->Belt.Array.length->Js.log

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/
