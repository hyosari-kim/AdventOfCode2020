let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.txt")

//parsing
/**
 */

type height = Cm(int) | In(int)

type eye = Amb | Blu | Grn | Gry | Hzl | Oth | Brn

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

type element = {key: string, value: string}

// string => string
let pick = (str, p) => {
  switch p {
  | "byr" => str->Js.String2.match_(%re("/byr:(\\w+)/"))
  | "iyr" => str->Js.String2.match_(%re("/iyr:(\\w+)/"))
  | "eyr" => str->Js.String2.match_(%re("/eyr:(\\w+)/"))
  | "hgt" => str->Js.String2.match_(%re("/hgt:(\\w+)/"))
  | "hcl" => str->Js.String2.match_(%re("/hcl:(#\\w+)/"))
  | "ecl" => str->Js.String2.match_(%re("/ecl:(\\w+)/"))
  | "pid" => str->Js.String2.match_(%re("/pid:(\\w+)/"))
  | "cid" => str->Js.String2.match_(%re("/cid:(\\w+)/"))
  | _ => None
  }->Belt.Option.map(v => v->Belt.Array.getExn(1))
}

//string => height
let setHeight = str => {
  switch str->Js.String2.sliceToEnd(~from=-2) {
  | "cm" =>
    str->Js.String2.slice(~from=0, ~to_=-2)->Belt.Int.fromString->Belt.Option.map(h => Cm(h))
  | "in" =>
    str->Js.String2.slice(~from=0, ~to_=-2)->Belt.Int.fromString->Belt.Option.map(h => In(h))
  | _ => None
  }
}

//string => eye
let setEye = str => {
  switch str {
  | "amb" => Some(Amb)
  | "blu" => Some(Blu)
  | "brn" => Some(Brn)
  | "gry" => Some(Gry)
  | "grn" => Some(Grn)
  | "hzl" => Some(Hzl)
  | "oth" => Some(Oth)
  | _ => None
  }
}

// array<string> => array<passport>
// string -> passport
//
input
->Js.String2.split("\n\n")
->Belt.Array.keepMap(p => {
  switch (
    p->pick("byr")->Belt.Option.flatMap(v => v->Belt.Int.fromString),
    p->pick("iyr")->Belt.Option.flatMap(v => v->Belt.Int.fromString),
    p->pick("eyr")->Belt.Option.flatMap(v => v->Belt.Int.fromString),
    p->pick("hgt")->Belt.Option.flatMap(v => v->setHeight),
    p->pick("hcl"),
    p->pick("ecl")->Belt.Option.flatMap(v => v->setEye),
    p->pick("pid"),
    p->pick("cid"),
  ) {
  | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid), cid) =>
    Some({
      byr: byr,
      iyr: iyr,
      eyr: eyr,
      hgt: hgt,
      hcl: hcl,
      ecl: ecl,
      pid: pid,
      cid: cid,
    })
  | _ => None
  }
})
->Belt.Array.length
->Js.log

// (Int, String)
// (String, Int)
// { a1: Int, a2: String }
// { v1: Int, v2: String }
