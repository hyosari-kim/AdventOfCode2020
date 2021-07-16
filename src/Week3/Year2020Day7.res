let input = Node.Fs.readFileAsUtf8Sync("input/Week3/Year2020Day7.txt")

//DAG (Directed Acyclic Graph)

// parse
// string -> {"key": string, "bags": option<array<(string, int)>>>}

let parse = (row: string) => {
  let key = row->Js.String2.match_(%re("/^\w+\s+\w+ bags/"))
  let bags = row->Js.String2.match_(%re("/\d+\s+\w+\s+\w+/g"))

  switch (key, bags) {
  | (Some(key), bags) =>
    Some({
      "key": key->Belt.Array.getExn(0)->Js.String2.replace(" bags", ""),
      "bags": bags->Belt.Option.map(arr =>
        arr
        ->Belt.Array.keepMap(bag => bag->Js.String2.match_(%re("/^(\d+) (\w+\s+\w+)/")))
        ->Belt.Array.map(bag => bag->Belt.Array.sliceToEnd(1))
        ->Belt.Array.keepMap(bag =>
          switch bag {
          | [num, name] =>
            switch num->Belt.Int.fromString {
            | Some(n) => Some((n, name))
            | _ => None
            }
          | _ => None
          }
        )
      ),
    })
  | _ => None
  }
}

// part 1
// Search

let rules = input->Js.String2.split("\n")->Belt.Array.keepMap(parse)
let keyword = "shiny gold"

let rec search = (rule, keyword) => {
  switch rule["bags"] {
  | Some(bags) =>
    bags->Belt.Array.reduce(0, (acc, bag) => {
      let (_, name) = bag

      acc + (
        name == keyword
          ? 1
          : search(rules->Belt.Array.keep(r => r["key"] == name)->Belt.Array.getExn(0), keyword)
      )
    })
  | None => 0
  }
}

rules
->Belt.Array.map(r =>
  switch r["key"] == keyword {
  | true => 1
  | false => search(r, keyword)
  }
)
->Belt.Array.keep(a => a > 0)
->Belt.Array.length
->(a => a - 1)
->Js.log

// part 2
// Search with sum

let rec search2 = rule => {
  switch rule["bags"] {
  | Some(bags) =>
    bags->Belt.Array.reduce(0, (acc, bag) => {
      let (cnt, name) = bag

      acc + cnt + cnt * search2(rules->Belt.Array.keep(r => r["key"] == name)->Belt.Array.getExn(0))
    })
  | None => 0
  }
}

let sg = rules->Belt.Array.keep(r => r["key"] === keyword)->Belt.Array.getExn(0)

sg->search2->Js.log
