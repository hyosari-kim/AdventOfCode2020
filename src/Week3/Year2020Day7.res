let input = Node.Fs.readFileAsUtf8Sync("input/Week3/Year2020Day7.txt")

//DAG (Directed Acyclic Graph)

// parse
// string -> {"key": string, "bags": option<array<(string, int)>>>}

type rule = {
  key: string,
  bags: array<(int, string)>,
}

let emptyBag = (0, "")

let parse = (row: string) => {
  let key = row->Js.String2.match_(%re("/^\w+\s+\w+ bags/"))
  let bags = row->Js.String2.match_(%re("/(\d+|no)\s+\w+\s+\w+/g"))

  switch (key, bags) {
  | (Some(key), Some(bags)) =>
    Some({
      key: key->Belt.Array.getExn(0)->Js.String2.replace(" bags", ""),
      bags: bags
      ->Belt.Array.keepMap(bag => bag->Js.String2.match_(%re("/^(\d+|no) (\w+\s+\w+)/")))
      ->Belt.Array.map(bag => bag->Belt.Array.sliceToEnd(1))
      ->Belt.Array.keepMap(bag =>
        switch bag {
        | [num, name] =>
          Some(num->Belt.Int.fromString->Belt.Option.mapWithDefault(emptyBag, n => (n, name)))
        | _ => None
        }
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
  rule.bags->Belt.Array.reduce(0, (acc, bag) => {
    let (_, name) = bag

    let rule = rules->Belt.Array.keep(r => r.key === name)->Belt.Array.get(0)

    acc + (name == keyword ? 1 : rule->Belt.Option.mapWithDefault(0, r => r->search(keyword)))
  })
}

rules
->Belt.Array.map(r =>
  switch r.key == keyword {
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
  rule.bags->Belt.Array.reduce(0, (acc, bag) => {
    let (cnt, name) = bag

    let rule = rules->Belt.Array.keep(r => r.key == name)->Belt.Array.get(0)

    acc + cnt + cnt * rule->Belt.Option.mapWithDefault(0, r => r->search2)
  })
}

let sg = rules->Belt.Array.keep(r => r.key === keyword)->Belt.Array.getExn(0)

sg->search2->Js.log
