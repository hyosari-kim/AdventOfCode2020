open Belt

let input = Node.Fs.readFileAsUtf8Sync("../input/Week5/Year2020Day9.txt")

// --- Day 9 XMAS encrption ------
/**
 N개의 숫자 다음의 수 a 는 이전 N개의 숫자 중 2개의 수의 합으로 나타내야 한다.
 단, 2개는 서로 같지 않다.
**/

let map2 = (a: option<'a>, b: option<'b>, f: ('a, 'b) => 'c) => {
  a->Option.flatMap(a' => b->Option.map(b' => f(a', b')))
}

let traverse = (l: List.t<option<'a>>) => {
  let acc = Some(list{})
  List.reduce(l, acc)((acc, a) => map2(a, acc)((a', acc') => List.add(acc', a')))->Option.map(l =>
    l->List.reverse
  )
}

// --- Parsing --- //
let numbers =
  input
  ->Js.String2.split("\n")
  ->Array.map(n => Float.fromString(n))
  ->List.fromArray
  ->traverse
  ->Option.getWithDefault(list{})

// ----- Part 1. 위의 원칙을 따르지 않는 수를 찾아내라 ------
// TODO: leftFold로 빠꿔볼 것

type xmas = Failed(float) | Succeed
let xMas = (pre: List.t<float>, i: float) => {
  let isSum = (result, a, b) => a +. b == result ? true : false

  let rec loop = (l: List.t<float>) => {
    switch l {
    | list{} => Failed(i)
    | list{head, ...tail} =>
      switch tail->List.keep(t => isSum(i, head, t))->List.size {
      | 0 => loop(tail)
      | _ => Succeed
      }
    }
  }

  loop(pre)
}

let rec findWeakness = (data: List.t<float>, preambleLength) => {
  let preamble = data->List.take(preambleLength)
  let after = data->List.get(preambleLength)

  map2(preamble, after)((p, a) => xMas(p, a))->Option.flatMap(result => {
    switch result {
    | Succeed =>
      switch data {
      | list{} => None
      | list{_, ...tail} => findWeakness(tail, preambleLength)
      }
    | Failed(weakness) => Some(weakness)
    }
  })
}

//---- Part 1 실행 ----/
let preambleLength = 25
let result = findWeakness(numbers, preambleLength)

switch result {
| Some(weakness) => Js.log(weakness)
| None => Js.log("Something wrong")
}

// ----- Part 2.  모두 더했을 때 Part 1에서 찾은 수가 나오는 시퀀스를 찾아라 ------

let seqSum = (seq: list<float>, acc: float, a: float) => (seq->List.concat(list{a}), a +. acc)

let dropFirst = (l: list<float>, acc: float) => {
  let acc' = List.head(l)->Option.map(h => acc -. h)->Option.getWithDefault(0.)
  let l' = List.drop(l, 1)->Option.getWithDefault(list{})

  (l', acc')
}

type compare = Low | Equal | High
let compareWeakness = (acc, weakness) =>
  switch acc < weakness {
  | true => Low
  | false =>
    switch acc == weakness {
    | true => Equal
    | false => High
    }
  }

// List<int> : sequence
// numbers.leftFold -> (sum, List<ints>)
let findSequence = (data: List.t<float>, weakness: float) => {
  let rec loop = (l: List.t<float>, seq: List.t<float>, acc: float) => {
    switch compareWeakness(acc, weakness) {
    | Low =>
      switch l {
      | list{} => None
      | list{head, ...tail} =>
        let (seq', acc') = seqSum(seq, acc, head)
        loop(tail, seq', acc')
      }
    | Equal => Some(seq)
    | High => {
        let (seq', acc') = dropFirst(seq, acc)
        loop(l, seq', acc')
      }
    }
  }

  loop(data, list{}, 0.)
}

let cmp = (f, seq) =>
  seq->List.reduce(None, (acc, i) =>
    switch acc {
    | None => i->Some
    | Some(acc) => f(acc, i)->Some
    }
  )
let min = cmp((acc, i) => acc > i ? i : acc)
let max = cmp((acc, i) => acc < i ? i : acc)

//---- Part2 실행 ---- //
let preambleLength = 25
let result =
  findWeakness(numbers, preambleLength)
  ->Option.flatMap(weakness => findSequence(numbers, weakness))
  ->Option.flatMap(seq => map2(min(seq), max(seq), (a, b) => a +. b))

switch result {
| Some(r') => Js.log(r')
| None => Js.log("Something wrong")
}
