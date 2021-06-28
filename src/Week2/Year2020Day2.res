// --- Day 2: Password Validation ---

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.txt")
// part1
/**
 * input line 당 policy 에 부합하는 password 의 갯수 구하시오.
 */

/*
1. password policy type, password type
    policy type : number-number char (ex. 1-2 a)
    password type: string (ex. abcdea)
2. policy 와 password 를 split(":")
    1. policy 안에서 frequency 와 character split(" ")
        p : { freq: (1,2), letter: "a"}
    2. password 는 trim
        pw: "abcdea"
3. parsing 된 최종 결과 
    type Row = {
        p: {freq: (number, number), letter: string},
        pw: string
    }
    type DB = list<Row>
4. count 가 freq 조건을 만족하는 경우만 keep
*/
type policy = {
  p: list<string>,
  letter: string,
}
type row = {
  policy: policy,
  pw: string,
}

let db =
  input
  ->Js.String2.split("\n")
  ->Belt.List.fromArray
  ->Belt.List.map(r => {
    let list{po, password, ..._} = r->Js.String2.split(":")->Belt.List.fromArray

    let policy =
      po
      ->Js.String2.split(" ")
      ->Belt.List.fromArray
      ->(
        p => {
          let list{pString, letter, ..._} = p
          {
            //tuple 만드는 법
            p: pString->Js.String2.split("-")->(ary => ary->Belt.List.fromArray),
            letter: letter,
          }
        }
      )

    {
      policy: policy,
      pw: password->Js.String2.trim,
    }
  })

db
->Belt.List.keep(r => {
  let {policy, pw} = r
  let {p, letter} = policy
  let list{atLeast, ...atMost} = p

  let minNum = atLeast->int_of_string
  let maxNum = atMost->Belt.List.getExn(0)->int_of_string

  let count =
    pw->Js.String2.split("")->Belt.Array.reduce(0, (acc, c) => c == letter ? acc + 1 : acc)

  minNum <= count && maxNum >= count
})
->Belt.List.length
->Js.log

/**
질문
1. List -> Tuple casting  하는 방법
2. int_of_string 은 error throw 하는데 더 좋은 방법이 있는지 : string to int type casting 할때
3. [] warnning 없애기 (노란줄 없애는 방법)
4. Record type 상속이 있는지. Doc 에서는 module type 에서 include 가 있던데 그냥 type 에서 쓰는 방법.

*/

// part2
/**
 * policy 의 숫자가 position 을 나타낼때 valid password count.
 * exactly one of these positions.
 */

/**
 * 1. password 에서 position get 해서 있는 경우 keep
 */

db
->Belt.List.keep(r => {
  let {policy, pw} = r
  let {p, letter} = policy
  let list{firstN, ...lastN} = p

  let firstPos = firstN->int_of_string
  let lastPos = lastN->Belt.List.getExn(0)->int_of_string

  let firstLetter = pw->Js.String2.get(firstPos - 1)
  let lastLetter = pw->Js.String2.get(lastPos - 1)

  (firstLetter == letter || lastLetter == letter) &&
    !(firstLetter == letter && lastLetter == letter)
})
->Belt.List.length
->Js.log
