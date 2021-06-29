// --- Day 2: Password Validation ---

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.txt")
// part1
/**
 * input line 당 policy 에 부합하는 password 의 갯수 구하시오.
 */

/*
1. policy type, pasword type
    policy type : number-number char (ex. 1-2 a)
    password type: string (ex. abcdea)
2. policy 와 password 를 split(":")
    1. policy 안에서 frequency 와 character split(" ")
        p : { freq: (1,2), letter: "a"}
    2. password 는 trim
        pw: "abcdea"
3. parsing 된 최종 결과 Record 로 생성
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

type db = list<row>

//함수 하나가 너무 많은 기능을 가지고 있지 않기. -> 세부 함수로 나눠볼것.

// List<Option<('a, 'b)>>
// -> list{Some, Some, Some, None}
// Option<List<('a, 'b)>> : 하나라도 None 일 경우 처리를 안하겠다.
// keepMap 일 경우 None element 의 경우는 pass. some 일때만 처리하겠다.

//error 처리에 대한 관점.

let db =
  input
  ->Js.String2.split("\n")
  ->Belt.List.fromArray
  ->Belt.List.keepMap(r =>
    switch r->Js.String2.split(":")->Belt.List.fromArray {
    | list{po, password} => Some((po, password))
    | _ => None
    }
  )
  ->Belt.List.map(r => {
    // 인자가 한개 or 없을때는 다루는 부분이 없다. = 2개 이상부터 다룰수 있음.
    r->Belt.Option.map(r => {
      let (po, password) = r

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
  })

db
->Belt.List.keep(({policy: {p, letter}, pw}) => {
  let count =
    pw->Js.String2.split("")->Belt.Array.reduce(0, (acc, c) => c == letter ? acc + 1 : acc)

  switch p {
  | list{atLeast, atMost, ..._} => atLeast->int_of_string <= count && atMost->int_of_string >= count
  | _ => false
  }
})
->Belt.List.length
->Js.log

let a = "123"->Belt.Int.fromString

/**
질문
1. List -> Tuple casting  하는 방법
 // 딱히 지정된 함수는 없음. 번거로운 작업이 있을 수 밖에 없음. 
2. int_of_string 은 error throw 하는데 더 좋은 방법이 있는지 : string to int type casting 할때
 //Belt.Int.FromString 이 있다.
3. 노란줄 Waning 없애는 방법.
    // 뜨는 이유? Rescript 에서는 함수가 total function 이어야 한다. 인자로 받는 모든 type에 대해서 다루겠다. partial function 들어오는 것에 대해서 일부는 처리하지 않겠다.
4. Record type 상속이 있는지. Doc 에서는 module type 에서 include 가 있던데 그냥 type 에서 쓰는 방법.
    없습니다.
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
->Belt.List.keep(({policy: {p: positions, letter}, pw}) => {
  switch positions {
  | list{firstN, lastN, ..._} =>
    let firstPos = firstN->int_of_string
    let lastPos = lastN->int_of_string

    let isFirstExsists = pw->Js.String2.get(firstPos - 1)->(l => l === letter)
    let isLastExists = pw->Js.String2.get(lastPos - 1)->(l => l === letter)

    //xor
    (isFirstExsists || isLastExists) && !(isFirstExsists && isLastExists)
  | _ => false
  }
})
->Belt.List.length
->Js.log
