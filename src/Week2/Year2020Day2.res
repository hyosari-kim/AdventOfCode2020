// --- Day 2: Password Validation ---

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

//함수 하나가 너무 많은 기능을 가지고 있지 않기. -> 세부 함수로 나눠볼것.

// List<Option<('a, 'b)>>
// -> list{Some, Some, Some, None}
// Option<List<('a, 'b)>> : 하나라도 None 일 경우 처리를 안하겠다.
// keepMap 일 경우 None element 의 경우는 pass. some 일때만 처리하겠다.

//error 처리에 대한 관점.

//refacting logic
/**
  전처리 - first trial
    input type : string
    output type : list<Option<{policy: Option<(string, string)>, pw: string}>>

    1. 라인별 split string -> array<string>
    2. list 로 변환 array<string> -> list<string>
    3. policy와 pw 분리 list<string> -> list<Option<(policy: string, pw: string)>>
      3-1. 결과 policy 와 pw 로 분리된 케이스는 Some, 아니면 None 
    4. policy 안에서 number, letter 로 분리 -> list<Option<{policy: Option<{policy:Option<(string, string)>, pw: string}>

  second trial
    1. input type : string
    2. output type: {
      min: int,
      max: int,
      letter: string,
      pw: string
    }

    *****

      질문. min, max dummy keep 하는 부분 더 간단히 할 수 있을까?
      option 인 것은 아예 취급 안하고 keepMap 을 써서 해결하고 싶은데 타입에러가 남.

    ********8

*/

type row = {
  min: int,
  max: int,
  letter: string,
  pw: string,
}

type db = list<row>

//전처리
let db =
  input
  ->Js.String2.split("\n")
  ->Belt.Array.map(p => p->Js.String2.splitByRe(%re("/[- :]/g")))
  ->Belt.Array.keepMap(p =>
    switch p {
    | [Some(min), Some(max), Some(letter), Some(_), Some(pw)] =>
      switch (min->Belt.Int.fromString, max->Belt.Int.fromString) {
      | (Some(min), Some(max)) =>
        Some({
          min: min,
          max: max,
          letter: letter,
          pw: pw->Js.String2.trim,
        })
      | _ => None
      }
    | _ => None
    }
  )
  ->Belt.List.fromArray

//part 1
db
->Belt.List.map(({min, max, letter, pw}) => {
  let countLetter = (a, b) => b == letter ? a + 1 : a
  let cnt = pw->Js.String2.split("")->Belt.Array.reduce(0, countLetter)

  (min, max, cnt)
})
->Belt.List.keep(r => {
  let (min, max, cnt) = r

  min <= cnt && max >= cnt
})
->Belt.List.length
->Js.log

// part2
/**
 * policy 의 숫자가 position 을 나타낼때 valid password count.
 * exactly one of these positions.
 */

/**
 * 1. password 에서 position get 해서 있는 경우 keep
 */

db
->Belt.List.map(({min, max, letter, pw}) => {
  (pw->Js.String2.get(min - 1), pw->Js.String2.get(max - 1), letter)
})
->Belt.List.keep(r => {
  let (f, l, letter) = r

  (f == letter || l == letter) && !(f == letter && l == letter)
})
->Belt.List.length
->Js.log
